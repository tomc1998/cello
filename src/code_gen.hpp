#pragma once

#include <iostream>
#include <unordered_map>
#include <memory>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

#include "error.hpp"
#include "ast_util.hpp"
#include "ast_function.hpp"
#include "lexer.hpp"
#include "scope.hpp"
#include "builtin_types.hpp"
#include "obj_file_gen.hpp"
#include "struct_decl.hpp"

namespace cello {

  void code_gen(lexer& l) {
    using namespace llvm;

    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    static LLVMContext llvm_ctx;
    static IRBuilder<> builder(llvm_ctx);
    std::unique_ptr<Module> module = make_unique<Module>("test_mod", llvm_ctx);

    scope global_scope;
    setup_builtin_types(llvm_ctx, global_scope);

    while(l.peek()) {
      if (l.peek() && l.peek()->val == "(") {
        l.next();
        if (l.peek() && l.peek()->val == "fn") {
          const auto function_opt = parse_function(l);
          if (function_opt) {
            const auto ft = function_opt->to_llvm_function_type(global_scope, llvm_ctx);
            if (!ft) { continue; }

            // Add functino decl to scope
            global_scope.symbol_table.insert(std::make_pair(function_opt->name,
                                                            named_value {*function_opt}));

            // If extern function, early return here - no need to create the
            // function definition
            if (function_opt->is_extern()) { continue; }

            const auto llvm_name = StringRef(function_opt->name.begin(), function_opt->name.size());
            const auto f = Function::Create(*ft, Function::ExternalLinkage, llvm_name, module.get());

            // Create scope and add args to scope
            auto function_scope = global_scope.create_subscope();
            for (unsigned ii = 0; ii < function_opt->args.size(); ++ii) {
              const auto arg_name = function_opt->args[ii].name;
              const auto arg_type = function_opt->args[ii].type.code_gen(global_scope);
              if (!arg_type) { continue; }
              const auto arg_value = f->arg_begin() + ii;
              const named_value nv { var { *arg_type, arg_value } };
              function_scope.symbol_table.insert(std::make_pair(arg_name, nv));
            }

            // Codegen exprs
            bool has_errored = false;
            BasicBlock *bb = BasicBlock::Create(llvm_ctx, "entry", f);
            builder.SetInsertPoint(bb);
            for (unsigned ii = 0; ii < function_opt->expressions.size(); ++ii) {
              const auto &e = function_opt->expressions[ii];
              const auto value_opt = e.code_gen(function_scope, builder);
              if (!value_opt) { has_errored = true; continue; }
              if (ii == function_opt->expressions.size() - 1) {
                builder.CreateRet(*value_opt);
              }
            }

            if (!has_errored) {
              f->print(errs());
            } else {
              print_all_errors();
              assert(false && "CRITICAL ERROR: There were errors in codegen.");
            }
          }
        }
      } else if (l.peek() && l.peek()->val == "struct") {
        auto res = parse_struct_type(l);
        if (!res) { continue; }
        type t { *res, 0 };
        global_scope.symbol_table.insert(std::make_pair(res->name, named_value { t }));
      } else {
        report_error(l.get_curr_source_label(),
                     std::string("Unexpected token ") + std::string(l.peek()->val));
        return;
      }
    }

    write_to_obj("test_mod.o", module.get());
  }
};
