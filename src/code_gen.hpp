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

#include "ast_util.hpp"
#include "ast_function.hpp"
#include "lexer.hpp"
#include "scope.hpp"
#include "builtin_types.hpp"

namespace cello {

  using namespace llvm;

  void code_gen(lexer& l) {
    static LLVMContext llvm_ctx;
    static IRBuilder<> builder(llvm_ctx);
    std::unique_ptr<Module> module = make_unique<Module>("test_mod", llvm_ctx);

    scope global_scope;
    setup_builtin_types(llvm_ctx, global_scope);

    while(l.peek()) {
      if (l.peek() && l.peek()->val == "(") {
        l.next();
        l.backup();
        if (l.peek() && l.peek()->val == "fn") {
          const auto function_opt = parse_function(l);
          if (function_opt) {
            std::cout << std::endl;
            std::cout << "FUNCTION " << function_opt->name << std::endl;
            std::cout << "\t~\tNum args: " << function_opt->args.size() << std::endl;
            std::cout << "\t~\tNum exprs: " << function_opt->expressions.size() << std::endl;
            for (const auto &arg : function_opt->args) {
              std::cout << "\t~\t" << arg.type.to_string() << " " << arg.name << std::endl;
            }

            // Code gen for this function
            std::vector<Type*> arg_types;
            for (const auto &a  : function_opt->args) {
              const auto arg_type = a.type.code_gen(global_scope);
              if (!arg_type) {goto continue_outer;}
              arg_types.push_back(*arg_type);
            }
            const auto return_type = function_opt->return_type.code_gen(global_scope);
            if (!return_type) { goto continue_outer; }
            const auto ft = FunctionType::get(*return_type, arg_types, false);
            const auto llvm_name = StringRef(function_opt->name.begin(), function_opt->name.size());
            const auto f = Function::Create(ft, Function::ExternalLinkage, llvm_name, module.get());

            // Create scope and add args to scope
            auto function_scope = global_scope.create_subscope();
            for (int ii = 0; ii < function_opt->args.size(); ++ii) {
              const auto arg_name = function_opt->args[ii].name;
              const auto arg_value = f->arg_begin() + ii;
              const named_value nv { named_value_type::var, arg_value };
              function_scope.symbol_table.insert(std::make_pair(arg_name, nv));
            }

            // Codegen exprs
            bool has_errored = false;
            BasicBlock *bb = BasicBlock::Create(llvm_ctx, "entry", f);
            builder.SetInsertPoint(bb);
            for (int ii = 0; ii < function_opt->expressions.size(); ++ii) {
              const auto &e = function_opt->expressions[ii];
              const auto value_opt = e.code_gen(function_scope, builder);
              if (!value_opt) { has_errored = true; continue; }
              if (ii == function_opt->expressions.size() - 1) {
                builder.CreateRet(*value_opt);
              }
            }

            f->print(errs());
          }
        }
      } else {
        report_error(l.get_curr_source_label(),
                     std::string("Unexpected token ") + std::string(l.peek()->val));
        return;
      }
    continue_outer:;
    }
  }
};
