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
          auto function_opt = parse_function(l);
          if (function_opt) {
            const auto f = function_opt->to_llvm_function(global_scope, builder, module.get());
            if (!f) { continue; }

            // Add function decl to scope
            global_scope.symbol_table.insert(std::make_pair(function_opt->name,
                                                            named_value {*function_opt}));
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
