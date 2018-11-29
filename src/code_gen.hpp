#pragma once

#include <iostream>
#include <unordered_map>
#include <memory>

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "ast_util.hpp"
#include "ast_function.hpp"
#include "lexer.hpp"

namespace cello {

  using namespace llvm;

  void code_gen(lexer& l) {
    static LLVMContext llvm_ctx;
    static IRBuilder<> builder(llvm_ctx);
    std::unique_ptr<Module> module = make_unique<Module>("test_mod", llvm_ctx);

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
          }
        }
      } else {
        report_error(l.get_curr_source_label(),
                     std::string("Unexpected token ") + std::string(l.peek()->val));
        return;
      }
    }
  }
};
