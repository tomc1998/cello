#pragma once

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <nonstd/string_view.hpp>

namespace cello {

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s) {
    s.symbol_table[nonstd::string_view("u64")] = named_value { named_value_type::type, (llvm::Value*)llvm::Type::getInt64Ty(c) };
    s.symbol_table[nonstd::string_view("c_char")] = named_value { named_value_type::type, (llvm::Value*)llvm::Type::getInt8Ty(c) };
    s.symbol_table[nonstd::string_view("void")] = named_value { named_value_type::type, (llvm::Value*)llvm::Type::getVoidTy(c) };
  }
}
