#include "builtin_types.hpp"

#include "scope.hpp"
#include "llvm/IR/LLVMContext.h"

namespace cello {

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s) {
    s.symbol_table[nonstd::string_view("u64")] = named_value    { builtin_ty_u64 };
    s.symbol_table[nonstd::string_view("c_char")] = named_value { builtin_ty_c_char };
    s.symbol_table[nonstd::string_view("void")] = named_value   { builtin_ty_void };
  }

}
