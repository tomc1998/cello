#pragma once

#include <nonstd/string_view.hpp>
#include "type.hpp"

namespace llvm {
  class LLVMContext;
}

namespace cello {

  struct scope;

  static type builtin_ty_u64    = { int_type { 8, false }, 0 };
  static type builtin_ty_c_char = { int_type { 1, false }, 0 };
  static type builtin_ty_void   = { void_type { } };

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s);
}
