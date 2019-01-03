#pragma once

#include <nonstd/string_view.hpp>
#include "type.hpp"

namespace llvm {
  class LLVMContext;
}

namespace cello {

  struct scope;

  // Special types
  extern type builtin_ty_void;

  // Int types
  extern type builtin_ty_u64;
  extern type builtin_ty_u32;
  extern type builtin_ty_u16;
  extern type builtin_ty_u8;

  // Float types
  extern type builtin_ty_f32;
  extern type builtin_ty_f64;

  // Bools
  extern type builtin_ty_bool;

  // Struct types
  extern type builtin_ty_slice_u8;

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s);
}
