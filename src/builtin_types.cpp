#include "builtin_types.hpp"

#include "scope.hpp"
#include "llvm/IR/LLVMContext.h"

namespace cello {

  type builtin_ty_u64  = { int_type { 8, false }, 0 };
  type builtin_ty_u32  = { int_type { 4, false }, 0 };
  type builtin_ty_u8   = { int_type { 1, false }, 0 };
  type builtin_ty_void = { void_type { } };

  // Struct datas
  struct_data builtin_ty_slice_u8_struct_data =
    { "__slice_u8", { { nonstd::string_view("ptr"), builtin_ty_u8.ptr(1) },
                      { nonstd::string_view("len"), builtin_ty_u32, } } };

  // Struct types
  type builtin_ty_slice_u8 = { struct_type { &builtin_ty_slice_u8_struct_data } };

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s) {
    s.symbol_table[nonstd::string_view("u64")] = named_value  { builtin_ty_u64 };
    s.symbol_table[nonstd::string_view("u32")] = named_value  { builtin_ty_u64 };
    s.symbol_table[nonstd::string_view("u8")] = named_value   { builtin_ty_u8 };
    s.symbol_table[nonstd::string_view("void")] = named_value { builtin_ty_void };
    s.symbol_table[nonstd::string_view("__slice_u8")] = named_value { builtin_ty_slice_u8 };
  }
}
