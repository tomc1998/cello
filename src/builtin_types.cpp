#include "builtin_types.hpp"

#include "scope.hpp"
#include "llvm/IR/LLVMContext.h"

namespace cello {

  type builtin_ty_u64  = { int_type { 8, false }, 0 };
  type builtin_ty_u32  = { int_type { 4, false }, 0 };
  type builtin_ty_u16  = { int_type { 2, false }, 0 };
  type builtin_ty_u8   = { int_type { 1, false }, 0 };
  type builtin_ty_f64  = { float_type { 8 }, 0 };
  type builtin_ty_f32  = { float_type { 4 }, 0 };
  type builtin_ty_void = { void_type { } };
  /** Bool is just a u8 */
  type builtin_ty_bool(builtin_ty_u8);

  // Struct datas
  struct_data builtin_ty_slice_u8_struct_data =
    { { { nonstd::string_view("ptr"), (type_ident{{}, type_symbol {"u8"}}).ptr() },
        { nonstd::string_view("len"), type_ident{{}, type_symbol {"u32"}}} } };

  // Struct types
  type builtin_ty_slice_u8 = { struct_type { "__slice_u8", &builtin_ty_slice_u8_struct_data } };

  /** Setup builtin types in the scope */
  void setup_builtin_types(llvm::LLVMContext &c, scope& s) {
    s.symbol_table[nonstd::string_view("u64")]  = named_value { builtin_ty_u64  };
    s.symbol_table[nonstd::string_view("u32")]  = named_value { builtin_ty_u32  };
    s.symbol_table[nonstd::string_view("u16")]  = named_value { builtin_ty_u16  };
    s.symbol_table[nonstd::string_view("u8")]   = named_value { builtin_ty_u8   };
    s.symbol_table[nonstd::string_view("bool")] = named_value { builtin_ty_bool };
    s.symbol_table[nonstd::string_view("f64")]  = named_value { builtin_ty_f64  };
    s.symbol_table[nonstd::string_view("f32")]  = named_value { builtin_ty_f32  };
    s.symbol_table[nonstd::string_view("void")] = named_value { builtin_ty_void };
    s.symbol_table[nonstd::string_view("__slice_u8")] = named_value { builtin_ty_slice_u8 };
  }
}
