#pragma once

#include <mapbox/variant.hpp>
#include <cstdint>

namespace llvm {
  class Type;
  class LLVMContext;
}

namespace cello {

  struct void_type {};

  struct int_type {
    /** 1, 2, 4, 8 (16?) */
    uint8_t num_bytes;
    bool is_signed;
    llvm::Type* to_llvm_type(llvm::LLVMContext &c) const;
  };

  /** A type in the typesystem */
  struct type {
    mapbox::util::variant<int_type, void_type> val;
    /** Number of levels of indirection - for int*, this is 1. */
    uint8_t num_ptr;
    llvm::Type* to_llvm_type(llvm::LLVMContext &c) const;
  };
}
