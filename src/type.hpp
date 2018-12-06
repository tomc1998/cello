#pragma once

#include <vector>
#include <mapbox/variant.hpp>
#include <cstdint>
#include <utility>
#include <nonstd/string_view.hpp>
#include <llvm/IR/DerivedTypes.h>

namespace llvm {
  class Type;
  class LLVMContext;
}

namespace cello {

  struct type;

  struct struct_data {
    nonstd::string_view name;
    std::vector<std::pair<nonstd::string_view, type>> fields;
    /** The elements for the LLVM struct. This is borrowed by the cached_llvm_type. */
    std::vector<llvm::Type*> llvm_elements;
    /** LLVM struct type cached - nullptr when not generated. */
    llvm::StructType* cached_llvm_type = nullptr;
    /** Returns the cached LLVM type, or creates one if not already. */
    llvm::StructType* to_llvm_type(llvm::LLVMContext &c) const;
  };

  struct struct_type {
    struct_data* val;
  };

  struct void_type {};

  struct int_type {
    /** 1, 2, 4, 8 (16?) */
    uint8_t num_bytes;
    bool is_signed;
    llvm::Type* to_llvm_type(llvm::LLVMContext &c) const;
  };

  /** A type in the typesystem */
  struct type {
    mapbox::util::variant<int_type, void_type, struct_type> val;
    /** Number of levels of indirection - for int*, this is 1. */
    uint8_t num_ptr;
    llvm::Type* to_llvm_type(llvm::LLVMContext &c) const;
    /** CLone this type, increment num_ptr */
    type ptr(int levels) const;
  };
}
