#pragma once

#include <vector>
#include <cassert>
#include "comptime_expr.hpp"
#include <nonstd/optional.hpp>

#include <cstdint>
#include <string>
#include <memory>
#include <nonstd/string_view.hpp>
#include <mapbox/variant.hpp>

#include <llvm/IR/Type.h>

#include "source_label.hpp"
#include <utility>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>

namespace llvm {
  class Type;
  class LLVMContext;
}

namespace cello {

  struct struct_data;
  class lexer;
  struct scope;
  struct struct_field;
  struct type;

  struct void_type {
    bool operator==(const void_type& other) const;
  };

  /** A struct type - the actual data of the fields are stored in struct_data,
      this is so that `type` can remain small & passed around by value since we
      don't own data. */
  struct struct_type {
    nonstd::string_view name;
    /** A pointer to the struct data. Should never be null! */
    struct_data* data;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    bool operator==(const struct_type& other) const;
  };

  struct float_type {
    /** 4, 8, (2, 16?)*/
    uint8_t num_bytes;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    bool operator==(const float_type& other) const;
  };

  struct int_type {
    /** 1, 2, 4, 8 (16?) */
    uint8_t num_bytes;
    bool is_signed;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    bool operator==(const int_type& other) const;
  };

  /** A type in the typesystem */
  struct type {
    mapbox::util::variant<int_type, float_type, void_type, struct_type> val;
    /** Number of levels of indirection - for int*, this is 1. */
    uint8_t num_ptr;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    /** CLone this type, increment num_ptr */
    type ptr(int levels) const;
    /** Auto-CasCannot coert val to other. Returns nullopt if an implicit cast is not
    possible. */
    nonstd::optional<llvm::Value*> coerce(const scope& s, llvm::IRBuilder<> &b,
                                        llvm::Value* from, const type& target) const;
    std::string to_str() const;
    bool operator==(const type& other) const;
    bool operator!=(const type& other) const;
  };

  std::ostream& operator<<(std::ostream& o, const struct_type& s);
}
