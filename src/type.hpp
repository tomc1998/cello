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

#include "type.hpp"
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
  struct type_ident;
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

  /** The container for the actual fields of a struct */
  struct struct_data {
    std::vector<struct_field> fields;
    const struct_field* find_field_with_name(nonstd::string_view field_name) const;
    /** Returns -1 if field is not found */
    int find_field_index_with_name(nonstd::string_view field_index) const;
    /** Finds the field with the given index - this is the index retrieved from
    find_field_index_with_name. Asserts if OOB. */
    const struct_field& get_field_with_index(unsigned ix) const;
  };

  /** Used fortype idents - potentially poorly names, pointers are actually just
  represented with the num_ptr field in struct type. */
  struct ptr_type {
    std::unique_ptr<type_ident> val;
    ptr_type(const ptr_type& other);
    ptr_type(type_ident* val) : val(val) {};
  };

  /** A symbol, representing a type. */
  struct type_symbol {
    nonstd::string_view val;
  };

  /** Call a function that returns a type. */
  struct type_function_call {
    nonstd::string_view name;
    std::vector<comptime_expr> args;
  };

  /** Type identifier, like (ptr (ptr c_char)). */
  struct type_ident {
    source_label sl;
    mapbox::util::variant<ptr_type, type_symbol, type_function_call> val;
    std::string to_string() const;
    nonstd::optional<type> code_gen(const scope& s) const;
    /** Clone this, surroud with pointer, return. source label will be null. */
    type_ident ptr() const;
  };

  nonstd::optional<type_ident> parse_type_ident(lexer &l);

  struct struct_field {
    nonstd::string_view name;
    type_ident field_type;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    /** Convert this to a type (rather than just type_ident)
     TODO Maybe cache this in the struct_field? */
    nonstd::optional<type> get_type(const scope& s) const;
  };

  std::ostream& operator<<(std::ostream& o, const struct_type& s);
}
