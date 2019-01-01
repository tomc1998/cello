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

  struct void_type {};

  /** A struct type - the actual data of the fields are stored in struct_data,
      this is so that `type` can remain small & passed around by value since we
      don't own data. */
  struct struct_type {
    nonstd::string_view name;
    /** A pointer to the struct data */
    struct_data* data;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
  };

  struct int_type {
    /** 1, 2, 4, 8 (16?) */
    uint8_t num_bytes;
    bool is_signed;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
  };

  /** A type in the typesystem */
  struct type {
    mapbox::util::variant<int_type, void_type, struct_type> val;
    /** Number of levels of indirection - for int*, this is 1. */
    uint8_t num_ptr;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    /** CLone this type, increment num_ptr */
    type ptr(int levels) const;
  };

  /** The container for the actual fields of a struct */
  struct struct_data {
    std::vector<struct_field> fields;
  };

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
    type_ident type;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
  };

  std::ostream& operator<<(std::ostream& o, const struct_type& s);
}
