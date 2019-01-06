#pragma once

#include <string>
#include "type.hpp"

namespace cello {

  struct type_ident;

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
}
