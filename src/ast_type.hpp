#pragma once

#include <vector>
#include <cassert>
#include "comptime_expr.hpp"
#include <nonstd/optional.hpp>

#include <string>
#include <memory>
#include <nonstd/string_view.hpp>
#include <mapbox/variant.hpp>

namespace cello {

  class lexer;
  struct type_ident;

  struct ptr_type {
    std::unique_ptr<type_ident> val;
    ptr_type(const ptr_type& other);
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
    mapbox::util::variant<ptr_type, type_symbol, type_function_call> val;
    std::string to_string() const;
  };

  nonstd::optional<type_ident> parse_type_ident(lexer &l);
}
