#pragma once

#include "ast_util.hpp"
#include "ast_expr.hpp"
#include "lexer.hpp"
#include "error.hpp"
#include "ast_type.hpp"
#include <nonstd/optional.hpp>
#include <cassert>

namespace cello {
  struct arg {
    nonstd::string_view name;
    type_ident type;
  };

  struct function {
    nonstd::string_view name;
    type_ident return_type;
    std::vector<arg> args;
    std::vector<expr> expressions;
  };

  nonstd::optional<arg> parse_arg(lexer &l);
  nonstd::optional<std::vector<arg>> parse_arg_list(lexer &l);
  nonstd::optional<function> parse_function(lexer &l);
}
