#pragma once

#include "ast_util.hpp"
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
    std::vector<arg> args;
  };

  nonstd::optional<arg> parse_arg(lexer &l) {
    ASSERT_TOK_TYPE_OR_ERROR_AND_RET(l, token_type::ident, "argument name");
    const auto arg_name = l.next()->val;
    const auto arg_type = parse_type_ident(l);
    if (!arg_type) { return nonstd::nullopt; }
    return { { arg_name, *arg_type } };
  }

  nonstd::optional<std::vector<arg>> parse_arg_list(lexer &l) {
    ASSERT_TOK_VAL_OR_ERROR_AND_RET(l, "(");
    l.next();
    std::vector<arg> args;
    while ((l.peek() && l.peek()->val != ")") || !l.peek()) {
      const auto arg_opt = parse_arg(l);
      if (!arg_opt) { return nonstd::nullopt; }
      args.push_back(*arg_opt);
    }
    ASSERT_TOK_VAL_OR_ERROR_AND_RET(l, ")");
    l.next();
    return args;
  }

  nonstd::optional<function> parse_function(lexer &l) {
    ASSERT_TOK_VAL_OR_ERROR_AND_RET(l, "fn");
    l.next();
    ASSERT_TOK_TYPE_OR_ERROR_AND_RET(l, token_type::ident, "function name");
    const auto name = l.next()->val;
    const auto arg_list_opt = parse_arg_list(l);
    if (!arg_list_opt) { return nonstd::nullopt; }
    consume_to_closing_paren(l);
    return { { name, *arg_list_opt } };
  };
}
