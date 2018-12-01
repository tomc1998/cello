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
      if (!arg_opt) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      args.push_back(*arg_opt);
    }

    if (!l.peek() || l.peek()->val != ")") {
      report_error(l.get_curr_source_label(), "Expected ')'");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    l.next();
    return args;
  }

  nonstd::optional<function> parse_function(lexer &l) {
    if (!l.peek() || l.peek()->val != "fn") {
      report_error(l.get_curr_source_label(), "Expected 'fn'");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    l.next();
    if (!l.peek() || l.peek()->type != token_type::ident) {
      report_error(l.get_curr_source_label(), "Expected function name");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    const auto name = l.next()->val;
    const auto return_type_opt = parse_type_ident(l);
    if (!return_type_opt) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    const auto arg_list_opt = parse_arg_list(l);
    if (!arg_list_opt) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }

    std::vector<expr> expr_list;
    const auto sl = l.get_curr_source_label();
    bool found_closing_paren = false;
    while(l.peek()) {
      if (l.peek()->val == ")") {
        found_closing_paren = true;
        l.next();
        break;
      }
      const auto e = parse_expr(l);
      if (!e) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        found_closing_paren = true;
        break;
      }
      expr_list.push_back(*e);
    }
    if (!found_closing_paren) {
      report_error(sl, "Unmatched paren");
      return nonstd::nullopt;
    }

    return { { name, *return_type_opt, *arg_list_opt, expr_list } };
  };
}
