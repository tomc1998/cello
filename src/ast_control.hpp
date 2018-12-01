#pragma once

/* Note: All struct here are embedded into cello::expr. This should all be
included from ast_expr. */

#include <memory>
#include <nonstd/optional.hpp>
#include "lexer.hpp"

namespace cello {

  struct expr;

  struct if_expr {
    std::unique_ptr<expr> cond;
    std::unique_ptr<expr> true_expr;
    std::unique_ptr<expr> false_expr;
    if_expr(const if_expr &other);
    if_expr(expr* cond, expr* true_expr, expr* false_expr);
  };

  nonstd::optional<if_expr> parse_if_expr(lexer &l);
}
