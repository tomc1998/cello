#include "ast_control.hpp"
#include "ast_util.hpp"
#include "error.hpp"
#include "ast_expr.hpp"

namespace cello {

  if_expr::if_expr(const if_expr &other)
    : cond(new expr(*other.cond)),
      true_expr(new expr(*other.true_expr)),
      false_expr(new expr(*other.false_expr)) {}

  if_expr::if_expr(expr* cond, expr* true_expr, expr* false_expr)
    : cond(cond), true_expr(true_expr), false_expr(false_expr) {}


  nonstd::optional<if_expr> parse_if_expr(lexer &l) {
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "if");
    l.next();
    const auto cond = parse_expr(l);
    if (!cond) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    const auto true_expr = parse_expr(l);
    if (!true_expr) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    const auto false_expr = parse_expr(l);
    if (!false_expr) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    return { { new expr(*cond), new expr(*true_expr), new expr(*false_expr) } };
  }

}
