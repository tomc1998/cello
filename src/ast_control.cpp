#include "ast_control.hpp"
#include "ast_util.hpp"
#include "error.hpp"
#include "ast_expr.hpp"

namespace cello {

  while_expr::while_expr(const while_expr &other)
    : cond(new expr(*other.cond)), body(other.body) {}


  while_expr::while_expr(expr* cond, std::vector<expr> body)
    : cond(cond), body(body) {}

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
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, ")");
    l.next();
    return { { new expr(*cond), new expr(*true_expr), new expr(*false_expr) } };
  }

  nonstd::optional<while_expr> parse_while_expr(lexer &l) {
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "while");
    l.next();
    std::cout << "AFTER WHILE: " << l.get_remaining_input() << std::endl;
    const auto cond = parse_expr(l);
    if (!cond) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }

    std::cout << "AFTER COND: " << l.get_remaining_input() << std::endl;

    std::vector<expr> body;
    while ((l.peek() && l.peek()->val != ")") || !l.peek()) {
      const auto expr_opt = parse_expr(l);
      if (!expr_opt) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      body.push_back(*expr_opt);
    }

    std::cout << "AFTER BODY: " << l.get_remaining_input() << std::endl;

    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, ")");
    l.next();
    return { { new expr(*cond), body } };
  }

}
