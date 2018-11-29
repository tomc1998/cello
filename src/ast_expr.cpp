#include "error.hpp"
#include "ast_expr.hpp"
#include "ast_util.hpp"
#include "lexer.hpp"

#include <llvm/ADT/StringRef.h>
#include <nonstd/optional.hpp>

namespace cello {
  bin_op_expr::bin_op_expr(const bin_op_expr &other)
    : lchild(new expr(*other.lchild)), rchild(new expr(*other.rchild)) {};

  set_var_expr::set_var_expr(const set_var_expr &other)
    : var(other.var), type(other.type), val(new expr(*other.val)) {};

  set_expr::set_expr(const set_expr &other) noexcept
    : var(other.var), val(new expr(*other.val)) {};

  bool is_bin_op(nonstd::string_view s) {
    return s == "+" || s == "-" || s == "*" || s == "/";
  }

  bin_op to_bin_op(nonstd::string_view s) {
    if (s == "+") { return bin_op::add; }
    else if (s == "-") { return bin_op::sub; }
    else if (s == "/") { return bin_op::div; }
    else if (s == "*") { return bin_op::mul; }
    assert(false);
  }

  nonstd::optional<expr> parse_expr(lexer &l) {
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "expression");
    if (l.peek()->val == "(") {
      l.next();
      ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "expression");
      if (is_bin_op(l.peek()->val)) {
        const auto op = to_bin_op(l.next()->val);
        const auto lchild = parse_expr(l);
        if (!lchild) { return nonstd::nullopt; }
        const auto rchild = parse_expr(l);
        if (!rchild) { return nonstd::nullopt; }
        if (!l.peek() || l.peek()->val != ")") {
          report_error(l.get_curr_source_label(), "Expected ')'");
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        l.next();
        return { { bin_op_expr { op, new expr(*lchild), new expr(*rchild) } } };
      }
    } else if (l.peek()->type == token_type::ident) {
      return { { variable { l.next()->val } } };
    } else if (l.peek()->type == token_type::int_lit) {
      const auto val = l.next()->val;
      const auto string_ref = llvm::StringRef(val.begin(), val.size());
      return { { int_lit { llvm::APInt(64, string_ref, 10) } } };
    }

    report_error(l.get_curr_source_label(),
                 "Unimplemented expr parsing");
    return nonstd::nullopt;
  }

  std::string expr::to_string() const {
    return val.match([&](function_call x) { return std::string("function_call"); },
                     [&](bin_op_expr x)   { return std::string("bin_op_expr"); },
                     [&](un_op_expr x)    { return std::string("un_op_expr"); },
                     [&](variable x)      { return std::string("variable(") + std::string(x.val) + ")"; },
                     [&](int_lit x)       { return std::string("int_lit"); },
                     [&](float_lit x)     { return std::string("float_lit"); },
                     [&](string_lit x)    { return std::string("string_lit"); },
                     [&](set_var_expr x)  { return std::string("set_var_expr"); },
                     [&](set_expr x)      { return std::string("set_expr"); });
  }
}
