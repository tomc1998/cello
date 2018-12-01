#include "error.hpp"
#include "ast_expr.hpp"
#include "ast_util.hpp"
#include "lexer.hpp"
#include "scope.hpp"

#include <string>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Value.h>
#include <llvm/ADT/StringRef.h>
#include <nonstd/optional.hpp>

namespace cello {
  bin_op_expr::bin_op_expr(const bin_op_expr &other)
    : op(other.op), lchild(new expr(*other.lchild)), rchild(new expr(*other.rchild)) {};

  mut_expr::mut_expr(const mut_expr &other)
    : var(other.var), type(other.type), val(new expr(*other.val)) {};
  let_expr::let_expr(const let_expr &other) noexcept
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
    const auto sl = l.get_curr_source_label();
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
        return { { sl, bin_op_expr { op, new expr(*lchild), new expr(*rchild) } } };
      } else if (l.peek()->val == "if") {
        const auto e = parse_if_expr(l);
        if (!e) { return nonstd::nullopt; }
        else { return { { sl, *e } }; };
      } else if (l.peek()->val == "let") {
        l.next();
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "variable name");
        const auto name = l.next()->val;
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "type");
        assert(l.peek()->val != "auto" && "Unimplemented type inference handling");
        const auto type = parse_type_ident(l);
        if (!type) {
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        const auto e = parse_expr(l);
        if (!e) {
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        if (!l.peek() || l.peek()->val != ")") {
          report_error(l.get_curr_source_label(), "Expected ')'");
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        l.next();
        return { { sl, let_expr { { name }, *type, new expr(*e) } } };
      }
    } else if (l.peek()->type == token_type::ident) {
      return { { sl, variable { l.next()->val } } };
    } else if (l.peek()->type == token_type::int_lit) {
      const auto val = l.next()->val;
      const auto string_ref = llvm::StringRef(val.begin(), val.size());
      return { { sl, int_lit { llvm::APInt(64, string_ref, 10) } } };
    }

    report_error(sl, std::string("Unimplemented expr parsing: ") + std::string(l.get_remaining_input()));
    CONSUME_TO_END_PAREN_OR_ERROR(l);
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
                     [&](if_expr x)       { return std::string("if_expr"); },
                     [&](mut_expr x)      { return std::string("mut_expr"); },
                     [&](let_expr x)      { return std::string("let_expr"); },
                     [&](set_expr x)      { return std::string("set_expr"); });
  }

  nonstd::optional<llvm::Value*> bin_op_expr::code_gen(const source_label &sl, scope &s, llvm::IRBuilder<> &b) const {
    const auto lval = lchild->code_gen(s, b);
    const auto rval = rchild->code_gen(s, b);
    if (!lval || !rval) { return nonstd::nullopt; }
    switch (op) {
    case bin_op::add: return { b.CreateAdd(*lval, *rval) };
    case bin_op::sub: return { b.CreateSub(*lval, *rval) };
    case bin_op::mul: return { b.CreateMul(*lval, *rval) };
    case bin_op::div: return { b.CreateSDiv(*lval, *rval) };
    }
    assert(false);
  }

  nonstd::optional<llvm::Value*> expr::code_gen(scope &s, llvm::IRBuilder<> &b) const {
    if (val.template is<bin_op_expr>()) {
      return val.template get<bin_op_expr>().code_gen(sl, s, b);
    } else if (val.template is<variable>()) {
      const auto &name = val.template get<variable>().val;
      const auto value = s.symbol_table.find(name);
      if (value == s.symbol_table.end()) {
        report_error(sl, std::string("Undefined variable '") + std::string(name) + "'");
        return nonstd::nullopt;
      }
      return { value->second.v };
    } else if (val.template is<let_expr>()) {
      const auto &e = val.template get<let_expr>();
      const auto e_val = e.val->code_gen(s, b);
      if (!e_val) { return nonstd::nullopt; }
      s.symbol_table.insert(std::make_pair(e.var.val, named_value {named_value_type::var, *e_val}));
      return *e_val;
    } else if (val.template is<int_lit>()) {
      return { llvm::ConstantInt::get(llvm::Type::getInt64Ty(b.getContext()),
                                      llvm::APInt(val.template get<int_lit>().val)) };
    }
    report_error(sl, std::string("Code gen not implemented for this type of expr: ") + to_string());
    return nonstd::nullopt;
  }
}
