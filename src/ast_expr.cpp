#include "error.hpp"
#include "ast_expr.hpp"
#include "ast_util.hpp"
#include "lexer.hpp"
#include "scope.hpp"
#include "builtin_types.hpp"

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
    return s == "+" || s == "-" || s == "*" || s == "/" || s == "==" || s == ">"
    || s == "<" || s == "<=" || s == ">=";
  }

  bin_op to_bin_op(nonstd::string_view s) {
    if (s == "+")       { return bin_op::add; }
    else if (s == "-")  { return bin_op::sub; }
    else if (s == "/")  { return bin_op::div; }
    else if (s == "*")  { return bin_op::mul; }
    else if (s == "<")  { return bin_op::lt; }
    else if (s == ">")  { return bin_op::gt; }
    else if (s == "<=") { return bin_op::le; }
    else if (s == ">=") { return bin_op::ge; }
    else if (s == "==") { return bin_op::eq; }
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
    case bin_op::div: return { b.CreateUDiv(*lval, *rval) };
    case bin_op::lt: return { b.CreateICmpULT(*lval, *rval) };
    case bin_op::le: return { b.CreateICmpULE(*lval, *rval) };
    case bin_op::gt: return { b.CreateICmpUGT(*lval, *rval) };
    case bin_op::ge: return { b.CreateICmpUGE(*lval, *rval) };
    case bin_op::eq: return { b.CreateICmpEQ(*lval, *rval) };
    default: assert(false);
    }
  }

  nonstd::optional<llvm::Value*> expr::code_gen(scope &s, llvm::IRBuilder<> &b) const {
    if (val.template is<bin_op_expr>()) {
      return val.template get<bin_op_expr>().code_gen(sl, s, b);
    } else if (val.template is<variable>()) {
      const auto &name = val.template get<variable>().val;
      const auto n_value = s.find_symbol_with_type(name, named_value_type::var);
      if (!n_value) {
        report_error(sl, std::string("Undefined variable '") + std::string(name) + "'");
        return nonstd::nullopt;
      }
      return { n_value->val.template get<var>().val };
    } else if (val.template is<let_expr>()) {
      const auto &e = val.template get<let_expr>();
      const auto e_val = e.val->code_gen(s, b);
      if (!e_val) { return nonstd::nullopt; }
      const auto e_type = e.type.code_gen(s);
      if (!e_type) { return nonstd::nullopt; }
      s.symbol_table.insert(std::make_pair(e.var.val, named_value { var { *e_type, *e_val } }));
      return *e_val;
    } else if (val.template is<if_expr>()) {
      const auto &e = val.template get<if_expr>();
      auto &llvm_ctx = b.getContext();
      const auto prev_block = b.GetInsertBlock();
      assert(prev_block->getParent());
      auto true_block = llvm::BasicBlock::Create(llvm_ctx, "true_block", prev_block->getParent());
      auto false_block = llvm::BasicBlock::Create(llvm_ctx, "false_block");
      const auto end_block = llvm::BasicBlock::Create(llvm_ctx, "end_block");

      // Generate condition
      const auto cond_val = e.cond->code_gen(s, b);
      b.CreateCondBr(*cond_val, true_block, false_block);

      // Generate true / false code
      // True
      b.SetInsertPoint(true_block);
      auto true_scope = s.create_subscope();
      const auto true_val = e.true_expr->code_gen(true_scope, b);
      if (!true_val) { return nonstd::nullopt; }
      b.CreateBr(end_block);
      true_block = b.GetInsertBlock();
      // False
      prev_block->getParent()->getBasicBlockList().push_back(false_block);
      b.SetInsertPoint(false_block);
      auto false_scope = s.create_subscope();
      const auto false_val = e.false_expr->code_gen(false_scope, b);
      if (!false_val) { return nonstd::nullopt; }
      b.CreateBr(end_block);
      false_block = b.GetInsertBlock();

      // 'Flush' all the current generated stuff
      prev_block->getParent()->getBasicBlockList().push_back(end_block);
      b.SetInsertPoint(end_block);

      auto this_type = get_type(s);
      if (!this_type) { return nonstd::nullopt; }

      auto phi = b.CreatePHI(this_type->to_llvm_type(llvm_ctx), 2, "iftmp");
      phi->addIncoming(*true_val, true_block);
      phi->addIncoming(*false_val, false_block);

      return { phi };
    } else if (val.template is<int_lit>()) {
      return { llvm::ConstantInt::get(llvm::Type::getInt64Ty(b.getContext()),
                                      llvm::APInt(val.template get<int_lit>().val)) };
    }
    report_error(sl, std::string("Code gen not implemented for this type of expr: ") + to_string());
    return nonstd::nullopt;
  }

  nonstd::optional<type> expr::get_type(const scope& s) const {
    if (val.template is<bin_op_expr>()) {
      // Assume lchild is the same type as rchild
      return val.template get<bin_op_expr>().lchild->get_type(s);
    } else if (val.template is<variable>()) {
      const auto v = s.find_symbol_with_type(val.template get<variable>().val, named_value_type::var);
      if (!v) { return nonstd::nullopt; }
      return { v->val.template get<var>().var_type };
    } else if (val.template is<let_expr>()) {
      return val.template get<let_expr>().type.code_gen(s);
    } else if (val.template is<if_expr>()) {
      // Assume true_val is the same type as false_val
      return val.template get<if_expr>().true_expr->get_type(s);
    } else if (val.template is<int_lit>()) {
      const auto &x = val.template get<int_lit>().val;
      if (x.isIntN(64)) { return { builtin_ty_u64 }; }
      else { assert(false && "Int lit won't fit, insert proper error here"); }
    }
    assert(false);
  }
}
