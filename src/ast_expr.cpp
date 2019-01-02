#include "error.hpp"
#include "ast_expr.hpp"
#include "ast_util.hpp"
#include "lexer.hpp"
#include "scope.hpp"
#include "builtin_types.hpp"
#include "string_lit_parse.hpp"

#include <iostream>
#include <string>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Value.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/APInt.h>
#include <nonstd/optional.hpp>

namespace cello {
  /* Copy constructors for stuff that contains a std::unique_ptr<expr> */
  bin_op_expr::bin_op_expr(const bin_op_expr &other)
    : op(other.op), lchild(new expr(*other.lchild)), rchild(new expr(*other.rchild)) {};
  field_access_expr::field_access_expr(const field_access_expr &other)
    : target(new expr(*other.target)), field_name(other.field_name) {};
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

  nonstd::optional<field_access_expr> parse_field_access_expr(lexer &l) {
    assert(l.next()->val == "field");
    const auto target = parse_expr(l);
    if (!target) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "Field name");
    const auto field_name = l.next()->val;
    if (!l.peek() || l.peek()->val != ")") {
      report_error(l.get_curr_source_label(), "Expected ')'");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    l.next();
    return { { new expr(*target), field_name } };
  }

  nonstd::optional<function_call> parse_function_call(lexer &l) {
    assert(l.peek()->type == token_type::ident);
    const auto name = l.next()->val;
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "function arguments");
    // Parse function arguments here
    std::vector<expr> args;
    while ((l.peek() && l.peek()->val != ")") || !l.peek()) {
      const auto expr_opt = parse_expr(l);
      if (!expr_opt) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      args.push_back(*expr_opt);
    }
    if (!l.peek() || l.peek()->val != ")") {
      report_error(l.get_curr_source_label(), "Expected ')'");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    l.next();
    return { function_call { name, args } };
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
      } else if (l.peek()->val == "field") {
        const auto e = parse_field_access_expr(l);
        if (!e) { return nonstd::nullopt; }
        else { return { { sl, *e } }; };
      } else if (l.peek()->val == "while") {
        const auto e = parse_while_expr(l);
        if (!e) { return nonstd::nullopt; }
        else { return { { sl, *e } }; };
      } else if (l.peek()->val == "if") {
        const auto e = parse_if_expr(l);
        if (!e) { return nonstd::nullopt; }
        else { return { { sl, *e } }; };
      } else if (l.peek()->val == "let" || l.peek()->val == "mut") {
        const auto expr_type = l.next()->val;
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
        if (expr_type == "let") { return { { sl, let_expr { { name }, *type, new expr(*e) } } }; }
        if (expr_type == "mut") { return { { sl, mut_expr { { name }, *type, new expr(*e) } } }; }
        assert(false && "Unreachable");
      } else if (l.peek()->val == "set") {
        l.next();
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "variable name");
        const auto name = l.next()->val;
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "expr value");
        if (l.peek()->val == ")") {
          report_error(l.get_curr_source_label(),
                       "Expected value for set expr, found (set <varname>) "
                       "(instead of (set <varname> <expr>))");
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
        return { { sl, set_expr { variable { name }, { new expr(*e) } } } };
      } else if (l.peek()->type == token_type::ident) { // Function call
        const auto function_call = parse_function_call(l);
        if (!function_call) { return nonstd::nullopt; }
        return { { sl, *function_call } };
      }
    } else if (l.peek()->type == token_type::ident) {
      return { { sl, variable { l.next()->val } } };
    } else if (l.peek()->type == token_type::c_string_lit) {
      const auto string_tok = *l.next();
      const auto s = nonstd::string_view(string_tok.val.begin() + 2, string_tok.val.size() - 3);
      return { { sl, c_string_lit { *escape_all(s) } } };
    } else if (l.peek()->type == token_type::string_lit) {
      // Strip the val of quotes (")
      const auto val = l.next()->val;
      nonstd::string_view stripped_val(val.cbegin() + 1, val.size() - 2);
      return { { sl, string_lit { stripped_val } } };
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
    return val.match([&](function_call x)     { return std::string("function_call"); },
                     [&](bin_op_expr x)       { return std::string("bin_op_expr"); },
                     [&](un_op_expr x)        { return std::string("un_op_expr"); },
                     [&](variable x)          { return std::string("variable(") + std::string(x.val) + ")"; },
                     [&](int_lit x)           { return std::string("int_lit"); },
                     [&](float_lit x)         { return std::string("float_lit"); },
                     [&](string_lit x)        { return std::string("string_lit"); },
                     [&](c_string_lit x)      { return std::string("c_string_lit"); },
                     [&](if_expr x)           { return std::string("if_expr"); },
                     [&](while_expr x)        { return std::string("while_expr"); },
                     [&](mut_expr x)          { return std::string("mut_expr"); },
                     [&](let_expr x)          { return std::string("let_expr"); },
                     [&](set_expr x)          { return std::string("set_expr"); },
                     [&](field_access_expr x) { return std::string("field_access_expr"); });
  }

  llvm::Constant* string_lit::code_gen(scope &s, llvm::IRBuilder<> &b) const {
    // TODO Generate non-null-terminated string (leave that up to the programmer with \0)
    auto &llvm_ctx = b.getContext();
    llvm::Constant* const string_ptr = b.CreateGlobalStringPtr(llvm::StringRef(val.cbegin(), val.size()));
    llvm::Constant* const string_len = llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), val.size());
    llvm::Constant* const * vals = new llvm::Constant* const[2] {string_ptr, string_len};
    return llvm::ConstantStruct::get((llvm::StructType*)builtin_ty_slice_u8.to_llvm_type(s, llvm_ctx),
                                     llvm::ArrayRef<llvm::Constant*>(vals, 2));
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

  nonstd::optional<llvm::Value*> field_access_expr::code_gen(scope &s, llvm::IRBuilder<> &b) const {
    assert(target->val.template is<variable>() && "We only support field access on variables");
    // First, get the target type
    const auto target_type = target->get_type(s);
    assert(target_type);
    assert(target_type->val.template is<struct_type>());
    // get struct data
    const auto data = target_type->val.template get<struct_type>().data;
    assert(data);
    // Find field
    int found_field = -1;
    for (int ii = 0; ii < (int)data->fields.size(); ++ii) {
      if (data->fields[ii].name == field_name) {
        found_field = ii;
        break;
      }
    }
    assert(found_field >= 0 && "Can't find field");
    // Codegen the target so we have a llvm::Value* to index
    auto target_llvm_val = target->code_gen(s, b);
    if (!target_llvm_val) { return nonstd::nullopt; }
    const auto gep = b.CreateStructGEP(*target_llvm_val, found_field);
    return { b.CreateLoad(gep) };
  }

  nonstd::optional<llvm::Value*> variable::code_gen(const source_label &sl, scope &s,
                                                    llvm::IRBuilder<> &b, bool deref_mut) const {
    const auto n_value = s.find_symbol_with_type(val, named_value_type::var);
    if (!n_value) {
      report_error(sl, std::string("Undefined variable '") + std::string(val) + "'");
      return nonstd::nullopt;
    }
    const auto &v = n_value->val.template get<var>();
    if (deref_mut) {
      return { v.is_mutable() ? b.CreateLoad(v.val) : v.val };
    } else {
      assert(v.is_mutable());
      return { v.val };
    }
  }

  nonstd::optional<llvm::Value*> expr::code_gen(scope &s, llvm::IRBuilder<> &b) const {
    auto &llvm_ctx = b.getContext();
    if (val.template is<bin_op_expr>()) {
      return val.template get<bin_op_expr>().code_gen(sl, s, b);
    } else if (val.template is<variable>()) {
      return val.template get<variable>().code_gen(sl, s, b);
    } else if (val.template is<let_expr>()) {
      const auto &e = val.template get<let_expr>();
      const auto e_val = e.val->code_gen(s, b);
      if (!e_val) { return nonstd::nullopt; }
      const auto e_type = e.type.code_gen(s);
      if (!e_type) { return nonstd::nullopt; }
      s.symbol_table.insert(std::make_pair(e.var.val, named_value { var { *e_type, *e_val, 0 } }));
      return *e_val;
    } else if (val.template is<mut_expr>()) {
      const auto &e = val.template get<mut_expr>();
      const auto e_val = e.val->code_gen(s, b);
      if (!e_val) { return nonstd::nullopt; }
      const auto e_type = e.type.code_gen(s);
      if (!e_type) { return nonstd::nullopt; }
      // Allocate space
      const auto alloca = b.CreateAlloca(e_type->to_llvm_type(s, llvm_ctx));
      // Store location in symbol table
      s.symbol_table.insert(std::make_pair(e.var.val, named_value {
            var { *e_type, alloca, var::FLAGS_MUT } }));
      b.CreateStore(*e_val, alloca);
      return alloca;
    } else if (val.template is<set_expr>()) {
      const auto &e = val.template get<set_expr>();
      // Find the var & gen the expr value
      const auto e_val = e.val->code_gen(s, b);
      const auto e_var = e.var.code_gen(sl, s, b, false);
      if (!e_val || !e_var) { return nonstd::nullopt; }
      return b.CreateStore(*e_val, *e_var);
    } else if (val.template is<field_access_expr>()) {
      return val.template get<field_access_expr>().code_gen(s, b);
    } else if (val.template is<while_expr>()) {
      const auto &e = val.template get<while_expr>();
      const auto prev_block = b.GetInsertBlock();
      assert(prev_block->getParent());
      auto body_cond_block = llvm::BasicBlock::Create(llvm_ctx, "loop_body_cond_block",
                                                      prev_block->getParent());
      auto body_block = llvm::BasicBlock::Create(llvm_ctx, "loop_body_block");
      auto end_block = llvm::BasicBlock::Create(llvm_ctx, "end_block");
      auto body_scope = s.create_subscope();
      b.CreateBr(body_cond_block);
      b.SetInsertPoint(body_cond_block);
      // Generate condition
      const auto cond_val = e.cond->code_gen(s, b);
      b.CreateCondBr(*cond_val, body_block, end_block);
      prev_block->getParent()->getBasicBlockList().push_back(body_block);
      b.SetInsertPoint(body_block);
      for (const auto &body_expr : e.body) {
        body_expr.code_gen(body_scope, b);
      }
      b.CreateBr(body_cond_block);
      prev_block->getParent()->getBasicBlockList().push_back(end_block);
      b.SetInsertPoint(end_block);
      return { llvm::Constant::getNullValue(llvm::Type::getDoubleTy(llvm_ctx)) };

    } else if (val.template is<if_expr>()) {
      const auto &e = val.template get<if_expr>();
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

      auto phi = b.CreatePHI(this_type->to_llvm_type(s, llvm_ctx), 2, "iftmp");
      phi->addIncoming(*true_val, true_block);
      phi->addIncoming(*false_val, false_block);

      return { phi };
    } else if (val.template is<function_call>()) {
      const auto &fc = val.template get<function_call>();
      // Get function
      const auto nv = s.find_symbol_with_type(fc.name, named_value_type::function);
      if (!nv) {
        report_error(sl, std::string("Unable to locate function with name '")
                     + std::string(fc.name) + "'");
        return nonstd::nullopt;
      }
      const auto &f = nv->val.template get<function>();
      const auto llvm_function = f.get_cached_llvm_function();
      assert(llvm_function);

      // Eval all exprs
      llvm::Value** args = new llvm::Value*[fc.arg_list.size()];
      for (unsigned ii = 0; ii < fc.arg_list.size(); ++ii) {
        const auto expr_opt = fc.arg_list[ii].code_gen(s, b);
        if (!expr_opt) { return nonstd::nullopt; }
        args[ii] = *expr_opt;
      }

      return b.CreateCall(llvm_function, llvm::ArrayRef<llvm::Value*>(args, fc.arg_list.size()));
    } else if (val.template is<string_lit>()) {
      return { val.template get<string_lit>().code_gen(s, b) };
    } else if (val.template is<int_lit>()) {
      return { llvm::ConstantInt::get(llvm::Type::getInt64Ty(b.getContext()),
                                      llvm::APInt(val.template get<int_lit>().val)) };
    } else if (val.template is<c_string_lit>()) {
      const auto &s = val.template get<c_string_lit>().val;
      return b.CreateGlobalStringPtr(llvm::StringRef(s.begin(), s.size()));
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
