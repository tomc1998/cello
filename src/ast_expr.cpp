#include "error.hpp"
#include "ast_expr.hpp"
#include "ast_util.hpp"
#include "lexer.hpp"
#include "scope.hpp"
#include "builtin_types.hpp"
#include "string_lit_parse.hpp"
#include "struct_type.hpp"

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

  bool bin_op_expr::is_bool_expr() const {
    switch (op) {
    case bin_op::gt:
    case bin_op::ge:
    case bin_op::lt:
    case bin_op::le:
    case bin_op::eq:
      return true;
    default:
      return false;
    }
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
    const auto name = parse_expr(l);
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
    return { function_call { new expr(*name), args } };
  }

  nonstd::optional<make_expr> parse_make_expr(lexer &l) {
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "make");
    l.next();
    const auto type_name = parse_type_ident(l);
    if (!type_name) {
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }

    std::vector<std::pair<nonstd::string_view, expr>> body;
    while ((l.peek() && l.peek()->val != ")") || !l.peek()) {
      const auto name = l.next()->val;
      ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "the rest of the struct initialisation");
      if (l.peek()->val == ")") {
        report_error(l.get_curr_source_label(), "Unexpected end of make expression");
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      const auto expr_opt = parse_expr(l);
      if (!expr_opt) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      body.push_back(std::make_pair(name, *expr_opt));
    }

    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, ")");
    l.next();
    return { { *type_name, body } };
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
      } else if (l.peek()->val == "make") {
        const auto e = parse_make_expr(l);
        if (!e) { return nonstd::nullopt; }
        else { return { { sl, *e } }; };
      } else if (l.peek()->val == "let" || l.peek()->val == "mut") {
        const auto expr_type = l.next()->val;
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "symbol name");
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
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "symbol name");
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
        return { { sl, set_expr { symbol { name }, new expr(*e) } } };
      } else {
        const auto function_call = parse_function_call(l);
        if (!function_call) { return nonstd::nullopt; }
        return { { sl, *function_call } };
      }
    } else if (l.peek()->type == token_type::ident) {
      return { { sl, symbol { l.next()->val } } };
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
                     [&](bin_op_expr x)       {
                       return std::string("bin_op_expr(") + x.lchild->to_string()
                         + "," + x.rchild->to_string() + ")";
                     },
                     [&](un_op_expr x)        { return std::string("un_op_expr"); },
                     [&](symbol x)          { return std::string("symbol(") + std::string(x.val) + ")"; },
                     [&](int_lit x)           { return std::string("int_lit"); },
                     [&](float_lit x)         { return std::string("float_lit"); },
                     [&](string_lit x)        { return std::string("string_lit"); },
                     [&](c_string_lit x)      { return std::string("c_string_lit"); },
                     [&](if_expr x)           { return std::string("if_expr"); },
                     [&](while_expr x)        { return std::string("while_expr"); },
                     [&](mut_expr x)          { return std::string("mut_expr"); },
                     [&](let_expr x)          { return std::string("let_expr"); },
                     [&](set_expr x)          { return std::string("set_expr"); },
                     [&](make_expr x)         { return std::string("make_expr"); },
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
    const auto lval = lchild->code_gen(s, b, nullptr);
    if (!lval) { return nonstd::nullopt; }
    const auto lchild_type = *lchild->get_type(s);
    const auto rval = rchild->code_gen(s, b, &lchild_type);
    if (!rval) { return nonstd::nullopt; }
    switch (op) {
    case bin_op::add: return { b.CreateAdd(*lval, *rval) };
    case bin_op::sub: return { b.CreateSub(*lval, *rval) };
    case bin_op::mul: return { b.CreateMul(*lval, *rval) };
    case bin_op::div: return { b.CreateUDiv(*lval, *rval) };
    case bin_op::lt: return { b.CreateICmpULT(*lval, *rval) };
    case bin_op::le: return { b.CreateICmpULE(*lval, *rval) };
    case bin_op::gt: return { b.CreateICmpUGT(*lval, *rval) };
    case bin_op::ge: return { b.CreateICmpUGE(*lval, *rval) };
    case bin_op::eq:
      nonstd::optional<llvm::Value*> v = { b.CreateICmpEQ(*lval, *rval) };
      return v;
    }
    assert(false);
  }

  nonstd::optional<llvm::Value*> field_access_expr::code_gen(const source_label &sl,
                                                             scope &s, llvm::IRBuilder<> &b) const {
    assert(target->val.template is<symbol>() && "We only support field access on symbols");
    // First, get the target type
    const auto target_type = target->get_type(s);
    if (!target_type) { return nonstd::nullopt; }
    if (!target_type->val.template is<struct_type>()) {
      report_error(sl, "Can't get a field on a var which is not a struct or a pointer to a struct");
      return nonstd::nullopt;
    }
    const auto data = target_type->val.template get<struct_type>().data;
    assert(data);
    // Find field
    int found_field = data->find_field_index_with_name(field_name);
    if (found_field < 0) {
      report_error(sl, std::string("Can't find field '") + std::string(field_name) + "'");
      return nonstd::nullopt;
    }
    // Codegen the target so we have a llvm::Value* to index
    auto target_llvm_val = target->code_gen(s, b, nullptr);
    if (!target_llvm_val) { return nonstd::nullopt; }
    if (target_type->num_ptr == 1) {
      // Create a GEP and a load
      const auto gep = b.CreateStructGEP(*target_llvm_val, (unsigned)found_field);
      return { b.CreateLoad(gep) };
    } else if (target_type->num_ptr == 0) {
      // Create an extractvalue, since this is a value type
      const auto found_field_heap = new unsigned((unsigned)found_field);
      return { b.CreateExtractValue(*target_llvm_val, { *found_field_heap }) };
    } else {
      report_error(sl, "Can't index a field transparently through more than 1 level of indirection");
      return nonstd::nullopt;
    }
  }

  nonstd::optional<llvm::Value*> symbol::find_as_var(const source_label &sl, const scope &s,
                                                    llvm::IRBuilder<> &b, bool deref_ptr) const {
    if (val == "this") {
      const auto this_ptr = s.get_this_ptr();
      if (!this_ptr) { return nonstd::nullopt; }
      else { return this_ptr->val; }
    }
    const auto n_value = s.find_symbol_with_type(val, named_value_type::var);
    if (!n_value) {
      report_error(sl, std::string("Undefined variable '") + std::string(val) + "'");
      return nonstd::nullopt;
    }
    const auto &v = n_value->val.template get<var>();
    if (deref_ptr) {
      return { v.is_pointer() ? b.CreateLoad(v.val) : v.val };
    } else {
      assert(v.is_pointer());
      return { v.val };
    }
  }

  const function* symbol::find_as_function(const source_label &sl, const scope &s) const {
    const auto n_value = s.find_symbol_with_type(val, named_value_type::function);
    if (!n_value) {
      report_error(sl, std::string("Undefined function '") + std::string(val) + "'");
      return nullptr;
    }
    return &n_value->val.template get<function>();
  }


  nonstd::optional<llvm::Value*> make_expr::code_gen(const source_label &sl, scope &s,
                                                     llvm::IRBuilder<> &b) const {
    const auto val_type = type_name.code_gen(s);
    if (!val_type) { return nonstd::nullopt; }
    if (!val_type->val.template is<struct_type>()) {
      report_error(sl, std::string("Cannot make type '") + type_name.to_string()
                   + "', as it is not a struct");
      return nonstd::nullopt;
    }
    const auto struct_data = val_type->val.template get<struct_type>().data;
    assert(struct_data);
    const auto llvm_type = val_type->to_llvm_type(s, b.getContext());
    llvm::Value* curr_val = llvm::UndefValue::get(llvm_type);
    // Pre-allocate the indices for the arrayrefs we're passing tollvm
    unsigned* indices = new unsigned[field_assignments.size()];
    unsigned curr_ix = 0;
    for (const auto &name_value_pair : field_assignments) {
      const auto &name = name_value_pair.first;
      const auto &val = name_value_pair.second;
      const auto field_ix = struct_data->find_field_index_with_name(name);
      if (field_ix < 0) { // Check field actually exists
        report_error(sl, std::string("Cannot find field '") + std::string(name)
                     + "' in struct type " + type_name.to_string());
        return nonstd::nullopt;
      }
      const auto field_type = struct_data->get_field_with_index(field_ix).get_type(s);
      assert(field_type);
      const auto llvm_val = val.code_gen(s, b, &*field_type);
      if (!llvm_val) { return nonstd::nullopt; }
      // Store the index on the heap so we can make an arrayref to it
      indices[curr_ix] = field_ix;
      curr_val = b.CreateInsertValue(curr_val, *llvm_val,
                                     llvm::ArrayRef<unsigned>(indices[curr_ix]));
      curr_ix += 1;
    }
    return curr_val;
  }

  nonstd::optional<llvm::Value*> expr::code_gen(scope &s, llvm::IRBuilder<> &b, const type* expected_type) const {
    auto &llvm_ctx = b.getContext();
    nonstd::optional<llvm::Value*> res = nonstd::nullopt;
    if (val.template is<bin_op_expr>()) {
      res = val.template get<bin_op_expr>().code_gen(sl, s, b);
    } else if (val.template is<symbol>()) {
      res = val.template get<symbol>().find_as_var(sl, s, b);
    } else if (val.template is<make_expr>()) {
      res = val.template get<make_expr>().code_gen(sl, s, b);
    } else if (val.template is<let_expr>()) {
      const auto &e = val.template get<let_expr>();
      const auto e_type = e.type.code_gen(s);
      if (!e_type) { return nonstd::nullopt; }
      const auto e_val = e.val->code_gen(s, b, &*e_type);
      if (!e_val) { return nonstd::nullopt; }
      assert(e_type.has_value());
      assert(e_val.has_value());
      s.symbol_table.insert(std::make_pair(e.var.val, named_value { var { *e_type, *e_val, 0 } }));
      res = *e_val;
    } else if (val.template is<mut_expr>()) {
      const auto &e = val.template get<mut_expr>();
      const auto e_type = e.type.code_gen(s);
      if (!e_type) { return nonstd::nullopt; }
      const auto e_val = e.val->code_gen(s, b, &*e_type);
      if (!e_val) { return nonstd::nullopt; }
      // Allocate space
      const auto alloca = b.CreateAlloca(e_type->to_llvm_type(s, llvm_ctx));
      // Store location in symbol table
      s.symbol_table.insert(std::make_pair(e.var.val, named_value {
            var { *e_type, alloca, var::FLAGS_MUT } }));
      b.CreateStore(*e_val, alloca);
      res = alloca;
    } else if (val.template is<set_expr>()) {
      const auto &e = val.template get<set_expr>();
      // Find the var & gen the expr value
      // TODO optimize this unneeded symbol lookup
      const auto n_value = s.find_symbol_with_type(e.var.val, named_value_type::var);
      const auto e_var = e.var.find_as_var(sl, s, b, false);
      if (!e_var) { return nonstd::nullopt; }
      const auto e_val = e.val->code_gen(s, b, &n_value->val.template get<var>().var_type);
      if (!e_val) { return nonstd::nullopt; }
      res = b.CreateStore(*e_val, *e_var);
    } else if (val.template is<field_access_expr>()) {
      res = val.template get<field_access_expr>().code_gen(sl, s, b);
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
      const auto cond_val_ = e.cond->code_gen(s, b, &builtin_ty_bool);
      assert(cond_val_);
      b.CreateCondBr(*cond_val_, body_block, end_block);
      prev_block->getParent()->getBasicBlockList().push_back(body_block);
      b.SetInsertPoint(body_block);
      for (const auto &body_expr : e.body) {
        body_expr.code_gen(body_scope, b, nullptr);
      }
      b.CreateBr(body_cond_block);
      prev_block->getParent()->getBasicBlockList().push_back(end_block);
      b.SetInsertPoint(end_block);
      res = { llvm::Constant::getNullValue(llvm::Type::getDoubleTy(llvm_ctx)) };

    } else if (val.template is<if_expr>()) {
      const auto &e = val.template get<if_expr>();
      const auto prev_block = b.GetInsertBlock();
      assert(prev_block->getParent());
      auto true_block = llvm::BasicBlock::Create(llvm_ctx, "true_block", prev_block->getParent());
      auto false_block = llvm::BasicBlock::Create(llvm_ctx, "false_block");
      const auto end_block = llvm::BasicBlock::Create(llvm_ctx, "end_block");

      // Generate condition
      const auto cond_val = e.cond->code_gen(s, b, &builtin_ty_bool);
      b.CreateCondBr(*cond_val, true_block, false_block);

      // Generate true / false code
      // True
      b.SetInsertPoint(true_block);
      auto true_scope = s.create_subscope();
      const auto true_val = e.true_expr->code_gen(true_scope, b, nullptr);
      if (!true_val) { return nonstd::nullopt; }
      b.CreateBr(end_block);
      true_block = b.GetInsertBlock();
      // False
      prev_block->getParent()->getBasicBlockList().push_back(false_block);
      b.SetInsertPoint(false_block);
      auto false_scope = s.create_subscope();
      const auto true_expr_type = *e.true_expr->get_type(s);
      const auto false_val = e.false_expr->code_gen(false_scope, b, &true_expr_type);
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

      res = { phi };
    } else if (val.template is<function_call>()) {
      const auto &fc = val.template get<function_call>();
      // Get function
      const auto f = fc.name->find_as_function(s);
      if (!f) { return nonstd::nullopt; }
      nonstd::optional<llvm::Value*> receiver = nonstd::nullopt;
      if (f->is_method()) {
        receiver = fc.name->find_receiver(s, b);
        assert(receiver);
      }
      const auto llvm_function = f->get_cached_llvm_function();
      assert(llvm_function);

      if (f->args.size() != fc.arg_list.size() && !f->is_var_args()) {
        report_error(sl, std::string("Expected ") + std::to_string(f->args.size()) + " args to call '"
                     + std::string(fc.name->to_string()) + "', but " + std::to_string(fc.arg_list.size())
                     + " args were supplied.");
        return nonstd::nullopt;
      }

      // Eval all exprs
      llvm::Value** args = new llvm::Value*[fc.arg_list.size() + (receiver ? 1 : 0)];
      if (receiver) {
        args[0] = *receiver;
      } // Add receiver (this*)
      for (unsigned ii = 0; ii < fc.arg_list.size(); ++ii) {
        type* arg_type = nullptr;
        if (f->args.size() > ii) {
          auto arg_type_opt = f->args[ii].type.code_gen(s);
          if (!arg_type_opt) { return nonstd::nullopt; }
          auto &arg_type_unwrap = *arg_type_opt;
          arg_type = &arg_type_unwrap;
        }
        const auto expr_opt = fc.arg_list[ii].code_gen(s, b, arg_type);
        if (!expr_opt) {
          return nonstd::nullopt;
        }
        args[ii + (receiver ? 1 : 0)] = *expr_opt;
      }

      res = b.CreateCall(llvm_function,
                         llvm::ArrayRef<llvm::Value*>(args, fc.arg_list.size()
                                                      + (receiver ? 1 : 0)));
    } else if (val.template is<string_lit>()) {
      res = { val.template get<string_lit>().code_gen(s, b) };
    } else if (val.template is<int_lit>()) {
      res = { llvm::ConstantInt::get(llvm::Type::getInt64Ty(b.getContext()),
                                      llvm::APInt(val.template get<int_lit>().val)) };
    } else if (val.template is<c_string_lit>()) {
      const auto &s = val.template get<c_string_lit>().val;
      res = b.CreateGlobalStringPtr(llvm::StringRef(s.begin(), s.size()));
    } else {
      report_error(sl, std::string("Code gen not implemented for this type of expr: ") + to_string());
      return nonstd::nullopt;
    }
    if (res) {
      const auto this_type = *get_type(s);
      if (expected_type && this_type != *expected_type) {
        // Try coerce types
        const auto coerced_type = this_type.coerce(s, b, *res, *expected_type);
        if (!coerced_type) {
          report_error(sl, std::string("Expected type ") +
                       expected_type->to_str() + ", got type " + this_type.to_str()
                       + ", and coercing these types is not supported.");
        }
        return coerced_type;
      } else {
        return *res;
      }
    } else { return nonstd::nullopt; }
  }

  nonstd::optional<type> expr::get_type(const scope& s) const {
    if (val.template is<bin_op_expr>()) {
      // Assume lchild is the same type as rchild
      const auto bop = val.template get<bin_op_expr>();
      if (bop.is_bool_expr()) { return { builtin_ty_bool }; }
      return val.template get<bin_op_expr>().lchild->get_type(s);
    } else if (val.template is<symbol>()) {
      if (val.template get<symbol>().val == "this") {
        const auto v = s.get_this_ptr();
        if (!v) {
          report_error(sl, std::string("Cannot find 'this' - is this a member function?"));
          return nonstd::nullopt;
        }
        return v->var_type;
      }
      const auto v = s.find_symbol_with_type(val.template get<symbol>().val, named_value_type::var);
      if (!v) {
        report_error(sl, std::string("Cannot find symbol with name '")
                     + std::string(val.template get<symbol>().val) + "'");
        return nonstd::nullopt;
      }
      return { v->val.template get<var>().var_type };
    } else if (val.template is<make_expr>()) {
      return val.template get<make_expr>().type_name.code_gen(s);
    } else if (val.template is<let_expr>()) {
      return val.template get<let_expr>().type.code_gen(s);
    } else if (val.template is<if_expr>()) {
      // Assume true_val is the same type as false_val
      return val.template get<if_expr>().true_expr->get_type(s);
    } else if (val.template is<int_lit>()) {
      const auto &x = val.template get<int_lit>().val;
      // TODO figure out precision ?
      if (x.isIntN(8)) { return { builtin_ty_u8 }; }
      else if (x.isIntN(16)) { return { builtin_ty_u16 }; }
      else if (x.isIntN(32)) { return { builtin_ty_u32 }; }
      else if (x.isIntN(64)) { return { builtin_ty_u64 }; }
      else { assert(false && "Int lit won't fit, insert proper error here"); }
    } else if (val.template is<function_call>()) {
      const auto f = val.template get<function_call>().name->find_as_function(s);
      if (!f) { return nonstd::nullopt; };
      return f->return_type.code_gen(s);
    } else if (val.template is<float_lit>()) {
      // TODO figure out precision ?
      return { builtin_ty_f32 };
    } else if (val.template is<string_lit>()) {
      return { builtin_ty_slice_u8 };
    } else if (val.template is<c_string_lit>()) {
      return { builtin_ty_u8.ptr(1) };
    } else if (val.template is<mut_expr>()) {
      return { builtin_ty_void };
    } else if (val.template is<set_expr>()) {
      return { builtin_ty_void };
    } else if (val.template is<while_expr>()) {
      return { builtin_ty_void };
    } else if (val.template is<field_access_expr>()) {
      const auto target_type = val.template get<field_access_expr>().target->get_type(s);
      if (!target_type) { return nonstd::nullopt; }
      if (!(target_type->val.template is<struct_type>())) {
        report_error(sl, std::string("Trying to access field of non-struct type: ")
                     + target_type->to_str());
        return nonstd::nullopt;
      }
      const auto struct_data = target_type->val.template get<struct_type>().data;
      assert(struct_data);
      const auto field_name = val.template get<field_access_expr>().field_name;
      const auto field = struct_data->find_field_with_name(field_name);
      if (!field) {
        report_error(sl, std::string("Unknown field name ") + std::string(field_name)
                     + " in type " + target_type->to_str());
        return nonstd::nullopt;
      }
      return field->field_type.code_gen(s);
    }
    std::cerr << "UNIMPLEMENTED: get_type(" << to_string() << ")" << std::endl;
    assert(false);
  }

  const function* expr::find_as_function(const scope &s) const {
    if (val.template is<field_access_expr>()) {
      // Make sure this is a struct method, then return the associated function
      const auto &fae = val.template get<field_access_expr>();
      // First, get the struct type
      const auto target = fae.target->get_type(s);
      if (!target) { return nullptr; }
      if (!target->val.template is<struct_type>()) {
        report_error(sl, std::string("Trying to get method on type '") + target->to_str()
                     + "', which is not a struct type.");
        return nullptr;
      }
      const auto &stype = target->val.template get<struct_type>();
      assert(stype.data);
      const auto f = stype.data->find_method_with_name(fae.field_name);
      if (!f) {
        report_error(sl, std::string("No method '") + std::string(fae.field_name)
                     + "', on type '" + std::string(stype.name) + "'");
        return nullptr;
      }
      return f;
    } else if (val.template is<symbol>()) {
      return val.template get<symbol>().find_as_function(sl, s);
    } else {
      report_error(sl, std::string("'") + to_string() + "' is not a function");
      return nullptr;
    }
  }

  nonstd::optional<llvm::Value*> expr::find_receiver(scope &s, llvm::IRBuilder<> &b) const {
    if (!val.template is<field_access_expr>()) {
      return nonstd::nullopt;
    }
    const auto &fae = val.template get<field_access_expr>();
    assert (fae.target->val.template is<symbol>() && "Only support getting receiver for symbols!");
    const auto &sym = fae.target->val.template get<symbol>();
    auto sym_var_opt = s.find_symbol_with_type(sym.val, named_value_type::var);
    if (!sym_var_opt) {
      report_error(sl, std::string("Cannot find receiver for symbol ") + std::string(sym.val));
      return nonstd::nullopt;
    }
    auto &sym_var = sym_var_opt->val.template get<var>();
    if (sym_var.is_pointer()) {
      // Is alreaedy a pointer
      return { sym_var.val };
    } else {
      // Allocate, then return a pointer to that allocation.
      const auto alloca = b.CreateAlloca(sym_var.var_type.to_llvm_type(s, b.getContext()));
      b.CreateStore(sym_var.val, alloca);
      // Make sure we set_allocated to true, indicating that in the future if we
      // want to get the address of this var, we can just use this allocation,
      // rather than re-allocating.
      sym_var.val = alloca;
      sym_var.set_allocated(true);
      return { alloca };
    }
  }

  bool expr::is_pointer(const scope& s) const {
    assert(val.template is<symbol>() && "Not implemented for non-symbol values");
    if (val.template get<symbol>().val == "this") {
      // FIXME We're assuming we can't take the address of the pointer to this - is this an issue?
      return false;
    }
    const auto nv = s.find_symbol_with_type(val.template get<symbol>().val, named_value_type::var);
    assert(nv);
    return nv->val.template get<var>().is_pointer();
  }
}
