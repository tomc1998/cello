#include "ast_function.hpp"
#include "scope.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>
#include <vector>

namespace cello {

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
    ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "function declaration");
    bool is_extern = false;
    if (l.peek()->val == "extern") {
      is_extern = true;
      l.next();
    }
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
      if (is_extern) {
        report_error(l.get_curr_source_label(), "Function body found for extern function");
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      const auto e = parse_expr(l);
      if (!e) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      expr_list.push_back(*e);
    }
    if (!found_closing_paren) {
      report_error(sl, "Unmatched paren");
      return nonstd::nullopt;
    }

    // Or together all the flag values
    char flags = is_extern ? function::FLAGS_IS_EXTERN : 0;

    return { function { name, *return_type_opt, *arg_list_opt, expr_list, flags } };
  };

  bool function::is_extern() const {
    return (flags & function::FLAGS_IS_EXTERN) != 0;
  }

  bool function::is_function_already_generated() const {
    return cached_function;
  }

  llvm::Function* function::get_cached_llvm_function() const {
    assert(cached_function);
    return cached_function;
  }

  nonstd::optional<llvm::Function*>
  function::to_llvm_function(scope& s, llvm::IRBuilder<> &b, llvm::Module* module) {
    auto &llvm_ctx = b.getContext();
    if (cached_function) { return { cached_function }; }
    const auto ft_opt = to_llvm_function_type(s, llvm_ctx);
    if (!ft_opt) { return nonstd::nullopt; }
    const auto ft = *ft_opt;
    const auto llvm_name = llvm::StringRef(name.begin(), name.size());
    cached_function =
      llvm::Function::Create(ft, llvm::Function::ExternalLinkage,
                             llvm_name, module);
    if (is_extern()) { return {cached_function}; }

    // Create scope and add args to scope
    auto function_scope = s.create_subscope();
    for (unsigned ii = 0; ii < args.size(); ++ii) {
      const auto arg_name = args[ii].name;
      const auto arg_type = args[ii].type.code_gen(s);
      if (!arg_type) { return nonstd::nullopt; }
      const auto arg_value = cached_function->arg_begin() + ii;
      const named_value nv { var { *arg_type, arg_value } };
      function_scope.symbol_table.insert(std::make_pair(arg_name, nv));
    }

    // Codegen exprs
    bool has_errored = false;
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(llvm_ctx, "entry", cached_function);
    b.SetInsertPoint(bb);
    for (unsigned ii = 0; ii < expressions.size(); ++ii) {
      const auto &e = expressions[ii];
      const auto value_opt = e.code_gen(function_scope, b);
      if (!value_opt) { has_errored = true; continue; }
      if (ii == expressions.size() - 1) {
        b.CreateRet(*value_opt);
      }
    }

    if (!has_errored) {
      cached_function->print(llvm::errs());
    } else {
      print_all_errors();
      assert(false && "CRITICAL ERROR: There were errors in codegen.");
    }

    return {cached_function};
  }

  nonstd::optional<llvm::FunctionType*>
  function::to_llvm_function_type(const scope& s, llvm::LLVMContext &c) {
    if (cached_function_type) { return { cached_function_type }; }
    // Code gen for this function
    std::vector<llvm::Type*> arg_types;
    for (const auto &a : args) {
      const auto arg_type = a.type.code_gen(s);
      if (!arg_type) { return nonstd::nullopt; }
      if (arg_type->val.template is<struct_type>() && arg_type->num_ptr == 0) {
        // Always take structs by pointer (copy at call-site, allow for
        // move optimisations)
        arg_types.push_back(llvm::PointerType::getUnqual(arg_type->to_llvm_type(s, c)));
      } else {
        arg_types.push_back(arg_type->to_llvm_type(s, c));
      }
    }
    const auto resolved_return_type = return_type.code_gen(s);
    if (!resolved_return_type) { return nonstd::nullopt; }
    cached_function_type = llvm::FunctionType::get(resolved_return_type->to_llvm_type(s, c),
                                            arg_types, false);
    return { cached_function_type };
  }

}
