#include "ast_function.hpp"
#include <llvm/IR/Type.h>
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

    return { { name, *return_type_opt, *arg_list_opt, expr_list, flags } };
  };

  bool function::is_extern() const {
    return (flags & function::FLAGS_IS_EXTERN) != 0;
  }

  nonstd::optional<llvm::FunctionType*>
  function::to_llvm_function_type(const scope& s, llvm::LLVMContext &c) const {
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
    const auto ft = llvm::FunctionType::get(resolved_return_type->to_llvm_type(s, c),
                                            arg_types, false);
    return { ft };
  }

}
