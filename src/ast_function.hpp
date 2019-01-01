#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include "ast_util.hpp"
#include "ast_expr.hpp"
#include "lexer.hpp"
#include "error.hpp"
#include "type.hpp"
#include <nonstd/optional.hpp>
#include <cassert>

namespace cello {
  struct arg {
    nonstd::string_view name;
    type_ident type;
  };

  struct function {
    constexpr static char FLAGS_IS_EXTERN = 1;

    nonstd::string_view name;
    type_ident return_type;
    std::vector<arg> args;
    /** Size = 0 if extern - see flags */
    std::vector<expr> expressions;
    /**
       bit 0 - FLAGS_IS_EXTERN
     */
    char flags;

    bool is_extern() const;
    nonstd::optional<llvm::FunctionType*>
    to_llvm_function_type(const scope& s, llvm::LLVMContext &c) const;
    /** Returns nullptr if is extern */
    nonstd::optional<llvm::Function*>
    to_llvm_function(const scope& s, llvm::LLVMContext &c) const;
  };

  nonstd::optional<arg> parse_arg(lexer &l);
  nonstd::optional<std::vector<arg>> parse_arg_list(lexer &l);
  nonstd::optional<function> parse_function(lexer &l);
}
