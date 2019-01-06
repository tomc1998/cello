#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include "ast_util.hpp"
#include "ast_expr.hpp"
#include "lexer.hpp"
#include "error.hpp"
#include <nonstd/optional.hpp>
#include <cassert>
#include <llvm/IR/IRBuilder.h>

namespace cello {
  struct arg {
    nonstd::string_view name;
    type_ident type;
  };

  class function {
  public:
    constexpr static char FLAGS_IS_EXTERN   = 0b00000001;
    constexpr static char FLAGS_IS_VAR_ARGS = 0b00000010;

    /** If this function is a method, this will be the struct's type. */
    nonstd::optional<type> this_type;

    /** Returns true if the function body has already been generated, and
    calling to_llvm_function will just return the cached function */
    bool is_function_already_generated() const;
    /** Does this belong to a struct */
    inline bool is_method() const { return this_type.has_value(); }
    bool is_extern() const;
    bool is_var_args() const;
    nonstd::optional<llvm::Function*>
    /**
       @param mangle - Override the function name if tihs is not nullopt. This
       will NOT re-mangle if a cached function is found. Will assert if mangle
       != nullopt and there is a cached function.
       Should be valid forever (i.e., just leak the memory, since I have no idea
       about the lifetime requirements on LLVM's side -.-)
     */
    to_llvm_function(scope& s, llvm::IRBuilder<> &b, llvm::Module* module,
                     nonstd::optional<nonstd::string_view> mangle = nonstd::nullopt);
    /** asserts is_function_already_generated() */
    llvm::Function* get_cached_llvm_function() const;

    nonstd::string_view name;
    type_ident return_type;
    std::vector<arg> args;
    /** Size = 0 if extern - see flags */
    std::vector<expr> expressions;
    /**
       bit 0 - FLAGS_IS_EXTERN
       bit 1 - FLAGS_IS_VAR_ARGS
    */
    char flags;

    function(nonstd::string_view name, type_ident return_type, std::vector<arg> args,
             std::vector<expr> expressions, char flags)
      : this_type(nonstd::nullopt), name(name), return_type(return_type), args(args),
        expressions(expressions), flags(flags) {};

  private:
    llvm::Function* cached_function = nullptr;
    /** Stored separately just in case we only need to generate a functiontype (?) */
    llvm::FunctionType* cached_function_type = nullptr;
    nonstd::optional<llvm::FunctionType*>
    to_llvm_function_type(const scope& s, llvm::LLVMContext &c);
  };

  nonstd::optional<arg> parse_arg(lexer &l);
  nonstd::optional<std::vector<arg>> parse_arg_list(lexer &l);
  nonstd::optional<function> parse_function(lexer &l);
}
