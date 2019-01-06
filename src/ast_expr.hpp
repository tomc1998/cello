#pragma once

#include "type.hpp"
#include "type_ident.hpp"
#include "ast_control.hpp"
#include "source_label.hpp"
#include "ast_control.hpp"
#include <memory>
#include <nonstd/optional.hpp>
#include <nonstd/string_view.hpp>
#include <llvm/IR/IRBuilder.h>

namespace llvm {
  class Value;
}

namespace cello {
  class lexer;
  struct expr;
  struct scope;
  class function;

  enum class bin_op {
    add, sub, div, mul, gt, ge, lt, le, eq
  };

  struct bin_op_expr {
    bin_op op;
    expr* lchild;
    expr* rchild;
    bin_op_expr(const bin_op_expr &other);
    bin_op_expr(bin_op op, expr* lchild,
                expr* rchild) : op(op), lchild(lchild), rchild(rchild) {}
    nonstd::optional<llvm::Value*> code_gen(const source_label &sl, scope &s, llvm::IRBuilder<> &b) const;
    bool is_bool_expr() const;
  };

  struct un_op_expr {};
  /** A string which references some item in the codebase. */
  struct symbol {
    nonstd::string_view val;
    /**
       Generates instructions to load this as a variable (assuming this IS a
       variable). Reports error if this is a function or type.
       @param deref_mut=true - If set to false, don't generate a load
       instruction for mutable symbols. This is useful when querying the
       symbol location.
       This function will assert if deref_mut is false, but the symbol is
       immutable (as the chances are this is a logic error on the compiler's
       part).
    */
    nonstd::optional<llvm::Value*> find_as_var(const source_label& sl, const scope &s,
                                               llvm::IRBuilder<> &b, bool deref_mut=true) const;
    /** Find a function matching this symbol. Returns nullptr if not found. */
    const function* find_as_function(const source_label &sl, const scope &s) const;
  };

  struct int_lit {
    llvm::APInt val;
  };
  struct float_lit {};

  struct string_lit {
    nonstd::string_view val;
    llvm::Constant* code_gen(scope &s, llvm::IRBuilder<> &b) const;
  };

  /** Just a string literal with 'c' at the end to indicate this is a c string */
  struct c_string_lit {
    nonstd::string_view val;
  };

  /** Make a struct */
  struct make_expr {
    type_ident type_name;
    /** Maps names to initialisation exprs */
    std::vector<std::pair<nonstd::string_view, expr>> field_assignments;
    nonstd::optional<llvm::Value*> code_gen(const source_label &sl, scope &s, llvm::IRBuilder<> &b) const;
  };

  /** Access an element on a struct. This is effectively just a GEP. */
  struct field_access_expr {
    expr* target;
    nonstd::string_view field_name;
    field_access_expr(expr* target, nonstd::string_view field_name)
      : target(target), field_name(field_name) {}
    field_access_expr(const field_access_expr& other);
    nonstd::optional<llvm::Value*> code_gen(const source_label &sl,
                                            scope &s, llvm::IRBuilder<> &b) const;
  };

  struct mut_expr {
    symbol var;
    type_ident type;
    expr* val;
    mut_expr(const mut_expr &other);
    mut_expr(symbol var, type_ident type, expr* val)
      : var(var), type(type), val(val) {};
  };

  /** Declare an immutable value */
  struct let_expr {
    symbol var;
    type_ident type;
    expr* val;
    let_expr(const let_expr &other) noexcept;
    let_expr(symbol var, type_ident type, expr* val)
      : var(var), type(type), val(val) {};
  };

  struct function_call {
    /** The 'name' of the function */
    expr* name;
    std::vector<expr> arg_list;
  };

  struct set_expr {
    symbol var;
    expr* val;
    set_expr(const set_expr &other) noexcept;
    set_expr(symbol var, expr* val)
      : var(var), val(val) {};
  };

  struct expr {
    source_label sl;
    mapbox::util::variant<function_call, bin_op_expr, un_op_expr, symbol, int_lit,
                          float_lit, string_lit, c_string_lit, mut_expr, set_expr, let_expr,
                          while_expr, if_expr, field_access_expr, make_expr> val;
    std::string to_string() const;
    /**
       Build this expression, returning the value of the expression as an LLVM
       value. Returns nullopt on error.

       @param expected_type - The expected type. This will either generate an
       error or cast the result of this expression to the given type. If this
       is nullptr, no cast is performed.
    */
    nonstd::optional<llvm::Value*> code_gen(scope &s, llvm::IRBuilder<> &b,
                                            const type* expected_type) const;
    /** Get the type of this expression */
    nonstd::optional<type> get_type(const scope& s) const;

    /** Resolve this expression into a function. Returns nullptr and reports and
    errors if not possible. */
    const function* find_as_function(const scope &s) const;
    /** If this is a field access to a method, return the receiver as a llvm
    *Value. Returns nullopt if not a method call. */
    nonstd::optional<llvm::Value*> find_receiver(scope &s, llvm::IRBuilder<> &b) const;
  };

  nonstd::optional<expr> parse_expr(lexer &l);
}
