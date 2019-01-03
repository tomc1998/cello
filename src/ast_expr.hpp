#pragma once

#include "type.hpp"
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

  enum class bin_op {
    add, sub, div, mul, gt, ge, lt, le, eq
  };

  struct bin_op_expr {
    bin_op op;
    std::unique_ptr<expr> lchild;
    std::unique_ptr<expr> rchild;
    bin_op_expr(const bin_op_expr &other);
    bin_op_expr(bin_op op, expr* lchild,
                expr* rchild) : op(op), lchild(lchild), rchild(rchild) {}
    nonstd::optional<llvm::Value*> code_gen(const source_label &sl, scope &s, llvm::IRBuilder<> &b) const;
    bool is_bool_expr() const;
  };

  struct un_op_expr {};
  struct variable {
    nonstd::string_view val;
    /**
       @param deref_mut=true - If set to false, don't generate a load
       instruction for mutable variables. This is useful when querying the
       variable location.
       This function will assert if deref_mut is false, but the variable is
       immutable (as the chances are this is a logic error on the compiler's
       part).
     */
    nonstd::optional<llvm::Value*> code_gen(const source_label& sl, scope &s,
                                            llvm::IRBuilder<> &b, bool deref_mut=true) const;
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

  /** Access an element on a struct. This is effectively just a GEP. */
  struct field_access_expr {
    std::unique_ptr<expr> target;
    nonstd::string_view field_name;
    field_access_expr(expr* target, nonstd::string_view field_name)
      : target(target), field_name(field_name) {}
    field_access_expr(const field_access_expr& other);
    nonstd::optional<llvm::Value*> code_gen(scope &s, llvm::IRBuilder<> &b) const;
  };

  struct mut_expr {
    variable var;
    type_ident type;
    std::unique_ptr<expr> val;
    mut_expr(const mut_expr &other);
    mut_expr(variable var, type_ident type, expr* val)
      : var(var), type(type), val(val) {};
  };

  /** Declare an immutable value */
  struct let_expr {
    variable var;
    type_ident type;
    std::unique_ptr<expr> val;
    let_expr(const let_expr &other) noexcept;
    let_expr(variable var, type_ident type, expr* val)
      : var(var), type(type), val(val) {};
  };

  struct function_call {
    nonstd::string_view name;
    std::vector<expr> arg_list;
  };

  struct set_expr {
    variable var;
    std::unique_ptr<expr> val;
    set_expr(const set_expr &other) noexcept;
    set_expr(variable var, expr* val)
      : var(var), val(val) {};
  };

  struct expr {
    source_label sl;
    mapbox::util::variant<function_call, bin_op_expr, un_op_expr, variable, int_lit,
                          float_lit, string_lit, c_string_lit, mut_expr, set_expr, let_expr,
                          while_expr, if_expr, field_access_expr> val;
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
  };

  nonstd::optional<expr> parse_expr(lexer &l);
}
