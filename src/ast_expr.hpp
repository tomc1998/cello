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

  struct function_call {};

  struct bin_op_expr {
    bin_op op;
    std::unique_ptr<expr> lchild;
    std::unique_ptr<expr> rchild;
    bin_op_expr(const bin_op_expr &other);
    bin_op_expr(bin_op op, expr* lchild,
                expr* rchild) : op(op), lchild(lchild), rchild(rchild) {}
    nonstd::optional<llvm::Value*> code_gen(const source_label &sl, scope &s, llvm::IRBuilder<> &b) const;
  };

  struct un_op_expr {};
  struct variable {
    nonstd::string_view val;
  };
  struct int_lit {
    llvm::APInt val;
  };
  struct float_lit {};
  struct string_lit {};

  struct mut_expr {
    variable var;
    type_ident type;
    std::unique_ptr<expr> val;
    mut_expr(const mut_expr &other);
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

  struct set_expr {
    variable var;
    std::unique_ptr<expr> val;
    set_expr(const set_expr &other) noexcept;
  };

  struct expr {
    source_label sl;
    mapbox::util::variant<function_call, bin_op_expr, un_op_expr, variable, int_lit,
                          float_lit, string_lit, mut_expr, set_expr, let_expr, if_expr> val;
    std::string to_string() const;
    /** Build this expression, returning the value of the expression as an LLVM
        value. Returns nullopt on error. */
    nonstd::optional<llvm::Value*> code_gen(scope &s, llvm::IRBuilder<> &b) const;
    /** Get the type of this expression */
    nonstd::optional<type> get_type(const scope& s) const;
  };

  nonstd::optional<expr> parse_expr(lexer &l);
}
