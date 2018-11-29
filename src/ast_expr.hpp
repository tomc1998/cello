#pragma once

#include "ast_type.hpp"
#include <llvm/ADT/APInt.h>
#include <memory>
#include <nonstd/optional.hpp>
#include <nonstd/string_view.hpp>

namespace cello {
  class lexer;
  struct expr;

  enum class bin_op {
    add, sub, div, mul
  };

  struct function_call {};

  struct bin_op_expr {
    bin_op op;
    std::unique_ptr<expr> lchild;
    std::unique_ptr<expr> rchild;
    bin_op_expr(const bin_op_expr &other);
    bin_op_expr(bin_op op, expr* lchild,
                expr* rchild) : op(op), lchild(lchild), rchild(rchild) {}
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

  struct set_var_expr {
    variable var;
    type_ident type;
    std::unique_ptr<expr> val;
    set_var_expr(const set_var_expr &other);
  };

  struct set_expr {
    variable var;
    std::unique_ptr<expr> val;
    set_expr(const set_expr &other) noexcept;
  };

  struct expr {
    mapbox::util::variant<function_call, bin_op_expr, un_op_expr, variable, int_lit,
                 float_lit, string_lit, set_var_expr, set_expr> val;
    std::string to_string() const;
  };

  nonstd::optional<expr> parse_expr(lexer &l);
}
