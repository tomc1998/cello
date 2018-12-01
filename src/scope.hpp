#pragma once

#include "type.hpp"
#include "ast_function.hpp"
#include <nonstd/string_view.hpp>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <map>
#include <nonstd/optional.hpp>
#include <mapbox/variant.hpp>

namespace cello {

  /** Used for selecting a symbol with a given type */
  enum class named_value_type { type, var, function };

  struct var {
    /** The type of this variable */
    type var_type;
    llvm::Value* val;
  };

  struct named_value {
    mapbox::util::variant<type, var, function> val;
    bool is_of_type(const named_value_type &t) const;
  };

  struct scope {
    scope* parent = nullptr;
    std::map<nonstd::string_view, named_value> symbol_table;
    scope create_subscope();

    /** Nullptr if not found */
    named_value* find_symbol_with_type(const nonstd::string_view name,
                                       const named_value_type type);
    /** Nullptr if not found */
    const named_value* find_symbol_with_type(const nonstd::string_view name,
                                       const named_value_type type) const;
  };
}
