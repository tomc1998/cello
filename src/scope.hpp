#pragma once

#include <nonstd/string_view.hpp>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <map>
#include <nonstd/optional.hpp>

namespace cello {

  enum class named_value_type { type, var, function };

  struct named_value {
    named_value_type type;
    llvm::Value* v;
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
