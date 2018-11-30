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
    const scope* parent = nullptr;
    std::map<nonstd::string_view, named_value> symbol_table;
    scope create_subscope() const;

    /** Get a type with a name */
    nonstd::optional<llvm::Type*> get_llvm_type(nonstd::string_view name) const;
  };
}
