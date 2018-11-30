#include "scope.hpp"


namespace cello {
  scope scope::create_subscope() const {
    return { this, {} };
  }

  nonstd::optional<llvm::Type*> scope::get_llvm_type(nonstd::string_view name) const {
    const auto res = symbol_table.find(name);
    if (res == symbol_table.end()) {
      if (parent) {
        return parent->get_llvm_type(name);
      } else {
        return nonstd::nullopt;
      }
    }
    const named_value& nv = res->second;
    if (nv.type == named_value_type::type) {
      return (llvm::Type*) nv.v;
    } else if (parent) {
      return parent->get_llvm_type(name);
    } else {
      return nonstd::nullopt;
    }
  }
}
