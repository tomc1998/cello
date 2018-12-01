#include "scope.hpp"


namespace cello {
  scope scope::create_subscope() {
    return { this, {} };
  }

  const named_value* scope::find_symbol_with_type(const nonstd::string_view name,
                                            const named_value_type type) const {
    const auto res = symbol_table.find(name);
    if (res == symbol_table.end()) {
      if (parent) {
        return parent->find_symbol_with_type(name, type);
      } else {
        return nullptr;
      }
    }
    const auto nv = &res->second;
    if (nv->type == type) {
      return nv;
    } else if (parent) {
      return parent->find_symbol_with_type(name, type);
    } else {
      return nullptr;
    }
  }

  named_value* scope::find_symbol_with_type(const nonstd::string_view name,
                                            const named_value_type type) {
    const auto res = symbol_table.find(name);
    if (res == symbol_table.end()) {
      if (parent) {
        return parent->find_symbol_with_type(name, type);
      } else {
        return nullptr;
      }
    }
    auto nv = &res->second;
    if (nv->type == type) {
      return nv;
    } else if (parent) {
      return parent->find_symbol_with_type(name, type);
    } else {
      return nullptr;
    }
  }

}
