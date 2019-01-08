#include "scope.hpp"


namespace cello {
  scope scope::create_subscope() {
    return { this, {} };
  }

  bool named_value::is_of_type(const named_value_type &t) const {
    switch (t) {
    case named_value_type::type:     return val.template is<type>();
    case named_value_type::var:      return val.template is<var>();
    case named_value_type::function: return val.template is<function>();
    }
    assert(false);
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
    if (nv->is_of_type(type)) {
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
    if (nv->is_of_type(type)) {
      return nv;
    } else if (parent) {
      return parent->find_symbol_with_type(name, type);
    } else {
      return nullptr;
    }
  }

  const named_value* scope::find_symbol(const nonstd::string_view name) const {
    const auto res = symbol_table.find(name);
    if (res == symbol_table.end()) {
      if (parent) {
        return parent->find_symbol(name);
      } else {
        return nullptr;
      }
    } else {
      return &res->second;
    }
  }

  named_value* scope::find_symbol(const nonstd::string_view name)  {
    const auto res = symbol_table.find(name);
    if (res == symbol_table.end()) {
      if (parent) {
        return parent->find_symbol(name);
      } else {
        return nullptr;
      }
    } else {
      return &res->second;
    }
  }

  const var* scope::get_this_ptr() const {
    if (this_ptr) { return &*this_ptr; }
    else if (parent) { return parent->get_this_ptr(); }
    else { return nullptr; }
  }

  llvm::Value* var::address_of(const scope& s, llvm::IRBuilder<> &b) {
    if (is_pointer()) {
      // Is alreaedy a pointer
      return val;
    } else {
      // Allocate, then return a pointer to that allocation.
      const auto alloca = b.CreateAlloca(var_type.to_llvm_type(s, b.getContext()));
      b.CreateStore(val, alloca);
      // Make sure we set_allocated to true, indicating that in the future if we
      // want to get the address of this var, we can just use this allocation,
      // rather than re-allocating.
      val = alloca;
      set_allocated(true);
      return alloca;
    }
  }
}
