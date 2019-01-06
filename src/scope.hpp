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
    static constexpr char FLAGS_MUT    = 0b00000001;
    static constexpr char FLAGS_MEMBER = 0b00000010;
    /** The type of this variable */
    type var_type;
    /** If is_mutable(), this will be a pointer (to a stack variable), and will
        need to be loaded */
    llvm::Value* val;
    /**
       Bit 0 - FLAGS_MUT
       Bit 1 - FLAGS_MEMBER - True if this is a member variable and requires
       chasing up 'this'
     */
    char flags = 0;

    inline bool is_mutable() const { return (flags & FLAGS_MUT) != 0; }
    inline bool is_member() const { return (flags & FLAGS_MEMBER) != 0; }
  };

  struct named_value {
    mapbox::util::variant<type, var, function> val;
    bool is_of_type(const named_value_type &t) const;
  };

  struct scope {
    scope* parent = nullptr;
    /** The current context's 'this' for methods */
    nonstd::optional<var> this_ptr = nonstd::nullopt;
    std::map<nonstd::string_view, named_value> symbol_table;
    scope create_subscope();

    /** Nullptr if not found */
    named_value* find_symbol_with_type(const nonstd::string_view name,
                                       const named_value_type type);
    /** Nullptr if not found */
    const named_value* find_symbol_with_type(const nonstd::string_view name,
                                       const named_value_type type) const;
    const named_value* find_symbol(const nonstd::string_view name) const;
    named_value* find_symbol(const nonstd::string_view name);
    /** nullptr if not in a method */
    const var* get_this_ptr() const;
  };
}
