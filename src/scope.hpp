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
    static constexpr char FLAGS_MUT       = 0b00000001;
    static constexpr char FLAGS_MEMBER    = 0b00000010;
    static constexpr char FLAGS_ALLOCATED = 0b00000100;
    /** The type of this variable */
    type var_type;
    /** If is_mutable() OR is_allocated(), this will be a pointer (to a stack
        variable), and will need to be loaded */
    llvm::Value* val;
    /**
       Bit 0 - FLAGS_MUT
       Bit 1 - FLAGS_MEMBER - True if this is a member variable and requires
       chasing up 'this'
       Bit 2 - FLAGS_ALLOCATED - True if this has already been allocated with
       llvm's alloca - this is for when you have non-mutable variables that you
       need to get the address of. To get the address, first alloca, then store
       the value - this bit is then set to true and the val is set to the
       pointer to the memory. If is_mutable, this bit is undefined.

       @note - Modify these through the set_X functions!
     */
    char flags = 0;

    /** If val is a pointer to memory, not a register value */
    inline bool is_pointer() const { return is_mutable() || is_allocated(); }
    inline bool is_mutable() const { return (flags & FLAGS_MUT) != 0; }
    inline bool is_member() const { return (flags & FLAGS_MEMBER) != 0; }
    inline bool is_allocated() const { return (flags & FLAGS_ALLOCATED) != 0; }
    inline void set_mutable(bool val) { flags |= val ? FLAGS_MUT : 0; }
    inline void set_member(bool val) { flags |= val ? FLAGS_MEMBER : 0; }
    inline void set_allocated(bool val) {flags |= val ? FLAGS_ALLOCATED : 0;}
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
