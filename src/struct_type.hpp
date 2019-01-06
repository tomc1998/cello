/** Unfortunately we need this file because of c++'s retarded #include system,
got to sort out those circular imports! */

#pragma once

#include <nonstd/string_view.hpp>
#include "type_ident.hpp"
#include "type.hpp"

namespace cello {

  struct struct_field;
  struct function;

  /** The container for the actual fields of a struct */
  struct struct_data {
    std::vector<struct_field> fields;
    std::vector<function> methods;
    const struct_field* find_field_with_name(nonstd::string_view field_name) const;
    const function* find_method_with_name(nonstd::string_view name) const;
    /** Returns -1 if field is not found */
    int find_field_index_with_name(nonstd::string_view field_index) const;
    /** Finds the field with the given index - this is the index retrieved from
        find_field_index_with_name. Asserts if OOB. */
    const struct_field& get_field_with_index(unsigned ix) const;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
  };

  struct struct_field {
    nonstd::string_view name;
    type_ident field_type;
    llvm::Type* to_llvm_type(const scope& s, llvm::LLVMContext &c) const;
    /** Convert this to a type (rather than just type_ident)
        TODO Maybe cache this in the struct_field? */
    nonstd::optional<type> get_type(const scope& s) const;
  };

}
