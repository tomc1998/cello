#include "struct_type.hpp"
#include "ast_function.hpp"
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

namespace cello {

  llvm::Type* struct_field::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    return field_type.code_gen(s)->to_llvm_type(s, c);
  }


  const struct_field* struct_data::find_field_with_name(nonstd::string_view field_name) const {
    for (const auto &f : fields) {
      if (f.name == field_name) { return &f; }
    }
    return nullptr;
  }

  const function* struct_data::find_method_with_name(nonstd::string_view name) const {
    for (const auto &f : methods) {
      if (f.name == name) { return &f; }
    }
    return nullptr;
  }

  int struct_data::find_field_index_with_name(nonstd::string_view field_name) const {
    for (unsigned ii = 0; ii < fields.size(); ++ii) {
      if (fields[ii].name == field_name) {
        return (int)ii;
      }
    }
    return -1;
  }

  const struct_field& struct_data::get_field_with_index(unsigned ix) const {
    assert(ix < fields.size());
    return fields[ix];
  }

  nonstd::optional<type> struct_field::get_type(const scope& s) const {
    return field_type.code_gen(s);
  }

  llvm::Type* struct_data::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    std::vector<llvm::Type*> llvm_fields;
    for (const auto &f : fields) {
      llvm_fields.push_back(f.to_llvm_type(s, c));
    }
    return llvm::StructType::get(c, { llvm_fields });
  }

}
