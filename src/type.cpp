#include "type.hpp"
#include <ostream>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

namespace cello {

  llvm::Type* int_type::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    return llvm::Type::getIntNTy(c, num_bytes * 8);
  }


  llvm::Type* struct_type::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    assert(data);
    std::vector<llvm::Type*> fields;
    for (const auto &f : data->fields) {
      fields.push_back(f.to_llvm_type(s, c));
    }
    return llvm::StructType::get(c, {fields});
  }

  llvm::Type* struct_field::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    return type.code_gen(s)->to_llvm_type(s, c);
  }

  llvm::Type* type::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    const auto inner = val.match([&] (int_type x) { return x.to_llvm_type(s, c); },
                                 [&] (void_type x) { return llvm::Type::getVoidTy(c); },
                                 [&] (struct_type x) { return x.to_llvm_type(s, c); });
    if (num_ptr == 0) { return inner; }
    auto curr_type = llvm::PointerType::getUnqual(inner);
    for (int ii = 1; ii < num_ptr; ++ii) {
      curr_type = llvm::PointerType::getUnqual(curr_type);
    }
    return curr_type;
  }

  std::ostream& operator<<(std::ostream& o, const struct_type& s) {
    o << s.name << "{";
    if (s.data) {
      for (const auto &f : s.data->fields) {
        o << f.name << ": " << f.type.to_string() << ",";
      }
      return o << "}";
    } else {
      return o << "NO ATTACHED DATA}";
    }
  }
}
