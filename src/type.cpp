#include "type.hpp"
#include <iostream>
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

namespace cello {

  llvm::Type* int_type::to_llvm_type(llvm::LLVMContext &c) const {
    return llvm::Type::getIntNTy(c, num_bytes * 8);
  }

  llvm::Type* type::to_llvm_type(llvm::LLVMContext &c) const {
    const auto inner = val.match([&] (int_type x) { return x.to_llvm_type(c); },
                                 [&] (void_type x) { return llvm::Type::getVoidTy(c); },
                                 [&] (struct_type stype) {
                                   struct_data* x = stype.val;
                                   std::cout << "Got struct data " << x << std::endl;
                                   if (x->cached_llvm_type) { return x->cached_llvm_type; }
                                   assert(x->llvm_elements.size() == 0);
                                   // Create the llvm type
                                   x->cached_llvm_type = llvm::StructType::create(c);
                                   for (const auto &field : x->fields) {
                                     const auto &field_type = field.second;
                                     x->llvm_elements.push_back(field_type.to_llvm_type(c));
                                   }
                                   x->cached_llvm_type->setBody(x->llvm_elements);
                                   return x->cached_llvm_type;
                                 });
    assert(inner);
    if (num_ptr == 0) { return inner; }
    auto curr_type = llvm::PointerType::getUnqual(inner);
    for (int ii = 1; ii < num_ptr; ++ii) {
      curr_type = llvm::PointerType::getUnqual(curr_type);
    }
    return curr_type;
  }

  type type::ptr(int levels) const {
    assert (levels > 0);
    type ret(*this);
    ret.num_ptr += levels;
    return ret;
  }
}
