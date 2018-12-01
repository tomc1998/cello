#include "type.hpp"
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

namespace cello {

  llvm::Type* int_type::to_llvm_type(llvm::LLVMContext &c) const {
    return llvm::Type::getIntNTy(c, num_bytes * 8);
  }

  llvm::Type* type::to_llvm_type(llvm::LLVMContext &c) const {
    const auto inner = val.match([&] (int_type x) { return x.to_llvm_type(c); },
                                 [&] (void_type x) { return llvm::Type::getVoidTy(c); });
    if (num_ptr == 0) { return inner; }
    auto curr_type = llvm::PointerType::getUnqual(inner);
    for (int ii = 1; ii < num_ptr; ++ii) {
      curr_type = llvm::PointerType::getUnqual(curr_type);
    }
    return curr_type;
  }

}
