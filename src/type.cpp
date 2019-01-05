#include "type.hpp"
#include <ostream>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>

namespace cello {

  llvm::Type* float_type::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    if (num_bytes == 4) {
      return llvm::Type::getFloatTy(c);
    } else if (num_bytes == 8) {
      return llvm::Type::getDoubleTy(c);
    } else {
      assert(false && "Unreachable");
    }
  }

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
    return field_type.code_gen(s)->to_llvm_type(s, c);
  }

  llvm::Type* type::to_llvm_type(const scope& s, llvm::LLVMContext &c) const {
    const auto inner = val.match([&] (int_type x) { return x.to_llvm_type(s, c); },
                                 [&] (float_type x) { return x.to_llvm_type(s, c); },
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
        o << f.name << ": " << f.field_type.to_string() << ",";
      }
      return o << "}";
    } else {
      return o << "NO ATTACHED DATA}";
    }
  }

  std::string type::to_str() const {
    return val.match([&] (int_type x) {
        return std::string(x.is_signed ? "i" : "u") + std::to_string(x.num_bytes * 8);
      },
      [&] (float_type x) { return std::string("f") + std::to_string(x.num_bytes * 8); },
      [&] (void_type x) { return "void"; },
      [&] (struct_type x) { return std::string("struct") + std::string(x.name); });
  }

  type type::ptr(int levels) const {
    assert (levels > 0);
    type ret(*this);
    ret.num_ptr += levels;
    return ret;
  }

  type_ident type_ident::ptr() const {
    return { source_label(), ptr_type(new type_ident(*this)) };
  }

  nonstd::optional<llvm::Value*> type::coerce(const scope& s, llvm::IRBuilder<> &b,
                                            llvm::Value* from, const type& target) const {
    // INT TO FLOAT
    if (val.template is<int_type>() && target.val.template is<float_type>()) {
      if (val.template get<int_type>().is_signed) {
        return { b.CreateSIToFP(from, target.to_llvm_type(s, b.getContext()), "cast-i-to-fp") };
      } else {
        return { b.CreateUIToFP(from, target.to_llvm_type(s, b.getContext()), "cast-u-to-fp") };
      }
    }
    // UPCAST INT
    else if (val.template is<int_type>() && target.val.template is<int_type>()) {
      const auto from_type = val.template get<int_type>();
      const auto to_type = target.val.template get<int_type>();
      if (from_type.is_signed == to_type.is_signed && from_type.num_bytes < to_type.num_bytes) {
        if (from_type.is_signed) {
          return { b.CreateSExt(from, to_type.to_llvm_type(s, b.getContext())) };
        } else {
          return { b.CreateZExt(from, to_type.to_llvm_type(s, b.getContext())) };
        }
      }
    }
    return nonstd::nullopt;
  }

  bool void_type::operator==(const void_type& other) const {return true;}
  bool int_type::operator==(const int_type& other) const {
    return num_bytes == other.num_bytes && is_signed == other.is_signed;
  }
  bool float_type::operator==(const float_type& other) const {
    return num_bytes == other.num_bytes;
  }
  bool struct_type::operator==(const struct_type& other) const {
    // TODO Is this sufficient? (namespacing?)
    return name == other.name;
  }

  bool type::operator==(const type& other) const {
    return val == other.val && num_ptr == other.num_ptr;
  }

  bool type::operator!=(const type& other) const {
    return !(*this == other);
  }

  const struct_field* struct_data::find_field_with_name(nonstd::string_view field_name) const {
    for (const auto &f : fields) {
      if (f.name == field_name) {
        return &f;
      }
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
}
