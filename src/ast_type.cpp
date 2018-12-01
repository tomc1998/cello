#include <iostream>

#include <llvm/IR/DerivedTypes.h>
#include "ast_util.hpp"
#include "ast_type.hpp"
#include "lexer.hpp"
#include "error.hpp"
#include "scope.hpp"

namespace cello {
  ptr_type::ptr_type(const ptr_type& other) : val(new type_ident(*other.val)) {}

  nonstd::optional<type_ident> parse_type_ident(lexer &l) {
    const auto sl = l.get_curr_source_label();
    if (l.peek() && l.peek()->type == token_type::ident) {
      return { { sl, type_symbol { l.next()->val } } };
    } else if (l.peek() && l.peek()->val == "(") {
      l.next();
      if (l.peek() && l.peek()->val == "ptr") {
        l.next();
        const auto type = parse_type_ident(l);
        if (!type) {
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        if (!l.peek() || l.peek()->val != ")") {
          report_error(sl, "Expected ')' to end the ptr type declaration");
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
        l.next();
        return { { sl, ptr_type { new type_ident(*type) } } };
      } else {
        report_error(sl, "Expected 'ptr', for a ptr type declaration");
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
    } else {
      report_error(l.get_curr_source_label(),
                   "Expected type, starting with '(' or an identifier");
      return nonstd::nullopt;
    }
  };

  std::string type_ident::to_string() const {
    return val.match([&] (ptr_type x) { return std::string("*") + x.val->to_string(); },
                     [&] (type_symbol x) { return x.val.to_string(); },
                     [&] (type_function_call x) { return std::string("GENERIC_TYPE"); });
  }

  nonstd::optional<type> type_ident::code_gen(const scope& s) const {
    if (val.template is<type_symbol>()) {
      const auto v = val.template get<type_symbol>().val;
      const auto type_opt = s.find_symbol_with_type(v, named_value_type::type);
      if (!type_opt) {
        report_error(sl, std::string("Undefined typename ") + std::string(v));
        return nonstd::nullopt;
      }
      return { type_opt->val.template get<type>() };
    } else if (val.template is<ptr_type>()) {
      auto type = val.template get<ptr_type>().val->code_gen(s);
      if (!type) { return nonstd::nullopt; }
      type->num_ptr ++;
      return type;
    }
    assert(false);
  }
}
