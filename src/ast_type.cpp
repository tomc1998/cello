#include "ast_type.hpp"
#include "lexer.hpp"
#include "error.hpp"
#include <nonstd/optional.hpp>

namespace cello {
  ptr_type::ptr_type(const ptr_type& other) : val(new type_ident(*other.val)) {}

  nonstd::optional<type_ident> parse_type_ident(lexer &l) {
    if (l.peek() && l.peek()->type == token_type::ident) {
      return { { type_symbol { l.next()->val } } };
    } else if (l.peek() && l.peek()->val == "(") {
      report_error(l.get_curr_source_label(),
                   "Unimplemented type parsing");
      return nonstd::nullopt;
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
}
