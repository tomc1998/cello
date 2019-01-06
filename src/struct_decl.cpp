#include "struct_decl.hpp"
#include "struct_type.hpp"
#include "type.hpp"

namespace cello {
  nonstd::optional<struct_type> parse_struct_type(lexer& l) {
    assert(l.next()->val == "struct");
    struct_type s;
    s.data = new struct_data {};

    // Get name
    if (!l.peek()) {
      report_error(l.get_curr_source_label(), "Expected struct name");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
    s.name = l.next()->val;

    // Get all types and methods
    while (l.peek() && l.peek()->val != ")") {
      if (l.peek()->val == "(") { // Function ?
        l.next();
        ASSERT_TOK_EXISTS_OR_ERROR_AND_RET(l, "Method declaration");
        if (l.peek() && l.peek()->val == "fn") { // Function!
          const auto f = parse_function(l);
          if (!f) { CONSUME_TO_END_PAREN_OR_ERROR(l); return nonstd::nullopt; }
          s.data->methods.push_back(*f);
          continue;
        } else {
          report_error(l.get_curr_source_label(), "Expected method declaration field, found '("
                       + std::string(l.peek()->val) + " ...'");
          CONSUME_TO_END_PAREN_OR_ERROR(l);
          return nonstd::nullopt;
        }
      }
      const auto field_name = l.next()->val;
      const auto field_type = parse_type_ident(l);
      if (!field_type) { CONSUME_TO_END_PAREN_OR_ERROR(l); return nonstd::nullopt; }
      struct_field f = struct_field {field_name, *field_type};
      s.data->fields.push_back(f);
    }

    // Add this to all member functions
    for (auto &m : s.data->methods) {
      m.this_type.emplace(type { struct_type { s }, 0 });
    }

    if (l.peek() && l.peek()->val == ")") {
      l.next();
      return { s };
    } else {
      report_error(l.get_curr_source_label(), "Expected ')' to end the struct type declaration");
      CONSUME_TO_END_PAREN_OR_ERROR(l);
      return nonstd::nullopt;
    }
  }
}
