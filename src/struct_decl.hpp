/** Controls parsing decls for structs */
#pragma once

#include <nonstd/optional.hpp>

#include "ast_util.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "type.hpp"
#include "type.hpp"

namespace cello {
  /** Expecting the tokenizer's 'next' to be the 'struct' token.
      @return - A new struct_type, with a newly allocated struct_data field.
   */
  inline nonstd::optional<struct_type> parse_struct_type(lexer& l) {
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

    // Get all types
    while (l.peek() && l.peek()->val != ")") {
      const auto field_name = l.next()->val;
      const auto field_type = parse_type_ident(l);
      if (!field_type) {
        CONSUME_TO_END_PAREN_OR_ERROR(l);
        return nonstd::nullopt;
      }
      struct_field f = struct_field {field_name, *field_type};
      s.data->fields.push_back(f);
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
