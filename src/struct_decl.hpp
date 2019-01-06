/** Controls parsing decls for structs */
#pragma once

#include <nonstd/optional.hpp>

#include "ast_function.hpp"
#include "ast_util.hpp"
#include "error.hpp"
#include "lexer.hpp"
#include "type.hpp"
#include "type.hpp"

namespace cello {
  /** Expecting the tokenizer's 'next' to be the 'struct' token.
      @return - A new struct_type, with a newly allocated struct_data field.
   */
  nonstd::optional<struct_type> parse_struct_type(lexer& l);
}
