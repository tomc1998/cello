#pragma once

#include "lexer.hpp"

/**
 * Assert a given condition - if the condition is false, report an error with
 * the given label and message, then return nonstd::nullopt.
 * requires that error.hpp and nonstd/optional.hpp are in scope.
 */
#define ASSERT_OR_ERROR_AND_RET(cond, label, msg) \
  if (!(cond)) {                                  \
    report_error(label, msg);                     \
    return nonstd::nullopt;                       \
  }

/**
 * Assert that a given token value is next - if the condition is false, report
 * an error with the given label and message, then return nonstd::nullopt.
 * requires that error.hpp and nonstd/optional.hpp are in scope.
 */
#define ASSERT_TOK_VAL_OR_ERROR_AND_RET(lexer, exp)       \
  ASSERT_OR_ERROR_AND_RET(lexer.peek(),                   \
                          lexer.get_curr_source_label(),  \
                          "Expected '('");                \
  ASSERT_OR_ERROR_AND_RET(lexer.peek()->val == exp,       \
                          lexer.get_curr_source_label(),  \
                          "Expected '" exp "'");

/**
 * Assert that a given token value is next - if the condition is false, report
 * an error with the given label and message, then return nonstd::nullopt.
 * requires that error.hpp and nonstd/optional.hpp are in scope.
 */
#define ASSERT_TOK_TYPE_OR_ERROR_AND_RET(lexer, exp, human_readable_exp) \
  ASSERT_OR_ERROR_AND_RET(lexer.peek(),                                 \
                          lexer.get_curr_source_label(),                \
                          "Expected '('");                              \
  ASSERT_OR_ERROR_AND_RET(lexer.peek()->type == exp,                    \
                          lexer.get_curr_source_label(),                \
                          "Expected " human_readable_exp);

namespace cello {

  /** Consume until the next closing paren, taking into account opening parens.
   *  The lexer should be INSIDE the exp to consume the end of - NOT at the start.
   *
   *  Here's an example:
   *  If the lexer is at position X:
   *  ( ( X ( ) ( ) ) )
   *  Then this function advances the lexer to:
   *  ( ( ( ) ( ) ) X )
   *
   *  Returns false if reached EOF without finding the closing brace.
   */
  inline bool consume_to_closing_paren(lexer &l) {
    int level = 0;
    while(l.peek()) {
      const auto v = l.next()->val;
      if (v == ")") {
        if (level-- == 0) { return true; }
      } else if (v == "(") { level++; }
    }
    return false;
  }
}
