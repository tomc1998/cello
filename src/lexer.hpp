#pragma once

#include <string>
#include <nonstd/optional.hpp>
#include <nonstd/string_view.hpp>

namespace cello {
  enum class token_type {
    float_lit, int_lit, string_lit, punc, ident, comment
  };

  struct token {
    nonstd::string_view val;
    token_type type;
  };

  class lexer {
    std::size_t input_ptr = 0;
    std::string input;
    nonstd::optional<token> curr_peek;
    /** Actually consume a token */
    nonstd::optional<token> consume();
    /** Just consume & throw away whitespace */
    void consume_whitespace();
  public:
    lexer(std::string data) : input(data) {}
    const nonstd::optional<token> &peek();
    nonstd::optional<token> next();
  };
}
