#pragma once

#include "source_label.hpp"

#include <string>
#include <nonstd/optional.hpp>
#include <nonstd/string_view.hpp>

namespace cello {
  enum class token_type {
    float_lit, int_lit, string_lit, c_string_lit, punc, ident, comment
  };

  struct token {
    nonstd::string_view val;
    token_type type;
  };

  class lexer {
    std::uint32_t curr_line = 1;
    std::size_t input_ptr = 0;
    nonstd::string_view file_name;
    std::string input;
    nonstd::optional<token> curr_peek;
    /** Actually consume a token */
    nonstd::optional<token> consume();
    /** Just consume & throw away whitespace */
    void consume_whitespace();
  public:
    lexer(nonstd::string_view file_name, std::string data) : file_name(file_name), input(data) {}
    const nonstd::optional<token> &peek();
    nonstd::optional<token> next();
    source_label get_curr_source_label();
    nonstd::string_view get_remaining_input();

    /** Used for handling errors properly */
    void backup();
    /** Used for handling errors properly - asserts if no lexer state */
    void restore();
  };
}
