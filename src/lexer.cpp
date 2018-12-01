#include "lexer.hpp"
#include "source_label.hpp"

#include <algorithm>
#include <cstdint>
#include <regex>
#include <exception>

std::regex r_ident = std::regex("[A-Za-z<>=\\/,\\.\\+\\*#'`@_\\-][0-9A-Za-z\\/,\\.\\+\\*#'`@_\\-]*");
std::regex r_punc = std::regex("(\\(|\\))");
std::regex r_float_lit = std::regex("[-+]?[0-9]\\.[0-9]");
std::regex r_int_lit = std::regex("(0b|0x)?[0-9]+");
std::regex r_string_lit = std::regex("\"(\\.|[^\"\\\\])*\"");
std::regex r_comment = std::regex(";.*$");


namespace cello {

  inline bool is_whitespace(char c) {
    return c == ' ' || c == '\n' || c == '\r' || c == '\t';
  }

  void lexer::consume_whitespace() {
    while (input_ptr < input.size() && is_whitespace(input[input_ptr])) {
      if (input[input_ptr] == '\n') { curr_line ++; }
      input_ptr ++;
    }
  }

  nonstd::optional<token> lexer::consume() {
    consume_whitespace();
    if (input_ptr >= input.size()) { return nonstd::nullopt; }
    std::cmatch m;
    const char* start = input.c_str() + input_ptr;
    const char* end = input.c_str() + input.size();
    nonstd::optional<token> tok = nonstd::nullopt;
    if (std::regex_search(start, end, m, r_comment, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::comment };
      input_ptr += m.length();
    } else if (std::regex_search(start, end, m, r_punc, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::punc };
      input_ptr += m.length();
    } else if (std::regex_search(start, end, m, r_string_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::string_lit };
      input_ptr += m.length();
    } else if (std::regex_search(start, end, m, r_int_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::int_lit };
      input_ptr += m.length();
    } else if (std::regex_search(start, end, m, r_float_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::float_lit };
      input_ptr += m.length();
    } else if (std::regex_search(start, end, m, r_ident, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (start, m.length()), token_type::ident };
      input_ptr += m.length();
    } else {
      throw std::runtime_error(std::string("Failed to match token input: ")
                               + input.substr(input_ptr, std::min(input_ptr + 20, input.size()))
                               + "...");
    }
    return tok;
  }

  const nonstd::optional<token> &lexer::peek() {
    if (curr_peek) { return curr_peek; }
    curr_peek = consume();
    return curr_peek;
  }

  nonstd::optional<token> lexer::next() {
    if (curr_peek) {
      auto res = curr_peek;
      curr_peek = nonstd::nullopt;
      return res;
    } else {
      return consume();
    }
  }

  source_label lexer::get_curr_source_label() {
    // Loop back, find line start
    auto pos = input_ptr;
    while(true) {
      if (input[pos] == '\n' || pos == 0)  { break; }
      pos --;
    }
    auto char_pos = (std::uint32_t)(input_ptr - std::min((pos + 1), input_ptr));
    if (curr_peek) { char_pos -= curr_peek->val.size(); }
    return { file_name, curr_line, char_pos };
  }

  static nonstd::optional<lexer> lexer_copy = nonstd::nullopt;

  nonstd::string_view lexer::get_remaining_input() {
    if (curr_peek) {
      return { input.c_str() + (input_ptr - curr_peek->val.size()), input.size()
          - (input_ptr - curr_peek->val.size())};
    } else {
      return { input.c_str() + input_ptr, input.size() - input_ptr};
    }
  }

  void lexer::backup() {
    lexer_copy = { *this };
  }

  void lexer::restore() {
    assert(lexer_copy);
    *this = *lexer_copy;
  }
}
