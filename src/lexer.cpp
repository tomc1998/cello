#include "lexer.hpp"

#include <regex>
#include <exception>

std::regex r_ident = std::regex("[A-Za-z\\/,\\.\\+\\*#'`@_\\-][0-9A-Za-z\\/,\\.\\+\\*#'`@_\\-]*");
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
      input_ptr ++;
    }
  }

  nonstd::optional<token> lexer::consume() {
    consume_whitespace();
    if (input_ptr >= input.size()) { return nonstd::nullopt; }
    std::cmatch m;
    auto curr_str = input.c_str() + input_ptr;
    nonstd::optional<token> tok = nonstd::nullopt;
    if (std::regex_search(curr_str, m, r_comment, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::comment };
      input_ptr += m.length();
    } else if (std::regex_search(curr_str, m, r_punc, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::punc };
      input_ptr += m.length();
    } else if (std::regex_search(curr_str, m, r_string_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::string_lit };
      input_ptr += m.length();
    } else if (std::regex_search(curr_str, m, r_int_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::int_lit };
      input_ptr += m.length();
    } else if (std::regex_search(curr_str, m, r_float_lit, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::float_lit };
      input_ptr += m.length();
    } else if (std::regex_search(curr_str, m, r_ident, std::regex_constants::match_continuous)) {
      tok = token { nonstd::string_view (curr_str, m.length()), token_type::ident };
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
}
