#include <iostream>
#include <fstream>

#include "ast_type.hpp"
#include "comptime_expr.hpp"
#include "code_gen.hpp"
#include "ast_function.hpp"
#include "lexer.hpp"

using namespace cello;

int main(int argc, char** argv) {
  auto file_name = "example-tests/test-00.cel";
  std::ifstream t(file_name);
  std::string file((std::istreambuf_iterator<char>(t)),
                   std::istreambuf_iterator<char>());

  lexer l(file_name, file);

  cello::code_gen(l);
  if (print_all_errors()) {
    return 1;
  } else {
    return 0;
  }
}
