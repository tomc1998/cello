#include <iostream>
#include <fstream>

#include "type.hpp"
#include "comptime_expr.hpp"
#include "code_gen.hpp"
#include "ast_function.hpp"
#include "lexer.hpp"
#include "arg_parse.hpp"

using namespace cello;

int main(int argc, const char** argv) {
  const auto args = parse_prog_arg_list(argc, argv);
  if (args.num_positional() == 0) {
    std::cerr << "Please pass a file to compile" << std::endl;
    return 1;
  } else if (args.num_positional() > 1) {
    std::cerr << "2 positional args passed - compiler only supports compiling 1 file" << std::endl;
    return 1;
  }
  auto file_name = std::string(args.get_positional(0).name);
  std::ifstream t(file_name);
  if (!t.good()) {
    std::cerr << "Can't read file '" << file_name << "'" << std::endl;
    return 1;
  }
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
