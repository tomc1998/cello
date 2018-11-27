#include <iostream>
#include <fstream>

#include "lexer.hpp"

using namespace cello;

int main(int argc, char** argv) {
  std::ifstream t("example-tests/test-00.cel");
  std::string file((std::istreambuf_iterator<char>(t)),
                   std::istreambuf_iterator<char>());
  std::cout << file << std::endl;

  lexer l(file);
  std::cout << "TOKENS:" << std::endl;
  while (l.peek()) {
    std::cout << l.next()->val << std::endl;
  }
}
