#include <iostream>
#include <fstream>

int main(int argc, char** argv) {
  std::ifstream t("example-tests/test-00.cel");
  std::string file((std::istreambuf_iterator<char>(t)),
                   std::istreambuf_iterator<char>());
  std::cout << file << std::endl;
}
