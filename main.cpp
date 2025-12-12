#include <iostream>
#include <string>

#include "lexer/lexer.h"
#include "parser/parser.h"

int main() {
  // Тестовый код на твоём языке
  std::string src = R"(
int g = 42;

int f(int a, bool b) {
  int x = 10;
  int y;
  y = g + x * 2;

  if (y > 10) {
    y = y - 1;
  } else {
    y = y + 1;
  }

  for (int i = 0; i < 5; i = i + 1) {
    y = y + i;
  }

  while (y > 0) {
    y = y - 1;
  }

  do {
    x = x + 1;
  } while (x < 20);

  return y;
}
)";

  try {
    Lexer lexer(src);
    Parser parser(lexer);

    parser.ParseProgram();

    std::cout << "Parse OK\n";
  } catch (const std::exception &e) {
    std::cerr << "Parse error: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
