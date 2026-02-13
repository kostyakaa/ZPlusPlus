#include <iostream>
#include <string>
#include <fstream>
#include <sstream>

#include "lexer/lexer.h"
#include "parser/parser.h"
#include "semanter/semanter.h"

int main() {
  const std::string inputPath = "../input.zpp";
  std::ifstream inputFile(inputPath);
  if (!inputFile.is_open()) {
    std::cerr << "Failed to open input file: " << inputPath << std::endl;
    return 1;
  }

  std::ostringstream buffer;
  buffer << inputFile.rdbuf();
  const std::string src = buffer.str();

  try {
    Lexer lexer(src);

    Parser parser(lexer);
    parser.ParseProgram();

    Lexer semLexer(src);
    SemanticAnalyzer sem(semLexer.Tokenize());
    sem.Analyze();

    std::cout << "Parse + semantic OK\n";
  } catch (const std::exception &e) {
    std::cerr << e.what() << std::endl;
    return 1;
  }

  return 0;
}
