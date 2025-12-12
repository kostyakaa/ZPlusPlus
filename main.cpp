#include "lexer/lexer.h"
#include <iostream>

const char* TokenTypeName(TokenType t) {
  switch (t) {
    case TokenType::END_OF_FILE: return "END_OF_FILE";
    case TokenType::UNKNOWN: return "UNKNOWN";

    case TokenType::IDENTIFIER: return "IDENTIFIER";
    case TokenType::INT_LITERAL: return "INT_LITERAL";
    case TokenType::DOUBLE_LITERAL: return "DOUBLE_LITERAL";
    case TokenType::BOOL_LITERAL: return "BOOL_LITERAL";
    case TokenType::CHAR_LITERAL: return "CHAR_LITERAL";
    case TokenType::STRING_LITERAL: return "STRING_LITERAL";

    case TokenType::KW_IF: return "KW_IF";
    case TokenType::KW_ELSE: return "KW_ELSE";
    case TokenType::KW_FOR: return "KW_FOR";
    case TokenType::KW_WHILE: return "KW_WHILE";
    case TokenType::KW_DO: return "KW_DO";
    case TokenType::KW_BREAK: return "KW_BREAK";
    case TokenType::KW_CONTINUE: return "KW_CONTINUE";
    case TokenType::KW_RETURN: return "KW_RETURN";

    case TokenType::KW_INT: return "KW_INT";
    case TokenType::KW_DOUBLE: return "KW_DOUBLE";
    case TokenType::KW_BOOL: return "KW_BOOL";
    case TokenType::KW_CHAR: return "KW_CHAR";
    case TokenType::KW_STRING: return "KW_STRING";

    case TokenType::LPAREN: return "LPAREN";
    case TokenType::RPAREN: return "RPAREN";
    case TokenType::LBRACE: return "LBRACE";
    case TokenType::RBRACE: return "RBRACE";
    case TokenType::LBRACKET: return "LBRACKET";
    case TokenType::RBRACKET: return "RBRACKET";
    case TokenType::COMMA: return "COMMA";
    case TokenType::SEMICOLON: return "SEMICOLON";
    case TokenType::DOT: return "DOT";

    case TokenType::ASSIGN: return "ASSIGN";
    case TokenType::PLUS: return "PLUS";
    case TokenType::MINUS: return "MINUS";
    case TokenType::STAR: return "STAR";
    case TokenType::SLASH: return "SLASH";
    case TokenType::PERCENT: return "PERCENT";

    case TokenType::OROR: return "OROR";
    case TokenType::ANDAND: return "ANDAND";
    case TokenType::OR: return "OR";
    case TokenType::XOR: return "XOR";
    case TokenType::AND: return "AND";
    case TokenType::BANG: return "BANG";
    case TokenType::TILDE: return "TILDE";

    case TokenType::EQ: return "EQ";
    case TokenType::NEQ: return "NEQ";
    case TokenType::LT: return "LT";
    case TokenType::GT: return "GT";
    case TokenType::LE: return "LE";
    case TokenType::GE: return "GE";

    case TokenType::SHL: return "SHL";
    case TokenType::SHR: return "SHR";
  }
  return "UNKNOWN";
}

int main() {
  std::string src = R"(
i wanna dance i wanna dance in the lights
i wanna rock i wanna rock ur body
meow meow meow meow
come on
)";
  Lexer lex(src);
  auto tokens = lex.Tokenize();

  for (const auto &t : tokens) {
    std::cout << TokenTypeName(t.type) << " '" << t.text << "' @" << t.line << ":"
              << t.col << "\n";
  }
}