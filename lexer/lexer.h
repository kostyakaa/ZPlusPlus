// Lexer.h
#pragma once

#include <array>
#include <string>
#include <vector>

// ===================== Token =====================
enum class TokenType {
  // special
  END_OF_FILE,
  UNKNOWN,

  // identifiers & literals
  IDENTIFIER,
  INT_LITERAL,
  DOUBLE_LITERAL,
  BOOL_LITERAL,
  CHAR_LITERAL,
  STRING_LITERAL,

  // keywords / types
  KW_IF,
  KW_ELSE,
  KW_FOR,
  KW_WHILE,
  KW_DO,
  KW_BREAK,
  KW_CONTINUE,
  KW_RETURN,

  KW_INT,
  KW_DOUBLE,
  KW_BOOL,
  KW_CHAR,
  KW_STRING,

  // punctuation
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACKET,
  RBRACKET,
  COMMA,
  SEMICOLON,
  DOT,

  // operators
  ASSIGN, // =
  PLUS,
  MINUS, // + -
  STAR,
  SLASH,
  PERCENT, // * / %

  OROR,
  ANDAND, // || &&
  OR,
  XOR,
  AND, // | ^ &
  BANG,
  TILDE, // ! ~

  EQ,
  NEQ, // == !=
  LT,
  GT,
  LE,
  GE, // < > <= >=
  SHL,
  SHR // << >>
};

struct Token {
  TokenType type{};
  std::string text;
  int line{};
  int col{};
};

// ===================== Trie (Бор) =====================
// ASCII trie: быстрый, простой, подходит для ключевых слов/операторов.
class Trie {
public:
  Trie();

  void Insert(const std::string &s, int value);
  // Точное совпадение всей строки
  int MatchExact(const std::string &s) const;

  // Самое длинное совпадение начиная с позиции pos в src:
  // возвращает value (или -1), а outLen = длина совпадения
  int MatchLongestFrom(const std::string &src, size_t pos,
                       size_t &outLen) const;

private:
  struct Node {
    std::array<int, 128> next{};
    int value; // -1 если не терминал
    Node();
  };

  std::vector<Node> nodes_;
};

// ===================== Lexer =====================
class Lexer {
public:
  explicit Lexer(std::string src);

  std::vector<Token> Tokenize();

  Token NextToken();
  Token PeekToken();

private:
  void InitTries();

  bool IsEnd() const;
  char Peek(int offset = 0) const;
  char Get();

  static bool IsLetter(char c);
  static bool IsDigit(char c);

  void SkipWhitespaceAndComments();

  [[nodiscard]] Token Make(TokenType type, const std::string &text, int line, int col) const;
  [[nodiscard]] Token EndOfFile() const;

  Token IdentifierOrKeywordOrBool();
  Token Number();
  Token CharLiteral();
  Token StringLiteral();
  Token OperatorOrPunctOrUnknown();

  std::string src_;
  size_t pos_{0};
  int line_{1};
  int col_{1};

  Trie keyword_trie_;
  Trie op_trie_;

  bool has_buffered_token_{false};
  Token buffered_token_{};
};
