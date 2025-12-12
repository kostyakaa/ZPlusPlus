#include "lexer.h"

#include <cctype>
#include <utility>

Trie::Node::Node() : value(-1) {
  next.fill(-1);
}

Trie::Trie() {
  nodes_.emplace_back();
}

void Trie::Insert(const std::string& s, int value) {
  int v = 0;
  for (unsigned char ch : s) {
    if (ch >= 128) return;
    int& to = nodes_[v].next[ch];
    if (to == -1) {
      to = (int)nodes_.size();
      nodes_.emplace_back();
    }
    v = to;
  }
  nodes_[v].value = value;
}

int Trie::MatchExact(const std::string& s) const {
  int v = 0;
  for (unsigned char ch : s) {
    if (ch >= 128) return -1;
    int to = nodes_[v].next[ch];
    if (to == -1) return -1;
    v = to;
  }
  return nodes_[v].value;
}

int Trie::MatchLongestFrom(const std::string& src, size_t pos, size_t& outLen) const {
  int v = 0;
  int lastVal = -1;
  size_t lastLen = 0;

  for (size_t i = pos; i < src.size(); ++i) {
    auto ch = static_cast<unsigned char>(src[i]);
    if (ch >= 128) break;
    int to = nodes_[v].next[ch];
    if (to == -1) break;

    v = to;
    if (nodes_[v].value != -1) {
      lastVal = nodes_[v].value;
      lastLen = (i - pos + 1);
    }
  }

  outLen = lastLen;
  return lastVal;
}

Lexer::Lexer(std::string src) : src_(std::move(src)) {
  InitTries();
}

void Lexer::InitTries() {
  keyword_trie_.Insert("if", (int)TokenType::KW_IF);
  keyword_trie_.Insert("else", (int)TokenType::KW_ELSE);
  keyword_trie_.Insert("for", (int)TokenType::KW_FOR);
  keyword_trie_.Insert("while", (int)TokenType::KW_WHILE);
  keyword_trie_.Insert("do", (int)TokenType::KW_DO);

  keyword_trie_.Insert("break", (int)TokenType::KW_BREAK);
  keyword_trie_.Insert("continue", (int)TokenType::KW_CONTINUE);
  keyword_trie_.Insert("return", (int)TokenType::KW_RETURN);

  keyword_trie_.Insert("int", (int)TokenType::KW_INT);
  keyword_trie_.Insert("double", (int)TokenType::KW_DOUBLE);
  keyword_trie_.Insert("bool", (int)TokenType::KW_BOOL);
  keyword_trie_.Insert("char", (int)TokenType::KW_CHAR);
  keyword_trie_.Insert("string", (int)TokenType::KW_STRING);


  op_trie_.Insert("||", (int)TokenType::OROR);
  op_trie_.Insert("&&", (int)TokenType::ANDAND);

  op_trie_.Insert("==", (int)TokenType::EQ);
  op_trie_.Insert("!=", (int)TokenType::NEQ);

  op_trie_.Insert("<=", (int)TokenType::LE);
  op_trie_.Insert(">=", (int)TokenType::GE);

  op_trie_.Insert("<<", (int)TokenType::SHL);
  op_trie_.Insert(">>", (int)TokenType::SHR);

  op_trie_.Insert("=", (int)TokenType::ASSIGN);

  op_trie_.Insert("<", (int)TokenType::LT);
  op_trie_.Insert(">", (int)TokenType::GT);

  op_trie_.Insert("+", (int)TokenType::PLUS);
  op_trie_.Insert("-", (int)TokenType::MINUS);
  op_trie_.Insert("*", (int)TokenType::STAR);
  op_trie_.Insert("/", (int)TokenType::SLASH);
  op_trie_.Insert("%", (int)TokenType::PERCENT);

  op_trie_.Insert("|", (int)TokenType::OR);
  op_trie_.Insert("^", (int)TokenType::XOR);
  op_trie_.Insert("&", (int)TokenType::AND);

  op_trie_.Insert("!", (int)TokenType::BANG);
  op_trie_.Insert("~", (int)TokenType::TILDE);

  op_trie_.Insert("(", (int)TokenType::LPAREN);
  op_trie_.Insert(")", (int)TokenType::RPAREN);
  op_trie_.Insert("{", (int)TokenType::LBRACE);
  op_trie_.Insert("}", (int)TokenType::RBRACE);
  op_trie_.Insert("[", (int)TokenType::LBRACKET);
  op_trie_.Insert("]", (int)TokenType::RBRACKET);

  op_trie_.Insert(",", (int)TokenType::COMMA);
  op_trie_.Insert(";", (int)TokenType::SEMICOLON);

  op_trie_.Insert(".", (int)TokenType::DOT);
}

bool Lexer::IsEnd() const { return pos_ >= src_.size(); }

char Lexer::Peek(int offset) const {
  size_t p = pos_ + (size_t)offset;
  if (p >= src_.size()) return '\0';
  return src_[p];
}

char Lexer::Get() {
  char c = Peek();
  if (c == '\0') return '\0';
  pos_++;
  if (c == '\n') { line_++; col_ = 1; }
  else { col_++; }
  return c;
}

bool Lexer::IsLetter(char c) {
  return std::isalpha((unsigned char)c) || c == '_';
}
bool Lexer::IsDigit(char c) {
  return std::isdigit((unsigned char)c);
}

void Lexer::SkipWhitespaceAndComments() {
  while (!IsEnd()) {
    char c = Peek();
    if (std::isspace((unsigned char)c)) {
      Get();
      continue;
    }

    if (c == '/' && Peek(1) == '/') {
      Get(); Get();
      while (!IsEnd() && Peek() != '\n') Get();
      continue;
    }

    if (c == '/' && Peek(1) == '*') {
      Get(); Get();
      while (!IsEnd()) {
        if (Peek() == '*' && Peek(1) == '/') { Get(); Get(); break; }
        Get();
      }
      continue;
    }

    break;
  }
}

Token Lexer::Make(TokenType type, const std::string& text, int line, int col) const {
  return Token{type, text, line, col};
}

Token Lexer::EndOfFile() const {
  return Make(TokenType::END_OF_FILE, "", line_, col_);
}

std::vector<Token> Lexer::Tokenize() {
  std::vector<Token> out;
  Token t;
  while ((t = NextToken()).type != TokenType::END_OF_FILE) out.push_back(t);
  out.push_back(t);
  return out;
}

Token Lexer::PeekToken() {
  if (!has_buffered_token_) {
    buffered_token_ = NextToken();
    has_buffered_token_ = true;
  }
  return buffered_token_;
}

Token Lexer::NextToken() {
  if (has_buffered_token_) {
    has_buffered_token_ = false;
    return buffered_token_;
  }

  SkipWhitespaceAndComments();
  if (IsEnd()) return EndOfFile();

  char c = Peek();
  if (IsLetter(c)) return IdentifierOrKeywordOrBool();
  if (IsDigit(c)) return Number();
  if (c == '\'') return CharLiteral();
  if (c == '"') return StringLiteral();

  return OperatorOrPunctOrUnknown();
}

Token Lexer::IdentifierOrKeywordOrBool() {
  int startLine = line_;
  int startCol = col_;

  std::string text;
  text.push_back(Get());
  while (!IsEnd() && (IsLetter(Peek()) || IsDigit(Peek()) || Peek() == '_')) {
    text.push_back(Get());
  }

  if (text == "true" || text == "false") {
    return Make(TokenType::BOOL_LITERAL, text, startLine, startCol);
  }

  int kw = keyword_trie_.MatchExact(text);
  if (kw != -1) return Make((TokenType)kw, text, startLine, startCol);

  return Make(TokenType::IDENTIFIER, text, startLine, startCol);
}

Token Lexer::Number() {
  int startLine = line_;
  int startCol = col_;
  std::string text;

  while (!IsEnd() && IsDigit(Peek())) text.push_back(Get());

  if (!IsEnd() && Peek() == '.' && IsDigit(Peek(1))) {
    text.push_back(Get());
    while (!IsEnd() && IsDigit(Peek())) text.push_back(Get());
    return Make(TokenType::DOUBLE_LITERAL, text, startLine, startCol);
  }

  return Make(TokenType::INT_LITERAL, text, startLine, startCol);
}

Token Lexer::CharLiteral() {
  int startLine = line_;
  int startCol = col_;

  Get();

  if (IsEnd() || Peek() == '\n') {
    return Make(TokenType::UNKNOWN, "unterminated char literal", startLine, startCol);
  }

  char ch = Get();

  if (Peek() != '\'') {
    if (!IsEnd()) Get();
    return Make(TokenType::UNKNOWN, "bad char literal", startLine, startCol);
  }

  Get();

  return Make(TokenType::CHAR_LITERAL, std::string(1, ch), startLine, startCol);
}

Token Lexer::StringLiteral() {
  int startLine = line_;
  int startCol = col_;

  Get();

  std::string text;
  while (!IsEnd() && Peek() != '"') {
    char c = Peek();
    if (c == '\n') {
      return Make(TokenType::UNKNOWN, "unterminated string literal", startLine, startCol);
    }
    text.push_back(Get());
  }

  if (IsEnd()) {
    return Make(TokenType::UNKNOWN, "unterminated string literal", startLine, startCol);
  }

  Get();
  return Make(TokenType::STRING_LITERAL, text, startLine, startCol);
}

Token Lexer::OperatorOrPunctOrUnknown() {
  int startLine = line_;
  int startCol = col_;

  size_t len = 0;
  int val = op_trie_.MatchLongestFrom(src_, pos_, len);

  if (val != -1 && len > 0) {
    std::string lexeme = src_.substr(pos_, len);
    for (size_t i = 0; i < len; ++i) Get();
    return Make((TokenType)val, lexeme, startLine, startCol);
  }

  char bad = Get();
  return Make(TokenType::UNKNOWN, std::string(1, bad), startLine, startCol);
}
