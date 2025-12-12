#include "parser.h"

#include "../lexer/lexer.h"   // ВАЖНО: путь тот же, что и в main.cpp
#include <sstream>

// ==================== низкоуровневые методы ====================

enum class TokenType;
Parser::Parser(Lexer &lexer) : lex_(lexer) {}

Token Parser::Peek() {
  return lex_.PeekToken();
}

Token Parser::Consume() {
  return lex_.NextToken();
}

bool Parser::Match(TokenType type) {
  if (Peek().type == type) {
    Consume();
    return true;
  }
  return false;
}

Token Parser::Expect(TokenType type, const char *expected) {
  Token t = Peek();
  if (t.type != type) {
    std::ostringstream oss;
    oss << "expected " << expected << " at " << t.line << ":" << t.col
        << ", got '" << t.text << "'";
    Error(oss.str(), t);
  }
  return Consume();
}

[[noreturn]] void Parser::Error(const std::string &message, const Token &t) {
  std::ostringstream oss;
  if (t.type == TokenType::END_OF_FILE) {
    oss << "Parse error at EOF: " << message;
  } else {
    oss << "Parse error at " << t.line << ":" << t.col << ": " << message;
  }
  throw std::runtime_error(oss.str());
}

bool Parser::IsTypeStart(TokenType t) {
  switch (t) {
    case TokenType::KW_INT:
    case TokenType::KW_DOUBLE:
    case TokenType::KW_BOOL:
    case TokenType::KW_CHAR:
    case TokenType::KW_STRING:
      return true;
    default:
      return false;
  }
}

bool Parser::IsExpressionStart(TokenType t) {
  switch (t) {
    case TokenType::INT_LITERAL:
    case TokenType::DOUBLE_LITERAL:
    case TokenType::BOOL_LITERAL:
    case TokenType::CHAR_LITERAL:
    case TokenType::STRING_LITERAL:
    case TokenType::IDENTIFIER:
    case TokenType::LPAREN:
    case TokenType::PLUS:
    case TokenType::MINUS:
    case TokenType::BANG:
    case TokenType::TILDE:
      return true;
    default:
      return false;
  }
}

bool Parser::IsStatementStart(TokenType t) {
  if (IsTypeStart(t) || IsExpressionStart(t)) return true;

  switch (t) {
    case TokenType::LBRACE:
    case TokenType::KW_IF:
    case TokenType::KW_WHILE:
    case TokenType::KW_DO:
    case TokenType::KW_FOR:
    case TokenType::KW_BREAK:
    case TokenType::KW_CONTINUE:
    case TokenType::KW_RETURN:
      return true;
    default:
      return false;
  }
}

// ==================== Program / TopLevel ====================

// Program ::= TopLevelList EOF
void Parser::ParseProgram() {
  ParseTopLevelList();
  Expect(TokenType::END_OF_FILE, "EOF");
}

// TopLevelList ::= TopLevel TopLevelList | eps
void Parser::ParseTopLevelList() {
  while (IsTypeStart(Peek().type)) {
    ParseTopLevel();
  }
}

// TopLevel ::= Type ID TopLevelAfterId
void Parser::ParseTopLevel() {
  ParseType();
  Expect(TokenType::IDENTIFIER, "identifier");
  ParseTopLevelAfterId();
}

// TopLevelAfterId ::= "(" ParamList ")" Block | VarDeclInitOpt ";"
void Parser::ParseTopLevelAfterId() {
  if (Match(TokenType::LPAREN)) {
    // FuncDecl
    ParseParamList();
    Expect(TokenType::RPAREN, "')' after parameter list");
    ParseBlock();
  } else {
    // Global var
    ParseVarDeclInitOpt();
    Expect(TokenType::SEMICOLON, "';' after global variable decl");
  }
}

// ==================== VarDecl / Type ====================

// VarDecl ::= Type ID VarDeclInitOpt
void Parser::ParseVarDecl() {
  ParseType();
  Expect(TokenType::IDENTIFIER, "identifier");
  ParseVarDeclInitOpt();
}

// VarDeclInitOpt ::= "=" Expression | eps
void Parser::ParseVarDeclInitOpt() {
  if (Match(TokenType::ASSIGN)) {
    ParseExpression();
  }
}

// Type ::= BaseType ArraySuffixOpt
void Parser::ParseType() {
  ParseBaseType();
  ParseArraySuffixOpt();
}

// BaseType ::= "int" | "double" | "bool" | "char" | "string"
void Parser::ParseBaseType() {
  TokenType t = Peek().type;
  if (!IsTypeStart(t)) {
    Error("expected type (int/double/bool/char/string)", Peek());
  }
  Consume();
}

// ArraySuffixOpt ::= "[" Expression "]" ArraySuffixOpt | eps
void Parser::ParseArraySuffixOpt() {
  while (Match(TokenType::LBRACKET)) {
    ParseExpression();
    Expect(TokenType::RBRACKET, "']' after array size");
  }
}

// ==================== Block / Statements ====================

// Block ::= "{" StatementList "}"
void Parser::ParseBlock() {
  Expect(TokenType::LBRACE, "'{' to start block");
  ParseStatementList();
  Expect(TokenType::RBRACE, "'}' to close block");
}

// StatementList ::= Statement StatementList | eps
void Parser::ParseStatementList() {
  while (IsStatementStart(Peek().type)) {
    ParseStatement();
  }
}

// Statement ::=
//   VarDecl ";"
// | Expression ";"
// | WhileStmt
// | ForStmt
// | DoWhileStmt
// | IfStmt
// | "break" ";"
// | "continue" ";"
// | "return" ReturnExprOpt ";"
// | Block
void Parser::ParseStatement() {
  Token t = Peek();

  // Block
  if (t.type == TokenType::LBRACE) {
    ParseBlock();
    return;
  }

  // if
  if (t.type == TokenType::KW_IF) {
    ParseIfStmt();
    return;
  }

  // while
  if (t.type == TokenType::KW_WHILE) {
    ParseWhileStmt();
    return;
  }

  // do-while
  if (t.type == TokenType::KW_DO) {
    ParseDoWhileStmt();
    return;
  }

  // for
  if (t.type == TokenType::KW_FOR) {
    ParseForStmt();
    return;
  }

  // break
  if (t.type == TokenType::KW_BREAK) {
    Consume();
    Expect(TokenType::SEMICOLON, "';' after 'break'");
    return;
  }

  // continue
  if (t.type == TokenType::KW_CONTINUE) {
    Consume();
    Expect(TokenType::SEMICOLON, "';' after 'continue'");
    return;
  }

  // return
  if (t.type == TokenType::KW_RETURN) {
    Consume();
    if (Peek().type != TokenType::SEMICOLON) {
      ParseExpression();  // ReturnExprOpt
    }
    Expect(TokenType::SEMICOLON, "';' after 'return'");
    return;
  }

  // VarDecl or Expression
  if (IsTypeStart(t.type)) {
    ParseVarDecl();
    Expect(TokenType::SEMICOLON, "';' after variable declaration");
    return;
  }

  if (IsExpressionStart(t.type)) {
    ParseExpression();
    Expect(TokenType::SEMICOLON, "';' after expression");
    return;
  }

  Error("expected statement", t);
}

// ==================== if / else ====================

// IfStmt ::= "if" "(" Expression ")" Statement ElseOpt
void Parser::ParseIfStmt() {
  Expect(TokenType::KW_IF, "'if'");
  Expect(TokenType::LPAREN, "'(' after 'if'");
  ParseExpression();
  Expect(TokenType::RPAREN, "')' after if condition");
  ParseStatement();
  ParseElseOpt();
}

// ElseOpt ::= "else" Statement | eps
void Parser::ParseElseOpt() {
  if (Match(TokenType::KW_ELSE)) {
    ParseStatement();
  }
}

// ==================== while / do / for ====================

// WhileStmt ::= "while" "(" Expression ")" Block
void Parser::ParseWhileStmt() {
  Expect(TokenType::KW_WHILE, "'while'");
  Expect(TokenType::LPAREN, "'(' after 'while'");
  ParseExpression();
  Expect(TokenType::RPAREN, "')' after while condition");
  ParseBlock();
}

// DoWhileStmt ::= "do" Block "while" "(" Expression ")" ";"
void Parser::ParseDoWhileStmt() {
  Expect(TokenType::KW_DO, "'do'");
  ParseBlock();
  Expect(TokenType::KW_WHILE, "'while' after do-block");
  Expect(TokenType::LPAREN, "'(' after 'while'");
  ParseExpression();
  Expect(TokenType::RPAREN, "')' after do-while condition");
  Expect(TokenType::SEMICOLON, "';' after do-while");
}

// ForStmt ::= "for" "(" ForInit ";" ForCond ";" ForStep ")" Block
void Parser::ParseForStmt() {
  Expect(TokenType::KW_FOR, "'for'");
  Expect(TokenType::LPAREN, "'(' after 'for'");
  ParseForInit();
  Expect(TokenType::SEMICOLON, "';' after for-init");
  ParseForCond();
  Expect(TokenType::SEMICOLON, "';' after for-condition");
  ParseForStep();
  Expect(TokenType::RPAREN, "')' after for-step");
  ParseBlock();
}

// ForInit ::= VarDecl | Expression | eps
void Parser::ParseForInit() {
  TokenType t = Peek().type;
  if (IsTypeStart(t)) {
    ParseVarDecl();
  } else if (IsExpressionStart(t)) {
    ParseExpression();
  } else {
    // eps
  }
}

// ForCond ::= Expression | eps
void Parser::ParseForCond() {
  if (IsExpressionStart(Peek().type)) {
    ParseExpression();
  }
}

// ForStep ::= Expression | eps
void Parser::ParseForStep() {
  if (IsExpressionStart(Peek().type)) {
    ParseExpression();
  }
}

// ==================== параметры функций ====================

// ParamList ::= Param ParamListTail | eps
void Parser::ParseParamList() {
  if (!IsTypeStart(Peek().type)) {
    // пустой список
    return;
  }
  ParseParam();
  ParseParamListTail();
}

// ParamListTail ::= "," Param ParamListTail | eps
void Parser::ParseParamListTail() {
  while (Match(TokenType::COMMA)) {
    ParseParam();
  }
}

// Param ::= Type ID
void Parser::ParseParam() {
  ParseType();
  Expect(TokenType::IDENTIFIER, "identifier in parameter");
}

// ==================== аргументы вызовов ====================

// ArgList ::= Expression ArgListTail | eps
void Parser::ParseArgList() {
  if (!IsExpressionStart(Peek().type)) {
    return; // eps
  }
  ParseExpression();
  ParseArgListTail();
}

// ArgListTail ::= "," Expression ArgListTail | eps
void Parser::ParseArgListTail() {
  while (Match(TokenType::COMMA)) {
    ParseExpression();
  }
}

// ==================== выражения ====================

// Expression ::= AssignmentExpr CommaTail
// CommaTail ::= "," AssignmentExpr CommaTail | eps
void Parser::ParseExpression() {
  ParseAssignmentExpr();
  while (Match(TokenType::COMMA)) {
    ParseAssignmentExpr();
  }
}

// AssignmentExpr ::= LogicalOrExpr ("=" AssignmentExpr)?
// (облегчённый вариант против LValue="..." из грамматики)
void Parser::ParseAssignmentExpr() {
  ParseLogicalOrExpr();
  if (Match(TokenType::ASSIGN)) {
    ParseAssignmentExpr();
  }
}

// LogicalOrExpr ::= LogicalAndExpr ("||" LogicalAndExpr)*
void Parser::ParseLogicalOrExpr() {
  ParseLogicalAndExpr();
  while (Match(TokenType::OROR)) {
    ParseLogicalAndExpr();
  }
}

// LogicalAndExpr ::= BitOrExpr ("&&" BitOrExpr)*
void Parser::ParseLogicalAndExpr() {
  ParseBitOrExpr();
  while (Match(TokenType::ANDAND)) {
    ParseBitOrExpr();
  }
}

// BitOrExpr ::= BitXorExpr ("|" BitXorExpr)*
void Parser::ParseBitOrExpr() {
  ParseBitXorExpr();
  while (Match(TokenType::OR)) {
    ParseBitXorExpr();
  }
}

// BitXorExpr ::= BitAndExpr ("^" BitAndExpr)*
void Parser::ParseBitXorExpr() {
  ParseBitAndExpr();
  while (Match(TokenType::XOR)) {
    ParseBitAndExpr();
  }
}

// BitAndExpr ::= EqualityExpr ("&" EqualityExpr)*
void Parser::ParseBitAndExpr() {
  ParseEqualityExpr();
  while (Match(TokenType::AND)) {
    ParseEqualityExpr();
  }
}

// EqualityExpr ::= RelationalExpr (("==" | "!=") RelationalExpr)*
void Parser::ParseEqualityExpr() {
  ParseRelationalExpr();
  for (;;) {
    if (Match(TokenType::EQ) || Match(TokenType::NEQ)) {
      ParseRelationalExpr();
    } else {
      break;
    }
  }
}

// RelationalExpr ::= ShiftExpr (("<" | ">" | "<=" | ">=") ShiftExpr)*
void Parser::ParseRelationalExpr() {
  ParseShiftExpr();
  for (;;) {
    TokenType t = Peek().type;
    if (t == TokenType::LT || t == TokenType::GT ||
        t == TokenType::LE || t == TokenType::GE) {
      Consume();
      ParseShiftExpr();
    } else {
      break;
    }
  }
}

// ShiftExpr ::= AddExpr (("<<" | ">>") AddExpr)*
void Parser::ParseShiftExpr() {
  ParseAddExpr();
  for (;;) {
    TokenType t = Peek().type;
    if (t == TokenType::SHL || t == TokenType::SHR) {
      Consume();
      ParseAddExpr();
    } else {
      break;
    }
  }
}

// AddExpr ::= MulExpr (("+" | "-") MulExpr)*
void Parser::ParseAddExpr() {
  ParseMulExpr();
  for (;;) {
    TokenType t = Peek().type;
    if (t == TokenType::PLUS || t == TokenType::MINUS) {
      Consume();
      ParseMulExpr();
    } else {
      break;
    }
  }
}

// MulExpr ::= UnaryExpr (("*" | "/" | "%") UnaryExpr)*
void Parser::ParseMulExpr() {
  ParseUnaryExpr();
  for (;;) {
    TokenType t = Peek().type;
    if (t == TokenType::STAR || t == TokenType::SLASH ||
        t == TokenType::PERCENT) {
      Consume();
      ParseUnaryExpr();
    } else {
      break;
    }
  }
}

// UnaryExpr ::= UnaryOp UnaryExpr | Primary
// UnaryOp ::= "+" | "-" | "!" | "~"
void Parser::ParseUnaryExpr() {
  TokenType t = Peek().type;
  if (t == TokenType::PLUS || t == TokenType::MINUS ||
      t == TokenType::BANG || t == TokenType::TILDE) {
    Consume();
    ParseUnaryExpr();
  } else {
    ParsePrimary();
  }
}

// Primary ::= PrimaryCore IndexSeq
void Parser::ParsePrimary() {
  ParsePrimaryCore();
  ParseIndexSeq();
}

// PrimaryCore ::= Literal | ID PrimaryIdTail | "(" Expression ")"
void Parser::ParsePrimaryCore() {
  Token t = Peek();
  switch (t.type) {
    case TokenType::INT_LITERAL:
    case TokenType::DOUBLE_LITERAL:
    case TokenType::BOOL_LITERAL:
    case TokenType::CHAR_LITERAL:
    case TokenType::STRING_LITERAL:
      ParseLiteral();
      return;

    case TokenType::IDENTIFIER: {
      Consume(); // ID
      if (Match(TokenType::LPAREN)) {
        // FuncCall
        ParseArgList();
        Expect(TokenType::RPAREN, "')' after argument list");
      }
      return;
    }

    case TokenType::LPAREN:
      Consume();
      ParseExpression();
      Expect(TokenType::RPAREN, "')' after expression");
      return;

    default:
      Error("expected primary expression", t);
  }
}

// IndexSeq ::= "[" Expression "]" IndexSeq | eps
void Parser::ParseIndexSeq() {
  while (Match(TokenType::LBRACKET)) {
    ParseExpression();
    Expect(TokenType::RBRACKET, "']' after index expression");
  }
}

// Literal ::= Number | BoolLiteral | CharLiteral | StringLiteral
// Number ::= IntLiteral | DoubleLiteral
void Parser::ParseLiteral() {
  TokenType t = Peek().type;
  switch (t) {
    case TokenType::INT_LITERAL:
    case TokenType::DOUBLE_LITERAL:
    case TokenType::BOOL_LITERAL:
    case TokenType::CHAR_LITERAL:
    case TokenType::STRING_LITERAL:
      Consume();
      break;
    default:
      Error("expected literal", Peek());
  }
}
