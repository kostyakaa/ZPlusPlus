#include "parser.h"

#include "../ast/ast.h"
#include <sstream>

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

ast::Program Parser::ParseProgram() {
  ast::Program program;
  program.decls = ParseTopLevelList();
  Expect(TokenType::END_OF_FILE, "EOF");
  return program;
}

std::vector<ast::TopLevelPtr> Parser::ParseTopLevelList() {
  std::vector<ast::TopLevelPtr> decls;
  while (IsTypeStart(Peek().type)) {
    decls.push_back(ParseTopLevel());
  }
  return decls;
}

ast::TopLevelPtr Parser::ParseTopLevel() {
  ast::TypeNode type = ParseType();
  Token id = Expect(TokenType::IDENTIFIER, "identifier");

  if (Match(TokenType::LPAREN)) {
    std::vector<ast::Param> params = ParseParamList();
    Expect(TokenType::RPAREN, "')' after parameter list");
    std::unique_ptr<ast::BlockStmt> body = ParseBlock();
    return std::make_unique<ast::FunctionDecl>(
        std::move(type), id.text, std::move(params), std::move(body));
  }

  ast::ExprPtr init = ParseVarDeclInitOpt();
  Expect(TokenType::SEMICOLON, "';' after global variable decl");
  return std::make_unique<ast::GlobalVarDecl>(
      std::move(type), id.text, std::move(init));
}

// ==================== VarDecl / Type ====================

ast::StmtPtr Parser::ParseVarDeclStmt() {
  ast::TypeNode type = ParseType();
  Token id = Expect(TokenType::IDENTIFIER, "identifier");
  ast::ExprPtr init = ParseVarDeclInitOpt();
  return std::make_unique<ast::VarDeclStmt>(
      std::move(type), id.text, std::move(init));
}

ast::ExprPtr Parser::ParseVarDeclInitOpt() {
  if (Match(TokenType::ASSIGN)) {
    return ParseExpression();
  }
  return nullptr;
}

ast::TypeNode Parser::ParseType() {
  ast::BaseType base = ParseBaseType();
  std::vector<ast::ExprPtr> dims = ParseArraySuffixOpt();
  return ast::TypeNode{base, std::move(dims)};
}

ast::BaseType Parser::ParseBaseType() {
  TokenType t = Peek().type;
  if (!IsTypeStart(t)) {
    Error("expected type (int/double/bool/char/string)", Peek());
  }
  Consume();
  switch (t) {
    case TokenType::KW_INT: return ast::BaseType::INT;
    case TokenType::KW_DOUBLE: return ast::BaseType::DOUBLE;
    case TokenType::KW_BOOL: return ast::BaseType::BOOL;
    case TokenType::KW_CHAR: return ast::BaseType::CHAR;
    case TokenType::KW_STRING: return ast::BaseType::STRING;
    default: break;
  }
  return ast::BaseType::INT;
}

std::vector<ast::ExprPtr> Parser::ParseArraySuffixOpt() {
  std::vector<ast::ExprPtr> dims;
  while (Match(TokenType::LBRACKET)) {
    dims.push_back(ParseExpression());
    Expect(TokenType::RBRACKET, "']' after array size");
  }
  return dims;
}

// ==================== Block / Statements ====================

std::unique_ptr<ast::BlockStmt> Parser::ParseBlock() {
  Expect(TokenType::LBRACE, "'{' to start block");
  auto block = std::make_unique<ast::BlockStmt>();
  block->statements = ParseStatementList();
  Expect(TokenType::RBRACE, "'}' to close block");
  return block;
}

std::vector<ast::StmtPtr> Parser::ParseStatementList() {
  std::vector<ast::StmtPtr> stmts;
  while (IsStatementStart(Peek().type)) {
    stmts.push_back(ParseStatement());
  }
  return stmts;
}

ast::StmtPtr Parser::ParseStatement() {
  Token t = Peek();

  if (t.type == TokenType::LBRACE) {
    return ParseBlock();
  }

  if (t.type == TokenType::KW_IF) {
    return ParseIfStmt();
  }

  if (t.type == TokenType::KW_WHILE) {
    return ParseWhileStmt();
  }

  if (t.type == TokenType::KW_DO) {
    return ParseDoWhileStmt();
  }

  if (t.type == TokenType::KW_FOR) {
    return ParseForStmt();
  }

  if (t.type == TokenType::KW_BREAK) {
    Consume();
    Expect(TokenType::SEMICOLON, "';' after 'break'");
    return std::make_unique<ast::BreakStmt>();
  }

  if (t.type == TokenType::KW_CONTINUE) {
    Consume();
    Expect(TokenType::SEMICOLON, "';' after 'continue'");
    return std::make_unique<ast::ContinueStmt>();
  }

  if (t.type == TokenType::KW_RETURN) {
    Consume();
    ast::ExprPtr expr;
    if (Peek().type != TokenType::SEMICOLON) {
      expr = ParseExpression();
    }
    Expect(TokenType::SEMICOLON, "';' after 'return'");
    return std::make_unique<ast::ReturnStmt>(std::move(expr));
  }

  if (IsTypeStart(t.type)) {
    auto decl = ParseVarDeclStmt();
    Expect(TokenType::SEMICOLON, "';' after variable declaration");
    return decl;
  }

  if (IsExpressionStart(t.type)) {
    auto expr = ParseExpression();
    Expect(TokenType::SEMICOLON, "';' after expression");
    return std::make_unique<ast::ExprStmt>(std::move(expr));
  }

  Error("expected statement", t);
}

// ==================== if / else ====================

ast::StmtPtr Parser::ParseIfStmt() {
  Expect(TokenType::KW_IF, "'if'");
  Expect(TokenType::LPAREN, "'(' after 'if'");
  ast::ExprPtr cond = ParseExpression();
  Expect(TokenType::RPAREN, "')' after if condition");
  ast::StmtPtr thenStmt = ParseStatement();
  ast::StmtPtr elseStmt = ParseElseOpt();
  return std::make_unique<ast::IfStmt>(
      std::move(cond), std::move(thenStmt), std::move(elseStmt));
}

ast::StmtPtr Parser::ParseElseOpt() {
  if (Match(TokenType::KW_ELSE)) {
    return ParseStatement();
  }
  return nullptr;
}

// ==================== while / do / for ====================

ast::StmtPtr Parser::ParseWhileStmt() {
  Expect(TokenType::KW_WHILE, "'while'");
  Expect(TokenType::LPAREN, "'(' after 'while'");
  ast::ExprPtr cond = ParseExpression();
  Expect(TokenType::RPAREN, "')' after while condition");
  std::unique_ptr<ast::BlockStmt> body = ParseBlock();
  return std::make_unique<ast::WhileStmt>(std::move(cond), std::move(body));
}

ast::StmtPtr Parser::ParseDoWhileStmt() {
  Expect(TokenType::KW_DO, "'do'");
  std::unique_ptr<ast::BlockStmt> body = ParseBlock();
  Expect(TokenType::KW_WHILE, "'while' after do-block");
  Expect(TokenType::LPAREN, "'(' after 'while'");
  ast::ExprPtr cond = ParseExpression();
  Expect(TokenType::RPAREN, "')' after do-while condition");
  Expect(TokenType::SEMICOLON, "';' after do-while");
  return std::make_unique<ast::DoWhileStmt>(std::move(body), std::move(cond));
}

ast::StmtPtr Parser::ParseForStmt() {
  Expect(TokenType::KW_FOR, "'for'");
  Expect(TokenType::LPAREN, "'(' after 'for'");
  ast::StmtPtr init = ParseForInit();
  Expect(TokenType::SEMICOLON, "';' after for-init");
  ast::ExprPtr cond = ParseForCond();
  Expect(TokenType::SEMICOLON, "';' after for-condition");
  ast::ExprPtr step = ParseForStep();
  Expect(TokenType::RPAREN, "')' after for-step");
  std::unique_ptr<ast::BlockStmt> body = ParseBlock();
  return std::make_unique<ast::ForStmt>(
      std::move(init), std::move(cond), std::move(step), std::move(body));
}

ast::StmtPtr Parser::ParseForInit() {
  TokenType t = Peek().type;
  if (IsTypeStart(t)) {
    return ParseVarDeclStmt();
  }
  if (IsExpressionStart(t)) {
    auto expr = ParseExpression();
    return std::make_unique<ast::ExprStmt>(std::move(expr));
  }
  return nullptr;
}

ast::ExprPtr Parser::ParseForCond() {
  if (IsExpressionStart(Peek().type)) {
    return ParseExpression();
  }
  return nullptr;
}

ast::ExprPtr Parser::ParseForStep() {
  if (IsExpressionStart(Peek().type)) {
    return ParseExpression();
  }
  return nullptr;
}

// ==================== Function params ====================

std::vector<ast::Param> Parser::ParseParamList() {
  std::vector<ast::Param> params;
  if (!IsTypeStart(Peek().type)) {
    return params;
  }
  params.push_back(ParseParam());
  while (Match(TokenType::COMMA)) {
    params.push_back(ParseParam());
  }
  return params;
}

ast::Param Parser::ParseParam() {
  ast::TypeNode type = ParseType();
  Token id = Expect(TokenType::IDENTIFIER, "identifier in parameter");
  return ast::Param{std::move(type), id.text};
}

// ==================== Call args ====================

std::vector<ast::ExprPtr> Parser::ParseArgList() {
  std::vector<ast::ExprPtr> args;
  if (!IsExpressionStart(Peek().type)) {
    return args;
  }
  args.push_back(ParseExpression());
  while (Match(TokenType::COMMA)) {
    args.push_back(ParseExpression());
  }
  return args;
}

// ==================== Expressions ====================

ast::ExprPtr Parser::ParseExpression() {
  auto expr = ParseAssignmentExpr();
  while (Match(TokenType::COMMA)) {
    auto rhs = ParseAssignmentExpr();
    expr = std::make_unique<ast::BinaryExpr>(TokenType::COMMA, std::move(expr), std::move(rhs));
  }
  return expr;
}

ast::ExprPtr Parser::ParseAssignmentExpr() {
  auto left = ParseLogicalOrExpr();
  if (Match(TokenType::ASSIGN)) {
    auto right = ParseAssignmentExpr();
    return std::make_unique<ast::BinaryExpr>(TokenType::ASSIGN, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseLogicalOrExpr() {
  auto left = ParseLogicalAndExpr();
  while (Match(TokenType::OROR)) {
    auto right = ParseLogicalAndExpr();
    left = std::make_unique<ast::BinaryExpr>(TokenType::OROR, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseLogicalAndExpr() {
  auto left = ParseBitOrExpr();
  while (Match(TokenType::ANDAND)) {
    auto right = ParseBitOrExpr();
    left = std::make_unique<ast::BinaryExpr>(TokenType::ANDAND, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseBitOrExpr() {
  auto left = ParseBitXorExpr();
  while (Match(TokenType::OR)) {
    auto right = ParseBitXorExpr();
    left = std::make_unique<ast::BinaryExpr>(TokenType::OR, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseBitXorExpr() {
  auto left = ParseBitAndExpr();
  while (Match(TokenType::XOR)) {
    auto right = ParseBitAndExpr();
    left = std::make_unique<ast::BinaryExpr>(TokenType::XOR, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseBitAndExpr() {
  auto left = ParseEqualityExpr();
  while (Match(TokenType::AND)) {
    auto right = ParseEqualityExpr();
    left = std::make_unique<ast::BinaryExpr>(TokenType::AND, std::move(left), std::move(right));
  }
  return left;
}

ast::ExprPtr Parser::ParseEqualityExpr() {
  auto left = ParseRelationalExpr();
  while (true) {
    if (Match(TokenType::EQ)) {
      auto right = ParseRelationalExpr();
      left = std::make_unique<ast::BinaryExpr>(TokenType::EQ, std::move(left), std::move(right));
    } else if (Match(TokenType::NEQ)) {
      auto right = ParseRelationalExpr();
      left = std::make_unique<ast::BinaryExpr>(TokenType::NEQ, std::move(left), std::move(right));
    } else {
      break;
    }
  }
  return left;
}

ast::ExprPtr Parser::ParseRelationalExpr() {
  auto left = ParseShiftExpr();
  while (true) {
    TokenType t = Peek().type;
    if (t == TokenType::LT || t == TokenType::GT ||
        t == TokenType::LE || t == TokenType::GE) {
      Consume();
      auto right = ParseShiftExpr();
      left = std::make_unique<ast::BinaryExpr>(t, std::move(left), std::move(right));
    } else {
      break;
    }
  }
  return left;
}

ast::ExprPtr Parser::ParseShiftExpr() {
  auto left = ParseAddExpr();
  while (true) {
    TokenType t = Peek().type;
    if (t == TokenType::SHL || t == TokenType::SHR) {
      Consume();
      auto right = ParseAddExpr();
      left = std::make_unique<ast::BinaryExpr>(t, std::move(left), std::move(right));
    } else {
      break;
    }
  }
  return left;
}

ast::ExprPtr Parser::ParseAddExpr() {
  auto left = ParseMulExpr();
  while (true) {
    TokenType t = Peek().type;
    if (t == TokenType::PLUS || t == TokenType::MINUS) {
      Consume();
      auto right = ParseMulExpr();
      left = std::make_unique<ast::BinaryExpr>(t, std::move(left), std::move(right));
    } else {
      break;
    }
  }
  return left;
}

ast::ExprPtr Parser::ParseMulExpr() {
  auto left = ParseUnaryExpr();
  while (true) {
    TokenType t = Peek().type;
    if (t == TokenType::STAR || t == TokenType::SLASH || t == TokenType::PERCENT) {
      Consume();
      auto right = ParseUnaryExpr();
      left = std::make_unique<ast::BinaryExpr>(t, std::move(left), std::move(right));
    } else {
      break;
    }
  }
  return left;
}

ast::ExprPtr Parser::ParseUnaryExpr() {
  TokenType t = Peek().type;
  if (t == TokenType::PLUS || t == TokenType::MINUS ||
      t == TokenType::BANG || t == TokenType::TILDE) {
    Consume();
    auto inner = ParseUnaryExpr();
    return std::make_unique<ast::UnaryExpr>(t, std::move(inner));
  }
  return ParsePrimary();
}

ast::ExprPtr Parser::ParsePrimary() {
  auto base = ParsePrimaryCore();
  std::vector<ast::ExprPtr> indices;
  while (Match(TokenType::LBRACKET)) {
    indices.push_back(ParseExpression());
    Expect(TokenType::RBRACKET, "']' after index expression");
  }
  if (indices.empty()) return base;
  return std::make_unique<ast::IndexExpr>(std::move(base), std::move(indices));
}

ast::ExprPtr Parser::ParsePrimaryCore() {
  Token t = Peek();
  switch (t.type) {
    case TokenType::INT_LITERAL:
    case TokenType::DOUBLE_LITERAL:
    case TokenType::BOOL_LITERAL:
    case TokenType::CHAR_LITERAL:
    case TokenType::STRING_LITERAL:
      return ParseLiteral();

    case TokenType::IDENTIFIER: {
      Consume();
      std::string name = t.text;
      if (Match(TokenType::LPAREN)) {
        auto args = ParseArgList();
        Expect(TokenType::RPAREN, "')' after argument list");
        return std::make_unique<ast::CallExpr>(std::move(name), std::move(args));
      }
      return std::make_unique<ast::IdentifierExpr>(std::move(name));
    }

    case TokenType::LPAREN: {
      Consume();
      auto expr = ParseExpression();
      Expect(TokenType::RPAREN, "')' after expression");
      return expr;
    }

    default:
      Error("expected primary expression", t);
  }
}

ast::ExprPtr Parser::ParseLiteral() {
  Token t = Peek();
  switch (t.type) {
    case TokenType::INT_LITERAL:
      Consume();
      return std::make_unique<ast::IntLiteralExpr>(t.text);
    case TokenType::DOUBLE_LITERAL:
      Consume();
      return std::make_unique<ast::DoubleLiteralExpr>(t.text);
    case TokenType::BOOL_LITERAL:
      Consume();
      return std::make_unique<ast::BoolLiteralExpr>(t.text == "true");
    case TokenType::CHAR_LITERAL:
      Consume();
      return std::make_unique<ast::CharLiteralExpr>(t.text);
    case TokenType::STRING_LITERAL:
      Consume();
      return std::make_unique<ast::StringLiteralExpr>(t.text);
    default:
      Error("expected literal", t);
  }
}
