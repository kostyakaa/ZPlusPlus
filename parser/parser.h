#pragma once

#include "../ast/ast.h"
#include "../lexer/lexer.h"

#include <stdexcept>
#include <string>
#include <vector>

class Parser {
public:
  explicit Parser(Lexer &lexer);

  // Entry point: throws std::runtime_error on syntax error
  ast::Program ParseProgram();

private:
  Lexer &lex_;

  // === low-level helpers ===
  Token Peek();
  Token Consume();
  bool Match(TokenType type);
  Token Expect(TokenType type, const char *expected);

  [[noreturn]] void Error(const std::string &message, const Token &t);

  static bool IsTypeStart(TokenType t);
  static bool IsExpressionStart(TokenType t);
  static bool IsStatementStart(TokenType t);

  // === nonterminals ===

  // Program ::= TopLevelList EOF
  std::vector<ast::TopLevelPtr> ParseTopLevelList();
  ast::TopLevelPtr ParseTopLevel();

  // VarDecl ::= Type ID VarDeclInitOpt
  ast::StmtPtr ParseVarDeclStmt();
  ast::ExprPtr ParseVarDeclInitOpt();

  // Type ::= BaseType ArraySuffixOpt
  ast::TypeNode ParseType();
  ast::BaseType ParseBaseType();
  std::vector<ast::ExprPtr> ParseArraySuffixOpt();

  // Block / Statements
  // Block ::= "{" StatementList "}"
  std::unique_ptr<ast::BlockStmt> ParseBlock();
  std::vector<ast::StmtPtr> ParseStatementList();
  ast::StmtPtr ParseStatement();

  // if / else
  ast::StmtPtr ParseIfStmt();
  ast::StmtPtr ParseElseOpt();

  // while / do / for
  ast::StmtPtr ParseWhileStmt();
  ast::StmtPtr ParseDoWhileStmt();
  ast::StmtPtr ParseForStmt();
  ast::StmtPtr ParseForInit();
  ast::ExprPtr ParseForCond();
  ast::ExprPtr ParseForStep();

  // Function params
  std::vector<ast::Param> ParseParamList();
  ast::Param ParseParam();

  // Call args
  std::vector<ast::ExprPtr> ParseArgList();

  // ==== expressions (precedence) ====
  ast::ExprPtr ParseExpression();
  ast::ExprPtr ParseAssignmentExpr();
  ast::ExprPtr ParseLogicalOrExpr();
  ast::ExprPtr ParseLogicalAndExpr();
  ast::ExprPtr ParseBitOrExpr();
  ast::ExprPtr ParseBitXorExpr();
  ast::ExprPtr ParseBitAndExpr();
  ast::ExprPtr ParseEqualityExpr();
  ast::ExprPtr ParseRelationalExpr();
  ast::ExprPtr ParseShiftExpr();
  ast::ExprPtr ParseAddExpr();
  ast::ExprPtr ParseMulExpr();
  ast::ExprPtr ParseUnaryExpr();
  ast::ExprPtr ParsePrimary();
  ast::ExprPtr ParsePrimaryCore();
  ast::ExprPtr ParseLiteral();
};
