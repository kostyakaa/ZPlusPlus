#pragma once

#include "../lexer/lexer.h"
#include <stdexcept>
#include <string>

class Parser {
public:
  explicit Parser(Lexer &lexer);

  // Входная точка: бросает std::runtime_error при синтаксической ошибке
  void ParseProgram();

private:
  Lexer &lex_;

  // === низкоуровневые хелперы ===
  Token Peek();
  Token Consume();
  bool Match(TokenType type);
  Token Expect(TokenType type, const char *expected);

  [[noreturn]] void Error(const std::string &message, const Token &t);

  static bool IsTypeStart(TokenType t);
  static bool IsExpressionStart(TokenType t);
  static bool IsStatementStart(TokenType t);

  // === нетерминалы ===

  // Program ::= TopLevelList EOF
  void ParseTopLevelList();
  void ParseTopLevel();
  void ParseTopLevelAfterId();

  // VarDecl ::= Type ID VarDeclInitOpt
  void ParseVarDecl();
  void ParseVarDeclInitOpt();

  // Type ::= BaseType ArraySuffixOpt
  // BaseType ::= "int" | "double" | "bool" | "char" | "string"
  // ArraySuffixOpt ::= "[" Expression "]" ArraySuffixOpt | eps
  void ParseType();
  void ParseBaseType();
  void ParseArraySuffixOpt();

  // Block / Statements
  // Block ::= "{" StatementList "}"
  // StatementList ::= Statement StatementList | eps
  void ParseBlock();
  void ParseStatementList();
  void ParseStatement();

  // if / else
  // IfStmt ::= "if" "(" Expression ")" Statement ElseOpt
  // ElseOpt ::= "else" Statement | eps
  void ParseIfStmt();
  void ParseElseOpt();

  // while / do / for
  // WhileStmt   ::= "while" "(" Expression ")" Block
  // DoWhileStmt ::= "do" Block "while" "(" Expression ")" ";"
  // ForStmt ::= "for" "(" ForInit ";" ForCond ";" ForStep ")" Block
  // ForInit ::= VarDecl | Expression | eps
  // ForCond ::= Expression | eps
  // ForStep ::= Expression | eps
  void ParseWhileStmt();
  void ParseDoWhileStmt();
  void ParseForStmt();
  void ParseForInit();
  void ParseForCond();
  void ParseForStep();

  // Параметры функции
  // ParamList ::= Param ParamListTail | eps
  // ParamListTail ::= "," Param ParamListTail | eps
  // Param ::= Type ID
  void ParseParamList();
  void ParseParam();
  void ParseParamListTail();

  // Аргументы вызовов
  // ArgList ::= Expression ArgListTail | eps
  // ArgListTail ::= "," Expression ArgListTail | eps
  void ParseArgList();
  void ParseArgListTail();

  // ==== выражения (с приоритетами) ====
  // Expression ::= AssignmentExpr CommaTail
  // CommaTail ::= "," AssignmentExpr CommaTail | eps
  void ParseExpression();

  // AssignmentExpr ::= LogicalOrExpr ("=" AssignmentExpr)?
  // (в оригинальной грамматике слева LValue, здесь это проверяется семантически)
  void ParseAssignmentExpr();

  void ParseLogicalOrExpr();   // ||
  void ParseLogicalAndExpr();  // &&
  void ParseBitOrExpr();       // |
  void ParseBitXorExpr();      // ^
  void ParseBitAndExpr();      // &
  void ParseEqualityExpr();    // == !=
  void ParseRelationalExpr();  // < > <= >=
  void ParseShiftExpr();       // << >>
  void ParseAddExpr();         // + -
  void ParseMulExpr();         // * / %
  void ParseUnaryExpr();       // + - ! ~
  void ParsePrimary();         // PrimaryCore IndexSeq
  void ParsePrimaryCore();     // Literal | ID [ "(" ArgList ")" ] | "(" Expression ")"
  void ParseIndexSeq();        // "[" Expression "]" ...
  void ParseLiteral();         // Number | BoolLiteral | CharLiteral | StringLiteral
};
