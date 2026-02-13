#pragma once

#include "../lexer/lexer.h"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

enum class BaseTypeKind {
  INT,
  DOUBLE,
  BOOL,
  CHAR,
  STRING,
  INVALID
};

struct TypeInfo {
  BaseTypeKind base{BaseTypeKind::INVALID};
  int dims{0};

  [[nodiscard]] bool IsValid() const { return base != BaseTypeKind::INVALID; }
  [[nodiscard]] bool IsScalar() const { return dims == 0; }
};

struct FunctionSig {
  TypeInfo returnType;
  std::vector<TypeInfo> params;
  Token declToken;
};

class SemanticAnalyzer {
public:
  explicit SemanticAnalyzer(std::vector<Token> tokens);

  void Analyze();

private:
  std::vector<Token> tokens_;
  size_t pos_{0};

  std::unordered_map<std::string, TypeInfo> globals_;
  std::unordered_map<std::string, FunctionSig> functions_;
  std::vector<std::unordered_map<std::string, TypeInfo>> scopes_;
  std::optional<TypeInfo> currentReturnType_;
  int loopDepth_{0};

  static bool IsTypeStart(TokenType t);
  static bool IsExpressionStart(TokenType t);
  static bool IsNumericScalar(const TypeInfo &t);
  static bool IsIntegralScalar(const TypeInfo &t);
  static bool IsBoolScalar(const TypeInfo &t);
  static TypeInfo BoolType();
  static BaseTypeKind TokenToBaseType(TokenType t);
  static bool CanAssign(const TypeInfo &dst, const TypeInfo &src);
  static TypeInfo CommonNumericType(const TypeInfo &a, const TypeInfo &b);
  static std::string BaseTypeToString(BaseTypeKind b);
  static std::string TypeToString(const TypeInfo &t);

  [[noreturn]] void ErrorAt(const Token &t, const std::string &message) const;

  Token Peek(int offset = 0) const;
  Token Consume();
  bool Match(TokenType type);
  Token Expect(TokenType type, const char *expected);

  void PushScope();
  void PopScope();
  void DeclareLocal(const Token &id, const TypeInfo &type);
  TypeInfo LookupVariable(const Token &id) const;
  void ExpectBoolCondition(const TypeInfo &cond, const Token &where);

  // Обработка глобальных
  void CollectTopLevelSymbols();
  TypeInfo ParseTypeShallow();
  std::vector<TypeInfo> ParseParamTypesShallow();
  void SkipBalancedUntilRBracketShallow();
  void SkipUntilSemicolonShallow();
  void SkipBlockShallow();

  // Остальное
  void ParseProgram();
  void ParseTopLevel();
  TypeInfo ParseType();
  std::vector<TypeInfo> ParseParamList(std::vector<Token> &namesOut);
  void ParseVarDeclInitOpt(const TypeInfo &declType, const Token &id, bool isGlobal);
  void ParseBlock(bool createScope = true);
  void ParseStatement();
  void ParseIfStmt();
  void ParseWhileStmt();
  void ParseDoWhileStmt();
  void ParseForStmt();

  std::optional<TypeInfo> TryParseLValue();
  TypeInfo ParseExpression();
  TypeInfo ParseAssignmentExpr();
  TypeInfo ParseLogicalOrExpr();
  TypeInfo ParseLogicalAndExpr();
  TypeInfo ParseBitOrExpr();
  TypeInfo ParseBitXorExpr();
  TypeInfo ParseBitAndExpr();
  TypeInfo ParseEqualityExpr();
  TypeInfo ParseRelationalExpr();
  TypeInfo ParseShiftExpr();
  TypeInfo ParseAddExpr();
  TypeInfo ParseMulExpr();
  TypeInfo ParseUnaryExpr();
  TypeInfo ParsePrimary();
  TypeInfo ParsePrimaryCore();
  std::vector<TypeInfo> ParseArgList();
};
