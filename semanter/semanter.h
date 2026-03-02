#pragma once

#include "../ast/ast.h"

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
};

class SemanticAnalyzer {
public:
  explicit SemanticAnalyzer(const ast::Program &program);

  void Analyze();

private:
  const ast::Program &program_;

  std::unordered_map<std::string, TypeInfo> globals_;
  std::unordered_map<std::string, FunctionSig> functions_;
  std::vector<std::unordered_map<std::string, TypeInfo>> scopes_;
  std::optional<TypeInfo> currentReturnType_;
  int loopDepth_{0};

  static bool IsNumericScalar(const TypeInfo &t);
  static bool IsIntegralScalar(const TypeInfo &t);
  static bool IsBoolScalar(const TypeInfo &t);
  static TypeInfo BoolType();
  static BaseTypeKind ToBaseType(ast::BaseType t);
  static bool CanAssign(const TypeInfo &dst, const TypeInfo &src);
  static TypeInfo CommonNumericType(const TypeInfo &a, const TypeInfo &b);
  static std::string BaseTypeToString(BaseTypeKind b);
  static std::string TypeToString(const TypeInfo &t);

  [[noreturn]] void Error(const std::string &message) const;

  void PushScope();
  void PopScope();
  void DeclareLocal(const std::string &name, const TypeInfo &type);
  TypeInfo LookupVariable(const std::string &name) const;
  void ExpectBoolCondition(const TypeInfo &cond, const std::string &where);

  TypeInfo TypeFromNodeShallow(const ast::TypeNode &t) const;
  TypeInfo TypeFromNode(const ast::TypeNode &t);

  void CollectTopLevelSymbols();
  void CollectTopLevelDecl(const ast::TopLevelDecl &decl);

  void AnalyzeTopLevelDecl(const ast::TopLevelDecl &decl);
  void AnalyzeFunction(const ast::FunctionDecl &fn);
  void AnalyzeGlobal(const ast::GlobalVarDecl &g);

  void AnalyzeStmt(const ast::Stmt &stmt);
  void AnalyzeBlock(const ast::BlockStmt &block, bool createScope);

  TypeInfo AnalyzeExpr(const ast::Expr &expr);
  std::optional<TypeInfo> AnalyzeLValue(const ast::Expr &expr);
};
