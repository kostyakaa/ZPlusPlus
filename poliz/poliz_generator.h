#pragma once

#include "../ast/ast.h"
#include "poliz.h"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class PolizGenerator {
public:
  Poliz Generate(const ast::Program &program);

private:
  enum class BaseType {
    INT,
    DOUBLE,
    BOOL,
    CHAR,
    STRING
  };

  struct TypeInfo {
    BaseType base;
    int dims;
  };

  struct VarInfo {
    TypeInfo type;
    int offset;
    std::vector<int> dims;
  };

  struct FuncInfo {
    TypeInfo returnType;
    std::vector<TypeInfo> params;
    std::vector<std::string> paramNames;
    std::vector<int> paramOffsets;
    std::vector<bool> paramIsArray;
    int address = -1;
  };

  Poliz code_;
  const ast::Program *program_{nullptr};

  std::unordered_map<std::string, FuncInfo> functions_;
  std::vector<std::unordered_map<std::string, VarInfo>> scopes_;

  int currentOffset_{0};

  std::vector<std::vector<int>> breakPatches_;
  std::vector<std::vector<int>> continuePatches_;
  std::vector<int> returnPatches_;

  std::vector<std::pair<int, std::string>> funcAddrPatches_;

private:
  static BaseType ToBaseType(ast::BaseType t);
  static const char *BaseTypeToString(BaseType t);
  static int BaseTypeSize(BaseType t);

  static std::string OpToString(TokenType t);
  static PolizOp BaseToAddressOp(BaseType t);
  static PolizOp BaseToLiteralOp(BaseType t);

  void PushScope();
  void PopScope();
  void DeclareVar(const std::string &name, const VarInfo &info);
  VarInfo LookupVar(const std::string &name) const;

  void CollectFunctionSigs();
  void CollectFunctionSig(const ast::FunctionDecl &fn);
  void PredeclareGlobals();

  void EmitProgram();
  void EmitTopLevel(const ast::TopLevelDecl &decl);
  void EmitFunction(const ast::FunctionDecl &fn);
  void EmitGlobalVar(const ast::GlobalVarDecl &g);

  void EmitStmt(const ast::Stmt &stmt);
  void EmitBlock(const ast::BlockStmt &block);

  void EmitExpr(const ast::Expr &expr);
  void EmitLValueAddress(const ast::Expr &expr);

  TypeInfo TypeFromNode(const ast::TypeNode &t) const;
  std::vector<int> EvalArrayDims(const ast::TypeNode &t);
  std::optional<long long> EvalConstInt(const ast::Expr &expr);
};
