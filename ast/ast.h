#pragma once

#include <memory>
#include <string>
#include <vector>

#include "../lexer/lexer.h"

namespace ast {

enum class BaseType {
  INT,
  DOUBLE,
  BOOL,
  CHAR,
  STRING
};

struct Expr;
struct Stmt;
struct TopLevelDecl;

using ExprPtr = std::unique_ptr<Expr>;
using StmtPtr = std::unique_ptr<Stmt>;
using TopLevelPtr = std::unique_ptr<TopLevelDecl>;

struct TypeNode {
  BaseType base;
  std::vector<ExprPtr> dims; // array sizes, one Expr per dimension
};

// ===================== Expressions =====================
struct Expr {
  virtual ~Expr() = default;
};

struct IdentifierExpr final : Expr {
  std::string name;
  explicit IdentifierExpr(std::string n) : name(std::move(n)) {}
};

struct IntLiteralExpr final : Expr {
  std::string value;
  explicit IntLiteralExpr(std::string v) : value(std::move(v)) {}
};

struct DoubleLiteralExpr final : Expr {
  std::string value;
  explicit DoubleLiteralExpr(std::string v) : value(std::move(v)) {}
};

struct BoolLiteralExpr final : Expr {
  bool value = false;
  explicit BoolLiteralExpr(bool v) : value(v) {}
};

struct CharLiteralExpr final : Expr {
  std::string value;
  explicit CharLiteralExpr(std::string v) : value(std::move(v)) {}
};

struct StringLiteralExpr final : Expr {
  std::string value;
  explicit StringLiteralExpr(std::string v) : value(std::move(v)) {}
};

struct UnaryExpr final : Expr {
  TokenType op;
  ExprPtr operand;
  UnaryExpr(TokenType op, ExprPtr operand)
      : op(op), operand(std::move(operand)) {}
};

struct BinaryExpr final : Expr {
  TokenType op;
  ExprPtr left;
  ExprPtr right;
  BinaryExpr(TokenType op, ExprPtr left, ExprPtr right)
      : op(op), left(std::move(left)), right(std::move(right)) {}
};

struct CallExpr final : Expr {
  std::string callee;
  std::vector<ExprPtr> args;
  CallExpr(std::string callee, std::vector<ExprPtr> args)
      : callee(std::move(callee)), args(std::move(args)) {}
};

struct IndexExpr final : Expr {
  ExprPtr base;
  std::vector<ExprPtr> indices;
  IndexExpr(ExprPtr base, std::vector<ExprPtr> indices)
      : base(std::move(base)), indices(std::move(indices)) {}
};

// ===================== Statements =====================
struct Stmt {
  virtual ~Stmt() = default;
};

struct BlockStmt final : Stmt {
  std::vector<StmtPtr> statements;
};

struct ExprStmt final : Stmt {
  ExprPtr expr;
  explicit ExprStmt(ExprPtr expr) : expr(std::move(expr)) {}
};

struct VarDeclStmt final : Stmt {
  TypeNode type;
  std::string name;
  ExprPtr init; // optional
  VarDeclStmt(TypeNode type, std::string name, ExprPtr init)
      : type(std::move(type)), name(std::move(name)), init(std::move(init)) {}
};

struct IfStmt final : Stmt {
  ExprPtr cond;
  StmtPtr thenBranch;
  StmtPtr elseBranch; // optional
  IfStmt(ExprPtr cond, StmtPtr thenBranch, StmtPtr elseBranch)
      : cond(std::move(cond)),
        thenBranch(std::move(thenBranch)),
        elseBranch(std::move(elseBranch)) {}
};

struct WhileStmt final : Stmt {
  ExprPtr cond;
  std::unique_ptr<BlockStmt> body;
  WhileStmt(ExprPtr cond, std::unique_ptr<BlockStmt> body)
      : cond(std::move(cond)), body(std::move(body)) {}
};

struct DoWhileStmt final : Stmt {
  std::unique_ptr<BlockStmt> body;
  ExprPtr cond;
  DoWhileStmt(std::unique_ptr<BlockStmt> body, ExprPtr cond)
      : body(std::move(body)), cond(std::move(cond)) {}
};

struct ForStmt final : Stmt {
  StmtPtr init; // VarDeclStmt or ExprStmt, optional
  ExprPtr cond; // optional
  ExprPtr step; // optional
  std::unique_ptr<BlockStmt> body;
  ForStmt(StmtPtr init, ExprPtr cond, ExprPtr step, std::unique_ptr<BlockStmt> body)
      : init(std::move(init)),
        cond(std::move(cond)),
        step(std::move(step)),
        body(std::move(body)) {}
};

struct BreakStmt final : Stmt {};
struct ContinueStmt final : Stmt {};

struct ReturnStmt final : Stmt {
  ExprPtr expr; // optional
  explicit ReturnStmt(ExprPtr expr) : expr(std::move(expr)) {}
};

// ===================== Top-level =====================
struct Param {
  TypeNode type;
  std::string name;
};

struct TopLevelDecl {
  virtual ~TopLevelDecl() = default;
};

struct FunctionDecl final : TopLevelDecl {
  TypeNode returnType;
  std::string name;
  std::vector<Param> params;
  std::unique_ptr<BlockStmt> body;
  FunctionDecl(TypeNode returnType,
               std::string name,
               std::vector<Param> params,
               std::unique_ptr<BlockStmt> body)
      : returnType(std::move(returnType)),
        name(std::move(name)),
        params(std::move(params)),
        body(std::move(body)) {}
};

struct GlobalVarDecl final : TopLevelDecl {
  TypeNode type;
  std::string name;
  ExprPtr init; // optional
  GlobalVarDecl(TypeNode type, std::string name, ExprPtr init)
      : type(std::move(type)), name(std::move(name)), init(std::move(init)) {}
};

struct Program {
  std::vector<TopLevelPtr> decls;
};

} // namespace ast
