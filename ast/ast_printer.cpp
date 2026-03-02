#include "ast_printer.h"

#include "../lexer/lexer.h"

#include <ostream>
#include <string>

namespace ast {
namespace {

void Indent(std::ostream &out, int depth) {
  for (int i = 0; i < depth; ++i) out << "  ";
}

std::string BaseTypeToString(BaseType t) {
  switch (t) {
    case BaseType::INT: return "int";
    case BaseType::DOUBLE: return "double";
    case BaseType::BOOL: return "bool";
    case BaseType::CHAR: return "char";
    case BaseType::STRING: return "string";
  }
  return "unknown";
}

std::string TokenTypeToOp(TokenType t) {
  switch (t) {
    case TokenType::ASSIGN: return "=";
    case TokenType::COMMA: return ",";
    case TokenType::OROR: return "||";
    case TokenType::ANDAND: return "&&";
    case TokenType::OR: return "|";
    case TokenType::XOR: return "^";
    case TokenType::AND: return "&";
    case TokenType::EQ: return "==";
    case TokenType::NEQ: return "!=";
    case TokenType::LT: return "<";
    case TokenType::GT: return ">";
    case TokenType::LE: return "<=";
    case TokenType::GE: return ">=";
    case TokenType::SHL: return "<<";
    case TokenType::SHR: return ">>";
    case TokenType::PLUS: return "+";
    case TokenType::MINUS: return "-";
    case TokenType::STAR: return "*";
    case TokenType::SLASH: return "/";
    case TokenType::PERCENT: return "%";
    case TokenType::BANG: return "!";
    case TokenType::TILDE: return "~";
    default: return "<op>";
  }
}

void PrintType(std::ostream &out, const TypeNode &type) {
  out << BaseTypeToString(type.base);
  for (size_t i = 0; i < type.dims.size(); ++i) {
    out << "[]";
  }
}

void PrintExpr(const Expr &expr, std::ostream &out, int depth);
void PrintStmt(const Stmt &stmt, std::ostream &out, int depth);
void PrintTopLevel(const TopLevelDecl &decl, std::ostream &out, int depth);

void PrintExpr(const Expr &expr, std::ostream &out, int depth) {
  if (auto e = dynamic_cast<const IdentifierExpr*>(&expr)) {
    Indent(out, depth);
    out << "Identifier " << e->name << "\n";
    return;
  }
  if (auto e = dynamic_cast<const IntLiteralExpr*>(&expr)) {
    Indent(out, depth);
    out << "IntLiteral " << e->value << "\n";
    return;
  }
  if (auto e = dynamic_cast<const DoubleLiteralExpr*>(&expr)) {
    Indent(out, depth);
    out << "DoubleLiteral " << e->value << "\n";
    return;
  }
  if (auto e = dynamic_cast<const BoolLiteralExpr*>(&expr)) {
    Indent(out, depth);
    out << "BoolLiteral " << (e->value ? "true" : "false") << "\n";
    return;
  }
  if (auto e = dynamic_cast<const CharLiteralExpr*>(&expr)) {
    Indent(out, depth);
    out << "CharLiteral '" << e->value << "'\n";
    return;
  }
  if (auto e = dynamic_cast<const StringLiteralExpr*>(&expr)) {
    Indent(out, depth);
    out << "StringLiteral \"" << e->value << "\"\n";
    return;
  }
  if (auto e = dynamic_cast<const UnaryExpr*>(&expr)) {
    Indent(out, depth);
    out << "Unary " << TokenTypeToOp(e->op) << "\n";
    PrintExpr(*e->operand, out, depth + 1);
    return;
  }
  if (auto e = dynamic_cast<const BinaryExpr*>(&expr)) {
    Indent(out, depth);
    out << "Binary " << TokenTypeToOp(e->op) << "\n";
    PrintExpr(*e->left, out, depth + 1);
    PrintExpr(*e->right, out, depth + 1);
    return;
  }
  if (auto e = dynamic_cast<const CallExpr*>(&expr)) {
    Indent(out, depth);
    out << "Call " << e->callee << "\n";
    for (const auto &arg : e->args) {
      PrintExpr(*arg, out, depth + 1);
    }
    return;
  }
  if (auto e = dynamic_cast<const IndexExpr*>(&expr)) {
    Indent(out, depth);
    out << "Index\n";
    Indent(out, depth + 1);
    out << "Base\n";
    PrintExpr(*e->base, out, depth + 2);
    Indent(out, depth + 1);
    out << "Indices\n";
    for (const auto &idx : e->indices) {
      PrintExpr(*idx, out, depth + 2);
    }
    return;
  }

  Indent(out, depth);
  out << "Expr <unknown>\n";
}

void PrintStmt(const Stmt &stmt, std::ostream &out, int depth) {
  if (auto s = dynamic_cast<const BlockStmt*>(&stmt)) {
    Indent(out, depth);
    out << "Block\n";
    for (const auto &st : s->statements) {
      PrintStmt(*st, out, depth + 1);
    }
    return;
  }
  if (auto s = dynamic_cast<const ExprStmt*>(&stmt)) {
    Indent(out, depth);
    out << "ExprStmt\n";
    PrintExpr(*s->expr, out, depth + 1);
    return;
  }
  if (auto s = dynamic_cast<const VarDeclStmt*>(&stmt)) {
    Indent(out, depth);
    out << "VarDecl " << s->name << " : ";
    PrintType(out, s->type);
    out << "\n";
    if (s->init) {
      Indent(out, depth + 1);
      out << "Init\n";
      PrintExpr(*s->init, out, depth + 2);
    }
    return;
  }
  if (auto s = dynamic_cast<const IfStmt*>(&stmt)) {
    Indent(out, depth);
    out << "If\n";
    Indent(out, depth + 1);
    out << "Cond\n";
    PrintExpr(*s->cond, out, depth + 2);
    Indent(out, depth + 1);
    out << "Then\n";
    PrintStmt(*s->thenBranch, out, depth + 2);
    if (s->elseBranch) {
      Indent(out, depth + 1);
      out << "Else\n";
      PrintStmt(*s->elseBranch, out, depth + 2);
    }
    return;
  }
  if (auto s = dynamic_cast<const WhileStmt*>(&stmt)) {
    Indent(out, depth);
    out << "While\n";
    Indent(out, depth + 1);
    out << "Cond\n";
    PrintExpr(*s->cond, out, depth + 2);
    Indent(out, depth + 1);
    out << "Body\n";
    PrintStmt(*s->body, out, depth + 2);
    return;
  }
  if (auto s = dynamic_cast<const DoWhileStmt*>(&stmt)) {
    Indent(out, depth);
    out << "DoWhile\n";
    Indent(out, depth + 1);
    out << "Body\n";
    PrintStmt(*s->body, out, depth + 2);
    Indent(out, depth + 1);
    out << "Cond\n";
    PrintExpr(*s->cond, out, depth + 2);
    return;
  }
  if (auto s = dynamic_cast<const ForStmt*>(&stmt)) {
    Indent(out, depth);
    out << "For\n";
    Indent(out, depth + 1);
    out << "Init\n";
    if (s->init) PrintStmt(*s->init, out, depth + 2);
    else { Indent(out, depth + 2); out << "<none>\n"; }
    Indent(out, depth + 1);
    out << "Cond\n";
    if (s->cond) PrintExpr(*s->cond, out, depth + 2);
    else { Indent(out, depth + 2); out << "<none>\n"; }
    Indent(out, depth + 1);
    out << "Step\n";
    if (s->step) PrintExpr(*s->step, out, depth + 2);
    else { Indent(out, depth + 2); out << "<none>\n"; }
    Indent(out, depth + 1);
    out << "Body\n";
    PrintStmt(*s->body, out, depth + 2);
    return;
  }
  if (dynamic_cast<const BreakStmt*>(&stmt)) {
    Indent(out, depth);
    out << "Break\n";
    return;
  }
  if (dynamic_cast<const ContinueStmt*>(&stmt)) {
    Indent(out, depth);
    out << "Continue\n";
    return;
  }
  if (auto s = dynamic_cast<const ReturnStmt*>(&stmt)) {
    Indent(out, depth);
    out << "Return\n";
    if (s->expr) PrintExpr(*s->expr, out, depth + 1);
    return;
  }

  Indent(out, depth);
  out << "Stmt <unknown>\n";
}

void PrintTopLevel(const TopLevelDecl &decl, std::ostream &out, int depth) {
  if (auto fn = dynamic_cast<const FunctionDecl*>(&decl)) {
    Indent(out, depth);
    out << "Function " << fn->name << " : ";
    PrintType(out, fn->returnType);
    out << "\n";
    Indent(out, depth + 1);
    out << "Params\n";
    for (const auto &p : fn->params) {
      Indent(out, depth + 2);
      out << p.name << " : ";
      PrintType(out, p.type);
      out << "\n";
    }
    Indent(out, depth + 1);
    out << "Body\n";
    PrintStmt(*fn->body, out, depth + 2);
    return;
  }
  if (auto g = dynamic_cast<const GlobalVarDecl*>(&decl)) {
    Indent(out, depth);
    out << "GlobalVar " << g->name << " : ";
    PrintType(out, g->type);
    out << "\n";
    if (g->init) {
      Indent(out, depth + 1);
      out << "Init\n";
      PrintExpr(*g->init, out, depth + 2);
    }
    return;
  }

  Indent(out, depth);
  out << "TopLevel <unknown>\n";
}

} // namespace

void PrintProgram(const Program &program, std::ostream &out) {
  out << "AST\n";
  for (const auto &decl : program.decls) {
    PrintTopLevel(*decl, out, 1);
  }
}

} // namespace ast
