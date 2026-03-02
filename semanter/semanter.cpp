#include "semanter.h"

#include <sstream>
#include <stdexcept>
#include <utility>

SemanticAnalyzer::SemanticAnalyzer(const ast::Program &program)
    : program_(program) {}

void SemanticAnalyzer::Analyze() {
  CollectTopLevelSymbols();
  for (const auto &decl : program_.decls) {
    AnalyzeTopLevelDecl(*decl);
  }
}

bool SemanticAnalyzer::IsNumericScalar(const TypeInfo &t) {
  if (!t.IsScalar()) return false;
  return t.base == BaseTypeKind::INT || t.base == BaseTypeKind::DOUBLE ||
         t.base == BaseTypeKind::CHAR;
}

bool SemanticAnalyzer::IsIntegralScalar(const TypeInfo &t) {
  if (!t.IsScalar()) return false;
  return t.base == BaseTypeKind::INT || t.base == BaseTypeKind::CHAR;
}

bool SemanticAnalyzer::IsBoolScalar(const TypeInfo &t) {
  return t.IsScalar() && t.base == BaseTypeKind::BOOL;
}

TypeInfo SemanticAnalyzer::BoolType() { return TypeInfo{BaseTypeKind::BOOL, 0}; }

BaseTypeKind SemanticAnalyzer::ToBaseType(ast::BaseType t) {
  switch (t) {
    case ast::BaseType::INT: return BaseTypeKind::INT;
    case ast::BaseType::DOUBLE: return BaseTypeKind::DOUBLE;
    case ast::BaseType::BOOL: return BaseTypeKind::BOOL;
    case ast::BaseType::CHAR: return BaseTypeKind::CHAR;
    case ast::BaseType::STRING: return BaseTypeKind::STRING;
  }
  return BaseTypeKind::INVALID;
}

bool SemanticAnalyzer::CanAssign(const TypeInfo &dst, const TypeInfo &src) {
  if (dst.dims != src.dims) return false;
  if (dst.dims > 0) return dst.base == src.base;
  if (dst.base == src.base) return true;
  if (dst.base == BaseTypeKind::DOUBLE &&
      (src.base == BaseTypeKind::INT || src.base == BaseTypeKind::CHAR)) {
    return true;
  }
  if (dst.base == BaseTypeKind::INT && src.base == BaseTypeKind::CHAR) {
    return true;
  }
  return false;
}

TypeInfo SemanticAnalyzer::CommonNumericType(const TypeInfo &a, const TypeInfo &b) {
  if (!IsNumericScalar(a) || !IsNumericScalar(b)) {
    return {BaseTypeKind::INVALID, 0};
  }
  if (a.base == BaseTypeKind::DOUBLE || b.base == BaseTypeKind::DOUBLE) {
    return {BaseTypeKind::DOUBLE, 0};
  }
  return {BaseTypeKind::INT, 0};
}

std::string SemanticAnalyzer::BaseTypeToString(BaseTypeKind b) {
  switch (b) {
    case BaseTypeKind::INT: return "int";
    case BaseTypeKind::DOUBLE: return "double";
    case BaseTypeKind::BOOL: return "bool";
    case BaseTypeKind::CHAR: return "char";
    case BaseTypeKind::STRING: return "string";
    default: return "<invalid>";
  }
}

std::string SemanticAnalyzer::TypeToString(const TypeInfo &t) {
  std::string s = BaseTypeToString(t.base);
  for (int i = 0; i < t.dims; ++i) s += "[]";
  return s;
}

[[noreturn]] void SemanticAnalyzer::Error(const std::string &message) const {
  throw std::runtime_error("Semantic error: " + message);
}

void SemanticAnalyzer::PushScope() { scopes_.emplace_back(); }
void SemanticAnalyzer::PopScope() { scopes_.pop_back(); }

void SemanticAnalyzer::DeclareLocal(const std::string &name, const TypeInfo &type) {
  if (scopes_.empty()) {
    Error("internal error: no active scope");
  }
  auto &cur = scopes_.back();
  if (cur.count(name) != 0) {
    Error("redeclaration of '" + name + "'");
  }
  cur[name] = type;
}

TypeInfo SemanticAnalyzer::LookupVariable(const std::string &name) const {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto found = it->find(name);
    if (found != it->end()) return found->second;
  }
  auto g = globals_.find(name);
  if (g != globals_.end()) return g->second;
  throw std::runtime_error("undeclared identifier '" + name + "'");
}

void SemanticAnalyzer::ExpectBoolCondition(const TypeInfo &cond, const std::string &where) {
  if (!IsBoolScalar(cond)) {
    Error(where + ": condition must be bool, got " + TypeToString(cond));
  }
}

TypeInfo SemanticAnalyzer::TypeFromNodeShallow(const ast::TypeNode &t) const {
  TypeInfo type{ToBaseType(t.base), 0};
  type.dims = static_cast<int>(t.dims.size());
  return type;
}

TypeInfo SemanticAnalyzer::TypeFromNode(const ast::TypeNode &t) {
  TypeInfo type{ToBaseType(t.base), 0};
  for (const auto &dimExpr : t.dims) {
    TypeInfo sz = AnalyzeExpr(*dimExpr);
    if (!IsIntegralScalar(sz)) {
      Error("array size must be integral, got " + TypeToString(sz));
    }
    type.dims++;
  }
  return type;
}

void SemanticAnalyzer::CollectTopLevelSymbols() {
  for (const auto &decl : program_.decls) {
    CollectTopLevelDecl(*decl);
  }
}

void SemanticAnalyzer::CollectTopLevelDecl(const ast::TopLevelDecl &decl) {
  if (auto fn = dynamic_cast<const ast::FunctionDecl*>(&decl)) {
    TypeInfo ret = TypeFromNodeShallow(fn->returnType);
    if (ret.dims > 0) {
      Error("function '" + fn->name + "' cannot return an array type");
    }
    std::vector<TypeInfo> params;
    params.reserve(fn->params.size());
    for (const auto &p : fn->params) {
      params.push_back(TypeFromNodeShallow(p.type));
    }
    if (functions_.count(fn->name) != 0 || globals_.count(fn->name) != 0) {
      Error("redefinition of top-level symbol '" + fn->name + "'");
    }
    functions_[fn->name] = FunctionSig{ret, std::move(params)};
    return;
  }

  if (auto g = dynamic_cast<const ast::GlobalVarDecl*>(&decl)) {
    if (functions_.count(g->name) != 0 || globals_.count(g->name) != 0) {
      Error("redefinition of top-level symbol '" + g->name + "'");
    }
    TypeInfo t = TypeFromNodeShallow(g->type);
    globals_[g->name] = t;
    return;
  }
}

void SemanticAnalyzer::AnalyzeTopLevelDecl(const ast::TopLevelDecl &decl) {
  if (auto fn = dynamic_cast<const ast::FunctionDecl*>(&decl)) {
    AnalyzeFunction(*fn);
    return;
  }
  if (auto g = dynamic_cast<const ast::GlobalVarDecl*>(&decl)) {
    AnalyzeGlobal(*g);
    return;
  }
}

void SemanticAnalyzer::AnalyzeFunction(const ast::FunctionDecl &fn) {
  TypeInfo declType = TypeFromNode(fn.returnType);
  if (declType.dims > 0) {
    Error("function '" + fn.name + "' cannot return an array type");
  }

  auto found = functions_.find(fn.name);
  if (found == functions_.end()) {
    Error("unknown function '" + fn.name + "'");
  }

  std::vector<TypeInfo> paramTypes;
  paramTypes.reserve(fn.params.size());
  for (const auto &p : fn.params) {
    paramTypes.push_back(TypeFromNode(p.type));
  }

  if (paramTypes.size() != found->second.params.size()) {
    Error("parameter count mismatch for '" + fn.name + "'");
  }

  for (size_t i = 0; i < paramTypes.size(); ++i) {
    if (paramTypes[i].base != found->second.params[i].base ||
        paramTypes[i].dims != found->second.params[i].dims) {
      Error("parameter type mismatch for function '" + fn.name + "'");
    }
  }

  if (declType.base != found->second.returnType.base ||
      declType.dims != found->second.returnType.dims) {
    Error("return type mismatch for function '" + fn.name + "'");
  }

  PushScope();
  for (size_t i = 0; i < fn.params.size(); ++i) {
    DeclareLocal(fn.params[i].name, paramTypes[i]);
  }
  currentReturnType_ = declType;
  AnalyzeBlock(*fn.body, false);
  currentReturnType_.reset();
  PopScope();
}

void SemanticAnalyzer::AnalyzeGlobal(const ast::GlobalVarDecl &g) {
  TypeInfo declType = TypeFromNode(g.type);
  if (g.init) {
    TypeInfo rhs = AnalyzeExpr(*g.init);
    if (!CanAssign(declType, rhs)) {
      Error("cannot initialize '" + g.name + "' of type " +
            TypeToString(declType) + " with " + TypeToString(rhs));
    }
  }
}

void SemanticAnalyzer::AnalyzeStmt(const ast::Stmt &stmt) {
  if (auto b = dynamic_cast<const ast::BlockStmt*>(&stmt)) {
    AnalyzeBlock(*b, true);
    return;
  }
  if (auto e = dynamic_cast<const ast::ExprStmt*>(&stmt)) {
    AnalyzeExpr(*e->expr);
    return;
  }
  if (auto v = dynamic_cast<const ast::VarDeclStmt*>(&stmt)) {
    TypeInfo declType = TypeFromNode(v->type);
    DeclareLocal(v->name, declType);
    if (v->init) {
      TypeInfo rhs = AnalyzeExpr(*v->init);
      if (!CanAssign(declType, rhs)) {
        Error("cannot initialize '" + v->name + "' of type " +
              TypeToString(declType) + " with " + TypeToString(rhs));
      }
    }
    return;
  }
  if (auto i = dynamic_cast<const ast::IfStmt*>(&stmt)) {
    TypeInfo cond = AnalyzeExpr(*i->cond);
    ExpectBoolCondition(cond, "if");
    AnalyzeStmt(*i->thenBranch);
    if (i->elseBranch) AnalyzeStmt(*i->elseBranch);
    return;
  }
  if (auto w = dynamic_cast<const ast::WhileStmt*>(&stmt)) {
    TypeInfo cond = AnalyzeExpr(*w->cond);
    ExpectBoolCondition(cond, "while");
    loopDepth_++;
    AnalyzeBlock(*w->body, true);
    loopDepth_--;
    return;
  }
  if (auto d = dynamic_cast<const ast::DoWhileStmt*>(&stmt)) {
    loopDepth_++;
    AnalyzeBlock(*d->body, true);
    loopDepth_--;
    TypeInfo cond = AnalyzeExpr(*d->cond);
    ExpectBoolCondition(cond, "do-while");
    return;
  }
  if (auto f = dynamic_cast<const ast::ForStmt*>(&stmt)) {
    PushScope();
    if (f->init) {
      AnalyzeStmt(*f->init);
    }
    if (f->cond) {
      TypeInfo cond = AnalyzeExpr(*f->cond);
      ExpectBoolCondition(cond, "for");
    }
    if (f->step) {
      AnalyzeExpr(*f->step);
    }
    loopDepth_++;
    AnalyzeBlock(*f->body, true);
    loopDepth_--;
    PopScope();
    return;
  }
  if (dynamic_cast<const ast::BreakStmt*>(&stmt)) {
    if (loopDepth_ <= 0) Error("'break' outside loop");
    return;
  }
  if (dynamic_cast<const ast::ContinueStmt*>(&stmt)) {
    if (loopDepth_ <= 0) Error("'continue' outside loop");
    return;
  }
  if (auto r = dynamic_cast<const ast::ReturnStmt*>(&stmt)) {
    if (!currentReturnType_.has_value()) {
      Error("'return' outside function");
    }
    if (!r->expr) {
      Error("return value is required");
    }
    TypeInfo ret = AnalyzeExpr(*r->expr);
    if (!CanAssign(*currentReturnType_, ret)) {
      Error("return type mismatch: expected " +
            TypeToString(*currentReturnType_) + ", got " + TypeToString(ret));
    }
    return;
  }
}

void SemanticAnalyzer::AnalyzeBlock(const ast::BlockStmt &block, bool createScope) {
  if (createScope) PushScope();
  for (const auto &stmt : block.statements) {
    AnalyzeStmt(*stmt);
  }
  if (createScope) PopScope();
}

std::optional<TypeInfo> SemanticAnalyzer::AnalyzeLValue(const ast::Expr &expr) {
  if (auto id = dynamic_cast<const ast::IdentifierExpr*>(&expr)) {
    try {
      return LookupVariable(id->name);
    } catch (const std::exception &) {
      return std::nullopt;
    }
  }

  if (auto idx = dynamic_cast<const ast::IndexExpr*>(&expr)) {
    TypeInfo base = AnalyzeExpr(*idx->base);
    if (base.dims <= 0) {
      Error("cannot index non-array value");
    }
    for (const auto &i : idx->indices) {
      TypeInfo t = AnalyzeExpr(*i);
      if (!IsIntegralScalar(t)) {
        Error("array index must be integral, got " + TypeToString(t));
      }
      base.dims--;
      if (base.dims < 0) {
        Error("too many indices for array");
      }
    }
    return base;
  }

  return std::nullopt;
}

TypeInfo SemanticAnalyzer::AnalyzeExpr(const ast::Expr &expr) {
  if (auto i = dynamic_cast<const ast::IntLiteralExpr*>(&expr)) {
    (void)i;
    return {BaseTypeKind::INT, 0};
  }
  if (auto d = dynamic_cast<const ast::DoubleLiteralExpr*>(&expr)) {
    (void)d;
    return {BaseTypeKind::DOUBLE, 0};
  }
  if (auto b = dynamic_cast<const ast::BoolLiteralExpr*>(&expr)) {
    (void)b;
    return {BaseTypeKind::BOOL, 0};
  }
  if (auto c = dynamic_cast<const ast::CharLiteralExpr*>(&expr)) {
    (void)c;
    return {BaseTypeKind::CHAR, 0};
  }
  if (auto s = dynamic_cast<const ast::StringLiteralExpr*>(&expr)) {
    (void)s;
    return {BaseTypeKind::STRING, 0};
  }
  if (auto id = dynamic_cast<const ast::IdentifierExpr*>(&expr)) {
    try {
      return LookupVariable(id->name);
    } catch (const std::exception &e) {
      Error(e.what());
    }
  }
  if (auto call = dynamic_cast<const ast::CallExpr*>(&expr)) {
    auto found = functions_.find(call->callee);
    if (found == functions_.end()) {
      Error("call to undeclared function '" + call->callee + "'");
    }
    if (call->args.size() != found->second.params.size()) {
      Error("wrong argument count in call to '" + call->callee + "'");
    }
    for (size_t i = 0; i < call->args.size(); ++i) {
      TypeInfo arg = AnalyzeExpr(*call->args[i]);
      if (!CanAssign(found->second.params[i], arg)) {
        Error("argument type mismatch in call to '" + call->callee + "'");
      }
    }
    return found->second.returnType;
  }
  if (auto idx = dynamic_cast<const ast::IndexExpr*>(&expr)) {
    TypeInfo base = AnalyzeExpr(*idx->base);
    if (base.dims <= 0) {
      Error("indexing non-array type " + TypeToString(base));
    }
    for (const auto &i : idx->indices) {
      TypeInfo t = AnalyzeExpr(*i);
      if (!IsIntegralScalar(t)) {
        Error("array index must be integral, got " + TypeToString(t));
      }
      base.dims--;
      if (base.dims < 0) {
        Error("too many indices for array");
      }
    }
    return base;
  }
  if (auto un = dynamic_cast<const ast::UnaryExpr*>(&expr)) {
    TypeInfo inner = AnalyzeExpr(*un->operand);
    if (un->op == TokenType::PLUS || un->op == TokenType::MINUS) {
      if (!IsNumericScalar(inner)) {
        Error("unary +/- requires numeric operand");
      }
      return inner.base == BaseTypeKind::DOUBLE ? inner : TypeInfo{BaseTypeKind::INT, 0};
    }
    if (un->op == TokenType::BANG) {
      if (!IsBoolScalar(inner)) {
        Error("unary '!' requires bool operand");
      }
      return BoolType();
    }
    if (un->op == TokenType::TILDE) {
      if (!IsIntegralScalar(inner)) {
        Error("unary '~' requires integral operand");
      }
      return {BaseTypeKind::INT, 0};
    }
    Error("unknown unary operator");
  }
  if (auto bin = dynamic_cast<const ast::BinaryExpr*>(&expr)) {
    TokenType op = bin->op;

    if (op == TokenType::ASSIGN) {
      auto lhs = AnalyzeLValue(*bin->left);
      if (!lhs.has_value()) {
        Error("left-hand side of assignment is not assignable");
      }
      TypeInfo rhs = AnalyzeExpr(*bin->right);
      if (!CanAssign(*lhs, rhs)) {
        Error("cannot assign " + TypeToString(rhs) + " to " + TypeToString(*lhs));
      }
      return *lhs;
    }

    if (op == TokenType::COMMA) {
      AnalyzeExpr(*bin->left);
      return AnalyzeExpr(*bin->right);
    }

    TypeInfo left = AnalyzeExpr(*bin->left);
    TypeInfo right = AnalyzeExpr(*bin->right);

    if (op == TokenType::OROR || op == TokenType::ANDAND) {
      if (!IsBoolScalar(left) || !IsBoolScalar(right)) {
        Error("logical operators require bool operands");
      }
      return BoolType();
    }

    if (op == TokenType::OR || op == TokenType::XOR || op == TokenType::AND) {
      if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
        Error("bitwise operators require integral operands");
      }
      return {BaseTypeKind::INT, 0};
    }

    if (op == TokenType::EQ || op == TokenType::NEQ) {
      bool ok = false;
      if (left.dims == right.dims && left.base == right.base) ok = true;
      if (left.IsScalar() && right.IsScalar() &&
          IsNumericScalar(left) && IsNumericScalar(right)) {
        ok = true;
      }
      if (!ok) {
        Error("incompatible operands for equality comparison");
      }
      return BoolType();
    }

    if (op == TokenType::LT || op == TokenType::GT ||
        op == TokenType::LE || op == TokenType::GE) {
      if (!IsNumericScalar(left) || !IsNumericScalar(right)) {
        Error("relational operators require numeric operands");
      }
      return BoolType();
    }

    if (op == TokenType::SHL || op == TokenType::SHR) {
      if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
        Error("shift operators require integral operands");
      }
      return {BaseTypeKind::INT, 0};
    }

    if (op == TokenType::PLUS || op == TokenType::MINUS) {
      if (op == TokenType::PLUS &&
          left.IsScalar() && right.IsScalar() &&
          left.base == BaseTypeKind::STRING && right.base == BaseTypeKind::STRING) {
        return {BaseTypeKind::STRING, 0};
      }
      TypeInfo common = CommonNumericType(left, right);
      if (!common.IsValid()) {
        Error("operator requires numeric operands");
      }
      return common;
    }

    if (op == TokenType::STAR || op == TokenType::SLASH || op == TokenType::PERCENT) {
      if (op == TokenType::PERCENT) {
        if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
          Error("operator '%' requires integral operands");
        }
        return {BaseTypeKind::INT, 0};
      }
      TypeInfo common = CommonNumericType(left, right);
      if (!common.IsValid()) {
        Error("operator requires numeric operands");
      }
      return common;
    }

    Error("unknown binary operator");
  }

  Error("unknown expression kind");
  return {BaseTypeKind::INVALID, 0};
}
