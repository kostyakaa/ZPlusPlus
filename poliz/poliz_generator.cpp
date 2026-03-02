#include "poliz_generator.h"

#include <limits>
#include <sstream>
#include <stdexcept>

namespace {

int SafeMul(int a, int b) {
  if (a == 0 || b == 0) return 0;
  long long v = static_cast<long long>(a) * static_cast<long long>(b);
  if (v > std::numeric_limits<int>::max()) {
    throw std::runtime_error("POLIZ: array size overflow");
  }
  return static_cast<int>(v);
}

} // namespace

Poliz PolizGenerator::Generate(const ast::Program &program) {
  program_ = &program;
  code_ = Poliz{};
  functions_.clear();
  scopes_.clear();
  currentOffset_ = 0;
  breakPatches_.clear();
  continuePatches_.clear();
  returnPatches_.clear();
  funcAddrPatches_.clear();

  CollectFunctionSigs();

  PushScope(); // globals
  PredeclareGlobals();
  int globalAllocIndex = code_.GiveSize();
  code_.AddEl({PolizOp::ALLOCATE, ""});

  EmitProgram();

  code_.UpdateEl({PolizOp::ALLOCATE, std::to_string(currentOffset_)}, globalAllocIndex);
  code_.AddEl({PolizOp::FREE, std::to_string(currentOffset_)});
  code_.AddEl({PolizOp::END_OF_PROGRAM, ""});

  PopScope();

  for (const auto &patch : funcAddrPatches_) {
    auto it = functions_.find(patch.second);
    if (it == functions_.end() || it->second.address < 0) {
      throw std::runtime_error("POLIZ: unresolved function '" + patch.second + "'");
    }
    code_.UpdateEl({PolizOp::FUNCTION_ADRESS, std::to_string(it->second.address)}, patch.first);
  }

  return code_;
}

PolizGenerator::BaseType PolizGenerator::ToBaseType(ast::BaseType t) {
  switch (t) {
    case ast::BaseType::INT: return BaseType::INT;
    case ast::BaseType::DOUBLE: return BaseType::DOUBLE;
    case ast::BaseType::BOOL: return BaseType::BOOL;
    case ast::BaseType::CHAR: return BaseType::CHAR;
    case ast::BaseType::STRING: return BaseType::STRING;
  }
  return BaseType::INT;
}

const char *PolizGenerator::BaseTypeToString(BaseType t) {
  switch (t) {
    case BaseType::INT: return "int";
    case BaseType::DOUBLE: return "double";
    case BaseType::BOOL: return "bool";
    case BaseType::CHAR: return "char";
    case BaseType::STRING: return "string";
  }
  return "unknown";
}

int PolizGenerator::BaseTypeSize(BaseType t) {
  switch (t) {
    case BaseType::INT: return sizeof(int);
    case BaseType::DOUBLE: return sizeof(double);
    case BaseType::BOOL: return sizeof(bool);
    case BaseType::CHAR: return sizeof(char);
    case BaseType::STRING: return sizeof(int);
  }
  return sizeof(int);
}

std::string PolizGenerator::OpToString(TokenType t) {
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
    default: break;
  }
  return "?";
}

PolizOp PolizGenerator::BaseToAddressOp(BaseType t) {
  switch (t) {
    case BaseType::INT: return PolizOp::ADRESS_INT;
    case BaseType::DOUBLE: return PolizOp::ADRESS_DOUBLE;
    case BaseType::BOOL: return PolizOp::ADRESS_BOOL;
    case BaseType::CHAR: return PolizOp::ADRESS_CHAR;
    case BaseType::STRING: return PolizOp::ADRESS_STRING;
  }
  return PolizOp::ADRESS_INT;
}

PolizOp PolizGenerator::BaseToLiteralOp(BaseType t) {
  switch (t) {
    case BaseType::INT: return PolizOp::INT;
    case BaseType::DOUBLE: return PolizOp::DOUBLE;
    case BaseType::BOOL: return PolizOp::BOOL;
    case BaseType::CHAR: return PolizOp::CHAR;
    case BaseType::STRING: return PolizOp::STRING;
  }
  return PolizOp::INT;
}

void PolizGenerator::PushScope() { scopes_.emplace_back(); }
void PolizGenerator::PopScope() { scopes_.pop_back(); }

void PolizGenerator::DeclareVar(const std::string &name, const VarInfo &info) {
  auto &cur = scopes_.back();
  if (cur.count(name) != 0) {
    throw std::runtime_error("POLIZ: redeclaration of '" + name + "'");
  }
  cur[name] = info;
}

PolizGenerator::VarInfo PolizGenerator::LookupVar(const std::string &name) const {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto found = it->find(name);
    if (found != it->end()) return found->second;
  }
  throw std::runtime_error("POLIZ: undeclared identifier '" + name + "'");
}

void PolizGenerator::CollectFunctionSigs() {
  for (const auto &decl : program_->decls) {
    if (auto fn = dynamic_cast<const ast::FunctionDecl*>(decl.get())) {
      CollectFunctionSig(*fn);
    }
  }
}

void PolizGenerator::CollectFunctionSig(const ast::FunctionDecl &fn) {
  if (functions_.count(fn.name) != 0) {
    throw std::runtime_error("POLIZ: duplicate function '" + fn.name + "'");
  }
  FuncInfo info{};
  info.returnType = TypeFromNode(fn.returnType);
  for (const auto &p : fn.params) {
    info.params.push_back(TypeFromNode(p.type));
    info.paramNames.push_back(p.name);
  }
  functions_[fn.name] = info;
}

void PolizGenerator::PredeclareGlobals() {
  currentOffset_ = 0;
  for (const auto &decl : program_->decls) {
    auto g = dynamic_cast<const ast::GlobalVarDecl*>(decl.get());
    if (!g) continue;
    TypeInfo t = TypeFromNode(g->type);
    std::vector<int> dims = EvalArrayDims(g->type);

    int totalSize = BaseTypeSize(t.base);
    if (!dims.empty()) {
      int count = 1;
      for (int d : dims) count = SafeMul(count, d);
      totalSize = SafeMul(totalSize, count);
    }

    VarInfo info{t, currentOffset_, dims};
    DeclareVar(g->name, info);
    currentOffset_ += totalSize;
  }
}

void PolizGenerator::EmitProgram() {
  for (const auto &decl : program_->decls) {
    EmitTopLevel(*decl);
  }
}

void PolizGenerator::EmitTopLevel(const ast::TopLevelDecl &decl) {
  if (auto fn = dynamic_cast<const ast::FunctionDecl*>(&decl)) {
    EmitFunction(*fn);
    return;
  }
  if (auto g = dynamic_cast<const ast::GlobalVarDecl*>(&decl)) {
    EmitGlobalVar(*g);
    return;
  }
}

void PolizGenerator::EmitFunction(const ast::FunctionDecl &fn) {
  auto it = functions_.find(fn.name);
  if (it == functions_.end()) {
    throw std::runtime_error("POLIZ: unknown function '" + fn.name + "'");
  }

  int savedOffset = currentOffset_;
  currentOffset_ = 0;

  int labelIndex = code_.GiveSize();
  code_.AddEl({PolizOp::POLIZ_LABEL, ""});
  code_.AddEl({PolizOp::POLIZ_GO, ""});

  int allocIndex = code_.GiveSize();
  code_.AddEl({PolizOp::ALLOCATE, ""});

  it->second.address = allocIndex;

  returnPatches_.clear();
  PushScope();

  int paramsSize = 0;
  it->second.paramOffsets.clear();
  it->second.paramIsArray.clear();
  for (size_t i = 0; i < it->second.params.size(); ++i) {
    TypeInfo t = it->second.params[i];
    bool isArray = t.dims > 0;
    int size = isArray ? sizeof(int) : BaseTypeSize(t.base);
    std::vector<int> dims = EvalArrayDims(fn.params[i].type);
    VarInfo v{t, paramsSize, dims};
    DeclareVar(it->second.paramNames[i], v);
    it->second.paramOffsets.push_back(paramsSize);
    it->second.paramIsArray.push_back(isArray);
    paramsSize += size;
  }
  currentOffset_ = paramsSize;

  for (const auto &stmt : fn.body->statements) {
    EmitStmt(*stmt);
  }

  int localsSize = currentOffset_ - paramsSize;
  int endTarget = code_.GiveSize();
  for (int idx : returnPatches_) {
    code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endTarget)}, idx);
  }
  code_.UpdateEl({PolizOp::ALLOCATE, std::to_string(localsSize)}, allocIndex);
  code_.AddEl({PolizOp::FREE, std::to_string(localsSize)});
  code_.AddEl({PolizOp::END_FUNCTION, ""});

  code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(code_.GiveSize())}, labelIndex);

  PopScope();
  currentOffset_ = savedOffset;
}

void PolizGenerator::EmitGlobalVar(const ast::GlobalVarDecl &g) {
  if (g.init) {
    EmitLValueAddress(ast::IdentifierExpr(g.name));
    EmitExpr(*g.init);
    code_.AddEl({PolizOp::OPERATION, "="});
  }
}

void PolizGenerator::EmitStmt(const ast::Stmt &stmt) {
  if (auto b = dynamic_cast<const ast::BlockStmt*>(&stmt)) {
    EmitBlock(*b);
    return;
  }
  if (auto e = dynamic_cast<const ast::ExprStmt*>(&stmt)) {
    EmitExpr(*e->expr);
    return;
  }
  if (auto v = dynamic_cast<const ast::VarDeclStmt*>(&stmt)) {
    TypeInfo t = TypeFromNode(v->type);
    std::vector<int> dims = EvalArrayDims(v->type);

    int totalSize = BaseTypeSize(t.base);
    if (!dims.empty()) {
      int count = 1;
      for (int d : dims) count = SafeMul(count, d);
      totalSize = SafeMul(totalSize, count);
    }

    VarInfo info{t, currentOffset_, dims};
    DeclareVar(v->name, info);
    currentOffset_ += totalSize;

    if (v->init) {
      EmitLValueAddress(ast::IdentifierExpr(v->name));
      EmitExpr(*v->init);
      code_.AddEl({PolizOp::OPERATION, "="});
    }
    return;
  }
  if (auto i = dynamic_cast<const ast::IfStmt*>(&stmt)) {
    EmitExpr(*i->cond);
    int falseLabel = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_FGO, ""});

    EmitStmt(*i->thenBranch);

    if (i->elseBranch) {
      int endLabel = code_.GiveSize();
      code_.AddEl({PolizOp::POLIZ_LABEL, ""});
      code_.AddEl({PolizOp::POLIZ_GO, ""});

      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(code_.GiveSize())}, falseLabel);
      EmitStmt(*i->elseBranch);
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(code_.GiveSize())}, endLabel);
    } else {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(code_.GiveSize())}, falseLabel);
    }
    return;
  }
  if (auto w = dynamic_cast<const ast::WhileStmt*>(&stmt)) {
    int startIp = code_.GiveSize();
    EmitExpr(*w->cond);
    int endLabel = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_FGO, ""});

    breakPatches_.push_back({});
    continuePatches_.push_back({});

    EmitStmt(*w->body);

    code_.AddEl({PolizOp::POLIZ_LABEL, std::to_string(startIp)});
    code_.AddEl({PolizOp::POLIZ_GO, ""});

    int endIp = code_.GiveSize();
    code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, endLabel);
    for (int idx : breakPatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, idx);
    }
    for (int idx : continuePatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(startIp)}, idx);
    }
    breakPatches_.pop_back();
    continuePatches_.pop_back();
    return;
  }
  if (auto d = dynamic_cast<const ast::DoWhileStmt*>(&stmt)) {
    int startIp = code_.GiveSize();
    breakPatches_.push_back({});
    continuePatches_.push_back({});

    EmitStmt(*d->body);
    int condIp = code_.GiveSize();
    EmitExpr(*d->cond);
    int endLabel = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_FGO, ""});
    code_.AddEl({PolizOp::POLIZ_LABEL, std::to_string(startIp)});
    code_.AddEl({PolizOp::POLIZ_GO, ""});

    int endIp = code_.GiveSize();
    code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, endLabel);
    for (int idx : breakPatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, idx);
    }
    for (int idx : continuePatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(condIp)}, idx);
    }
    breakPatches_.pop_back();
    continuePatches_.pop_back();
    return;
  }
  if (auto f = dynamic_cast<const ast::ForStmt*>(&stmt)) {
    PushScope();
    int savedOffset = currentOffset_;
    int allocIndex = code_.GiveSize();
    code_.AddEl({PolizOp::ALLOCATE, ""});

    if (f->init) {
      EmitStmt(*f->init);
    }

    int condIp = code_.GiveSize();
    if (f->cond) {
      EmitExpr(*f->cond);
    } else {
      code_.AddEl({PolizOp::BOOL, "true"});
    }

    int endLabel = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_FGO, ""});

    breakPatches_.push_back({});
    continuePatches_.push_back({});

    EmitStmt(*f->body);

    int stepIp = code_.GiveSize();
    if (f->step) {
      EmitExpr(*f->step);
    }
    code_.AddEl({PolizOp::POLIZ_LABEL, std::to_string(condIp)});
    code_.AddEl({PolizOp::POLIZ_GO, ""});

    int endIp = code_.GiveSize();
    code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, endLabel);
    for (int idx : breakPatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(endIp)}, idx);
    }
    for (int idx : continuePatches_.back()) {
      code_.UpdateEl({PolizOp::POLIZ_LABEL, std::to_string(stepIp)}, idx);
    }
    breakPatches_.pop_back();
    continuePatches_.pop_back();

    int size = currentOffset_ - savedOffset;
    code_.UpdateEl({PolizOp::ALLOCATE, std::to_string(size)}, allocIndex);
    code_.AddEl({PolizOp::FREE, std::to_string(size)});
    currentOffset_ = savedOffset;
    PopScope();
    return;
  }
  if (dynamic_cast<const ast::BreakStmt*>(&stmt)) {
    int labelIndex = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_GO, ""});
    if (breakPatches_.empty()) {
      throw std::runtime_error("POLIZ: 'break' outside loop");
    }
    breakPatches_.back().push_back(labelIndex);
    return;
  }
  if (dynamic_cast<const ast::ContinueStmt*>(&stmt)) {
    int labelIndex = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_GO, ""});
    if (continuePatches_.empty()) {
      throw std::runtime_error("POLIZ: 'continue' outside loop");
    }
    continuePatches_.back().push_back(labelIndex);
    return;
  }
  if (auto r = dynamic_cast<const ast::ReturnStmt*>(&stmt)) {
    if (r->expr) {
      EmitExpr(*r->expr);
    }
    int labelIndex = code_.GiveSize();
    code_.AddEl({PolizOp::POLIZ_LABEL, ""});
    code_.AddEl({PolizOp::POLIZ_GO, ""});
    returnPatches_.push_back(labelIndex);
    return;
  }
}

void PolizGenerator::EmitBlock(const ast::BlockStmt &block) {
  PushScope();
  int startOffset = currentOffset_;
  int allocIndex = code_.GiveSize();
  code_.AddEl({PolizOp::ALLOCATE, ""});

  for (const auto &stmt : block.statements) {
    EmitStmt(*stmt);
  }

  int size = currentOffset_ - startOffset;
  code_.UpdateEl({PolizOp::ALLOCATE, std::to_string(size)}, allocIndex);
  code_.AddEl({PolizOp::FREE, std::to_string(size)});
  currentOffset_ = startOffset;
  PopScope();
}

void PolizGenerator::EmitExpr(const ast::Expr &expr) {
  if (auto e = dynamic_cast<const ast::IntLiteralExpr*>(&expr)) {
    code_.AddEl({PolizOp::INT, e->value});
    return;
  }
  if (auto e = dynamic_cast<const ast::DoubleLiteralExpr*>(&expr)) {
    code_.AddEl({PolizOp::DOUBLE, e->value});
    return;
  }
  if (auto e = dynamic_cast<const ast::BoolLiteralExpr*>(&expr)) {
    code_.AddEl({PolizOp::BOOL, e->value ? "true" : "false"});
    return;
  }
  if (auto e = dynamic_cast<const ast::CharLiteralExpr*>(&expr)) {
    code_.AddEl({PolizOp::CHAR, e->value});
    return;
  }
  if (auto e = dynamic_cast<const ast::StringLiteralExpr*>(&expr)) {
    code_.AddEl({PolizOp::STRING, e->value});
    return;
  }
  if (auto e = dynamic_cast<const ast::IdentifierExpr*>(&expr)) {
    EmitLValueAddress(*e);
    return;
  }
  if (auto e = dynamic_cast<const ast::IndexExpr*>(&expr)) {
    EmitLValueAddress(*e);
    return;
  }
  if (auto e = dynamic_cast<const ast::UnaryExpr*>(&expr)) {
    EmitExpr(*e->operand);
    code_.AddEl({PolizOp::UNARY_OPERATION, OpToString(e->op)});
    return;
  }
  if (auto e = dynamic_cast<const ast::BinaryExpr*>(&expr)) {
    if (e->op == TokenType::ASSIGN) {
      EmitLValueAddress(*e->left);
      EmitExpr(*e->right);
      code_.AddEl({PolizOp::OPERATION, "="});
      return;
    }
    EmitExpr(*e->left);
    EmitExpr(*e->right);
    code_.AddEl({PolizOp::OPERATION, OpToString(e->op)});
    return;
  }
  if (auto e = dynamic_cast<const ast::CallExpr*>(&expr)) {
    for (const auto &arg : e->args) {
      EmitExpr(*arg);
    }
    int addrIndex = code_.GiveSize();
    code_.AddEl({PolizOp::FUNCTION_ADRESS, ""});
    code_.AddEl({PolizOp::CALL_FUNCTION, ""});
    funcAddrPatches_.push_back({addrIndex, e->callee});
    return;
  }
}

void PolizGenerator::EmitLValueAddress(const ast::Expr &expr) {
  if (auto id = dynamic_cast<const ast::IdentifierExpr*>(&expr)) {
    VarInfo v = LookupVar(id->name);
    if (v.type.dims > 0) {
      // arrays must be indexed before use
      throw std::runtime_error("POLIZ: array '" + id->name + "' must be indexed");
    }
    code_.AddEl({PolizOp::INT, std::to_string(v.offset)});
    code_.AddEl({BaseToAddressOp(v.type.base), ""});
    return;
  }
  if (auto idx = dynamic_cast<const ast::IndexExpr*>(&expr)) {
    auto baseId = dynamic_cast<const ast::IdentifierExpr*>(idx->base.get());
    if (!baseId) {
      throw std::runtime_error("POLIZ: invalid array base in index");
    }
    VarInfo v = LookupVar(baseId->name);
    if (v.type.dims <= 0) {
      throw std::runtime_error("POLIZ: indexing non-array '" + baseId->name + "'");
    }
    if (static_cast<int>(idx->indices.size()) != v.type.dims) {
      throw std::runtime_error("POLIZ: array index count mismatch for '" + baseId->name + "'");
    }

    int all = 1;
    for (int d : v.dims) all = SafeMul(all, d);

    for (size_t i = 0; i < idx->indices.size(); ++i) {
      all /= v.dims[i];
      EmitExpr(*idx->indices[i]);
      code_.AddEl({PolizOp::INT, std::to_string(all)});
      code_.AddEl({PolizOp::OPERATION, "*"});
      if (i != 0) code_.AddEl({PolizOp::OPERATION, "+"});
    }

    int elemSize = BaseTypeSize(v.type.base);
    code_.AddEl({PolizOp::INT, std::to_string(elemSize)});
    code_.AddEl({PolizOp::OPERATION, "*"});

    code_.AddEl({PolizOp::INT, std::to_string(v.offset)});
    code_.AddEl({PolizOp::OPERATION, "+"});

    code_.AddEl({BaseToAddressOp(v.type.base), ""});
    return;
  }

  throw std::runtime_error("POLIZ: invalid lvalue");
}

PolizGenerator::TypeInfo PolizGenerator::TypeFromNode(const ast::TypeNode &t) const {
  TypeInfo info{ToBaseType(t.base), static_cast<int>(t.dims.size())};
  return info;
}

std::vector<int> PolizGenerator::EvalArrayDims(const ast::TypeNode &t) {
  std::vector<int> dims;
  dims.reserve(t.dims.size());
  for (const auto &expr : t.dims) {
    auto v = EvalConstInt(*expr);
    if (!v.has_value()) {
      throw std::runtime_error("POLIZ: array size must be constant integer");
    }
    if (*v <= 0) {
      throw std::runtime_error("POLIZ: array size must be positive");
    }
    if (*v > std::numeric_limits<int>::max()) {
      throw std::runtime_error("POLIZ: array size too large");
    }
    dims.push_back(static_cast<int>(*v));
  }
  return dims;
}

std::optional<long long> PolizGenerator::EvalConstInt(const ast::Expr &expr) {
  if (auto e = dynamic_cast<const ast::IntLiteralExpr*>(&expr)) {
    return std::stoll(e->value);
  }
  if (auto e = dynamic_cast<const ast::BoolLiteralExpr*>(&expr)) {
    return e->value ? 1LL : 0LL;
  }
  if (auto e = dynamic_cast<const ast::CharLiteralExpr*>(&expr)) {
    if (!e->value.empty()) return static_cast<long long>(e->value[0]);
    return 0LL;
  }
  if (auto e = dynamic_cast<const ast::UnaryExpr*>(&expr)) {
    auto v = EvalConstInt(*e->operand);
    if (!v.has_value()) return std::nullopt;
    switch (e->op) {
      case TokenType::PLUS: return *v;
      case TokenType::MINUS: return -*v;
      case TokenType::BANG: return (*v == 0) ? 1LL : 0LL;
      case TokenType::TILDE: return ~(*v);
      default: return std::nullopt;
    }
  }
  if (auto e = dynamic_cast<const ast::BinaryExpr*>(&expr)) {
    if (e->op == TokenType::COMMA) {
      auto r = EvalConstInt(*e->right);
      return r;
    }
    auto a = EvalConstInt(*e->left);
    auto b = EvalConstInt(*e->right);
    if (!a.has_value() || !b.has_value()) return std::nullopt;
    switch (e->op) {
      case TokenType::PLUS: return *a + *b;
      case TokenType::MINUS: return *a - *b;
      case TokenType::STAR: return *a * *b;
      case TokenType::SLASH:
        if (*b == 0) return std::nullopt;
        return *a / *b;
      case TokenType::PERCENT:
        if (*b == 0) return std::nullopt;
        return *a % *b;
      case TokenType::SHL: return *a << *b;
      case TokenType::SHR: return *a >> *b;
      case TokenType::OR: return *a | *b;
      case TokenType::XOR: return *a ^ *b;
      case TokenType::AND: return *a & *b;
      case TokenType::EQ: return (*a == *b) ? 1LL : 0LL;
      case TokenType::NEQ: return (*a != *b) ? 1LL : 0LL;
      case TokenType::LT: return (*a < *b) ? 1LL : 0LL;
      case TokenType::GT: return (*a > *b) ? 1LL : 0LL;
      case TokenType::LE: return (*a <= *b) ? 1LL : 0LL;
      case TokenType::GE: return (*a >= *b) ? 1LL : 0LL;
      default: return std::nullopt;
    }
  }
  return std::nullopt;
}
