#include "semanter.h"

#include <sstream>
#include <stdexcept>
#include <utility>

SemanticAnalyzer::SemanticAnalyzer(std::vector<Token> tokens)
    : tokens_(std::move(tokens)) {}

void SemanticAnalyzer::Analyze() {
  CollectTopLevelSymbols();
  pos_ = 0;
  ParseProgram();
}

bool SemanticAnalyzer::IsTypeStart(TokenType t) {
  return t == TokenType::KW_INT || t == TokenType::KW_DOUBLE ||
         t == TokenType::KW_BOOL || t == TokenType::KW_CHAR ||
         t == TokenType::KW_STRING;
}

bool SemanticAnalyzer::IsExpressionStart(TokenType t) {
  return t == TokenType::INT_LITERAL || t == TokenType::DOUBLE_LITERAL ||
         t == TokenType::BOOL_LITERAL || t == TokenType::CHAR_LITERAL ||
         t == TokenType::STRING_LITERAL || t == TokenType::IDENTIFIER ||
         t == TokenType::LPAREN || t == TokenType::PLUS ||
         t == TokenType::MINUS || t == TokenType::BANG ||
         t == TokenType::TILDE;
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

BaseTypeKind SemanticAnalyzer::TokenToBaseType(TokenType t) {
  switch (t) {
    case TokenType::KW_INT: return BaseTypeKind::INT;
    case TokenType::KW_DOUBLE: return BaseTypeKind::DOUBLE;
    case TokenType::KW_BOOL: return BaseTypeKind::BOOL;
    case TokenType::KW_CHAR: return BaseTypeKind::CHAR;
    case TokenType::KW_STRING: return BaseTypeKind::STRING;
    default: return BaseTypeKind::INVALID;
  }
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

[[noreturn]] void SemanticAnalyzer::ErrorAt(const Token &t, const std::string &message) const {
  std::ostringstream oss;
  if (t.type == TokenType::END_OF_FILE) {
    oss << "Semantic error at EOF: " << message;
  } else {
    oss << "Semantic error at " << t.line << ":" << t.col << ": " << message;
  }
  throw std::runtime_error(oss.str());
}

Token SemanticAnalyzer::Peek(int offset) const {
  if (offset < 0) {
    size_t off = static_cast<size_t>(-offset);
    if (off > pos_) return tokens_.front();
    return tokens_[pos_ - off];
  }
  size_t p = pos_ + static_cast<size_t>(offset);
  if (p >= tokens_.size()) return tokens_.back();
  return tokens_[p];
}

Token SemanticAnalyzer::Consume() {
  Token t = Peek();
  if (pos_ < tokens_.size()) ++pos_;
  return t;
}

bool SemanticAnalyzer::Match(TokenType type) {
  if (Peek().type == type) {
    Consume();
    return true;
  }
  return false;
}

Token SemanticAnalyzer::Expect(TokenType type, const char *expected) {
  Token t = Peek();
  if (t.type != type) {
    std::ostringstream oss;
    oss << "expected " << expected << ", got '" << t.text << "'";
    ErrorAt(t, oss.str());
  }
  return Consume();
}

void SemanticAnalyzer::PushScope() { scopes_.emplace_back(); }
void SemanticAnalyzer::PopScope() { scopes_.pop_back(); }

void SemanticAnalyzer::DeclareLocal(const Token &id, const TypeInfo &type) {
  if (scopes_.empty()) {
    ErrorAt(id, "internal error: no active scope");
  }
  auto &cur = scopes_.back();
  if (cur.count(id.text) != 0) {
    ErrorAt(id, "redeclaration of '" + id.text + "'");
  }
  cur[id.text] = type;
}

TypeInfo SemanticAnalyzer::LookupVariable(const Token &id) const {
  for (auto it = scopes_.rbegin(); it != scopes_.rend(); ++it) {
    auto found = it->find(id.text);
    if (found != it->end()) return found->second;
  }
  auto g = globals_.find(id.text);
  if (g != globals_.end()) return g->second;
  throw std::runtime_error("undeclared identifier '" + id.text + "'");
}

void SemanticAnalyzer::ExpectBoolCondition(const TypeInfo &cond, const Token &where) {
  if (!IsBoolScalar(cond)) {
    ErrorAt(where, "condition must be bool, got " + TypeToString(cond));
  }
}

void SemanticAnalyzer::CollectTopLevelSymbols() {
  size_t saved = pos_;
  pos_ = 0;

  while (IsTypeStart(Peek().type)) {
    TypeInfo declType = ParseTypeShallow();
    Token id = Expect(TokenType::IDENTIFIER, "identifier");

    if (Match(TokenType::LPAREN)) {
      if (declType.dims > 0) {
        ErrorAt(id, "function '" + id.text + "' cannot return an array type");
      }
      std::vector<TypeInfo> params = ParseParamTypesShallow();
      Expect(TokenType::RPAREN, "')'");

      if (functions_.count(id.text) != 0 || globals_.count(id.text) != 0) {
        ErrorAt(id, "redefinition of top-level symbol '" + id.text + "'");
      }
      functions_[id.text] = FunctionSig{declType, params, id};

      SkipBlockShallow();
    } else {
      if (functions_.count(id.text) != 0 || globals_.count(id.text) != 0) {
        ErrorAt(id, "redefinition of top-level symbol '" + id.text + "'");
      }
      globals_[id.text] = declType;
      SkipUntilSemicolonShallow();
    }
  }

  Expect(TokenType::END_OF_FILE, "EOF");
  pos_ = saved;
}

TypeInfo SemanticAnalyzer::ParseTypeShallow() {
  Token t = Peek();
  if (!IsTypeStart(t.type)) ErrorAt(t, "expected type");
  TypeInfo type{TokenToBaseType(Consume().type), 0};
  while (Match(TokenType::LBRACKET)) {
    SkipBalancedUntilRBracketShallow();
    type.dims++;
  }
  return type;
}

std::vector<TypeInfo> SemanticAnalyzer::ParseParamTypesShallow() {
  std::vector<TypeInfo> params;
  if (!IsTypeStart(Peek().type)) return params;

  while (true) {
    TypeInfo t = ParseTypeShallow();
    Expect(TokenType::IDENTIFIER, "parameter name");
    params.push_back(t);
    if (!Match(TokenType::COMMA)) break;
  }
  return params;
}

void SemanticAnalyzer::SkipBalancedUntilRBracketShallow() {
  int nested = 1;
  while (nested > 0) {
    Token t = Consume();
    if (t.type == TokenType::END_OF_FILE) ErrorAt(t, "unterminated array suffix");
    if (t.type == TokenType::LBRACKET) nested++;
    if (t.type == TokenType::RBRACKET) nested--;
  }
}

void SemanticAnalyzer::SkipUntilSemicolonShallow() {
  int paren = 0;
  int bracket = 0;
  while (true) {
    Token t = Consume();
    if (t.type == TokenType::END_OF_FILE) ErrorAt(t, "expected ';'");
    if (t.type == TokenType::LPAREN) paren++;
    if (t.type == TokenType::RPAREN) paren--;
    if (t.type == TokenType::LBRACKET) bracket++;
    if (t.type == TokenType::RBRACKET) bracket--;
    if (t.type == TokenType::SEMICOLON && paren == 0 && bracket == 0) break;
  }
}

void SemanticAnalyzer::SkipBlockShallow() {
  Expect(TokenType::LBRACE, "'{'");
  int depth = 1;
  while (depth > 0) {
    Token t = Consume();
    if (t.type == TokenType::END_OF_FILE) ErrorAt(t, "unterminated block");
    if (t.type == TokenType::LBRACE) depth++;
    if (t.type == TokenType::RBRACE) depth--;
  }
}

void SemanticAnalyzer::ParseProgram() {
  while (IsTypeStart(Peek().type)) {
    ParseTopLevel();
  }
  Expect(TokenType::END_OF_FILE, "EOF");
}

void SemanticAnalyzer::ParseTopLevel() {
  TypeInfo declType = ParseType();
  Token id = Expect(TokenType::IDENTIFIER, "identifier");

  if (Match(TokenType::LPAREN)) {
    if (declType.dims > 0) {
      ErrorAt(id, "function '" + id.text + "' cannot return an array type");
    }
    auto found = functions_.find(id.text);
    if (found == functions_.end()) {
      ErrorAt(id, "unknown function '" + id.text + "'");
    }

    std::vector<Token> paramNames;
    std::vector<TypeInfo> paramTypes = ParseParamList(paramNames);
    Expect(TokenType::RPAREN, "')'");

    if (paramTypes.size() != found->second.params.size()) {
      ErrorAt(id, "parameter count mismatch for '" + id.text + "'");
    }

    for (size_t i = 0; i < paramTypes.size(); ++i) {
      if (paramTypes[i].base != found->second.params[i].base ||
          paramTypes[i].dims != found->second.params[i].dims) {
        ErrorAt(paramNames[i], "parameter type mismatch for function '" + id.text + "'");
      }
    }

    if (declType.base != found->second.returnType.base ||
        declType.dims != found->second.returnType.dims) {
      ErrorAt(id, "return type mismatch for function '" + id.text + "'");
    }

    PushScope();
    for (size_t i = 0; i < paramNames.size(); ++i) {
      DeclareLocal(paramNames[i], paramTypes[i]);
    }
    currentReturnType_ = declType;
    ParseBlock(false);
    currentReturnType_.reset();
    PopScope();
  } else {
    ParseVarDeclInitOpt(declType, id, true);
    Expect(TokenType::SEMICOLON, "';'");
  }
}

TypeInfo SemanticAnalyzer::ParseType() {
  Token t = Peek();
  if (!IsTypeStart(t.type)) ErrorAt(t, "expected type");

  TypeInfo type{TokenToBaseType(Consume().type), 0};
  while (Match(TokenType::LBRACKET)) {
    Token idxTok = Peek();
    TypeInfo sz = ParseExpression();
    if (!IsIntegralScalar(sz)) {
      ErrorAt(idxTok, "array size must be integral, got " + TypeToString(sz));
    }
    Expect(TokenType::RBRACKET, "']'");
    type.dims++;
  }
  return type;
}

std::vector<TypeInfo> SemanticAnalyzer::ParseParamList(std::vector<Token> &namesOut) {
  std::vector<TypeInfo> params;
  if (!IsTypeStart(Peek().type)) return params;

  while (true) {
    TypeInfo t = ParseType();
    Token id = Expect(TokenType::IDENTIFIER, "parameter name");
    params.push_back(t);
    namesOut.push_back(id);
    if (!Match(TokenType::COMMA)) break;
  }
  return params;
}

void SemanticAnalyzer::ParseVarDeclInitOpt(const TypeInfo &declType, const Token &id,
                                           bool isGlobal) {
  if (!isGlobal) {
    DeclareLocal(id, declType);
  }
  if (Match(TokenType::ASSIGN)) {
    TypeInfo rhs = ParseExpression();
    if (!CanAssign(declType, rhs)) {
      ErrorAt(id, "cannot initialize '" + id.text + "' of type " +
                      TypeToString(declType) + " with " + TypeToString(rhs));
    }
  }
}

void SemanticAnalyzer::ParseBlock(bool createScope) {
  Expect(TokenType::LBRACE, "'{'");
  if (createScope) PushScope();

  while (Peek().type != TokenType::RBRACE) {
    ParseStatement();
  }

  Expect(TokenType::RBRACE, "'}'");
  if (createScope) PopScope();
}

void SemanticAnalyzer::ParseStatement() {
  Token t = Peek();

  if (t.type == TokenType::LBRACE) {
    ParseBlock(true);
    return;
  }
  if (t.type == TokenType::KW_IF) {
    ParseIfStmt();
    return;
  }
  if (t.type == TokenType::KW_WHILE) {
    ParseWhileStmt();
    return;
  }
  if (t.type == TokenType::KW_DO) {
    ParseDoWhileStmt();
    return;
  }
  if (t.type == TokenType::KW_FOR) {
    ParseForStmt();
    return;
  }
  if (t.type == TokenType::KW_BREAK) {
    Consume();
    if (loopDepth_ <= 0) ErrorAt(t, "'break' outside loop");
    Expect(TokenType::SEMICOLON, "';'");
    return;
  }
  if (t.type == TokenType::KW_CONTINUE) {
    Consume();
    if (loopDepth_ <= 0) ErrorAt(t, "'continue' outside loop");
    Expect(TokenType::SEMICOLON, "';'");
    return;
  }
  if (t.type == TokenType::KW_RETURN) {
    Consume();
    if (!currentReturnType_.has_value()) {
      ErrorAt(t, "'return' outside function");
    }
    if (Peek().type == TokenType::SEMICOLON) {
      ErrorAt(t, "return value is required");
    }
    TypeInfo ret = ParseExpression();
    if (!CanAssign(*currentReturnType_, ret)) {
      ErrorAt(t, "return type mismatch: expected " +
                      TypeToString(*currentReturnType_) + ", got " + TypeToString(ret));
    }
    Expect(TokenType::SEMICOLON, "';'");
    return;
  }
  if (IsTypeStart(t.type)) {
    TypeInfo declType = ParseType();
    Token id = Expect(TokenType::IDENTIFIER, "identifier");
    ParseVarDeclInitOpt(declType, id, false);
    Expect(TokenType::SEMICOLON, "';'");
    return;
  }
  if (IsExpressionStart(t.type)) {
    ParseExpression();
    Expect(TokenType::SEMICOLON, "';'");
    return;
  }

  ErrorAt(t, "expected statement");
}

void SemanticAnalyzer::ParseIfStmt() {
  Token ifTok = Expect(TokenType::KW_IF, "'if'");
  Expect(TokenType::LPAREN, "'('");
  TypeInfo cond = ParseExpression();
  ExpectBoolCondition(cond, ifTok);
  Expect(TokenType::RPAREN, "')'");
  ParseStatement();
  if (Match(TokenType::KW_ELSE)) {
    ParseStatement();
  }
}

void SemanticAnalyzer::ParseWhileStmt() {
  Token whileTok = Expect(TokenType::KW_WHILE, "'while'");
  Expect(TokenType::LPAREN, "'('");
  TypeInfo cond = ParseExpression();
  ExpectBoolCondition(cond, whileTok);
  Expect(TokenType::RPAREN, "')'");
  loopDepth_++;
  ParseBlock(true);
  loopDepth_--;
}

void SemanticAnalyzer::ParseDoWhileStmt() {
  Expect(TokenType::KW_DO, "'do'");
  loopDepth_++;
  ParseBlock(true);
  loopDepth_--;
  Token whileTok = Expect(TokenType::KW_WHILE, "'while'");
  Expect(TokenType::LPAREN, "'('");
  TypeInfo cond = ParseExpression();
  ExpectBoolCondition(cond, whileTok);
  Expect(TokenType::RPAREN, "')'");
  Expect(TokenType::SEMICOLON, "';'");
}

void SemanticAnalyzer::ParseForStmt() {
  Token forTok = Expect(TokenType::KW_FOR, "'for'");
  Expect(TokenType::LPAREN, "'('");

  PushScope();

  if (IsTypeStart(Peek().type)) {
    TypeInfo declType = ParseType();
    Token id = Expect(TokenType::IDENTIFIER, "identifier");
    ParseVarDeclInitOpt(declType, id, false);
  } else if (IsExpressionStart(Peek().type)) {
    ParseExpression();
  }
  Expect(TokenType::SEMICOLON, "';'");

  if (IsExpressionStart(Peek().type)) {
    TypeInfo cond = ParseExpression();
    ExpectBoolCondition(cond, forTok);
  }
  Expect(TokenType::SEMICOLON, "';'");

  if (IsExpressionStart(Peek().type)) {
    ParseExpression();
  }
  Expect(TokenType::RPAREN, "')'");

  loopDepth_++;
  ParseBlock(true);
  loopDepth_--;

  PopScope();
}

std::optional<TypeInfo> SemanticAnalyzer::TryParseLValue() {
  size_t saved = pos_;
  if (Peek().type != TokenType::IDENTIFIER) return std::nullopt;

  Token id = Consume();
  TypeInfo t;
  try {
    t = LookupVariable(id);
  } catch (const std::exception &) {
    pos_ = saved;
    return std::nullopt;
  }

  while (Match(TokenType::LBRACKET)) {
    if (t.dims <= 0) {
      ErrorAt(id, "cannot index non-array '" + id.text + "'");
    }
    Token idxTok = Peek();
    TypeInfo idx = ParseExpression();
    if (!IsIntegralScalar(idx)) {
      ErrorAt(idxTok, "array index must be integral, got " + TypeToString(idx));
    }
    Expect(TokenType::RBRACKET, "']'");
    t.dims--;
  }
  return t;
}

TypeInfo SemanticAnalyzer::ParseExpression() {
  TypeInfo t = ParseAssignmentExpr();
  while (Match(TokenType::COMMA)) {
    t = ParseAssignmentExpr();
  }
  return t;
}

TypeInfo SemanticAnalyzer::ParseAssignmentExpr() {
  size_t saved = pos_;
  std::optional<TypeInfo> lhs = TryParseLValue();
  if (lhs.has_value() && Match(TokenType::ASSIGN)) {
    TypeInfo rhs = ParseAssignmentExpr();
    if (!CanAssign(*lhs, rhs)) {
      ErrorAt(Peek(-1), "cannot assign " + TypeToString(rhs) + " to " + TypeToString(*lhs));
    }
    return *lhs;
  }
  pos_ = saved;
  return ParseLogicalOrExpr();
}

TypeInfo SemanticAnalyzer::ParseLogicalOrExpr() {
  TypeInfo left = ParseLogicalAndExpr();
  while (Match(TokenType::OROR)) {
    TypeInfo right = ParseLogicalAndExpr();
    if (!IsBoolScalar(left) || !IsBoolScalar(right)) {
      ErrorAt(Peek(-1), "operator '||' requires bool operands");
    }
    left = BoolType();
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseLogicalAndExpr() {
  TypeInfo left = ParseBitOrExpr();
  while (Match(TokenType::ANDAND)) {
    TypeInfo right = ParseBitOrExpr();
    if (!IsBoolScalar(left) || !IsBoolScalar(right)) {
      ErrorAt(Peek(-1), "operator '&&' requires bool operands");
    }
    left = BoolType();
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseBitOrExpr() {
  TypeInfo left = ParseBitXorExpr();
  while (Match(TokenType::OR)) {
    TypeInfo right = ParseBitXorExpr();
    if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
      ErrorAt(Peek(-1), "operator '|' requires integral operands");
    }
    left = {BaseTypeKind::INT, 0};
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseBitXorExpr() {
  TypeInfo left = ParseBitAndExpr();
  while (Match(TokenType::XOR)) {
    TypeInfo right = ParseBitAndExpr();
    if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
      ErrorAt(Peek(-1), "operator '^' requires integral operands");
    }
    left = {BaseTypeKind::INT, 0};
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseBitAndExpr() {
  TypeInfo left = ParseEqualityExpr();
  while (Match(TokenType::AND)) {
    TypeInfo right = ParseEqualityExpr();
    if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
      ErrorAt(Peek(-1), "operator '&' requires integral operands");
    }
    left = {BaseTypeKind::INT, 0};
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseEqualityExpr() {
  TypeInfo left = ParseRelationalExpr();
  while (Peek().type == TokenType::EQ || Peek().type == TokenType::NEQ) {
    Consume();
    TypeInfo right = ParseRelationalExpr();
    bool ok = false;
    if (left.dims == right.dims && left.base == right.base) ok = true;
    if (left.IsScalar() && right.IsScalar() &&
        IsNumericScalar(left) && IsNumericScalar(right)) {
      ok = true;
    }
    if (!ok) {
      ErrorAt(Peek(-1), "incompatible operands for equality comparison");
    }
    left = BoolType();
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseRelationalExpr() {
  TypeInfo left = ParseShiftExpr();
  while (true) {
    TokenType op = Peek().type;
    if (op != TokenType::LT && op != TokenType::GT &&
        op != TokenType::LE && op != TokenType::GE) {
      break;
    }
    Consume();
    TypeInfo right = ParseShiftExpr();
    if (!IsNumericScalar(left) || !IsNumericScalar(right)) {
      ErrorAt(Peek(-1), "relational operators require numeric operands");
    }
    left = BoolType();
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseShiftExpr() {
  TypeInfo left = ParseAddExpr();
  while (Peek().type == TokenType::SHL || Peek().type == TokenType::SHR) {
    Consume();
    TypeInfo right = ParseAddExpr();
    if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
      ErrorAt(Peek(-1), "shift operators require integral operands");
    }
    left = {BaseTypeKind::INT, 0};
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseAddExpr() {
  TypeInfo left = ParseMulExpr();
  while (true) {
    TokenType op = Peek().type;
    if (op != TokenType::PLUS && op != TokenType::MINUS) break;
    Consume();
    TypeInfo right = ParseMulExpr();

    if (op == TokenType::PLUS &&
        left.IsScalar() && right.IsScalar() &&
        left.base == BaseTypeKind::STRING && right.base == BaseTypeKind::STRING) {
      left = {BaseTypeKind::STRING, 0};
    } else {
      TypeInfo common = CommonNumericType(left, right);
      if (!common.IsValid()) {
        ErrorAt(Peek(-1), "operator requires numeric operands");
      }
      left = common;
    }
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseMulExpr() {
  TypeInfo left = ParseUnaryExpr();
  while (true) {
    TokenType op = Peek().type;
    if (op != TokenType::STAR && op != TokenType::SLASH && op != TokenType::PERCENT) break;
    Consume();
    TypeInfo right = ParseUnaryExpr();

    if (op == TokenType::PERCENT) {
      if (!IsIntegralScalar(left) || !IsIntegralScalar(right)) {
        ErrorAt(Peek(-1), "operator '%' requires integral operands");
      }
      left = {BaseTypeKind::INT, 0};
    } else {
      TypeInfo common = CommonNumericType(left, right);
      if (!common.IsValid()) {
        ErrorAt(Peek(-1), "operator requires numeric operands");
      }
      left = common;
    }
  }
  return left;
}

TypeInfo SemanticAnalyzer::ParseUnaryExpr() {
  TokenType op = Peek().type;
  if (op == TokenType::PLUS || op == TokenType::MINUS ||
      op == TokenType::BANG || op == TokenType::TILDE) {
    Token tok = Consume();
    TypeInfo inner = ParseUnaryExpr();

    if (op == TokenType::PLUS || op == TokenType::MINUS) {
      if (!IsNumericScalar(inner)) {
        ErrorAt(tok, "unary +/- requires numeric operand");
      }
      return inner.base == BaseTypeKind::DOUBLE ? inner : TypeInfo{BaseTypeKind::INT, 0};
    }
    if (op == TokenType::BANG) {
      if (!IsBoolScalar(inner)) {
        ErrorAt(tok, "unary '!' requires bool operand");
      }
      return BoolType();
    }
    if (!IsIntegralScalar(inner)) {
      ErrorAt(tok, "unary '~' requires integral operand");
    }
    return {BaseTypeKind::INT, 0};
  }
  return ParsePrimary();
}

TypeInfo SemanticAnalyzer::ParsePrimary() {
  TypeInfo t = ParsePrimaryCore();
  while (Match(TokenType::LBRACKET)) {
    Token idxTok = Peek();
    TypeInfo idx = ParseExpression();
    if (!IsIntegralScalar(idx)) {
      ErrorAt(idxTok, "array index must be integral, got " + TypeToString(idx));
    }
    Expect(TokenType::RBRACKET, "']'");
    if (t.dims <= 0) {
      ErrorAt(idxTok, "indexing non-array type " + TypeToString(t));
    }
    t.dims--;
  }
  return t;
}

TypeInfo SemanticAnalyzer::ParsePrimaryCore() {
  Token t = Peek();
  if (t.type == TokenType::INT_LITERAL) {
    Consume();
    return {BaseTypeKind::INT, 0};
  }
  if (t.type == TokenType::DOUBLE_LITERAL) {
    Consume();
    return {BaseTypeKind::DOUBLE, 0};
  }
  if (t.type == TokenType::BOOL_LITERAL) {
    Consume();
    return {BaseTypeKind::BOOL, 0};
  }
  if (t.type == TokenType::CHAR_LITERAL) {
    Consume();
    return {BaseTypeKind::CHAR, 0};
  }
  if (t.type == TokenType::STRING_LITERAL) {
    Consume();
    return {BaseTypeKind::STRING, 0};
  }
  if (t.type == TokenType::IDENTIFIER) {
    Token id = Consume();
    if (Match(TokenType::LPAREN)) {
      auto found = functions_.find(id.text);
      if (found == functions_.end()) {
        ErrorAt(id, "call to undeclared function '" + id.text + "'");
      }
      std::vector<TypeInfo> args = ParseArgList();
      Expect(TokenType::RPAREN, "')'");
      if (args.size() != found->second.params.size()) {
        ErrorAt(id, "wrong argument count in call to '" + id.text + "'");
      }
      for (size_t i = 0; i < args.size(); ++i) {
        if (!CanAssign(found->second.params[i], args[i])) {
          ErrorAt(id, "argument type mismatch in call to '" + id.text + "'");
        }
      }
      return found->second.returnType;
    }
    try {
      return LookupVariable(id);
    } catch (const std::exception &e) {
      ErrorAt(id, e.what());
    }
  }
  if (Match(TokenType::LPAREN)) {
    TypeInfo inside = ParseExpression();
    Expect(TokenType::RPAREN, "')'");
    return inside;
  }
  ErrorAt(t, "expected primary expression");
}

std::vector<TypeInfo> SemanticAnalyzer::ParseArgList() {
  std::vector<TypeInfo> args;
  if (!IsExpressionStart(Peek().type)) return args;
  args.push_back(ParseExpression());
  while (Match(TokenType::COMMA)) {
    args.push_back(ParseExpression());
  }
  return args;
}
