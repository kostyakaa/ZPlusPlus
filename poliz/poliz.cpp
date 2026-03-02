#include "poliz.h"

#include <stdexcept>

const char *PolizOpToString(PolizOp op) {
  switch (op) {
    case PolizOp::INT: return "INT";
    case PolizOp::CHAR: return "CHAR";
    case PolizOp::BOOL: return "BOOL";
    case PolizOp::DOUBLE: return "DOUBLE";
    case PolizOp::STRING: return "STRING";
    case PolizOp::OPERATION: return "OPERATION";
    case PolizOp::POLIZ_GO: return "POLIZ_GO";
    case PolizOp::POLIZ_FGO: return "POLIZ_FGO";
    case PolizOp::CALL_FUNCTION: return "CALL_FUNCTION";
    case PolizOp::FUNCTION_ADRESS: return "FUNCTION_ADRESS";
    case PolizOp::END_OF_PROGRAM: return "END_OF_PROGRAM";
    case PolizOp::POLIZ_LABEL: return "POLIZ_LABEL";
    case PolizOp::ALLOCATE: return "ALLOCATE";
    case PolizOp::FREE: return "FREE";
    case PolizOp::ADRESS_INT: return "ADRESS_INT";
    case PolizOp::ADRESS_BOOL: return "ADRESS_BOOL";
    case PolizOp::ADRESS_CHAR: return "ADRESS_CHAR";
    case PolizOp::ADRESS_DOUBLE: return "ADRESS_DOUBLE";
    case PolizOp::ADRESS_STRING: return "ADRESS_STRING";
    case PolizOp::UNARY_OPERATION: return "UNARY_OPERATION";
    case PolizOp::CALL_PRINT: return "CALL_PRINT";
    case PolizOp::CALL_READ: return "CALL_READ";
    case PolizOp::TO_DOUBLE: return "TO_DOUBLE";
    case PolizOp::TO_INT: return "TO_INT";
    case PolizOp::TO_CHAR: return "TO_CHAR";
    case PolizOp::TO_BOOL: return "TO_BOOL";
    case PolizOp::END_FUNCTION: return "END_FUNCTION";
  }
  return "UNKNOWN";
}

void Poliz::UpdateEl(std::pair<PolizOp, std::string> s, int ind) {
  if (ind < 0 || ind >= static_cast<int>(code_.size())) {
    throw std::runtime_error("Poliz: incorrect index");
  }
  code_[ind] = std::move(s);
}

void Poliz::AddEl(std::pair<PolizOp, std::string> s) {
  code_.push_back(std::move(s));
}

std::pair<PolizOp, std::string> Poliz::GiveEl(int ind) const {
  if (ind < 0 || ind >= static_cast<int>(code_.size())) {
    throw std::runtime_error("Poliz: incorrect index");
  }
  return code_[ind];
}

int Poliz::GiveSize() const {
  return static_cast<int>(code_.size());
}
