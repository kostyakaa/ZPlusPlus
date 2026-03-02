#pragma once

#include <string>
#include <utility>
#include <vector>

enum class PolizOp {
  INT,
  CHAR,
  BOOL,
  DOUBLE,
  STRING,
  OPERATION,
  POLIZ_GO,
  POLIZ_FGO,
  CALL_FUNCTION,
  FUNCTION_ADRESS,
  END_OF_PROGRAM,
  POLIZ_LABEL,
  ALLOCATE,
  FREE,
  ADRESS_INT,
  ADRESS_BOOL,
  ADRESS_CHAR,
  ADRESS_DOUBLE,
  ADRESS_STRING,
  UNARY_OPERATION,
  CALL_PRINT,
  CALL_READ,
  TO_DOUBLE,
  TO_INT,
  TO_CHAR,
  TO_BOOL,
  END_FUNCTION
};

const char *PolizOpToString(PolizOp op);

class Poliz {
public:
  void UpdateEl(std::pair<PolizOp, std::string> s, int ind);
  void AddEl(std::pair<PolizOp, std::string> s);
  std::pair<PolizOp, std::string> GiveEl(int ind) const;
  int GiveSize() const;

private:
  std::vector<std::pair<PolizOp, std::string>> code_;
};
