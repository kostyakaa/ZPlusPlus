#pragma once

#include "ast.h"

#include <iosfwd>

namespace ast {

void PrintProgram(const Program &program, std::ostream &out);

} // namespace ast
