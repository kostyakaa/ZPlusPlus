#include "poliz_printer.h"

#include <ostream>

void PrintPoliz(const Poliz &poliz, std::ostream &out) {
  for (int i = 0; i < poliz.GiveSize(); ++i) {
    auto el = poliz.GiveEl(i);
    out << i << " " << PolizOpToString(el.first);
    if (!el.second.empty()) {
      out << " " << el.second;
    }
    out << "\n";
  }
}
