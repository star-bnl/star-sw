#include "heed++/code/HeedCondElectron.h"

// 2003, I. Smirnov

namespace Heed {

void HeedCondElectron::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "HeedCondElectron (l=" << l << ")\n";
  Ifile << "(" << x << ", " << y << ", " << z << ")\n";
}
}
