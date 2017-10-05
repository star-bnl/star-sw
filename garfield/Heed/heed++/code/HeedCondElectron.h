#ifndef HEEDCONDELECTRON
#define HEEDCONDELECTRON

#include <vector>
#include "wcpplib/geometry/vec.h"

namespace Heed {

/// Conduction electrons deposited in the sensitive medium.
/// Usually these are electron-ion pairs created by the delta-electron.
/// In addition, the delta-electron is itself converted in conduction electron
/// at the end of its route. In this case the ion may be located somewhere else.
/// To reduce the computer expenses, the position of conduction electron
/// is determined only in the local coordinate system ("deepest" volume).
///
/// 2003, I. Smirnov

class HeedCondElectron {
 public:
  /// Position (in the local system).
  point ptloc;
  /// Time
  double time;
  HeedCondElectron() : ptloc(), time(0.) {}
  /// Constructor
  HeedCondElectron(const point& fptloc, const double ftime)
      : ptloc(fptloc), time(ftime) {}
  void print(std::ostream& file, int l) const;
};
}

#endif
