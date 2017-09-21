#ifndef HEEDCONDELECTRON
#define HEEDCONDELECTRON

#include <vector>
#include "wcpplib/geometry/vec.h"

namespace Heed {

/// Conduction electrons deposited in gas.
/// Usually these are electron-ion pairs created by the delta-electron.
/// But the delta-electron is itself converted in conduction electron
/// at the end of its route. In this case the ion may be located somewhere else.
/// To reduce the computer expenses, the position of conduction electron
/// is determined only in the local coordinate system, that is in the
/// most deep volume.
///
/// 2003, I. Smirnov

class HeedCondElectron {
 public:
  /// Position (in the local system, the last system from tid)
  point ptloc;
  /// Time
  double time;
  /// Default constructor
  HeedCondElectron() {}
  /// Constructor
  HeedCondElectron(const point& fptloc, const double ftime)
      : ptloc(fptloc), time(ftime) {}
  /// Destructor
  virtual ~HeedCondElectron() {}
  virtual void print(std::ostream& file, int l) const;
};

/// To make the conduction electrons generated, the volume
/// must be derived from class SensitiveVolume.

class SensitiveVolume {
 public:
  std::vector<HeedCondElectron> conduction_electron_bank;
  std::vector<HeedCondElectron> conduction_ion_bank;
  SensitiveVolume() {}
};
}

#endif
