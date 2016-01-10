#ifndef HEEDCONDELECTRON
#define HEEDCONDELECTRON

#include <vector>
#include "wcpplib/geometry/vec.h"

/*
Conduction electrons deposited in gas.
Usually these are electron-ion pairs created by the delta-electron.
But the delta-electron is itself converted in conduction electron
at the end of its route. In this case the ion may be located somewhere else.
To reduce the computer expenses, the position of conduction electron
is determined only in the local coordinate system, that is in the
most deep volume.

To make the conduction electrons generated, the volume
must be derived from class SensitiveVolume.

2003, I. Smirnov

*/

namespace Heed {

class HeedCondElectron {
 public:
  // position (in the local system, the last system from tid)
  point ptloc;
  // time
  double time;
  // Constructors
  HeedCondElectron() {}
  HeedCondElectron(point fptloc, double ftime) : ptloc(fptloc), time(ftime) {}
  // Destructor
  virtual ~HeedCondElectron() {}
  virtual void print(std::ostream& file, int l) const;
};

class SensitiveVolume {
 public:
  std::vector<HeedCondElectron> conduction_electron_bank;
  SensitiveVolume() {}
};

}

#endif
