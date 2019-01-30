#ifndef HEEDCONDELECTRON
#define HEEDCONDELECTRON

#include <vector>
#include "wcpplib/geometry/vec.h"

namespace Heed {

/// Conduction electrons deposited in the sensitive medium.
/// Usually these are electron-ion pairs created by the delta-electron.
/// In addition, the delta-electron is itself converted to a conduction electron
/// at the end of its path. In this case the ion may be located somewhere else.
/// To reduce the computer expenses, the position of a conduction electron
/// is determined only in the local coordinate system ("deepest" volume).
///
/// 2003, I. Smirnov

class HeedCondElectron {
 public:
  /// X coordinate (in the local system).
  double x = 0.;
  /// Y coordinate (in the local system).
  double y = 0.;
  /// Z coordinate (in the local system).
  double z = 0.;
  /// Time.
  double time = 0.;
  
  /// Default constructor
  HeedCondElectron() = default;
  /// Constructor
  HeedCondElectron(const point& fpt, const double ftime)
      : x(fpt.v.x), y(fpt.v.y), z(fpt.v.z), time(ftime) {}
  /// Constructor
  HeedCondElectron(const double fx, const double fy, const double fz,
                   const double ftime) 
      : x(fx), y(fy), z(fz), time(ftime) {}
  void print(std::ostream& file, int l) const;
};
}

#endif
