#ifndef StPicoCommon_h
#define StPicoCommon_h

// C++ headers
#include <set>

//_________________
namespace StarPicoDst {

  /// Define detector side
  enum DetectorSide {
    Undefined = 0,
    East = -1,
    West = 1
  };

  extern std::set<DetectorSide> detectorSides;

  /// Define charge
  enum Charge {
    Negative = -1,
    Positive = 1
  };

}
#endif
