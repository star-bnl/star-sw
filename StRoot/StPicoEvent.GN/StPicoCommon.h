#ifndef StPicoCommon_h
#define StPicoCommon_h

#include <set>

//_________________
namespace StarPicoDst {

  enum DetectorSide {
    Undefined = 0,
    East = -1,
    West = 1
  };

  extern std::set<DetectorSide> detectorSides;

  enum Charge {
    Negative = -1,
    Positive = 1
  };

}
#endif
