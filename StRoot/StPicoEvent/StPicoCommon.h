#ifndef StPicoCommon_h
#define StPicoCommon_h

#include <set>


namespace StarPicoDst
{


enum DetectorSide {
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
