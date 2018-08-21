#ifndef StxMapUtilities_h
#define StxMapUtilities_h

#include "TString.h"
class StxDetector;
class StxHit;
class StTpcHit;
class StxKalmanTrackNode;
class StxDetectorNode;

//Structure for hit map key
struct HitMapKey {
  bool operator==(const HitMapKey&) const;
  double refangle;
  double position;
};

//Functor for ordering hit map key
struct MapKeyLessThan{
  MapKeyLessThan() : reftolerance(.01), postolerance(.01) {};
    bool operator() (const HitMapKey&, const HitMapKey&) const;
    double reftolerance;
    double postolerance;
};

// Structure for material, shape, or detector name map key
struct NameMapKey {
    NameMapKey(const Char_t * str){ name = str; }
    NameMapKey(){}
    bool operator==(const NameMapKey&) const;
    bool operator<(const NameMapKey&) const;
    TString name;
};
//Detector sorter
struct StxDetectorNodePositionLessThan {bool operator() (const StxDetectorNode *, const StxDetectorNode *) const;};
struct SetHitUsed{void operator() (StxKalmanTrackNode&);};
#endif


