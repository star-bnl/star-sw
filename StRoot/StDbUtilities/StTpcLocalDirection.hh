// * $Id: StTpcLocalDirection.hh,v 1.1 2004/03/05 17:22:54 fisyak Exp $
#ifndef ST_TPC_LOCAL_DIRECTION_HH
#define ST_TPC_LOCAL_DIRECTION_HH
#include "StTpcLocalCoordinate.hh"
class StTpcLocalDirection : public StTpcLocalCoordinate {
public:
  StTpcLocalDirection() : StTpcLocalCoordinate() {}
  StTpcLocalDirection(const double x, const double y, const double z) : StTpcLocalCoordinate(x,y,z) {}
  StTpcLocalDirection(const StThreeVector<double>& xyz) : StTpcLocalCoordinate(xyz) {}
  virtual ~StTpcLocalDirection() {}
};
ostream& operator<<(ostream&, const StTpcLocalDirection&);
#endif
