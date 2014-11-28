/*********************************************************************
 *
 * $Id: StTpcLocalDirection.hh,v 1.2 2004/06/05 23:31:09 fisyak Exp $
 **********************************************************************/
#ifndef ST_TPC_LOCAL_DIRECTION_HH
#define ST_TPC_LOCAL_DIRECTION_HH
#include "StTpcCoordinate.h"
class StTpcLocalDirection : public StTpcCoordinate {
public:
  StTpcLocalDirection() :  StTpcCoordinate(0,0,0,0,0) {}
  StTpcLocalDirection(double x, double y, double z) :
    StTpcCoordinate(x,y,z,0,0) {}
  StTpcLocalDirection(const StThreeVector<double>& xyz) :
    StTpcCoordinate(xyz,0,0) {}
  StTpcLocalDirection(double x, double y, double z, int sector, int row) :
    StTpcCoordinate(x,y,z,sector,row) {}
  StTpcLocalDirection(const StThreeVector<double>& xyz, int sector, int row) :
    StTpcCoordinate(xyz,sector,row) {}
  virtual ~StTpcLocalDirection() {}
};
ostream& operator<<(ostream&, const StTpcLocalDirection&);
#endif
