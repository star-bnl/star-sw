/*********************************************************************
 *
 * $Id: StTpcLocalSectorDirection.hh,v 1.2 2004/06/05 23:31:10 fisyak Exp $
 *
 * Author: brian May 20, 1998
 *
 **********************************************************************/
#ifndef ST_TPC_LOCAL_SECTOR_DIRECTION_HH
#define ST_TPC_LOCAL_SECTOR_DIRECTION_HH
#include "StTpcCoordinate.h"
class StTpcLocalSectorDirection : public StTpcCoordinate {
public:
  StTpcLocalSectorDirection() : StTpcCoordinate(0,0,0,0,0) {}
  StTpcLocalSectorDirection(double x, double y, double z) :
    StTpcCoordinate(x,y,z,0,0) {}
  StTpcLocalSectorDirection(const StThreeVector<double>& xyz) :
    StTpcCoordinate(xyz,0,0) {}
  StTpcLocalSectorDirection(double x, double y, double z, int sector, int row=0) :
    StTpcCoordinate(x,y,z,sector,row) {}
  StTpcLocalSectorDirection(const StThreeVector<double>& xyz, int sector, int row=0) :
    StTpcCoordinate(xyz,sector,row) {}
  virtual ~StTpcLocalSectorDirection() {}
};
ostream& operator<<(ostream&, const StTpcLocalSectorDirection&);
#endif
