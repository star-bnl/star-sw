/*********************************************************************
 *
 * $Id: StTpcLocalSectorAlignedDirection.hh,v 1.2 2004/06/05 23:31:09 fisyak Exp $
 **********************************************************************/
#ifndef ST_TPC_LOCAL_SECTOR_ALIGNED_DIRECTION_HH
#define ST_TPC_LOCAL_SECTOR_ALIGNED_DIRECTION_HH
#include "StTpcLocalSectorDirection.hh"
class StTpcLocalSectorAlignedDirection : public StTpcLocalSectorDirection {
public:
  StTpcLocalSectorAlignedDirection() :  StTpcLocalSectorDirection(0,0,0,0,0) {}
  StTpcLocalSectorAlignedDirection(double x, double y, double z) :
    StTpcLocalSectorDirection(x,y,z,0,0) {}
  StTpcLocalSectorAlignedDirection(const StThreeVector<double>& xyz) :
    StTpcLocalSectorDirection(xyz,0,0) {}
  StTpcLocalSectorAlignedDirection(double x, double y, double z, int sector, int row = 0) :
    StTpcLocalSectorDirection(x,y,z,sector,row) {}
  StTpcLocalSectorAlignedDirection(const StThreeVector<double>& xyz, int sector, int row = 0) :
    StTpcLocalSectorDirection(xyz,sector,row) {}
  virtual ~StTpcLocalSectorAlignedDirection() {}
};
ostream& operator<<(ostream&, const StTpcLocalSectorAlignedDirection&);
#endif
