// * $Id: StTpcLocalSectorDirection.hh,v 1.1 2004/03/05 17:22:55 fisyak Exp $
#ifndef ST_TPC_LOCAL_SECTOR_DIRECTION_HH
#define ST_TPC_LOCAL_SECTOR_DIRECTION_HH
#include "StTpcLocalSectorCoordinate.hh"
class StTpcLocalSectorDirection : public StTpcLocalSectorCoordinate {
public:
  StTpcLocalSectorDirection(int sector=12) : 
    StTpcLocalSectorCoordinate(0.,1.,0.,sector) {}
  StTpcLocalSectorDirection(const double x, const double y, const double z, int sector=12) : 
    StTpcLocalSectorCoordinate(x,y,z,sector) {}
  StTpcLocalSectorDirection(const StThreeVector<double>& xyz, int sector) :
    StTpcLocalSectorCoordinate(xyz,sector) {}
  virtual ~StTpcLocalSectorDirection() {}
};
// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorDirection&);
#endif
