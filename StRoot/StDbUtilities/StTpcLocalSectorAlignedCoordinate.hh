//  $Id: StTpcLocalSectorAlignedCoordinate.hh,v 1.1 2004/03/05 17:22:54 fisyak Exp $
#ifndef ST_TPC_LOCAL_ALIGNEDCOORDINATE_HH
#define ST_TPC_LOCAL_ALIGNEDCOORDINATE_HH
#include "StTpcLocalSectorCoordinate.hh"

class StTpcLocalSectorAlignedCoordinate : public StTpcLocalSectorCoordinate {
public:
  StTpcLocalSectorAlignedCoordinate() : StTpcLocalSectorCoordinate() {}
  StTpcLocalSectorAlignedCoordinate(const double x, const double y, const double z, int sector) : 
    StTpcLocalSectorCoordinate(x,y,z,sector) {}
  StTpcLocalSectorAlignedCoordinate(const StThreeVector<double>& xyz, int sector) : StTpcLocalSectorCoordinate(xyz,sector) {}
  virtual ~StTpcLocalSectorAlignedCoordinate() {}
};
// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorAlignedCoordinate&);
#endif
