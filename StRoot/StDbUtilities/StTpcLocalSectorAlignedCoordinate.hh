/*********************************************************************
 *
 * $Id: StTpcLocalSectorAlignedCoordinate.hh,v 1.2 2004/06/05 23:31:09 fisyak Exp $
 **********************************************************************/
#ifndef ST_TPC_LOCAL_SECTOR_ALIGNED_COORDINATE_HH
#define ST_TPC_LOCAL_SECTOR_ALIGNED_COORDINATE_HH
#include "StTpcLocalSectorCoordinate.hh"
class StTpcLocalSectorAlignedCoordinate : public StTpcLocalSectorCoordinate {
public:
  StTpcLocalSectorAlignedCoordinate() :  StTpcLocalSectorCoordinate(0,0,0,0,0) {}
  StTpcLocalSectorAlignedCoordinate(double x, double y, double z) :
    StTpcLocalSectorCoordinate(x,y,z,0,0) {}
  StTpcLocalSectorAlignedCoordinate(const StThreeVector<double>& xyz) :
    StTpcLocalSectorCoordinate(xyz,0,0) {}
  StTpcLocalSectorAlignedCoordinate(double x, double y, double z, int sector, int row = 0) :
    StTpcLocalSectorCoordinate(x,y,z,sector,row) {}
  StTpcLocalSectorAlignedCoordinate(const StThreeVector<double>& xyz, int sector, int row = 0) :
    StTpcLocalSectorCoordinate(xyz,sector,row) {}
  virtual ~StTpcLocalSectorAlignedCoordinate() {}
};
ostream& operator<<(ostream&, const StTpcLocalSectorAlignedCoordinate&);
#endif
