//  $Id: StTpcLocalSectorAlignedDirection.hh,v 1.1 2004/03/05 17:22:55 fisyak Exp $
#ifndef ST_TPC_LOCAL_ALIGNEDDIRECTION_HH
#define ST_TPC_LOCAL_ALIGNEDDIRECTION_HH
#include "StTpcLocalSectorDirection.hh"

class StTpcLocalSectorAlignedDirection : public StTpcLocalSectorDirection {
public:
  StTpcLocalSectorAlignedDirection(int sector=12, int row=1) : 
    StTpcLocalSectorDirection(sector), mFromRow(row)  {}
  StTpcLocalSectorAlignedDirection(const double x, const double y, const double z, int sector, int row=1) : 
    StTpcLocalSectorDirection(x,y,z,sector), mFromRow(row)  {}
  StTpcLocalSectorAlignedDirection(const StThreeVector<double>& xyz, int sector, int row=1) : 
    StTpcLocalSectorDirection(xyz,sector), mFromRow(row) {}
  virtual ~StTpcLocalSectorAlignedDirection() {}
  int   fromRow()   const {return mFromRow;}
private:
  int   mFromRow;
};
// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorAlignedDirection&);
#endif
