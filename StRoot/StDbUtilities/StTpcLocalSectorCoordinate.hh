/*********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.hh,v 1.4 2004/03/05 17:22:55 fisyak Exp $
 *
 * Author: brian Jan 26, 1999
 *
 **********************************************************************
 *
 * Description:  Local Sector
 *
 **********************************************************************
 *
 * $Log: StTpcLocalSectorCoordinate.hh,v $
 * Revision 1.4  2004/03/05 17:22:55  fisyak
 * Add TPC transformations for direction, aligned sectors, protection in order to stay in the same sector when moving from/to Pad coordinates
 *
 * Revision 1.3  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2000/02/02 23:01:39  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:09  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.3  1999/10/25 18:38:29  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1999/10/04 16:05:59  long
 * change mVolumeId to mFromSector
 *
 * Revision 1.2  1999/10/01 17:15:00 Hui Long
 * replace mVolumeId by fromSector
 *
 * Revision 1.1  1999/01/28 02:48:12  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_TPC_LOCAL_SECTOR_COORDINATE_HH
#define ST_TPC_LOCAL_SECTOR_COORDINATE_HH
#include <Stiostream.h>
#include "StThreeVector.hh"

class StTpcLocalSectorCoordinate
{
public:
  StTpcLocalSectorCoordinate(const double x=0, const double y=0, const double z=0, const int sect=0) : 
    mPosition(x,y,z), mFromSector(sect) {}
  StTpcLocalSectorCoordinate(const StThreeVector<double>& position, const int sect) :
    mPosition(position), mFromSector(sect) {}
  virtual ~StTpcLocalSectorCoordinate() {}
  int operator==(const StTpcLocalSectorCoordinate&) const;
  int operator!=(const StTpcLocalSectorCoordinate&) const;
  
  // access functions provided by StThreeVector
  const StThreeVector<double>& position()  const { return(mPosition); }
  int  fromSector()                 const { return(mFromSector); }
  StThreeVector<double>& position()              { return(mPosition); }

protected:
  StThreeVector<double> mPosition;
  int            mFromSector;
};
// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorCoordinate&);
#endif
