/***********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.cc,v 1.2 1999/10/04 15:25:53 long Exp $
 *
 * Author:  brian Jan 26, 1999
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 ************************************************************************
 *
 * $Log: StTpcLocalSectorCoordinate.cc,v $
 * Revision 1.2  1999/10/04 15:25:53  long
 * change mVolumeId to mFromSector
 *
 * Revision 1.2  1999/10/01 Hui Long
 * change memeber data mVolumeId to mFromSector
 * Revision 1.1  1999/01/28 02:47:40  lasiuk
 * Initial Revision
 *
 ***********************************************************************/
#include "StTpcLocalSectorCoordinate.hh"

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate() {/**/}

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate(const double x, const double y, const double z, const int sect)
    : mPos(x,y,z), mFromSector(sect) { /* nopt */}

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate(const StThreeVector<double>& pos, const int sect)
    : mPos(pos), mFromSector(sect) { /* nopt */ }

StTpcLocalSectorCoordinate::~StTpcLocalSectorCoordinate() {/**/}

// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalSectorCoordinate& a)
{
    return os << "TPC_Local _Sector( ("
	      << a.pos().x()  << ", "
	      << a.pos().y()  << ", "
	      << a.pos().z()  << "),"
	      << a.fromSector() << ")";
}
