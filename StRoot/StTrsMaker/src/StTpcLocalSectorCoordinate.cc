/***********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.cc,v 1.1 1999/01/28 02:47:40 lasiuk Exp $
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
 * Revision 1.1  1999/01/28 02:47:40  lasiuk
 * Initial Revision
 *
 ***********************************************************************/
#include "StTpcLocalSectorCoordinate.hh"

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate() {/**/}

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate(const double x, const double y, const double z, const int id)
    : mPos(x,y,z), mVolumeId(id) { /* nopt */}

StTpcLocalSectorCoordinate::StTpcLocalSectorCoordinate(const StThreeVector<double>& pos, const int id)
    : mPos(pos), mVolumeId(id) { /* nopt */ }

StTpcLocalSectorCoordinate::~StTpcLocalSectorCoordinate() {/**/}

// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalSectorCoordinate& a)
{
    return os << "TPC_Local _Sector( ("
	      << a.pos().x()  << ", "
	      << a.pos().y()  << ", "
	      << a.pos().z()  << "),"
	      << a.volumeId() << ")";
}
