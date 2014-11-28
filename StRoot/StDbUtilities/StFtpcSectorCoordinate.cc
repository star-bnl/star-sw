/***********************************************************************
 *
 * $Id: StFtpcSectorCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Ftpc Sector Coordinate
 *
 ************************************************************************
 *
 * $Log: StFtpcSectorCoordinate.cc,v $
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:07  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 *
 ***********************************************************************/
#include "StFtpcSectorCoordinate.hh"

static const char rcsid[] = "$Id: StFtpcSectorCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $";

StFtpcSectorCoordinate::StFtpcSectorCoordinate() {/**/}

StFtpcSectorCoordinate::StFtpcSectorCoordinate(const int plane, const int sector)
    : mPlane(plane), mSector(sector) {/**/}

StFtpcSectorCoordinate::~StFtpcSectorCoordinate() {/**/}

int
StFtpcSectorCoordinate::operator==(const StFtpcSectorCoordinate& p) const
{

    return (p.mPlane     == mPlane  &&
	    p.mSector    == mSector);
}

int
StFtpcSectorCoordinate::operator!=(const StFtpcSectorCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

// Non-Member function
ostream& operator<<(ostream& os, const StFtpcSectorCoordinate& a)
{
    return os << "(plane= "   << a.plane()
	      << ", sector= " << a.sector();
}
