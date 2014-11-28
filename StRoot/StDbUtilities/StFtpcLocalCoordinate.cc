/***********************************************************************
 *
 * $Id: StFtpcLocalCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Ftpc Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StFtpcLocalCoordinate.cc,v $
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
#include "StFtpcLocalCoordinate.hh"

static const char rcsid[] = "$Id: StFtpcLocalCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $";

    
StFtpcLocalCoordinate::StFtpcLocalCoordinate() {/**/}

StFtpcLocalCoordinate::StFtpcLocalCoordinate(const double x, const double y, const double z)
    : mPosition(x,y,z) { /* nopt */}

StFtpcLocalCoordinate::StFtpcLocalCoordinate(const StThreeVector<double>& position)
    : mPosition(position) { /* nopt */ }

StFtpcLocalCoordinate::~StFtpcLocalCoordinate() {/**/}

int StFtpcLocalCoordinate::operator==(const StFtpcLocalCoordinate& p) const
{
    return p.mPosition == mPosition;
}

int
StFtpcLocalCoordinate::operator!=(const StFtpcLocalCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

// Non-member Functions
ostream& operator<<(ostream& os, const StFtpcLocalCoordinate& a)
{
    return os << "FTPC_Local( ("
	      << a.position().x()  << ", "
	      << a.position().y()  << ", "
	      << a.position().z()  << ")";
}
