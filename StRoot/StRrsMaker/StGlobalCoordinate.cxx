/*************************************************************************
 *
 * $Id: StGlobalCoordinate.cxx,v 1.1 2000/02/08 16:34:06 lasiuk Exp $
 *
 * Author:  brian Jan 27, 2000
 *
 **************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StGlobalCoordinate.cxx,v $
 * Revision 1.1  2000/02/08 16:34:06  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 *************************************************************************/
#include "StGlobalCoordinate.h"

StGlobalCoordinate::StGlobalCoordinate() {/**/}

StGlobalCoordinate::StGlobalCoordinate(const double x, const double y, const double z)
    : mPos(x,y,z) { /**/ }

StGlobalCoordinate::StGlobalCoordinate(const StThreeVector<double>& x)
    : mPos(x) {/**/}

StGlobalCoordinate::~StGlobalCoordinate() {/**/}

// Non-member functions
ostream& operator<<(ostream& os, const StGlobalCoordinate& a)
{
    return os << "Global: "
	      << a.position().x() << ", "
	      << a.position().y() << ", "
	      << a.position().z();
}
