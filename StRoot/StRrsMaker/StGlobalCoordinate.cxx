/*************************************************************************
 *
 * $Id: StGlobalCoordinate.cxx,v 2.0 2000/08/09 16:16:58 gans Exp $
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
 * Revision 2.0  2000/08/09 16:16:58  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.2  2000/02/08 23:45:48  lasiuk
 * Default constructor initializer changed for CC4.2
 *
 * Revision 1.1  2000/02/08 16:34:06  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 *************************************************************************/
#include "StGlobalCoordinate.h"

StGlobalCoordinate::StGlobalCoordinate()
    : mPos(0,0,0) {/**/}

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
