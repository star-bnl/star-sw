/*************************************************************************
 *
 * $Id: StGlobalCoordinate.cc,v 1.1 1998/11/10 17:12:20 fisyak Exp $
 *
 * Author:  brian May 20, 1998
 *
 **************************************************************************
 *
 * Description:  Raw data information along with access
 *               functions
 *
 ***************************************************************************
 *
 * $Log: StGlobalCoordinate.cc,v $
 * Revision 1.1  1998/11/10 17:12:20  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:20  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:20:38  lasiuk
 * remove 'St' from variable declarations
 *
 * Revision 1.1  1998/05/21 21:27:56  lasiuk
 * Initial revision
 *
 *
 *************************************************************************/
#include "StGlobalCoordinate.hh"

StGlobalCoordinate::StGlobalCoordinate() {/**/}

StGlobalCoordinate::StGlobalCoordinate(const double& x, const double& y, const double& z)
{
    StGlobalCoordinate(StThreeVector<double>(x,y,z));
}

StGlobalCoordinate::StGlobalCoordinate(const StThreeVector<double>& x)
    : mPos(x) {/**/}

StGlobalCoordinate::~StGlobalCoordinate() {/**/}

// Non-member functions
ostream& operator<<(ostream& os, const StGlobalCoordinate& a)
{
    return os << "TPC_GC ("
	      << a.pos().x() << ", "
	      << a.pos().y() << ", "
	      << a.pos().z() << ")";
}
