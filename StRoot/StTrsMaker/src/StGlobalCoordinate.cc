/*************************************************************************
 *
 * $Id: StGlobalCoordinate.cc,v 1.3 1999/10/25 18:38:48 calderon Exp $
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
 * Revision 1.3  1999/10/25 18:38:48  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1998/11/16 19:41:58  lasiuk
 * constructor do not use reference for double&
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

StGlobalCoordinate::StGlobalCoordinate(const double x, const double y, const double z)
    : mPosition(x,y,z) { /**/ }

StGlobalCoordinate::StGlobalCoordinate(const StThreeVector<double>& x)
    : mPosition(x) {/**/}

StGlobalCoordinate::~StGlobalCoordinate() {/**/}

// Non-member functions
ostream& operator<<(ostream& os, const StGlobalCoordinate& a)
{
    return os << "TPC_GC ("
	      << a.position().x() << ", "
	      << a.position().y() << ", "
	      << a.position().z() << ")";
}
