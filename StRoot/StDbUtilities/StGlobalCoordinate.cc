/*************************************************************************
 *
 * $Id: StGlobalCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $
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

static const char rcsid[] = "$Id: StGlobalCoordinate.cc,v 1.2 2000/02/02 23:01:38 calderon Exp $";

    
StGlobalCoordinate::StGlobalCoordinate() {/**/}

StGlobalCoordinate::StGlobalCoordinate(const double x, const double y, const double z)
    : mPosition(x,y,z) { /**/ }

StGlobalCoordinate::StGlobalCoordinate(const StThreeVector<double>& x)
    : mPosition(x) {/**/}

StGlobalCoordinate::~StGlobalCoordinate() {/**/}

int
StGlobalCoordinate::operator==(const StGlobalCoordinate& p) const
{
    return p.mPosition == mPosition;
}

int
StGlobalCoordinate::operator!=(const StGlobalCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

void
StGlobalCoordinate::setPosition(const StThreeVector<double>& val) { mPosition = val; }
    
// Non-member functions
ostream& operator<<(ostream& os, const StGlobalCoordinate& a)
{
    return os << "GC ( "
	      << a.position().x() << ", "
	      << a.position().y() << ", "
	      << a.position().z() << ")";
}
