/***********************************************************************
 *
 * $Id: StTpcLocalCoordinate.cc,v 1.1 1999/11/19 19:01:08 calderon Exp $
 *
 * Author:  brian May 20, 1998
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 ************************************************************************
 *
 * $Log: StTpcLocalCoordinate.cc,v $
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.3  1999/10/25 18:38:49  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1998/11/16 19:41:57  lasiuk
 * constructor do not use reference for double&
 *
 * Revision 1.1  1998/11/10 17:12:21  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:20:35  lasiuk
 * remove 'St' from variable declarations
 *
 * Revision 1.1  1998/05/21 21:27:57  lasiuk
 * Initial revision
 *
 *
 ***********************************************************************/
#include "StTpcLocalCoordinate.hh"

static const char rcsid[] = "$Id: StTpcLocalCoordinate.cc,v 1.1 1999/11/19 19:01:08 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StTpcLocalCoordinate)
#endif
    
StTpcLocalCoordinate::StTpcLocalCoordinate() {/**/}

StTpcLocalCoordinate::StTpcLocalCoordinate(const float x, const float y, const float z)
    : mPosition(x,y,z) { /* nopt */}

StTpcLocalCoordinate::StTpcLocalCoordinate(const StThreeVectorF& position)
    : mPosition(position) { /* nopt */ }

StTpcLocalCoordinate::~StTpcLocalCoordinate() {/**/}

int
StTpcLocalCoordinate::operator==(const StTpcLocalCoordinate& p) const
{
    return p.mPosition == mPosition;
}

int
StTpcLocalCoordinate::operator!=(const StTpcLocalCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}


// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalCoordinate& a)
{
    return os << "TPC_Local ("
	      << a.position().x() << ", "
	      << a.position().y() << ", "
	      << a.position().z() << ")";
}
