/***********************************************************************
 *
 * $Id: StSvtLocalCoordinate.cc,v 1.3 2000/08/21 16:16:26 calderon Exp $
 *
 * Author:  Helen C
 *
 ************************************************************************
 *
 * Description:  Svt Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtLocalCoordinate.cc,v $
 * Revision 1.3  2000/08/21 16:16:26  calderon
 * Helen's first version of Svt Coordinate classes.
 *
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 *
 ***********************************************************************/
#include "StSvtLocalCoordinate.hh"

static const char rcsid[] = "$Id: StSvtLocalCoordinate.cc,v 1.3 2000/08/21 16:16:26 calderon Exp $";

    
StSvtLocalCoordinate::StSvtLocalCoordinate() {/**/}

StSvtLocalCoordinate::StSvtLocalCoordinate(const double x, const double y, const double z)
    : mPosition(x,y,z) { /* nopt */}

StSvtLocalCoordinate::StSvtLocalCoordinate(const StThreeVector<double>& position)
    : mPosition(position) { /* nopt */ }

StSvtLocalCoordinate::~StSvtLocalCoordinate() {/**/}

int
StSvtLocalCoordinate::operator==(const StSvtLocalCoordinate& p) const
{
    return p.mPosition == mPosition;
}

int
StSvtLocalCoordinate::operator!=(const StSvtLocalCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}


// Non-member Functions
ostream& operator<<(ostream& os, const StSvtLocalCoordinate& a)
{
    return os << "SVT_Local( "
	      << a.position().x()  << ", "
	      << a.position().y()  << ", "
	      << a.position().z()  << ")";
}
