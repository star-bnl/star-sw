/***********************************************************************
 *
 * $Id: StSvtWaferCoordinate.cc,v 1.1 1999/11/19 19:01:08 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Svt Wafer Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtWaferCoordinate.cc,v $
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
#include "StSvtWaferCoordinate.hh"

static const char rcsid[] = "$Id: StSvtWaferCoordinate.cc,v 1.1 1999/11/19 19:01:08 calderon Exp $";

#ifdef PERSISTENT
ClassImp(StSvtWaferCoordinate)
#endif
    
StSvtWaferCoordinate::StSvtWaferCoordinate() {/**/}

StSvtWaferCoordinate::StSvtWaferCoordinate(const int layer, const int ladder, const int wafer)
    : mLayer(layer), mLadder(ladder), mWafer(wafer) {/**/}

StSvtWaferCoordinate::~StSvtWaferCoordinate() {/**/}

int
StSvtWaferCoordinate::operator==(const StSvtWaferCoordinate& p) const
{

    return (p.mLayer     == mLayer  &&
	    p.mLadder    == mLadder &&
	    p.mWafer     == mWafer);
}

int
StSvtWaferCoordinate::operator!=(const StSvtWaferCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

// Non-Member function
ostream& operator<<(ostream& os, const StSvtWaferCoordinate& a)
{
    return os << "(layer= "   << a.layer()
	      << ", ladder= " << a.ladder()
	      << ", wafer= "  << a.wafer()
	      << ")";
}
