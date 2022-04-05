/***********************************************************************
 *
 * $Id: StSvtWaferCoordinate.cc,v 1.3 2000/08/21 16:16:26 calderon Exp $
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
#include "StSvtWaferCoordinate.hh"

static const char rcsid[] = "$Id: StSvtWaferCoordinate.cc,v 1.3 2000/08/21 16:16:26 calderon Exp $";

    
StSvtWaferCoordinate::StSvtWaferCoordinate() {/**/}

StSvtWaferCoordinate::StSvtWaferCoordinate(const int layer, const int ladder, const int wafer, const int hybrid, const double anode, const double timebucket)
    : mLayer(layer), mLadder(ladder), mWafer(wafer), mHybrid(hybrid), mAnode(anode), mTimeBucket(timebucket) {/**/}

StSvtWaferCoordinate::~StSvtWaferCoordinate() {/**/}

int
StSvtWaferCoordinate::operator==(const StSvtWaferCoordinate& p) const
{

    return (p.mLayer      == mLayer  &&
	    p.mLadder     == mLadder &&
	    p.mWafer      == mWafer  &&
            p.mHybrid     == mHybrid &&
            p.mAnode      == mAnode  &&
            p.mTimeBucket == mTimeBucket);
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
	      << ", hybrid= "  << a.hybrid()
	      << ", anode= "  << a.anode()
	      << ", timebucket= "  << a.timebucket()
	      << ")";
}
