/*******************************************************************
 *
 * $Id: StTpcPadCoordinate.cc,v 1.2 2000/02/02 23:01:39 calderon Exp $
 *
 * Author: brian Feb 6, 1998
 *
 *******************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 *******************************************************************
 *
 * $Log: StTpcPadCoordinate.cc,v $
 * Revision 1.2  2000/02/02 23:01:39  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:09  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.1  1998/11/10 17:12:21  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:20:34  lasiuk
 * remove 'St' from variable declarations
 *
 * Revision 1.1  1998/05/21 21:27:57  lasiuk
 * Initial revision
 *
 *
 *******************************************************************/
#include "StTpcPadCoordinate.hh"

static const char rcsid[] = "$Id: StTpcPadCoordinate.cc,v 1.2 2000/02/02 23:01:39 calderon Exp $";

StTpcPadCoordinate::StTpcPadCoordinate() {/**/}

StTpcPadCoordinate::StTpcPadCoordinate(const int sector, const int row, const int pad, const int tb)
    : mSector(sector), mRow(row), mPad(pad), mTimeBucket(tb) {/**/}

StTpcPadCoordinate::~StTpcPadCoordinate() {/**/}

int
StTpcPadCoordinate::operator==(const StTpcPadCoordinate& p) const
{
    return (p.mSector     == mSector &&
	    p.mRow        == mRow    &&
	    p.mPad        == mPad    &&
	    p.mTimeBucket == mTimeBucket);
}

int
StTpcPadCoordinate::operator!=(const StTpcPadCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

// Non-Member function
ostream& operator<<(ostream& os, const StTpcPadCoordinate& a)
{
    return os << "(sector= " << a.sector()
	      << ", row= "    << a.row()
	      << ", pad= "    << a.pad()
	      << ", tbuck= "  << a.timeBucket() << ")";
}
