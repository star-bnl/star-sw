/*******************************************************************
 *
 * $Id: StTpcPadCoordinate.cc,v 1.1 1998/11/10 17:12:21 fisyak Exp $
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

StTpcPadCoordinate::StTpcPadCoordinate() {/**/}

StTpcPadCoordinate::StTpcPadCoordinate(const int sector, const int row, const int pad, const int tb)
    : mSector(sector), mRow(row), mPad(pad), mTimeBucket(tb) {/**/}

StTpcPadCoordinate::~StTpcPadCoordinate() {/**/}


// Non-Member function
ostream& operator<<(ostream& os, const StTpcPadCoordinate& a)
{
    return os << "(sector= " << a.sector()
	      << ", row= "    << a.row()
	      << ", pad= "    << a.pad()
	      << ", tbuck= "  << a.timeBucket() << ")";
}
