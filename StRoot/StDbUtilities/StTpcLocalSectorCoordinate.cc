/***********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.cc,v 1.3 2004/03/05 17:22:55 fisyak Exp $
 *
 * Author:  brian Jan 26, 1999
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 ************************************************************************
 *
 * $Log: StTpcLocalSectorCoordinate.cc,v $
 * Revision 1.3  2004/03/05 17:22:55  fisyak
 * Add TPC transformations for direction, aligned sectors, protection in order to stay in the same sector when moving from/to Pad coordinates
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
 * Revision 1.3  1999/10/25 18:38:49  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1999/10/04 15:25:53  long
 * change mVolumeId to mFromSector
 *
 * Revision 1.2  1999/10/01 Hui Long
 * change memeber data mVolumeId to mFromSector
 * Revision 1.1  1999/01/28 02:47:40  lasiuk
 * Initial Revision
 *
 ***********************************************************************/
#include "StTpcLocalSectorCoordinate.hh"
static const char rcsid[] = "$Id: StTpcLocalSectorCoordinate.cc,v 1.3 2004/03/05 17:22:55 fisyak Exp $";
int StTpcLocalSectorCoordinate::operator==(const StTpcLocalSectorCoordinate& p) const
{
    return (p.mPosition   == mPosition &&
	    p.mFromSector == mFromSector);
}

int
StTpcLocalSectorCoordinate::operator!=(const StTpcLocalSectorCoordinate& p) const
{
    return !(*this == p);  // use operator==()
}

// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalSectorCoordinate& a)
{
    return os << "TPC_Local_Sector( ("
	      << a.position().x()  << ", "
	      << a.position().y()  << ", "
	      << a.position().z()  << "),"
	      << a.fromSector() << ")";
}
