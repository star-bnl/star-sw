/***********************************************************************
 *
 * $Id: StFtpcSectorCoordinate.hh,v 1.3 2003/09/02 17:57:51 perev Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Ftpc Sector Coordinate
 *
 ************************************************************************
 *
 * $Log: StFtpcSectorCoordinate.hh,v $
 * Revision 1.3  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
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
 *
 ***********************************************************************/
#ifndef ST_FTPC_SECTOR_COORDINATE_HH
#define ST_FTPC_SECTOR_COORDINATE_HH
#include <Stiostream.h>

class StFtpcSectorCoordinate
{
public:
    StFtpcSectorCoordinate();
    StFtpcSectorCoordinate(const int, const int);

    virtual ~StFtpcSectorCoordinate();
    //StFtpcSectorCoordinate(const StFtpcSectorCoordinate&);
    //StFtpcSectorCoordinate& operator=(cont StFtpcSectorCoordinate&);
    
    int operator==(const StFtpcSectorCoordinate&) const;
    int operator!=(const StFtpcSectorCoordinate&) const;
    // access functions
    const int plane()       const;
    const int sector()      const;

    void setPlane(int);
    void setSector(int);
    
private:
    int mPlane;
    int mSector;
};

const inline int StFtpcSectorCoordinate::plane()     const {return(mPlane);}
const inline int StFtpcSectorCoordinate::sector()        const {return(mSector);}
inline void StFtpcSectorCoordinate::setPlane(int p) {mPlane = p;}
inline void StFtpcSectorCoordinate::setSector(int s) {mSector = s;}
// Non-member
ostream& operator<<(ostream&, const StFtpcSectorCoordinate&);

#endif
