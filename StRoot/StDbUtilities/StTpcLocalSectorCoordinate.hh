/*********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.hh,v 1.1 1999/11/19 19:01:09 calderon Exp $
 *
 * Author: brian Jan 26, 1999
 *
 **********************************************************************
 *
 * Description:  Local Sector
 *
 **********************************************************************
 *
 * $Log: StTpcLocalSectorCoordinate.hh,v $
 * Revision 1.1  1999/11/19 19:01:09  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.3  1999/10/25 18:38:29  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1999/10/04 16:05:59  long
 * change mVolumeId to mFromSector
 *
 * Revision 1.2  1999/10/01 17:15:00 Hui Long
 * replace mVolumeId by fromSector
 *
 * Revision 1.1  1999/01/28 02:48:12  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifndef ST_TPC_LOCAL_SECTOR_COORDINATE_HH
#define ST_TPC_LOCAL_SECTOR_COORDINATE_HH

#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

#include "StThreeVectorF.hh"

class StTpcLocalSectorCoordinate
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StTpcLocalSectorCoordinate();
    StTpcLocalSectorCoordinate(const float, const float, const float, const int);
    StTpcLocalSectorCoordinate(const StThreeVectorF&, const int);

    virtual ~StTpcLocalSectorCoordinate();
    //StTpcLocalSectorCoordinate(const StTpcLocalCoordinate&);
    //StTpcLocalSectorCoordinate& operator=(const StTpcLocalCoordinate&);

    int operator==(const StTpcLocalSectorCoordinate&) const;
    int operator!=(const StTpcLocalSectorCoordinate&) const;
     
    // access functions provided by StThreeVector
    const StThreeVectorF& position()  const;
    int  fromSector()                 const;
    StThreeVectorF& position();

private:
    StThreeVectorF mPosition;
    int            mFromSector;
#ifdef PERSISTENT
    ClassDef(StTpcLocalSectorCoordinate,1)
#endif
    
};

inline const StThreeVectorF& StTpcLocalSectorCoordinate::position() const { return(mPosition); }
inline StThreeVectorF& StTpcLocalSectorCoordinate::position() { return(mPosition); }
inline int StTpcLocalSectorCoordinate::fromSector() const { return(mFromSector); }//HL

// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorCoordinate&);
#endif
