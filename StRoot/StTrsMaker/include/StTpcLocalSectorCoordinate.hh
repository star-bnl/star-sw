/*********************************************************************
 *
 * $Id: StTpcLocalSectorCoordinate.hh,v 1.2 1999/10/04 16:05:59 long Exp $
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

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StTpcLocalSectorCoordinate {
public:
    StTpcLocalSectorCoordinate();
    StTpcLocalSectorCoordinate(const double, const double, const double, const int);
    StTpcLocalSectorCoordinate(const StThreeVector<double>&, const int);

    virtual ~StTpcLocalSectorCoordinate();
    //StTpcLocalSectorCoordinate(const StTpcLocalCoordinate&);
    //StTpcLocalSectorCoordinate& operator=(const StTpcLocalCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& pos()  const;
    //  int   volumeId()                    const;
     int  fromSector()                       const;//HL
    // To Modify Coordinates
    StThreeVector<double>& pos();

private:
    StThreeVector<double> mPos;
  // int                   mVolumeId;
     int                   mFromSector;
    
};

inline const StThreeVector<double>& StTpcLocalSectorCoordinate::pos() const { return(mPos); }
inline StThreeVector<double>& StTpcLocalSectorCoordinate::pos() { return(mPos); }
//inline int StTpcLocalSectorCoordinate::volumeId() const { return(mVolumeId); }
inline int StTpcLocalSectorCoordinate::fromSector() const { return(mFromSector); }//HL
// Non-member
ostream& operator<<(ostream&, const StTpcLocalSectorCoordinate&);
#endif
