/***********************************************************************
 *
 * $Id: StFtpcSectorCoordinate.hh,v 1.1 1999/11/19 19:01:07 calderon Exp $
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
#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

class StFtpcSectorCoordinate
#ifdef PERSISTENT
    : public StObject
#endif
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
#ifdef PERSISTENT
    ClassDef(StFtpcSectorCoordinate,1)
#endif
};

const inline int StFtpcSectorCoordinate::plane()     const {return(mPlane);}
const inline int StFtpcSectorCoordinate::sector()        const {return(mSector);}
inline void StFtpcSectorCoordinate::setPlane(int p) {mPlane = p;}
inline void StFtpcSectorCoordinate::setSector(int s) {mSector = s;}
// Non-member
ostream& operator<<(ostream&, const StFtpcSectorCoordinate&);

#endif
