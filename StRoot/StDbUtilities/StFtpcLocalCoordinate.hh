/***********************************************************************
 *
 * $Id: StFtpcLocalCoordinate.hh,v 1.1 1999/11/19 19:01:07 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Ftpc Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StFtpcLocalCoordinate.hh,v $
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
#ifndef ST_FTPC_LOCAL_COORDINATE_HH
#define ST_FTPC_LOCAL_COORDINATE_HH
#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

#include "StThreeVectorF.hh"

class StFtpcLocalCoordinate
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StFtpcLocalCoordinate();
    StFtpcLocalCoordinate(const float, const float, const float);
    StFtpcLocalCoordinate(const StThreeVectorF&);

    virtual ~StFtpcLocalCoordinate();
    //StFtpcLocalCoordinate(const StTpcLocalCoordinate&);
    //StFtpcLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StFtpcLocalCoordinate&) const;
    int operator!=(const StFtpcLocalCoordinate&) const;
    // access functions provided by StThreeVector
    const StThreeVectorF& position()  const;
    StThreeVectorF& position();
    void setPosition(const StThreeVectorF&);
    
private:
    StThreeVectorF mPosition;
#ifdef PERSISTENT
    ClassDef(StFtpcLocalCoordinate,1)
#endif
};

inline const StThreeVectorF& StFtpcLocalCoordinate::position() const { return(mPosition); }
inline StThreeVectorF& StFtpcLocalCoordinate::position() { return(mPosition); }
inline void StFtpcLocalCoordinate::setPosition(const StThreeVectorF& val) { mPosition = val; }

// Non-member
ostream& operator<<(ostream&, const StFtpcLocalCoordinate&);
#endif
    
