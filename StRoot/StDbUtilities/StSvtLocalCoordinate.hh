/***********************************************************************
 *
 * $Id: StSvtLocalCoordinate.hh,v 1.1 1999/11/19 19:01:08 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Svt Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtLocalCoordinate.hh,v $
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
#ifndef ST_SVT_LOCAL_COORDINATE_HH
#define ST_SVT_LOCAL_COORDINATE_HH
#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

#include "StThreeVectorF.hh"

class StSvtLocalCoordinate
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StSvtLocalCoordinate();
    StSvtLocalCoordinate(const float, const float, const float);
    StSvtLocalCoordinate(const StThreeVectorF&);

    virtual ~StSvtLocalCoordinate();
    //StSvtLocalCoordinate(const StTpcLocalCoordinate&);
    //StSvtLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StSvtLocalCoordinate&) const;
    int operator!=(const StSvtLocalCoordinate&) const;

    // access functions provided by StThreeVector
    const StThreeVectorF& position()  const;
    StThreeVectorF& position();
    void setPosition(const StThreeVectorF&);
protected:
    StThreeVectorF mPosition;

#ifdef PERSISTENT
    ClassDef(StSvtLocalCoordinate,1)
#endif
};

inline const StThreeVectorF& StSvtLocalCoordinate::position() const { return(mPosition); }
inline StThreeVectorF& StSvtLocalCoordinate::position() { return(mPosition); }
inline void StSvtLocalCoordinate::setPosition(const StThreeVectorF& val) { mPosition = val; }

// Non-member
ostream& operator<<(ostream&, const StSvtLocalCoordinate&);
#endif
    
