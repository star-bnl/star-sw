/*********************************************************************
 *
 * $Id: StTpcLocalCoordinate.hh,v 1.1 1999/11/19 19:01:08 calderon Exp $
 *
 * Author: brian May 20, 1998
 *
 **********************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 **********************************************************************
 *
 * $Log: StTpcLocalCoordinate.hh,v $
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.4  1999/10/25 18:38:29  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.3  1998/11/16 19:40:19  lasiuk
 * constructors do not use reference for doubles
 *
 * Revision 1.2  1998/11/13 21:29:29  lasiuk
 * allow setting coordinates individually
 *
 * Revision 1.1  1998/11/10 17:12:06  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:21:03  lasiuk
 * remove 'St' from variable declarations
 * add set functions in local Coordinates
 *
 * Revision 1.1  1998/05/21 21:27:38  lasiuk
 * Initial revision
 *
 *
 **********************************************************************/
#ifndef ST_TPC_LOCAL_COORDINATE_HH
#define ST_TPC_LOCAL_COORDINATE_HH

#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

#include "StThreeVectorF.hh"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StTpcLocalCoordinate
#ifdef PERSISTENT
    : public StObject
#endif
{
public:
    StTpcLocalCoordinate();
    StTpcLocalCoordinate(const float, const float, const float);
    StTpcLocalCoordinate(const StThreeVectorF&);

    virtual ~StTpcLocalCoordinate();
    //StTpcLocalCoordinate(const StTpcLocalCoordinate&);
    //StTpcLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StTpcLocalCoordinate&) const;
    int operator!=(const StTpcLocalCoordinate&) const;
    // access functions provided by StThreeVector
    const StThreeVectorF& position()  const;
    void setPosition(const StThreeVectorF&);
    StThreeVectorF& position();

protected:
    StThreeVectorF mPosition;
#ifdef PERSISTENT
    ClassDef(StTpcLocalCoordinate,1)
#endif
};

inline const StThreeVectorF& StTpcLocalCoordinate::position() const { return(mPosition); }
inline StThreeVectorF& StTpcLocalCoordinate::position() { return(mPosition); }
inline void StTpcLocalCoordinate::setPosition(const StThreeVectorF& val) { mPosition = val; }

// Non-member
ostream& operator<<(ostream&, const StTpcLocalCoordinate&);
#endif
