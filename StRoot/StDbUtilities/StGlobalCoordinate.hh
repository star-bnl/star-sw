/***********************************************************************
 *
 * $Id: StGlobalCoordinate.hh,v 1.1 1999/11/19 19:01:07 calderon Exp $
 *
 * Author:  brian Feb 6, 1998
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 *************************************************************************
 *
 * $Log: StGlobalCoordinate.hh,v $
 * Revision 1.1  1999/11/19 19:01:07  calderon
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
 * Revision 1.2  1998/11/16 19:40:21  lasiuk
 * constructors do not use reference for doubles
 *
 * Revision 1.1  1998/11/10 17:12:04  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:21:06  lasiuk
 * remove 'St' from variable declarations
 * add set functions in local Coordinates
 *
 * Revision 1.1  1998/05/21 21:27:37  lasiuk
 * Initial revision
 *
 *
 ***********************************************************************/
#ifndef ST_GLOBAL_COORDINATE_HH
#define ST_GLOBAL_COORDINATE_HH

#include <iostream.h>

#ifdef PERSISTENT
#include "StObject.h"
#endif

#include "StThreeVectorF.hh"

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

class StGlobalCoordinate
#ifdef PERSISTENT
: public StObject
#endif
{         
public:
    StGlobalCoordinate();
    StGlobalCoordinate(const float, const float, const float);
    StGlobalCoordinate(const StThreeVectorF&);

    virtual ~StGlobalCoordinate();
    //StGlobalCoordinate(const StGlobalCoordinate&);
    //StGlobalCoordinate& operator=(const StGlobalCoordinate&);
    
    int operator==(const StGlobalCoordinate&) const;
    int operator!=(const StGlobalCoordinate&) const;
    // access functions provided by StThreeVector
    virtual const StThreeVectorF& position() const;
    virtual void setPosition(const StThreeVectorF&);
    
protected:
    StThreeVectorF mPosition;

#ifdef PERSISTENT
    ClassDef(StGlobalCoordinate,1)
#endif
};

// Non-Member
ostream& operator<<(ostream&, const StGlobalCoordinate&);

#endif
