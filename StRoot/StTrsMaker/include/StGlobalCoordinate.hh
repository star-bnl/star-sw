/***********************************************************************
 *
 * $Id: StGlobalCoordinate.hh,v 1.4 2000/02/10 01:21:46 calderon Exp $
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
 * Revision 1.4  2000/02/10 01:21:46  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
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

//#include "StGlobals.hh"
#include "StThreeVector.hh"

class StGlobalCoordinate {         
public:
    StGlobalCoordinate();
    StGlobalCoordinate(const double, const double, const double);
    StGlobalCoordinate(const StThreeVector<double>&);

    virtual ~StGlobalCoordinate();
    //StGlobalCoordinate(const StGlobalCoordinate&);
    //StGlobalCoordinate& operator=(const StGlobalCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& position() const;
    
private:
    StThreeVector<double> mPosition;
};

inline const StThreeVector<double>& StGlobalCoordinate::position() const {return(mPosition);}

// Non-Member
ostream& operator<<(ostream&, const StGlobalCoordinate&);

#endif
