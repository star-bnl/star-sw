/***********************************************************************
 *
 * $Id: StGlobalCoordinate.hh,v 1.1 1998/11/10 17:12:04 fisyak Exp $
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
 * Revision 1.1  1998/11/10 17:12:04  fisyak
 * Put Brian trs versin into StRoot
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

#include "StGlobals.hh"
#include "StThreeVector.hh"


class StGlobalCoordinate {         
public:
    StGlobalCoordinate();
    StGlobalCoordinate(const double&, const double&, const double&);
    StGlobalCoordinate(const StThreeVector<double>&);

    virtual ~StGlobalCoordinate();
    //StGlobalCoordinate(const StGlobalCoordinate&);
    //StGlobalCoordinate& operator=(const StGlobalCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& pos() const;
    
private:
    StThreeVector<double> mPos;
};

inline const StThreeVector<double>& StGlobalCoordinate::pos() const {return(mPos);}

// Non-Member
ostream& operator<<(ostream&, const StGlobalCoordinate&);

#endif
