/*********************************************************************
 *
 * $Id: StTpcLocalCoordinate.hh,v 1.3 1998/11/16 19:40:19 lasiuk Exp $
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

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StTpcLocalCoordinate {
public:
    StTpcLocalCoordinate();
    StTpcLocalCoordinate(const double, const double, const double);
    StTpcLocalCoordinate(const StThreeVector<double>&);

    virtual ~StTpcLocalCoordinate();
    //StTpcLocalCoordinate(const StTpcLocalCoordinate&);
    //StTpcLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& pos()  const;

    // To Modify Coordinates
    StThreeVector<double>& pos();

private:
    StThreeVector<double> mPos;
};

inline const StThreeVector<double>& StTpcLocalCoordinate::pos() const { return(mPos); }
inline StThreeVector<double>& StTpcLocalCoordinate::pos() { return(mPos); }
// Non-member
ostream& operator<<(ostream&, const StTpcLocalCoordinate&);
#endif
