/*********************************************************************
 *
 * $Id: StRichLocalCoordinate.h,v 2.0 2000/08/09 16:17:01 gans Exp $
 *
 * Author: brian Jan 20, 2000
 *
 **********************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 **********************************************************************
 *
 * $Log: StRichLocalCoordinate.h,v $
 * Revision 2.0  2000/08/09 16:17:01  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.1  2000/02/08 16:34:09  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 **********************************************************************/
#ifndef ST_RICH_LOCAL_COORDINATE_HH
#define ST_RICH_LOCAL_COORDINATE_HH

#include <iostream.h>

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StRichLocalCoordinate {
public:
    StRichLocalCoordinate();
    StRichLocalCoordinate(const double, const double, const double);
    StRichLocalCoordinate(const StThreeVector<double>&);

    virtual ~StRichLocalCoordinate();
    //StRichLocalCoordinate(const StRichLocalCoordinate&);
    //StRichLocalCoordinate& operator=(const StRichLocalCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& position()  const;

    // To Modify Coordinates
    StThreeVector<double>& position();

private:
    StThreeVector<double> mPos;
};

inline const StThreeVector<double>&
StRichLocalCoordinate::position() const { return(mPos); }

inline StThreeVector<double>&
StRichLocalCoordinate::position() { return(mPos); }
// Non-member
ostream& operator<<(ostream&, const StRichLocalCoordinate&);
#endif
