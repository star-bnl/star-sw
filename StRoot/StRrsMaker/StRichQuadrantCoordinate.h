/*********************************************************************
 *
 * $Id: StRichQuadrantCoordinate.h,v 1.1 2000/02/08 16:34:11 lasiuk Exp $
 *
 * Author: brian Jan 20, 2000
 *
 **********************************************************************
 *
 * Description:  Quadrant!
 *
 **********************************************************************
 *
 * $Log: StRichQuadrantCoordinate.h,v $
 * Revision 1.1  2000/02/08 16:34:11  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 **********************************************************************/
#ifndef ST_RICH_QUADRANT_COORDINATE_HH
#define ST_RICH_QUADRANT_COORDINATE_HH

#include <iostream.h>

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StRichQuadrantCoordinate {
public:
    StRichQuadrantCoordinate();
    StRichQuadrantCoordinate(const double, const double, const double, const int);
    StRichQuadrantCoordinate(const StThreeVector<double>&, const int);

    virtual ~StRichQuadrantCoordinate();
    //StRichQuadrantCoordinate(const StRichQuadrantCoordinate&);
    //StRichQuadrantCoordinate& operator=(const StRichQuadrantCoordinate&);
    
    // access functions provided by StThreeVector
    const StThreeVector<double>& position()  const;
    const int quadrant()                     const;

    // To Modify Coordinates
    StThreeVector<double>& position();
    void setQuadrant(int);

private:
    StThreeVector<double> mPos;
    int                   mQuadrant;
};

inline const StThreeVector<double>&
StRichQuadrantCoordinate::position() const { return(mPos); }

inline StThreeVector<double>&
StRichQuadrantCoordinate::position() { return(mPos); }

inline const int
StRichQuadrantCoordinate::quadrant()  const { return(mQuadrant); }

inline void
StRichQuadrantCoordinate::setQuadrant(int q) { mQuadrant = q; }

// Non-member
ostream& operator<<(ostream&, const StRichQuadrantCoordinate&);
#endif
