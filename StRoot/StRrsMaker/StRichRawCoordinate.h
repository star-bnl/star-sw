/***********************************************************************
 *
 * $Id: StRichRawCoordinate.h,v 1.2 2000/04/05 16:03:58 lasiuk Exp $
 *
 * Author:  brian Jan 27, 2000
 *
 ************************************************************************
 *
 * Description:  Global Coordinate
 *
 *************************************************************************
 *
 * $Log: StRichRawCoordinate.h,v $
 * Revision 1.2  2000/04/05 16:03:58  lasiuk
 * fractional pad/column to give 1-1 correspondence
 *
 * Revision 1.1  2000/02/08 16:34:12  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 ***********************************************************************/
#ifndef ST_RICH_RAW_COORDINATE_HH
#define ST_RICH_RAW_COORDINATE_HH
#include <iostream.h>

#include "StGlobals.hh"

class StRichRawCoordinate {
public:
    StRichRawCoordinate();
    StRichRawCoordinate(const double pad, const double row);

    virtual ~StRichRawCoordinate();
    //StRichRawCoordinate(const StRichRawCoordinate&);
    //StRichRawCoordinate& operator=(cont StRichRawCoordinate&);
    
    // access functions
    const double row()     const;
    const double pad()     const;
    const double column()  const;
    void setRow(double);
    void setPad(double);
    void setColumn(double);
    
private:
    double mRow;
    double mPad;
};

const inline double StRichRawCoordinate::row()        const {return(mRow);}
const inline double StRichRawCoordinate::pad()        const {return(mPad);}
const inline double StRichRawCoordinate::column()     const {return(mPad);}
inline void StRichRawCoordinate::setRow(double r)    {mRow = r;}
inline void StRichRawCoordinate::setPad(double p)    {mPad = p;}
inline void StRichRawCoordinate::setColumn(double p) {mPad = p;}
// Non-member
ostream& operator<<(ostream&, const StRichRawCoordinate&);

#endif
