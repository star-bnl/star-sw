/***********************************************************************
 *
 * $Id: StRichRawCoordinate.h,v 1.1 2000/02/08 16:34:12 lasiuk Exp $
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
 * Revision 1.1  2000/02/08 16:34:12  lasiuk
 * Initial Revision:  eventually for StUtilities
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
    StRichRawCoordinate(const int pad, const int row);

    virtual ~StRichRawCoordinate();
    //StRichRawCoordinate(const StRichRawCoordinate&);
    //StRichRawCoordinate& operator=(cont StRichRawCoordinate&);
    
    // access functions
    const int row()     const;
    const int pad()     const;
    const int column()  const;
    void setRow(int);
    void setPad(int);
    void setColumn(int);
    
private:
    int mRow;
    int mPad;
};

const inline int StRichRawCoordinate::row()        const {return(mRow);}
const inline int StRichRawCoordinate::pad()        const {return(mPad);}
const inline int StRichRawCoordinate::column()     const {return(mPad);}
inline void StRichRawCoordinate::setRow(int r)    {mRow = r;}
inline void StRichRawCoordinate::setPad(int p)    {mPad = p;}
inline void StRichRawCoordinate::setColumn(int p) {mPad = p;}
// Non-member
ostream& operator<<(ostream&, const StRichRawCoordinate&);

#endif
