/***********************************************************************
 *
 * $Id: StSvtWaferCoordinate.hh,v 1.3 2000/02/02 23:16:00 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Svt Wafer Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtWaferCoordinate.hh,v $
 * Revision 1.3  2000/02/02 23:16:00  calderon
 * remove using namespace std
 *
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
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
#ifndef ST_SVT_WAFER_COORDINATE_HH
#define ST_SVT_WAFER_COORDINATE_HH
#include <iostream.h>

class StSvtWaferCoordinate
{ 
public:
    StSvtWaferCoordinate();
    StSvtWaferCoordinate(const int, const int, const int);

    virtual ~StSvtWaferCoordinate();
    //StSvtWaferCoordinate(const StSvtWaferCoordinate&);
    //StSvtWaferCoordinate& operator=(cont StSvtWaferCoordinate&);
    
    int operator==(const StSvtWaferCoordinate&) const;
    int operator!=(const StSvtWaferCoordinate&) const;

    // access functions
    const int layer()       const;
    const int ladder()      const;
    const int wafer()       const;

    void setLayer(int);
    void setLadder(int);
    void setWafer(int);
    
protected:
    int mLayer;
    int mLadder;
    int mWafer;
};

const inline int StSvtWaferCoordinate::layer()   const {return(mLayer);}
const inline int StSvtWaferCoordinate::ladder()  const {return(mLadder);}
const inline int StSvtWaferCoordinate::wafer()   const {return(mWafer);}
inline void StSvtWaferCoordinate::setLayer(int l)  {mLayer  = l;}
inline void StSvtWaferCoordinate::setLadder(int d) {mLadder = d;}
inline void StSvtWaferCoordinate::setWafer(int w)  {mWafer  = w;}
// Non-member
ostream& operator<<(ostream&, const StSvtWaferCoordinate&);

#endif
