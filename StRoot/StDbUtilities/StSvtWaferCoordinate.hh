/***********************************************************************
 *
 * $Id: StSvtWaferCoordinate.hh,v 1.6 2003/09/02 17:57:51 perev Exp $
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
 * Revision 1.6  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2001/11/06 20:57:03  caines
 * Add function to get barrel number
 *
 * Revision 1.4  2000/08/21 16:16:26  calderon
 * Helen's first version of Svt Coordinate classes.
 *
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
#include <Stiostream.h>

class StSvtWaferCoordinate
{ 
public:
    StSvtWaferCoordinate();
    StSvtWaferCoordinate(const int, const int, const int, const int, const double, const double);

    virtual ~StSvtWaferCoordinate();
    //StSvtWaferCoordinate(const StSvtWaferCoordinate&);
    //StSvtWaferCoordinate& operator=(cont StSvtWaferCoordinate&);
    
    int operator==(const StSvtWaferCoordinate&) const;
    int operator!=(const StSvtWaferCoordinate&) const;

    // access functions
    const int barrel()      const;
    const int layer()       const;
    const int ladder()      const;
    const int wafer()       const;
    const int hybrid()      const;
    const double anode()    const;
    const double timebucket() const;

    void setLayer(int);
    void setLadder(int);
    void setWafer(int);
    void setHybrid(int);
    void setAnode(double);
    void setTimeBucket(double);
    
protected:
    int mLayer;
    int mLadder;
    int mWafer;
    int mHybrid;
    double mAnode;
    double mTimeBucket;
};

const inline int StSvtWaferCoordinate::barrel() 
  const {return(((mLayer-1)/2)+1);}
const inline int StSvtWaferCoordinate::layer()   const {return(mLayer);}
const inline int StSvtWaferCoordinate::ladder()  const {return(mLadder);}
const inline int StSvtWaferCoordinate::wafer()   const {return(mWafer);}
const inline int StSvtWaferCoordinate::hybrid()   const {return(mHybrid);}
const inline double StSvtWaferCoordinate::anode()  const {return(mAnode);}
const inline double StSvtWaferCoordinate::timebucket()   const {return(mTimeBucket);}
inline void StSvtWaferCoordinate::setLayer(int l)  {mLayer  = l;}
inline void StSvtWaferCoordinate::setLadder(int d) {mLadder = d;}
inline void StSvtWaferCoordinate::setWafer(int w)  {mWafer  = w;}
inline void StSvtWaferCoordinate::setHybrid(int h)  {mHybrid  = h;}
inline void StSvtWaferCoordinate::setAnode(double a)  {mAnode = a;}
inline void StSvtWaferCoordinate::setTimeBucket(double t)  {mTimeBucket  = t;}
// Non-member
ostream& operator<<(ostream&, const StSvtWaferCoordinate&);

#endif
