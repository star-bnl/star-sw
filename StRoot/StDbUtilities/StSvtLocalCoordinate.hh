/***********************************************************************
 *
 * $Id: StSvtLocalCoordinate.hh,v 1.3 2000/08/21 16:16:26 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Svt Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtLocalCoordinate.hh,v $
 * Revision 1.3  2000/08/21 16:16:26  calderon
 * Helen's first version of Svt Coordinate classes.
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
#ifndef ST_SVT_LOCAL_COORDINATE_HH
#define ST_SVT_LOCAL_COORDINATE_HH
#include <iostream.h>

#include "StThreeVector.hh"

class StSvtLocalCoordinate
{
public:
    StSvtLocalCoordinate();
    StSvtLocalCoordinate(const double, const double, const double);
    StSvtLocalCoordinate(const StThreeVector<double>&);

    virtual ~StSvtLocalCoordinate();
    //StSvtLocalCoordinate(const StTpcLocalCoordinate&);
    //StSvtLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StSvtLocalCoordinate&) const;
    int operator!=(const StSvtLocalCoordinate&) const;

    // access functions provided by StThreeVector
    const StThreeVector<double>& position()  const;
    StThreeVector<double>& position();
    const int layer()       const;
    const int ladder()      const;
    const int wafer()       const;
    const int hybrid()      const;
    void setPosition(const StThreeVector<double>&);
    void setLayer(int);
    void setLadder(int);
    void setWafer(int);
    void setHybrid(int);
protected:
    StThreeVector<double> mPosition;
    int mLayer;
    int mLadder;
    int mWafer;
    int mHybrid;

};

inline const StThreeVector<double>& StSvtLocalCoordinate::position() const { return(mPosition); }
inline StThreeVector<double>& StSvtLocalCoordinate::position() { return(mPosition); }
inline void StSvtLocalCoordinate::setPosition(const StThreeVector<double>& val) { mPosition = val; }
const inline int StSvtLocalCoordinate::layer()   const {return(mLayer);}
const inline int StSvtLocalCoordinate::ladder()  const {return(mLadder);}
const inline int StSvtLocalCoordinate::wafer()   const {return(mWafer);}
const inline int StSvtLocalCoordinate::hybrid()   const {return(mHybrid);}

inline void StSvtLocalCoordinate::setLayer(int l)  {mLayer  = l;}
inline void StSvtLocalCoordinate::setLadder(int d) {mLadder = d;}
inline void StSvtLocalCoordinate::setWafer(int w)  {mWafer  = w;}
inline void StSvtLocalCoordinate::setHybrid(int h)  {mHybrid  = h;}


// Non-member
ostream& operator<<(ostream&, const StSvtLocalCoordinate&);
#endif
    
