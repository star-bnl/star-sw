/***********************************************************************
 *
 * $Id: StSvtLocalCoordinate.hh,v 1.5 2007/03/21 16:41:07 fisyak Exp $
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
 * Revision 1.5  2007/03/21 16:41:07  fisyak
 * Use Ivan Kotov's drift velocities
 *
 * Revision 1.4  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
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
#include <Stiostream.h>

#include "StThreeVector.hh"

class StSvtLocalCoordinate {
 public:
  StSvtLocalCoordinate();
  StSvtLocalCoordinate(const double, const double, const double);
  StSvtLocalCoordinate(const StThreeVector<double>&);
  
  virtual ~StSvtLocalCoordinate();
  
  int operator==(const StSvtLocalCoordinate&) const;
  int operator!=(const StSvtLocalCoordinate&) const;

  // access functions provided by StThreeVector
  const StThreeVector<double>& position()  const  { return(mPosition); }
  StThreeVector<double>& position()               { return(mPosition); }
  const int barrel()      const  {return((mLayer-1)/2+1);}
  const int layer()       const  {return(mLayer);}
  const int ladder()      const  {return(mLadder);}
  const int wafer()       const  {return(mWafer);}
  const int hybrid()      const  {return(mHybrid);}
  void setPosition(const StThreeVector<double>& val) { mPosition = val; }
  void setLayer(int l)   {mLayer  = l;}
  void setLadder(int d)  {mLadder = d;}
  void setWafer(int  w)  {mWafer  = w;}
  void setHybrid(int h)  {mHybrid  = h;}
 protected:
  StThreeVector<double> mPosition;
  int mLayer;
  int mLadder;
  int mWafer;
  int mHybrid;
};
ostream& operator<<(ostream&, const StSvtLocalCoordinate&);
#endif
    
