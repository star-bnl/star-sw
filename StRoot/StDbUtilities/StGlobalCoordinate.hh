/***********************************************************************
 *
 * $Id: StGlobalCoordinate.hh,v 1.6 2011/01/18 14:34:28 fisyak Exp $
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
 * Revision 1.6  2011/01/18 14:34:28  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.5  2005/07/06 19:10:34  fisyak
 * Add TpcCoordinate transormation classes to dictionary, use templated StThreeVector
 *
 * Revision 1.4  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/04/28 16:40:35  calderon
 * added constructor taking StThreeVectorF, because that's what
 * StHits and StMcHits have.
 *
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:07  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 * Revision 1.3  1999/10/25 18:38:29  calderon
 * changed mPos and pos() to mPosition and position() to
 * be compatible with StEvent/StMcEvent.
 *
 * Revision 1.2  1998/11/16 19:40:21  lasiuk
 * constructors do not use reference for doubles
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

#include <Stiostream.h>

#include "StThreeVectorF.hh"

class StGlobalCoordinate
{         
public:
  StGlobalCoordinate() {}
  StGlobalCoordinate(const double x, const double y, const double z) : mPosition(x,y,z) { }
  StGlobalCoordinate(const double *x) : mPosition(x) { }
  StGlobalCoordinate(const StThreeVector<double>& x) : mPosition(x) {}
  StGlobalCoordinate(const StThreeVectorF& x) : mPosition(x.x(), x.y(), x.z()) {}
  
  virtual ~StGlobalCoordinate() {}
  int operator==(const StGlobalCoordinate& p) const {return p.mPosition == mPosition;}
  int operator!=(const StGlobalCoordinate& p) const {return !(*this == p);}
  // access functions provided by StThreeVector
  virtual const StThreeVector<double>& position() const {return *&mPosition;}
  virtual       StThreeVector<double>& position()       {return *&mPosition;}
  virtual void setPosition(const StThreeVector<double>& val) {mPosition = val; }
    
protected:
    StThreeVector<double> mPosition;

};
// Non-Member
ostream& operator<<(ostream&, const StGlobalCoordinate&);
#endif
