/*!
 * \class StEEmcStripEndPointData_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Simple class to hold the end points of the strips
 *
*/

#ifndef _ST_EEMC_STRIP_END_POINT_DATA_H_
#define _ST_EEMC_STRIP_END_POINT_DATA_H_

#include <vector>
#include <Rtypes.h>

// forward declaration
class StEEmcStripEndPointData_t;

// typedef
typedef std::vector< StEEmcStripEndPointData_t > StEEmcStripEndPointDataVec_t;

// The class
class StEEmcStripEndPointData_t {
 public:
   // constructor
   StEEmcStripEndPointData_t() : mStripIndex(0), mX(0), mY1(0), mY2(0) { /* */ };
   StEEmcStripEndPointData_t( Short_t stripIndex, Float_t x, Float_t y1, Float_t y2 ) : mStripIndex(stripIndex), mX(x), mY1(y1), mY2(y2) { /* */ };
   ~StEEmcStripEndPointData_t() { /* */ };

   // less than if X values are less than (needed to find strips by X position)
   Bool_t operator<( const StEEmcStripEndPointData_t& other ) const { return (mX < other.mX); };

   Short_t getStripIndex() const { return mStripIndex; };
   Float_t getX() const { return mX; };
   Float_t getY1() const { return mY1; };
   Float_t getY2() const { return mY2; };

 protected:
   Short_t mStripIndex;   // 0-287
   Float_t mX, mY1, mY2;  // endpoints

 private:
   /// Make class available to root
   ClassDef(StEEmcStripEndPointData_t,1);
};

#endif

/*
 * $Id: StEEmcStripEndPointData.h,v 1.1 2012/08/29 15:44:17 sgliske Exp $
 * $Log: StEEmcStripEndPointData.h,v $
 * Revision 1.1  2012/08/29 15:44:17  sgliske
 * Moved from offline/users/sgliske/StRoot/StEEmcPool
 *
 *
 */
