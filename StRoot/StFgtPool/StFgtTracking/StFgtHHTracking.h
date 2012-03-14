/***************************************************************************
 *
 * $Id: StFgtHHTracking.h,v 1.2 2012/03/14 22:22:40 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: FGT tracking algorithm using (H)elix tracks and
 * (H)ough transformations.
 *
 ***************************************************************************
 *
 * $Log: StFgtHHTracking.h,v $
 * Revision 1.2  2012/03/14 22:22:40  sgliske
 * update
 *
 * Revision 1.1  2012/03/07 15:38:04  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_HH_TRACKING_
#define _ST_FGT_HH_TRACKING_

#include "StFgtTracking.h"
#include <vector>

#include "StRoot/StFgtUtil/StFgtConsts.h"

struct StFgtHHelix {
   Int_t idx1, idx2;               // track indices of primary hits
   Float_t x0, y0, r, slope, phi0; // parameters of the helix
   Float_t vertZ;                  // derived quantity--z of the vertex

   StFgtHHelix( Int_t i1 = -1, Int_t i2 = -1, Float_t x = 0, Float_t y = 0, Float_t R = 0, Float_t m = 0, Float_t phi = 0, Float_t z = -10000 ) : 
      idx1(i1), idx2(i2), x0(x), y0(y), r(R), slope(m), phi0(phi), vertZ(z) { /* */ };
};

typedef std::vector< StFgtHHelix > StFgtHHelixVec;

class StFgtHHTracking : public StFgtTracking {
 public:
   // constructors
   StFgtHHTracking( const Char_t* name = "fgtTracking" );

   // deconstructor
   virtual ~StFgtHHTracking();

   // default equals operator and copy constructor OK

   // uses parent's ::Make()
   virtual void Clear( const Option_t *opt = "" );

 protected:
   // array of helixes
   StFgtHHelixVec mHelixVec;
   Float_t mMinRad, mMaxRad;

   // X, Y position of the beam -- fixed
   TVector2 mBeamPos;

   // find the tracks
   virtual Int_t findTracks();

 private:   
   ClassDef(StFgtHHTracking,1);

}; 

#endif
