/***************************************************************************
 *
 * $Id: StFgtLHHTracking.h,v 1.1 2012/03/14 21:04:17 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: FGT tracking algorithm using (L)ine tracks and (H)ough
 * transformations, folowed by estimating the helix parameters once
 * the points along the line are selected.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHHTracking.h,v $
 * Revision 1.1  2012/03/14 21:04:17  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_LHH_TRACKING_
#define _ST_FGT_LHH_TRACKING_

#include "StFgtTracking.h"
#include <vector>

#include "StRoot/StFgtUtil/StFgtConsts.h"

struct StFgtLHHLine {
   Int_t pointIdx1, pointIdx2;     // the point index values
   Int_t discIdx1, discIdx2;       // the discs of the first and second points.  discIdx1 < discIdx2
   Float_t mx, my, bx, by;         // parameters of the line
   Float_t vertZ;                  // derived quantity--z of the vertex

   StFgtLHHLine( Int_t pointIdx1_, Int_t pointIdx2_, Int_t discIdx1_, Int_t discIdx2_,
                 Float_t mx_, Float_t my_, Float_t bx_, Float_t by_ ) :
      pointIdx1(pointIdx1_), pointIdx2(pointIdx2_), discIdx1(discIdx1_), discIdx2(discIdx2_),
      mx(mx_), my(my_), bx(bx_), by(by_) {
      vertZ = ( mx || my ? -( mx*bx + my*by )/(mx*mx+my*my) : -10000 );
   };
};

typedef std::vector< StFgtLHHLine > StFgtLHHLineVec;

class StFgtLHHTracking : public StFgtTracking {
 public:
   // constructors
   StFgtLHHTracking( const Char_t* name = "fgtTracking" );

   // deconstructor
   virtual ~StFgtLHHTracking();

   // default equals operator and copy constructor OK

   // uses parent's ::Make()
   virtual void Clear( const Option_t *opt = "" );

 protected:
   // array of helixes
   StFgtLHHLineVec mLineVec;

   // find the tracks
   virtual Int_t findTracks();

 private:   
   ClassDef(StFgtLHHTracking,1);

}; 

#endif
