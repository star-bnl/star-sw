/***************************************************************************
 *
 * $Id: StFgtLHHTracking.h,v 1.2 2012/03/14 22:22:40 sgliske Exp $
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
 * Revision 1.2  2012/03/14 22:22:40  sgliske
 * update
 *
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

   StFgtLHHLine() : pointIdx1(0), pointIdx2(0), discIdx1(0), discIdx2(0),
                    mx(0), my(0), bx(0), by(0), vertZ(-1e11) { /* */ };


   StFgtLHHLine( Int_t pointIdx1_, Int_t pointIdx2_, Int_t discIdx1_, Int_t discIdx2_,
                 Float_t mx_, Float_t my_, Float_t bx_, Float_t by_ ) :
      pointIdx1(pointIdx1_), pointIdx2(pointIdx2_), discIdx1(discIdx1_), discIdx2(discIdx2_),
      mx(mx_), my(my_), bx(bx_), by(by_) {
      vertZ = ( mx || my ? -( mx*bx + my*by )/(mx*mx+my*my) : -1e11 );
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
   enum { kFgtNumDiscPairs = kFgtNumDiscs*(kFgtNumDiscs-1)/2 };
   StFgtLHHLineVec mLineVec[ kFgtNumDiscPairs ];

   // find the tracks
   virtual Int_t findTracks();

   Float_t distanceSqLineToPoint( const StFgtLHHLine& line, const TVector3& pointPos ) const;
   Float_t distanceSqBetween( const StFgtLHHLine& line1, const StFgtLHHLine& line2 ) const;


 private:   
   ClassDef(StFgtLHHTracking,1);

}; 

#endif
