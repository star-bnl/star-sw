/***************************************************************************
 *
 * $Id: StFgtLHTracking.h,v 1.1 2012/03/16 21:51:22 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: FGT tracking algorithm first fitting (L)ine tracks and
 * then fitting helix parameters.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHTracking.h,v $
 * Revision 1.1  2012/03/16 21:51:22  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_LH_TRACKING_
#define _ST_FGT_LH_TRACKING_

#include "StFgtTracking.h"
#include <vector>

#include "StRoot/StFgtUtil/StFgtConsts.h"

struct StFgtLHLine {
   std::vector< Int_t> pointIdx;    // the point index values
   UShort_t discBits;              // the disc involved, stored as a bit array--0x1 is disc 0.
   Double_t mx, my, bx, by;         // parameters of the line
   Double_t vertZ;                  // derived quantity--z of the vertex

   StFgtLHLine() : mx(0), my(0), bx(0), by(0), vertZ(-1e11) { /* */ };


   StFgtLHLine( UShort_t bits, Double_t mx_, Double_t my_, Double_t bx_, Double_t by_ ) :
      discBits(bits), mx(mx_), my(my_), bx(bx_), by(by_) {
      vertZ = ( mx || my ? -( mx*bx + my*by )/(mx*mx+my*my) : -1e11 );
   };
};

typedef std::vector< StFgtLHLine > StFgtLHLineVec;

class StFgtLHTracking : public StFgtTracking {
 public:
   // constructors
   StFgtLHTracking( const Char_t* name = "fgtTracking" );

   // deconstructor
   virtual ~StFgtLHTracking();

   // default equals operator and copy constructor OK

   // uses parent's ::Make()
   virtual void Clear( const Option_t *opt = "" );

   void setNumPoints( Int_t val );
   void setFitThres( Double_t val );
   void setInclusionThres( Double_t val );

 protected:
   // array of lines
   StFgtLHLineVec mLineVec;

   // parameters
   Int_t mPoints;            // number of points to use to make the lines
   Double_t mFitThres;        // threshold for sum of squared perp distances between line and points [cm^2]
   Double_t mInclusionThres;  // threshold for sq. perp. dist. between line and point--whether to include point with the track [cm^2]

   // find the tracks
   virtual Int_t findTracks();
   void makePointTuples( StFgtTrPointVec& points, Int_t startDiscIdx, UShort_t discBitArray );
   void makeLine( StFgtTrPointVec& points, UShort_t discBitArray );

   Double_t perpDistSqLineToPoint( const StFgtLHLine& line, const TVector3& pointPos ) const;

 private:   
   ClassDef(StFgtLHTracking,1);

}; 

// inline functions

inline Double_t StFgtLHTracking::perpDistSqLineToPoint( const StFgtLHLine& line, const TVector3& pointPos ) const {
   Double_t dx = ( line.mx*pointPos.Z() + line.bx - pointPos.X() );
   Double_t dy = ( line.my*pointPos.Z() + line.by - pointPos.Y() );

   return dx*dx + dy*dy;
};

inline void StFgtLHTracking::setNumPoints( Int_t val ){ mPoints = val; };
inline void StFgtLHTracking::setFitThres( Double_t val ){ mFitThres = val; };
inline void StFgtLHTracking::setInclusionThres( Double_t val ){ mInclusionThres = val; };

#endif
