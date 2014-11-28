/***************************************************************************
 *
 * $Id: StFgtLHTracking.h,v 1.3 2012/04/11 22:13:30 sgliske Exp $
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
 * Revision 1.3  2012/04/11 22:13:30  sgliske
 * update
 *
 * Revision 1.2  2012/04/09 21:08:24  sgliske
 * many bugs fixed--seems to be working
 *
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
   UShort_t discBits;               // the disc involved, stored as a bit array--0x1 is disc 0.
   Double_t mx, my, bx, by;         // parameters of the line
   Double_t vertZ;                  // derived quantity--z of the vertex
   Double_t res;                    // residual

   StFgtLHLine() : mx(0), my(0), bx(0), by(0), vertZ(-1e11), res(0) { /* */ };

   StFgtLHLine( UShort_t bits, Double_t mx_, Double_t my_, Double_t bx_, Double_t by_) :
      discBits(bits), mx(mx_), my(my_), bx(bx_), by(by_) {
      vertZ = ( mx || my ? -( mx*bx + my*by )/(mx*mx+my*my) : -1e11 );
   };
};

struct StFgtLHTrack {
   StFgtLHLine line;
   //StFgtLHHelix helix;

   Int_t pointPerDisc[ kFgtNumDiscs ];
   Double_t resSqPerDisc[ kFgtNumDiscs ];
   Double_t effResSq;

   StFgtLHTrack( const StFgtLHLine& lineIn ) : line( lineIn ) {
      for( Int_t i=0; i<kFgtNumDiscs; ++i ){
         pointPerDisc[i] = -1;
         resSqPerDisc[i] = 1e10;
      };
   };

   StFgtLHTrack() {
      for( Int_t i=0; i<kFgtNumDiscs; ++i ){
         pointPerDisc[i] = -1;
         resSqPerDisc[i] = 1e10;
      };
   };
};

typedef std::vector< StFgtLHLine > StFgtLHLineVec;
typedef std::vector< StFgtLHTrack > StFgtLHTrackVec;

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
   void setNumAgreeThres( Int_t val );
   void setFitThres( Double_t val );
   void setIncludeThres( Double_t val );
   void setUseVertex( Bool_t val );

   const StFgtLHTrackVec& getTrackVec();

 protected:
   // containers
   StFgtLHLineVec mLineVec;      // array of lines
   StFgtLHTrackVec mTrackVec;  // array of tracks

   // parameters
   Int_t mPoints;            // number of points to use to make the lines
   Double_t mFitThres;       // threshold for sqrt of sum of squared perp distances between line and points [cm]
   Double_t mIncludeThres;   // threshold for sq. perp. dist. between line and point--whether to include point with the track [cm^2]
   Int_t mNumAgreeThres;     // how many points the same for different tracks for of the tracks to be removed
   Bool_t mUseVertex;        // whether to use the vertex point for FGT tracking

   // find the tracks
   virtual Int_t findTracks();
   void makePointTuples( StFgtTrPointVec& points, Int_t startDiscIdx, UShort_t discBitArray );
   void makeLine( StFgtTrPointVec& points, UShort_t discBitArray, StFgtLHLine* linePtr = 0 );

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
inline void StFgtLHTracking::setNumAgreeThres( Int_t val ){ mNumAgreeThres = val; };
inline void StFgtLHTracking::setFitThres( Double_t val ){ mFitThres = val; };
inline void StFgtLHTracking::setIncludeThres( Double_t val ){ mIncludeThres = val; };
inline void StFgtLHTracking::setUseVertex( Bool_t val ){ mUseVertex = val; };

inline const StFgtLHTrackVec& StFgtLHTracking::getTrackVec(){ return mTrackVec; };


#endif
