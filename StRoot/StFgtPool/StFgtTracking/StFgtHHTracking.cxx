/***************************************************************************
 *
 * $Id: StFgtHHTracking.cxx,v 1.3 2012/03/16 21:51:26 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtHHTracking.cxx,v $
 * Revision 1.3  2012/03/16 21:51:26  sgliske
 * update
 *
 * Revision 1.2  2012/03/14 22:22:40  sgliske
 * update
 *
 * Revision 1.1  2012/03/07 15:38:04  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtHHTracking.h"

#define DEBUG

// constructor
StFgtHHTracking::StFgtHHTracking( const Char_t* name ) : StFgtTracking( name ), mBeamPos(0,0), mMinRad( 2500 ), mMaxRad( 55000 ) { /* */ };

// note: (radius in cm) x (0.15 GeV/cm/c) = (pT in GeV/c)
// Default range of 2500-55000 cm = 3.75 to 87.5 GeV

// deconstructor
StFgtHHTracking::~StFgtHHTracking(){ /* */ };

void StFgtHHTracking::Clear( const Option_t *opt ){
   mHelixVec.clear();
};

// find the tracks
Int_t StFgtHHTracking::findTracks(){

   // Make triplets of points in the XY plane.  One point is the XY of
   // the beam pos.  Also require that the beam pos point is not in
   // the middle of the other two.

   StFgtTrPointVec::const_iterator iter1, iter2;

   Float_t beamNorm2 = mBeamPos.Mod2();

   for( Int_t disc1 = 0; disc1 < kFgtNumDiscs-1; ++disc1 ){
      StFgtTrPointVec &pointVec1 = mPointVecPerDisc[ disc1 ];

      for( iter1 = pointVec1.begin(); iter1 != pointVec1.end(); ++iter1 ){
         TVector2 delta1 = (iter1->pos.XYvector() - mBeamPos);

         // parameters for the line equidistance from this point and the beamXY
         Float_t m1 = - delta1.X() / delta1.Y();
         Float_t b1 = ( iter1->pos.Perp2() - beamNorm2 ) / delta1.Y() * 0.5;

         // #ifdef DEBUG
         //          LOG_INFO << "Pts "
         //                   << "( " << mBeamPos.X()   << ", " << mBeamPos.Y()   << ") "
         //                   << "( " << iter1->pos.X() << ", " << iter1->pos.Y() << ") "
         //                   << "make the line y = " << m1 << " x + " << b1 << endm;
         // #endif

         for( Int_t disc2 = disc1+1; disc2 < kFgtNumDiscs; ++disc2 ){
            StFgtTrPointVec &pointVec2 = mPointVecPerDisc[ disc2 ];

            for( iter2 = pointVec2.begin(); iter2 != pointVec2.end(); ++iter2 ){
               TVector2 delta2 = (iter2->pos.XYvector() - mBeamPos);

               // parameters for the line equidistance from this point and the beamXY
               Float_t m2 = - delta2.X() / delta2.Y();
               Float_t b2 = ( iter2->pos.Perp2() - beamNorm2 ) / delta2.Y() * 0.5;

               // #ifdef DEBUG
               //          LOG_INFO << "Pts "
               //                   << "( " << mBeamPos.X()   << ", " << mBeamPos.Y()   << ") "
               //                   << "( " << iter2->pos.X() << ", " << iter2->pos.Y() << ") "
               //                   << "make the line y = " << m2 << " x + " << b2 << endm;
               // #endif

               // x0, y0 of helix at intersection of two lines
               Float_t x0 = - ( b2 - b1 )/( m2 - m1 );
               Float_t y0 = m1*x0 + b1;
               Float_t r = (mBeamPos - TVector2( x0, y0 )).Mod();

               // #ifdef DEBUG
               //                LOG_INFO << "Hits " << iter1->ptIdx << ", " << iter2->ptIdx << ' '
               //                         << "Pts "
               //                         << "( " << mBeamPos.X()   << ", " << mBeamPos.Y()   << ") "
               //                         << "( " << iter1->pos.X() << ", " << iter1->pos.Y() << ") "
               //                         << "( " << iter2->pos.X() << ", " << iter2->pos.Y() << ") "
               //                         << " make helix at " 
               //                         << "( " << x0 << ", " << y0 << ") and r = " << r << endm;
               // #endif
               if( r > mMinRad && r < mMaxRad )
                  mHelixVec.push_back( StFgtHHelix( iter1->ptIdx, iter2->ptIdx, x0, y0, r ) );
            };
         };
      };
   };

#ifdef DEBUG
   if( !mHelixVec.empty() ){
      LOG_INFO << GetEventNumber() << " mPointsTot = " << mPointsTot << endm;
      LOG_INFO << GetEventNumber() << " have " << mHelixVec.size() << " points in the Hough space" << endm;

      StFgtHHelixVec::iterator helixIter;
      for( helixIter = mHelixVec.begin(); helixIter != mHelixVec.end(); ++helixIter ){
         LOG_INFO << helixIter->idx1 << ' ' << helixIter->idx2 << ' ' << atan2( helixIter->y0, helixIter->x0 ) << ' ' << helixIter->r << endm;
      };
   };
#endif

   return kStOk;
};

ClassImp( StFgtHHTracking );

/*
               Float_t phi1    = std::atan2( iter1->pos.Y() - y0, iter1->pos.X() - x0 );
               Float_t phi2    = std::atan2( iter1->pos.Y() - y0, iter1->pos.X() - x0 );

               Float_t zslope = ( StFgtGeom::getDiscZ( disc2 ) - StFgtGeom::getDiscZ( disc1 ) ) / ( phi2 - phi1 );
               Float zb = StFgtGeom::getDiscZ( disc2 ) - zslope*phi2;
*/
