/***************************************************************************
 *
 * $Id: StFgtLHHTracking.cxx,v 1.2 2012/03/14 22:22:40 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHHTracking.cxx,v $
 * Revision 1.2  2012/03/14 22:22:40  sgliske
 * update
 *
 * Revision 1.1  2012/03/14 21:04:17  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#include "StFgtLHHTracking.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#define DEBUG

// constructor
StFgtLHHTracking::StFgtLHHTracking( const Char_t* name ) : StFgtTracking( name ){ /* */ };

// deconstructor
StFgtLHHTracking::~StFgtLHHTracking(){ /* */ };

void StFgtLHHTracking::Clear( const Option_t *opt ){
   for( Int_t discIdx = 0; discIdx < kFgtNumDiscPairs; ++discIdx )
      mLineVec[discIdx].clear();
};

// find the tracks
Int_t StFgtLHHTracking::findTracks(){

   // Make lines out of pairs of points.  Points must come from different discs.

   StFgtTrPointVec::const_iterator iter1, iter2;

   Int_t nLines = 0;
   for( Int_t disc1 = 0, discIdx = 0; disc1 < kFgtNumDiscs-1; ++disc1 ){
      StFgtTrPointVec &pointVec1 = mPointVecPerDisc[ disc1 ];

      for( Int_t disc2 = disc1+1; disc2 < kFgtNumDiscs; ++disc2, ++discIdx ){
         StFgtTrPointVec &pointVec2 = mPointVecPerDisc[ disc2 ];

         Int_t idxIter1 = 0;
         for( iter1 = pointVec1.begin(); iter1 != pointVec1.end(); ++iter1, ++idxIter1 ){
            Int_t idxIter2 = 0;
            for( iter2 = pointVec2.begin(); iter2 != pointVec2.end(); ++iter2, ++idxIter2 ){

               Float_t deltaZ = (iter1->pos.Z() - iter2->pos.Z());
               assert( deltaZ ); // already required different discs, so this must be true

               Float_t mx = (iter1->pos.X() - iter2->pos.X()) / deltaZ;
               Float_t my = (iter1->pos.Y() - iter2->pos.Y()) / deltaZ;
               Float_t bx = mx*iter1->pos.Z() - iter1->pos.X();
               Float_t by = my*iter1->pos.Z() - iter1->pos.Y();

               mLineVec[discIdx].push_back( StFgtLHHLine( idxIter1, idxIter2, disc1, disc2, mx, my, bx, by ) );
               ++nLines;
            };
         };
      };
   };

#ifdef DEBUG
   if( nLines ){
      LOG_INFO << GetEventNumber() << " mPointsTot = " << mPointsTot << endm;
      LOG_INFO << GetEventNumber() << " have " << nLines << " points in the Hough space" << endm;

      StFgtLHHLineVec::iterator lineIter;

      for( Int_t disc1 = 0, discIdx = 0; disc1 < kFgtNumDiscs-1; ++disc1 ){
         for( Int_t disc2 = disc1+1; disc2 < kFgtNumDiscs; ++disc2, ++discIdx ){
            for( lineIter = mLineVec[discIdx].begin(); lineIter != mLineVec[discIdx].end(); ++lineIter ){
               LOG_INFO << lineIter->pointIdx1 << ' ' << lineIter->pointIdx2 << ' '
                        << lineIter->discIdx1 << ' ' << lineIter->discIdx2 << ' '
                        << lineIter->mx << ' ' << lineIter->bx << ' '
                        << lineIter->my << ' ' << lineIter->by << ' '
                        << lineIter->vertZ << endm;
            };
         };
      };
   };
#endif

   // make pairs of lines, and see if any are "close" to each other
   if( nLines ){
      StFgtLHHLineVec::iterator lineIter1;
      StFgtLHHLineVec::iterator lineIter2;

      Float_t minDist = 1e10;
      for( Int_t discIdx1 = 0; discIdx1 < kFgtNumDiscPairs; ++discIdx1 ){
         for( Int_t discIdx2 = discIdx1+1; discIdx2 < kFgtNumDiscPairs; ++discIdx2 ){
            for( lineIter1 = mLineVec[discIdx1].begin(); lineIter1 != mLineVec[discIdx1].end(); ++lineIter1 ){
               for( lineIter2 = mLineVec[discIdx2].begin(); lineIter2 != mLineVec[discIdx2].end(); ++lineIter2 ){
                  Float_t dist = distanceSqBetween( *lineIter1, *lineIter2 );
                  if( dist < minDist )
                     minDist = dist;

                  LOG_INFO << "dist between ( " << lineIter1->pointIdx1 << ' ' << lineIter1->pointIdx2 << ' '
                           << lineIter1->discIdx1 << ' ' << lineIter1->discIdx2 << " ) "
                           << "( " << lineIter2->pointIdx1 << ' ' << lineIter2->pointIdx2 << ' '
                           << lineIter2->discIdx1 << ' ' << lineIter2->discIdx2 << " ) "
                           << dist << ' ' << minDist << endm;
               };
            };
         };
      };

      LOG_INFO << GetEventNumber() << " min dist is " << minDist << endm;
   };

   return kStOk;
};

// the sum of the distances squared in the xy planes at the z positions of the center of STAR (z=0) and the last disc
Float_t StFgtLHHTracking::distanceSqBetween( const StFgtLHHLine& line1, const StFgtLHHLine& line2 ) const {
//   Float_t d2 = 0;
//    d2 += distanceSqLineToPoint( line1, mPointVecPerDisc[ line2.discIdx1 ][ line2.pointIdx1 ].pos );
//    d2 += distanceSqLineToPoint( line1, mPointVecPerDisc[ line2.discIdx2 ][ line2.pointIdx2 ].pos );
//    d2 += distanceSqLineToPoint( line2, mPointVecPerDisc[ line1.discIdx1 ][ line1.pointIdx1 ].pos );
//    d2 += distanceSqLineToPoint( line2, mPointVecPerDisc[ line1.discIdx2 ][ line1.pointIdx2 ].pos );
   Float_t dx = line1.bx - line1.bx; 
   Float_t dy = line1.by - line1.by; 
   Float_t d2 = dx*dx + dy*dy;

   static Float_t z = StFgtGeom::getDiscZ( kFgtNumDiscs-1 );
   dx = line1.mx*z + line1.bx - line2.mx*z - line1.bx; 
   dy = line1.my*z + line1.by - line2.my*z - line1.by; 
   d2 += dx*dx + dy*dy;

   return d2;
};

Float_t StFgtLHHTracking::distanceSqLineToPoint( const StFgtLHHLine& line, const TVector3& pointPos ) const {
   Float_t numerator = line.mx*(pointPos.X()-line.bx) + line.my*(pointPos.Y()-line.by) + pointPos.Z();
   Float_t denominator = line.mx*line.mx + line.my*line.my + 1;
   Float_t z = numerator / denominator;
   TVector3 linePos( line.mx*z + line.bx, line.my*z + line.by, z );
   return (linePos - pointPos).Mag2();
};

ClassImp( StFgtLHHTracking );
