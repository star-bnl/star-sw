/***************************************************************************
 *
 * $Id: StFgtLHHTracking.cxx,v 1.1 2012/03/14 21:04:17 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtLHHTracking.cxx,v $
 * Revision 1.1  2012/03/14 21:04:17  sgliske
 * creation
 *
 *
 *
 **************************************************************************/

#include "StFgtLHHTracking.h"

#define DEBUG

// constructor
StFgtLHHTracking::StFgtLHHTracking( const Char_t* name ) : StFgtTracking( name ){ /* */ };

// deconstructor
StFgtLHHTracking::~StFgtLHHTracking(){ /* */ };

void StFgtLHHTracking::Clear( const Option_t *opt ){
   mLineVec.clear();
};

// find the tracks
Int_t StFgtLHHTracking::findTracks(){

   // Make lines out of pairs of points.  Points must come from different discs.

   StFgtTrPointVec::const_iterator iter1, iter2;

   for( Int_t disc1 = 0; disc1 < kFgtNumDiscs-1; ++disc1 ){
      StFgtTrPointVec &pointVec1 = mPointVecPerDisc[ disc1 ];

      for( iter1 = pointVec1.begin(); iter1 != pointVec1.end(); ++iter1 ){

         for( Int_t disc2 = disc1+1; disc2 < kFgtNumDiscs; ++disc2 ){
            StFgtTrPointVec &pointVec2 = mPointVecPerDisc[ disc2 ];

            for( iter2 = pointVec2.begin(); iter2 != pointVec2.end(); ++iter2 ){

               Float_t deltaZ = (iter1->pos.Z() - iter2->pos.Z());
               assert( deltaZ ); // already required different discs, so this must be true

               Float_t mx = (iter1->pos.X() - iter2->pos.X()) / deltaZ;
               Float_t my = (iter1->pos.Y() - iter2->pos.Y()) / deltaZ;
               Float_t bx = mx*iter1->pos.Z() - iter1->pos.X();
               Float_t by = my*iter1->pos.Z() - iter1->pos.Y();

               mLineVec.push_back( StFgtLHHelix( iter1->trIdx, iter2->trIdx, disc1, disc2, mx, my, bx, by ) );
            };
         };
      };
   };

#ifdef DEBUG
   if( !mLineVec.empty() ){
      LOG_INFO << GetEventNumber() << " mPointsTot = " << mPointsTot << endm;
      LOG_INFO << GetEventNumber() << " have " << mLineVec.size() << " points in the Hough space" << endm;

      StFgtLHLineVec::iterator lineIter;
      for( lineIter = mLineVec.begin(); lineIter != mLineVec.end(); ++lineIter ){
         LOG_INFO << lineIter->pointIdx1 << ' ' << lineIter->pointIdx2 << ' '
                  << lineIter->discIdx1 << ' ' << lineIter->discIdx2 << ' '
                  << lineIter->vertZ << endm;
      };
   };
#endif

   return kStOk;
};

ClassImp( StFgtLHHTracking );
