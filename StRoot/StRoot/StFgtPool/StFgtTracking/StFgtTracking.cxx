/***************************************************************************
 *
 * $Id: StFgtTracking.cxx,v 1.4 2012/04/11 22:13:30 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtTracking.cxx,v $
 * Revision 1.4  2012/04/11 22:13:30  sgliske
 * update
 *
 * Revision 1.3  2012/04/09 21:08:24  sgliske
 * many bugs fixed--seems to be working
 *
 * Revision 1.2  2012/03/14 22:22:40  sgliske
 * update
 *
 * Revision 1.1  2012/03/07 15:38:04  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtTracking.h"

#include "StRoot/StFgtUtil/StFgtConsts.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"

#include "StRoot/StMuDSTMaker/COMMON/StMuDst.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtStrip.h"
#include "StRoot/StMuDSTMaker/COMMON/StMuFgtCluster.h"

//#define DEBUG
//#define DEBUG2

// initialize structure value
Int_t StFgtTrPoint::lastIdx = -1;

// constructor
StFgtTracking::StFgtTracking( const Char_t* name ) : StMaker( name ) { /* */ };

// deconstructor
StFgtTracking::~StFgtTracking(){ /* */ };

Int_t StFgtTracking::Init(){ 
   StFgtTracking::Clear();
   return kStOk;
};

Int_t StFgtTracking::Make(){
   // for some reason simply calling Clear() does not work
   // need to actually cally StFgtTracking::Clear();
   StFgtTracking::Clear();

   Int_t ierr = computePointsFromStEvent();

   if( ierr )
      ierr = computePointsFromMuDst();

   if( ierr ){
      LOG_ERROR << "No valid input" << endm;
   } else {
      ierr = makePoints();

#ifdef DEBUG
      LOG_INFO << "mPointsTot = " << mPointsTot << endm;
#endif

      if( !ierr && mPointsTot )
         ierr = findTracks();
   };

   return ierr;
};


Int_t StFgtTracking::computePointsFromStEvent(){
   Int_t ierr = kStFatal;

   StEvent* eventPtr = 0;
   StFgtCollection *fgtCollectionPtr = 0;
   StFgtHitCollection *fgtHitColPtr = 0;

   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( eventPtr ) {
      fgtCollectionPtr = eventPtr->fgtCollection();

      if( fgtCollectionPtr ){
         // got this far, so flag that this is the right input.
         ierr = kStOk;

         // loop over discs
         for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
            fgtHitColPtr = fgtCollectionPtr->getHitCollection( disc );

            if( fgtHitColPtr ){
               const StSPtrVecFgtHit& hitVec = fgtHitColPtr->getHitVec();
               StSPtrVecFgtHitConstIterator hitIter;

               Int_t idx = 0;
               for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter, ++idx )
                  addClus( idx, (*hitIter)->getCentralStripGeoId(), (*hitIter)->getPositionR(), (*hitIter)->getPositionPhi() );
            };
         };
      };
   };

   return ierr;
};

Int_t StFgtTracking::computePointsFromMuDst(){
   Int_t ierr = kStFatal;

   // get pointer to input
   const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");

   if( muDst ){
      TClonesArray *fgtClusters = muDst->fgtArray( muFgtClusters );

      if( fgtClusters ){
         // flag this is the correct input
         ierr = kStOk;

         Int_t nClusters = fgtClusters->GetEntriesFast();
#ifdef DEBUG
         LOG_INFO << "Number of clusters " << nClusters << endm;
#endif

         for( Int_t i = 0; i < nClusters; ++i ){
            StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[i] );
            if( clus )
               addClus( i, clus->getCentralStripGeoId(), clus->getR(), clus->getPhi() );
         };
      };
   };

   return ierr;
};

void StFgtTracking::addClus( Int_t clusIdx, Int_t geoId, Float_t rPos, Float_t pPos ){
   Short_t disc,  quad,  strip;
   Char_t  layer;
   StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

#ifdef DEBUG2
   LOG_INFO << "Clus " << clusIdx << " at " << StFgtGeom::encodeGeoName( disc, quad, layer, strip ) << endm;
#endif

   Int_t octIdx = (quad*2 + ( StFgtGeom::getOctant( layer, strip ) == 'S' ))*kFgtNumDiscs + disc;

   if( layer == 'R' )
      mRclusVecPerOctDisc[ octIdx ].push_back( StFgtTrClus( clusIdx, rPos ) );
   else
      mPclusVecPerOctDisc[ octIdx ].push_back( StFgtTrClus( clusIdx, pPos ) );
};

Int_t StFgtTracking::makePoints(){
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      Double_t discZ = StFgtGeom::getDiscZ( disc );

      StFgtTrPointVec &pointVec = mPointVecPerDisc[disc];

      for( Int_t oct = 0; oct < kFgtNumOctantsPerDisc; ++oct ){
         Int_t octIdx = oct*kFgtNumDiscs + disc;

         StFgtTrClusVec &rClusVec = mRclusVecPerOctDisc[octIdx];
         StFgtTrClusVec &pClusVec = mPclusVecPerOctDisc[octIdx];

         if( !rClusVec.empty() && !pClusVec.empty() ){
            StFgtTrClusVec::iterator rClusIter = rClusVec.begin();
            StFgtTrClusVec::iterator pClusIter = pClusVec.begin();

            for( rClusVec.begin(); rClusIter != rClusVec.end(); ++rClusIter )
               for( pClusIter = pClusVec.begin(); pClusIter != pClusVec.end(); ++pClusIter )
                  pointVec.push_back( StFgtTrPoint( *rClusIter, *pClusIter, discZ ) );
         };

#ifdef DEBUG
         if( rClusVec.size() || pClusVec.size() ){
            LOG_INFO << "oct " << disc+1 << (Char_t)(oct/2+'A') << "." << ( oct%2 ? 'S' : 'L' )
                     << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << " points per disc " << pointVec.size() << endm;
         };
#endif
      };

      mPointsTot += pointVec.size();

#ifdef DEBUG
      if( pointVec.size() ){
         LOG_INFO << "Disc " << disc+1 << " contributes " << pointVec.size() << " new points, for a total of " << mPointsTot << " clusters" << endm;
      };
#endif
   };

#ifdef DEBUG2
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      StFgtTrPointVec &pointVec = mPointVecPerDisc[disc];
      for( UInt_t i=0; i<pointVec.size(); ++i ){
         LOG_INFO << "Disc " << disc+1 << " point " << pointVec[i].trIdx << " ( " << pointVec[i].rIdx << ", " << pointVec[i].pIdx << " )"
                  << " ( " << pointVec[i].pos.X() << ", " << pointVec[i].pos.Y() << " )" << endm;
      };
   };
#endif

   return kStOk;
};


void StFgtTracking::Clear( const Option_t *opt ){
   mPointsTot = 0;
   StFgtTrPoint::lastIdx = -1;

   for( Int_t discOct = 0; discOct < kFgtNumDiscs*kFgtNumOctantsPerDisc; ++discOct ){
      mRclusVecPerOctDisc[discOct].clear();
      mPclusVecPerOctDisc[discOct].clear();
   };
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc )
      mPointVecPerDisc[disc].clear();
};

ClassImp( StFgtTracking );
