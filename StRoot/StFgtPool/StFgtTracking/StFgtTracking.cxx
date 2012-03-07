/***************************************************************************
 *
 * $Id: StFgtTracking.cxx,v 1.1 2012/03/07 15:38:04 sgliske Exp $
 * Author: S. Gliske, March 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StFgtTracking.cxx,v $
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

#define DEBUG

// constructor
StFgtTracking::StFgtTracking( const Char_t* name ) : StMaker( name ) { /* */ };

// deconstructor
StFgtTracking::~StFgtTracking(){ /* */ };

Int_t StFgtTracking::Init(){ 
   Clear();
   return kStOk;
};

Int_t StFgtTracking::Make(){
   Clear();

#ifdef DEBUG
   {
      Int_t disc = 0;
      StFgtTrClusVec &rClusVec = mRclusVecPerDisc[disc];
      StFgtTrClusVec &pClusVec = mPclusVecPerDisc[disc];

      LOG_INFO << "A disc " << disc+1 << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << endm;
   };
#endif

   Int_t ierr = computePointsFromStEvent();

#ifdef DEBUG
   {
      Int_t disc = 0;
      StFgtTrClusVec &rClusVec = mRclusVecPerDisc[disc];
      StFgtTrClusVec &pClusVec = mPclusVecPerDisc[disc];

      LOG_INFO << "B disc " << disc+1 << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << endm;
   };
#endif


   if( ierr )
      ierr = computePointsFromMuDst();

#ifdef DEBUG
   {
      Int_t disc = 0;
      StFgtTrClusVec &rClusVec = mRclusVecPerDisc[disc];
      StFgtTrClusVec &pClusVec = mPclusVecPerDisc[disc];

      LOG_INFO << "C disc " << disc+1 << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << endm;
   };
#endif

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
               for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter, ++idx ){
#ifdef DEBUG
                  //LOG_INFO << "Adding '" << (*hitIter)->getLayer() << "' cluster to disc " << disc+1 << endm;
#endif

                  if( (*hitIter)->getLayer() == 'R' )
                     mRclusVecPerDisc[ disc ].push_back( StFgtTrClus( idx, (*hitIter)->getPositionR() ) );
                  else
                     mPclusVecPerDisc[ disc ].push_back( StFgtTrClus( idx, (*hitIter)->getPositionPhi() ) );
               };
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

#ifdef DEBUG
   Int_t disc = 0;
   StFgtTrClusVec &rClusVec = mRclusVecPerDisc[disc];
   StFgtTrClusVec &pClusVec = mPclusVecPerDisc[disc];

   LOG_INFO << "disc " << disc+1 << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << endm;
#endif


   if( muDst ){
      TClonesArray *fgtClusters = muDst->fgtArray( muFgtClusters );

      if( fgtClusters ){
         // flag this is the correct input
         ierr = kStOk;

         Int_t nClusters = fgtClusters->GetEntriesFast();

         for( Int_t i = 0; i < nClusters; ++i ){
            StMuFgtCluster* clus = static_cast< StMuFgtCluster* >( (*fgtClusters)[i] );
            if( clus ){
               // determine which octant this is in
               Int_t geoId = clus->getCentralStripGeoId();

               Short_t disc,  quad,  strip;
               Char_t  layer;
               StFgtGeom::decodeGeoId( geoId, disc, quad, layer, strip );

#ifdef DEBUG
               //LOG_INFO << "Adding '" << layer << "' cluster to disc " << disc+1 << endm;
#endif

               if( layer == 'R' )
                  mRclusVecPerDisc[ disc ].push_back( StFgtTrClus( i, clus->getR() ) );
               else
                  mPclusVecPerDisc[ disc ].push_back( StFgtTrClus( i, clus->getPhi() ) );
            };
         };
      };
   };

   return ierr;
};

Int_t StFgtTracking::makePoints(){
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      Double_t discZ = StFgtGeom::getDiscZ( disc );

      StFgtTrClusVec &rClusVec = mRclusVecPerDisc[disc];
      StFgtTrClusVec &pClusVec = mPclusVecPerDisc[disc];
      StFgtTrPointVec &pointVec = mPointVecPerDisc[disc];

      if( !rClusVec.empty() && !pClusVec.empty() ){
         StFgtTrClusVec::iterator rClusIter = rClusVec.begin();
         StFgtTrClusVec::iterator pClusIter = pClusVec.begin();

         for( ; rClusIter != rClusVec.end(); ++rClusIter )
            for( ; pClusIter != pClusVec.end(); ++pClusIter )
               pointVec.push_back( StFgtTrPoint( *rClusIter, *pClusIter, discZ ) );
      };
#ifdef DEBUG
      LOG_INFO << "disc " << disc+1 << " r/phi clusters? " << rClusVec.size() << ' ' << pClusVec.size() << endm;
#endif

      mPointsTot += pointVec.size();
   };

   return kStOk;
};


void StFgtTracking::Clear( const Option_t *opt ){
   mPointsTot = 0;
   for( Int_t disc = 0; disc < kFgtNumDiscs; ++disc ){
      mRclusVecPerDisc[disc].clear();
      mPclusVecPerDisc[disc].clear();
      mPointVecPerDisc[disc].clear();
   };
};

ClassImp( StFgtTracking );
