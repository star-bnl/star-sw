/*!
 * \class StEEmcStripClusterFinderMorhac_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See header for description.
 *
*/

#include <list>
#include <TMath.h>
#include <TH1.h>

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

#include "StEEmcStripClusterFinderMorhac.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

//#define DEBUG
//#define DEBUG2
//#define DEBUG3

StEEmcStripClusterFinderMorhac_t::StEEmcStripClusterFinderMorhac_t( Int_t maxNumPoints, Float_t resolution ) : 
   mRemoveBkg( 1 ),
   mDoMarkov( 1 ),
   mMaxNumPoints( maxNumPoints ),
   mMinStripsPerCluster( 5 ),
   mNumDeconIters( 3 ),
   mAverWindow( 3 ),
   mNumSmoothIters( 10 ),
   mWidth( 3 ),
   mThreshold( 5 ),
   mMinPeakEnergy( 0.01 ),
   mMinClusterEnergy( 0.01 )
{
   peakFinderPtr = new TSpectrum( maxNumPoints, resolution );

   // initialize a few parent variables
   mLayer = U_LAYER;
   mSector = 0;
   mIsReady = 1;
};

StEEmcStripClusterFinderMorhac_t::~StEEmcStripClusterFinderMorhac_t(){
   delete peakFinderPtr;
};


/// find some clusters
Int_t StEEmcStripClusterFinderMorhac_t::find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& clusters ){
   clusters.clear();
   Int_t ierr = kStOk;

   if( mThreshold > 100 ){
      LOG_WARN << "Must specificy a threshold below 100.  Resetting to 99.9" << endm;
      mThreshold = 99.9;
   };

   if( mThreshold <= 0 ){
      LOG_WARN << "Must specificy a positive threshold.  Resetting to 1" << endm;
      mThreshold = 1;
   };

   // skip if not enough strips hit
   if( stripArray.nStrips < mMinStripsPerCluster )
      return ierr;

   // clear array
   for( Float_t *p = mStripEnergyArray, *p2 = mSmoothedEnergyArray; p != &mStripEnergyArray[kEEmcNumStrips]; ++p, ++p2 )
      (*p) = (*p2) = 0;

   // copy 
   Int_t stripIdx = 0;
   for( const EEmcElement_t *strip = stripArray.strip; strip != &stripArray.strip[288]; ++strip, ++stripIdx ){
      if( !strip->fail && strip->energy ){
         mStripEnergyArray[    stripIdx ] = strip->energy;
         mSmoothedEnergyArray[ stripIdx ] = strip->energy;

#ifdef DEBUG_INPUT
         cout << "ccc " << iter->index() << ' ' << iter->energy() << endl;
#endif
      };
   };

   // copied from TSpectrum::Search function, so set OK sigma
   Float_t sigma = mWidth;
   if( mWidth < 1 ) {
      sigma = kEEmcNumStrips/mMaxNumPoints;
      if (sigma < 1)
         sigma = 1;
      if (sigma > 8)
         sigma = 8;
   };

   // smooth, if requested
   if( mNumSmoothIters ){
      Float_t *fPtr;
      Double_t *dPtr;

      // copy
      for( fPtr = mSmoothedEnergyArray, dPtr = mStripEnergyArrayTemp; fPtr != &mSmoothedEnergyArray[kEEmcNumStrips]; ++fPtr, ++dPtr )
         (*dPtr) = (*fPtr);

      // smooth
      TH1::SmoothArray( kEEmcNumStrips, mStripEnergyArrayTemp, mNumSmoothIters );

      // copy back
      for( fPtr = mSmoothedEnergyArray, dPtr = mStripEnergyArrayTemp; fPtr != &mSmoothedEnergyArray[kEEmcNumStrips]; ++fPtr, ++dPtr )
         (*fPtr) = (*dPtr);

      // std::copy( mStripEnergyArrayTemp, mStripEnergyArrayTemp+kEEmcNumStrips, mSmoothedEnergyArray );
   };

   Int_t nPeaks = peakFinderPtr->SearchHighRes( mSmoothedEnergyArray, mDeconvoluted, kEEmcNumStrips, sigma,
                                                mThreshold, mRemoveBkg, mNumDeconIters, mDoMarkov, mAverWindow );

#ifdef DEBUG
   LOG_INFO << getEventNum() << " sector " << mSector << " layer " << mLayer << " found " << nPeaks << " peaks." << endm;
#endif

   // assign strips to nearest peak
   // should do this more intelligently in the future

#ifdef DEBUG4
   if( nPeaks ){
      Float_t *peakX = peakFinderPtr->GetPositionX();
      Float_t *peakY = peakFinderPtr->GetPositionY();

      LOG_INFO << "Morhac results" << endm;
      for( Int_t i=0; i<nPeaks; ++i ){
         LOG_INFO << peakX[i] << ' ' << peakY[i] << endm;
      };
   };
#endif

   std::vector< Float_t > peakPos;
   if( nPeaks ){
      Float_t *peakPosRAW = peakFinderPtr->GetPositionX();
      peakPos.reserve( nPeaks );

      // estimate energy of peak strip and its adjacent neighbors
      for( Int_t i=0; i<nPeaks; ++i ){
         Int_t idx = peakPosRAW[i]+0.5;
         Float_t energy = mStripEnergyArray[ idx ];
         if( idx )
            energy += mStripEnergyArray[ idx-1 ];
         if( idx+1 < kEEmcNumStrips )
            energy += mStripEnergyArray[ idx+1 ];

         if( energy > mMinPeakEnergy )
            peakPos.push_back( peakPosRAW[i] );

#ifdef DEBUG3
         LOG_INFO << "Morhac peak " << i << " at " <<  peakPosRAW[i] << ' ' << energy << endm;
#endif
      };
      nPeaks = peakPos.size();
   };

   if( nPeaks ){
      // rename for the sort
      std::vector< Float_t >& sortedPeakPos = peakPos;
      std::sort( sortedPeakPos.begin(), sortedPeakPos.end() );

      std::vector< Float_t > midPoints;
      midPoints.reserve( nPeaks - 1 );

      clusters.reserve( nPeaks );
      std::vector< Int_t > stripsPerClus;
      stripsPerClus.reserve( kEEmcNumStrips );

      for( Int_t i=0; i<nPeaks; ++i ){
         stripsPerClus.clear();

         clusters.push_back( StSimpleCluster_t( ++mLastClusterID ) );
         StSimpleCluster_t& clus = clusters.back();
         clus.setMeanX( sortedPeakPos[i] );

         Float_t pos1 = 0, pos2 = kEEmcNumStrips;
         if( i != 0 )
            pos1 = 0.5*(sortedPeakPos[i-1] + sortedPeakPos[i]) + 0.5;
         if( i != nPeaks-1 )
            pos2 = 0.5*(sortedPeakPos[i+1] + sortedPeakPos[i]) + 0.5;

         Float_t energy = 0;
         for( Int_t i=pos1; i<pos2; ++i )
            if( mStripEnergyArray[i] ){
               stripsPerClus.push_back( i );
               energy += mStripEnergyArray[i];
            };

         Int_t n = stripsPerClus.size();

#ifdef DEBUG2
         LOG_INFO << "Number of strips " << n << " vs " << mMinStripsPerCluster << " energy " << energy << " vs " << mMinClusterEnergy << endm;
#endif

         if( n >= mMinStripsPerCluster && energy > mMinClusterEnergy ){
            TArrayS& memArr = clus.getMemberArray();
            TArrayF& wArr = clus.getWeightArray();

            memArr.Set( n );
            wArr.Set( n );

            Float_t maxStripE = 0;
            Int_t seedIdx = -1;

            for( Int_t i = 0; i < n; ++i ){
               Int_t idx = stripsPerClus.back();
               stripsPerClus.pop_back();

               memArr[i] = idx;
               wArr[i] = 1;

               const Float_t& thisE = mStripEnergyArray[ idx ];
               if( thisE > maxStripE ){
                  maxStripE = thisE;
                  seedIdx = i;
               };
            };

            clus.setEnergy( energy );
            clus.setSeedIdx( seedIdx );

#ifdef DEBUG
            LOG_INFO << "Final results: event " << getEventNum() << " sector/layer " << mSector << '/'
                     << (mLayer ? 'v' : 'u') << ' ' << clus << endm;
#endif

         } else {
            clusters.pop_back();
         };
      };
   };

   return ierr;
};

ClassImp( StEEmcStripClusterFinderMorhac_t );

/*
 * $Id: StEEmcStripClusterFinderMorhac.cxx,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderMorhac.cxx,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
