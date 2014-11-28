/*!
 * \class StEEmcStripClusterFinderTSP_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See header for description.
 *
*/

#include <TH1.h>
#include <assert.h>
#include <algorithm>

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

#include "StEEmcStripClusterFinderTSP.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

//#define DEBUG_CHILD
//#define DEBUG
//#define DEBUG2
//#define DEBUG_INPUT

StEEmcStripClusterFinderTSP_t::StEEmcStripClusterFinderTSP_t() :
   mNumSmoothIters( 10 ),
   mMinStripsPerCluster( 5 ),
   mMaxDist( 5 ),
   mSearchMargin( 3 ),
   mSeedAbsThres( 0.0005 ),
   mSeedRelThres( 0.15 ),
   mAbsPeakValleyThres( 0.0015 ),
   mAnomalySupFactor( 0.1 )
{
   // initialize a few parent variables
   mLayer = U_LAYER;
   mSector = 0;
   mIsReady = 1;
};

StEEmcStripClusterFinderTSP_t::~StEEmcStripClusterFinderTSP_t(){
   // nothing to do
};


/// find some clusters
Int_t StEEmcStripClusterFinderTSP_t::find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& clusters ){
   clusters.clear();
   Int_t ierr = kStOk;

   // skip if not enough strips hit
   if( (UInt_t)(stripArray.nStrips) < mMinStripsPerCluster )
      return ierr;

   // clear array
   for( Double_t *p1 = mStripEnergyArray, *p2 = mSmoothedEnergyArrayA, *p3 = mSmoothedEnergyArrayB; p1 != &mStripEnergyArray[kEEmcNumStrips]; ++p1, ++p2, ++p3 )
      (*p1) = (*p2) = (*p3) = 0;

   // copy
   mSmallestIdx = kEEmcNumStrips;
   mLargestIdx = -1;

   Int_t stripIdx = 0;
   for( const EEmcElement_t *strip = stripArray.strip; strip != &stripArray.strip[288]; ++strip, ++stripIdx ){
      if( !strip->fail && strip->energy ){
         if( mSmallestIdx > stripIdx )
            mSmallestIdx = stripIdx;
         if( mLargestIdx < stripIdx )
             mLargestIdx = stripIdx;

         mStripEnergyArray[     stripIdx ] = strip->energy;
         mSmoothedEnergyArrayA[ stripIdx ] = strip->energy;
      };
   };

   if( mSmallestIdx < mLargestIdx ){
      // i.e. there are some strips that do not fail and have non-zero energy

      // smooth step A
      TH1::SmoothArray( kEEmcNumStrips, mSmoothedEnergyArrayA, mNumSmoothIters );

      // only need to look on the part of array where there is nonzero energy
      assert( mSearchMargin > 0 );
      Int_t iStart = mSmallestIdx - mSearchMargin;  // just a little margin to be safe
      Int_t iEnd = mLargestIdx + mSearchMargin;
      if( iStart < 1 )
         iStart = 1;
      if( iEnd > kEEmcNumStrips-1 )
         iEnd = kEEmcNumStrips-1;

      // smooth step B: remove anomalies and copy to energy array B
      for( Double_t *p0 = &mSmoothedEnergyArrayA[ iStart-1 ], *p1 = &mSmoothedEnergyArrayA[ iStart ], *p2 = &mSmoothedEnergyArrayA[ iStart+1 ], *pB = &mSmoothedEnergyArrayB[ iStart ];
           p1 != &mSmoothedEnergyArrayA[iEnd]; ++p0, ++p1, ++p2, ++pB ){

         Double_t sum = ((*p0) + (*p2))*0.5;
         Double_t upper = sum*(1 + mAnomalySupFactor );
         Double_t lower = sum*(1 - mAnomalySupFactor );
         (*pB) = ( (*p1) > upper ? upper : ( (*p1) < lower ? lower : (*p1) ) );
      };

#ifdef DEBUG_INPUT
      for( Int_t idx = mSmallestIdx; idx <= mLargestIdx; ++idx ){
         cout << "bbb " << mSector << ' ' << (mLayer ? 'v' : 'u' ) << ' ' << idx << ' ' << mStripEnergyArray[idx] << ' ' << mSmoothedEnergyArrayB[idx] << endl;
      };
#endif

      // find largest strip energy and set peek threshold
      Double_t maxEnergy = *std::max_element( mSmoothedEnergyArrayB, mSmoothedEnergyArrayB+kEEmcNumStrips );
      Double_t thres = std::max( mSeedAbsThres, mSeedRelThres*maxEnergy );

#ifdef DEBUG2
      LOG_INFO << "Sector " << mSector << '/' << mLayer << " Max energy strip is " << maxEnergy << ", thres is " << thres << endm;
#endif

      // find all the peeks above threshold and save them as seeds


      // vector of seeds
      IntVec_t seedVec;
   
      // peak and valley energies
      std::list< Double_t > peakE, valleyE;
      valleyE.push_back( 0 );
      Double_t smallestValley = 0;

      for( Double_t *p1 = &mSmoothedEnergyArrayB[iStart], *p0 = p1-1, *p2 = p1+1; p1 != &mSmoothedEnergyArrayB[iEnd]; ++p0, ++p1, ++p2 ){
         if( *p1 > thres && *p1 > *p0 && *p1 > *p2 ){
            // register the last valley
            valleyE.push_back( smallestValley );

            if( !seedVec.empty() ){
               // see if the peak before should still be a seed or not
               if( peakE.back() - valleyE.back() < mAbsPeakValleyThres ){
                  peakE.pop_back();
                  valleyE.pop_back();
                  seedVec.pop_back();
               };
            };

            // see if this peak should be a seed
            if( *p1 - valleyE.back() >= mAbsPeakValleyThres ){
               peakE.push_back( *p1 );
               seedVec.push_back( std::distance( mSmoothedEnergyArrayB, p1 ) );
            };

            // set smallest valley to a value larger than any possible
            // valley height
            smallestValley = *p1;
         };

         if( *p1 < smallestValley && ((*p1 < *p0 && *p1 < *p2) || ( *p1 == 0 && *p0 != 0 )) )
            smallestValley = *p1;
      };

#ifdef DEBUG
      LOG_INFO << "Sector " << mSector << '/' << mLayer << " Found " << seedVec.size() << " seeds" << endm;
#endif
#ifdef DEBUG2
      if( !seedVec.empty() ){
         LOG_INFO << "\t\tSeeds are ";
         for( IntVec_t::iterator iter = seedVec.begin(); iter != seedVec.end(); ++iter ){
            LOG_INFO << *iter << ' ';
         };
         LOG_INFO << endm;
      };
#endif

      if( !seedVec.empty() ){

         // for each seed, find the positions where energy deposition starts to increase
         IntVec_t leftStrip, rightStrip;
         IntVec_t::iterator iter;

         for( iter = seedVec.begin(); iter != seedVec.end(); ++iter ){
            // left side

            Double_t *p0, *p1, *p2;

            // left side
            for( p1 = &mSmoothedEnergyArrayB[*iter], p0 = p1-1; p1 != &mSmoothedEnergyArrayB[iStart] && (*p1) >= (*p0) && (*p1); --p1, --p0 );
            leftStrip.push_back( std::distance( mSmoothedEnergyArrayB, p1 ) );

            // right side
            for( p1 = &mSmoothedEnergyArrayB[*iter], p2 = p1+1; p1 != &mSmoothedEnergyArrayB[iEnd] && (*p1) >= (*p2) && (*p1); ++p1, ++p2 );
            rightStrip.push_back( std::distance( mSmoothedEnergyArrayB, p1 ) );

#ifdef DEBUG2
            LOG_INFO << "Cluster with seed " << *iter << " may span " << leftStrip.back() << " to " << rightStrip.back() << endm;
#endif
         };

         IntVec_t stripsPerClus;
         stripsPerClus.reserve( 50 );

         Int_t idx = 0;
         Int_t lastIdx = seedVec.size() - 1;
         for( iter = seedVec.begin(); iter != seedVec.end(); ++iter, ++idx ){
            stripsPerClus.clear();

            // find left position
            UInt_t left = leftStrip[idx];
            if( *iter - left < mMaxDist )
               left = std::max( (Int_t)(*iter - mMaxDist), (Int_t)( idx > 0 ? rightStrip[ idx-1 ] : 0 ) );  // comparison must be done with Ints, not UInts, or will fail

            // find right position
            UInt_t right = rightStrip[idx];
            if( right - *iter < mMaxDist )
               right = std::min( *iter + mMaxDist, (UInt_t)( idx < lastIdx ? leftStrip[ idx+1 ] : kEEmcNumStrips-1 ) );
            // set right to be the one just past the last to include
            ++right;

#ifdef DEBUG2
            LOG_INFO << "Cluster with seed " << *iter << " spans " << left << " to " << right << endm;
#endif

            Int_t seedIdx = *iter;
            Double_t energy = 0;
            Double_t energySq = 0;
            Double_t weightedMean = 0;
            Double_t weightedVar = 0;
            Int_t idx = left;
            for( Double_t *p = &mStripEnergyArray[left]; p != &mStripEnergyArray[right]; ++p, ++idx ){
               if( *p ){
                  //cout << "TSP -- clus " << *iter << " strip " << idx << " E " << *p << ' ' << energy << endl;

                  stripsPerClus.push_back( idx );
                  energy += *p;
                  energySq += (*p)*(*p);
                  weightedMean += idx*(*p);
                  weightedVar += idx*(*p)*(*p);
               };
            };
            if( energy && stripsPerClus.size() >= mMinStripsPerCluster ){
               weightedMean /= energy;
               weightedVar /= energy;
               weightedVar -= weightedMean*weightedMean;
               weightedVar *= 1/( 1 - energySq/energy/energy);

               // add cluster
               clusters.push_back( StSimpleCluster_t( ++mLastClusterID ) );
               StSimpleCluster_t& clus = clusters.back();

               Int_t n = stripsPerClus.size();

               TArrayS& memArr = clus.getMemberArray();
               TArrayF& wArr = clus.getWeightArray();

               memArr.Set( n );
               wArr.Set( n );

               Int_t internalSeedIdx = 0;
               for( Int_t i = 0; i < n; ++i ){
                  Int_t idx = stripsPerClus.back();
                  stripsPerClus.pop_back();
                  memArr[i] = idx;
                  wArr[i] = 1;

                  if( idx == seedIdx )
                     internalSeedIdx = i;
               };

               clus.setMeanX( weightedMean );
               clus.setMeanY( weightedVar );
               clus.setEnergy( mSmoothedEnergyArrayB[ seedIdx ] );
               clus.setSeedIdx( internalSeedIdx );
            }; 
         }; // iterating over seeds
      }; // have seeds
   }; // non-zero number of valid strips
#ifdef DEBUG
   for( UInt_t i=0; i<clusters.size(); ++i ){
      LOG_INFO << "Final results: event " << getEventNum() << " sector/layer " << mSector << '/'
               << (mLayer ? 'v' : 'u') << ' ' << clusters[i] << endm;
   };
#endif
#ifdef DEBUG_CHILD
   for( UInt_t i=0; i<clusters.size(); ++i ){
      LOG_INFO << "TSP final results: event " << getEventNum() << " sector/layer " << mSector << '/'
               << (mLayer ? 'v' : 'u') << ' ' << clusters[i] << " sigma " << clusters[i].getMeanY() << endm;
   };
#endif

   return ierr;
};

ClassImp( StEEmcStripClusterFinderTSP_t );

/*
 * $Id: StEEmcStripClusterFinderTSP.cxx,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderTSP.cxx,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
