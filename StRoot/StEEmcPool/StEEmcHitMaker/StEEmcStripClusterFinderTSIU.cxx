/*!
 * \class StEEmcStripClusterFinderTSIU_t
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

#include "StEEmcStripClusterFinderTSIU.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

StEEmcStripClusterFinderTSIU_t::StEEmcStripClusterFinderTSIU_t() :
   mNumSmoothIters( 5 ),
   mNumStripsPerSide( 2 ),
   mMinStripsPerCluster( 4 ),
   mSeedAbsThres( 0.001 ),
   mSeedRelThres( 0.05 ),
   mMinEnergyPerCluster( 0.010 )
{
   // initialize a few parent variables
   mLayer = U_LAYER;
   mSector = 0;
   mIsReady = 1;
};

StEEmcStripClusterFinderTSIU_t::~StEEmcStripClusterFinderTSIU_t(){
   // nothing to do
};


/// find some clusters
Int_t StEEmcStripClusterFinderTSIU_t::find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& clusters ){
   clusters.clear();
   Int_t ierr = kStOk;

   assert( mNumStripsPerSide > 0 );                   // make sure enough side strips
   assert( mNumStripsPerSide < 0.3*kEEmcNumStrips );  // make sure not too many side strips
   assert( mMinEnergyPerCluster > 0 );             // make sure a positive (non-zero) threshold

   // skip if not enough strips hit
   if( (UInt_t)(stripArray.nStrips) < mMinStripsPerCluster )
      return ierr;

   // clear array
   for( Double_t *p1 = mStripEnergyArray, *p2 = mSmoothedEnergyArray; p1 != &mStripEnergyArray[kEEmcNumStrips]; ++p1, ++p2 )
      (*p1) = (*p2) = 0;

   // copy
   UInt_t smallestIdx = kEEmcNumStrips;
   UInt_t largestIdx = 0;

   Bool_t existsStripOverThres = 0;

   UInt_t stripIdx = 0;
   for( const EEmcElement_t *strip = stripArray.strip; strip != &stripArray.strip[288]; ++strip, ++stripIdx ){
      if( !strip->fail && strip->energy ){
         if( smallestIdx > stripIdx )
            smallestIdx = stripIdx;
         if( largestIdx < stripIdx )
             largestIdx = stripIdx;

         mStripEnergyArray[    stripIdx ] = strip->energy;
         mSmoothedEnergyArray[ stripIdx ] = strip->energy;

         if( strip->energy > mSeedAbsThres )
            existsStripOverThres = 1;
      };
   };

   if( smallestIdx < largestIdx && existsStripOverThres ){
      // i.e. there are some strips that do not fail and have non-zero energy

      // include one extra (empty) strip on either side
      if( smallestIdx > 0 )
         --smallestIdx;
      if( largestIdx < kEEmcNumStrips-1 )
         ++largestIdx;

      // make sure not too close to the edge, as then cannot check cluster shape
      if( smallestIdx < mNumStripsPerSide )
         smallestIdx = mNumStripsPerSide;
      if( largestIdx > kEEmcNumStrips-mNumStripsPerSide-1 )
         largestIdx = kEEmcNumStrips-mNumStripsPerSide-1;

      // smooth
      TH1::SmoothArray( kEEmcNumStrips, mSmoothedEnergyArray, mNumSmoothIters );

      // find largest strip energy and set peek threshold to max of abs and rel thresholds.
      Double_t thres = mSeedAbsThres;
      if( mSeedRelThres ){
         Double_t maxEnergy = *std::max_element( mSmoothedEnergyArray, mSmoothedEnergyArray+kEEmcNumStrips );
         Double_t newThres = mSeedRelThres*maxEnergy;
         if( newThres > thres )
            thres = newThres;
      };

      // find all the peeks above threshold and make clusters

      UInt_t seedPos = smallestIdx;
      for( Double_t *ePtr = &mSmoothedEnergyArray[smallestIdx]; ePtr != &mSmoothedEnergyArray[largestIdx]; ++ePtr, ++seedPos ){
         if( *ePtr > thres && *(ePtr-1) < *ePtr && *(ePtr+1) < *ePtr ){
            // passes energy threshold and part of shape cut

            // declarations
            Double_t clusE = *ePtr;
            Double_t clusPos = seedPos*clusE;
            Bool_t passes = 1;
            UInt_t numStrips = 1;

            // check shape, number of actual strips with energy, and compute energy and position

            // left side
            Int_t idx = seedPos - mNumStripsPerSide;
            for( Double_t *ePtr2 = ePtr-mNumStripsPerSide; ePtr2 < ePtr && passes; ++ePtr2, ++idx ){
               passes = ( *ePtr2 < *(ePtr2+1) );
               clusE += *ePtr2;
               clusPos += *ePtr2 * idx;

               if( mStripEnergyArray[idx] )
                  ++numStrips;
            };

            // right side
            idx = seedPos + 1;
            for( Double_t *ePtr2 = ePtr+1; ePtr2 < ePtr+mNumStripsPerSide+1 && passes; ++ePtr2, ++idx ){
               passes = ( *ePtr2 < *(ePtr2-1) );
               clusE += *ePtr2;
               clusPos += *ePtr2 * idx;

               if( mStripEnergyArray[idx] )
                  ++numStrips;
            };

            if( passes && clusE > mMinEnergyPerCluster && numStrips >= mMinStripsPerCluster ){
               // cluster passes, add a new cluster and set some values

               clusters.push_back( StSimpleCluster_t( ++mLastClusterID ) );
               StSimpleCluster_t& clus = clusters.back();

               // set some values
               clus.setMeanX( clusPos/clusE );
               clus.setMeanY( 0 );
               clus.setEnergy( clusE );
               clus.setSeedIdx( 0 );

               // get the array of used (member) strips
               TArrayS& memArr = clus.getMemberArray();
               TArrayF& wArr = clus.getWeightArray();

               memArr.Set( numStrips );
               wArr.Set( numStrips );

               // set the seed first
               memArr[0] = seedPos;
               wArr[0] = 1;

               //cout << seedPos << ' ' << mNumStripsPerSide << ' ' << numStrips << endl;

               // next set the other strips
               for( UInt_t idx = seedPos - mNumStripsPerSide, i=1; idx <= seedPos + mNumStripsPerSide; ++idx ){
                  if( mStripEnergyArray[idx] && idx != seedPos ){
                     //cout << idx << ' ' << mStripEnergyArray[idx] << ' ' << i << endl;

                     memArr[i] = idx;
                     wArr[i++] = 1;
                  };
               };

            }; // adding the cluster
         }; // potential seeds
      }; // looping over the energy array
   }; // non-zero number of valid strips

   return ierr;
};

ClassImp( StEEmcStripClusterFinderTSIU_t );

/*
 * $Id: StEEmcStripClusterFinderTSIU.cxx,v 1.3 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderTSIU.cxx,v $
 * Revision 1.3  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.2  2013/01/02 20:30:31  sgliske
 * numStrips >= mMinStripsPerCluster rather than
 * numStrips > mMinStripsPerCluster
 *
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
