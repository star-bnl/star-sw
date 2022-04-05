/*!
 * \class StripClusterFinderIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * See description in the header file StripClusterFinderIU.h
*/

#include <algorithm>

#include <Rtypes.h>
#include <TMath.h>
#include <TArrayS.h>
#include <TArrayF.h>

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"

#include "StEEmcStripClusterFinderIU.h"
#include "StSimpleCluster.h"
#include "StFinderAlg.h"

/// constructor
StEEmcStripClusterFinderIU_t::StEEmcStripClusterFinderIU_t() : StEEmcStripClusterFinder_t(),
                                                               mIgnoreCorners( true ),
                                                               mUseNaiveFloorShape( true ),
                                                               mApplyClusterSplitting( 1 ),
                                                               mMaxExtent( 3 ),
                                                               mMinStripsPerCluster( 3 ),
                                                               mMinSeedDistance( 3 ),
                                                               mSeedFloor( 2.0 ),
                                                               mNeedsToBeCleared( 1 )
{
   mSeedEnergyThres[ U_LAYER ] = 0.003;
   mSeedEnergyThres[ V_LAYER ] = 0.003;
   mLayer = U_LAYER;
   mSector = 0;
   mIsReady = 1;
};

void StEEmcStripClusterFinderIU_t::setMaxExtent( Int_t maxExtent ){
   mMaxExtent = maxExtent;

   if( mMaxExtent < 3 ){
      LOG_WARN << "StEEmcStripClusterFinderIU_t::setMaxExtent" << endm;
      LOG_WARN << "\t maxExtent must be >= 2, not " << maxExtent << endm;
      LOG_WARN << "\t defaulting to minimum: maxExtent = 2" << endm;
      maxExtent = 2;
   };
};

void StEEmcStripClusterFinderIU_t::setMinStripsPerCluster( Int_t min ){
   mMinStripsPerCluster = min;

   if( mMinStripsPerCluster < 2 ){
      LOG_WARN << "StEEmcStripClusterFinderIU_t::setMinStripsPerCluster" << endm;
      LOG_WARN << "\t MinStripsPerCluster must be >= 1, not " << mMinStripsPerCluster << endm;
      LOG_WARN << "\t setting to default value of 3" << endm;
      mMinStripsPerCluster = 3;
   };
};

void StEEmcStripClusterFinderIU_t::clear(){
   // don't waste time if clear isn't needed
   if( mNeedsToBeCleared ){
      // reset array for keeping track of strips not in clusters
      // negative flags that ID is invalid---but make sure IDs are still unique
      for( Int_t i = 0; i < kEEmcNumStrips; ++i ){
         mClosestClusterIDForEachStrip[i] = -i-1;
         mFirstClusterIDForEachStrip[i] = -i-1;
      };

      // clear seed floor array
      for( Float_t *f_ptr = mSeedFloorArray; f_ptr != mSeedFloorArray + kEEmcNumStrips; ++f_ptr )
         (*f_ptr) = 0;

      mNeedsToBeCleared = 0;
   };
};

/// find some clusters
Int_t StEEmcStripClusterFinderIU_t::find( const ESmdLayer_t& stripArray, StSimpleClusterVec_t& clusterVec ){

   if( mNeedsToBeCleared ){
      // forgot to clear it, so do it now
      clear();
   };

   //LOG_INFO << "********** StEEmcStripClusterFinderIU_t::find(...) **********" << endm;

   // just return if no hit strips, or if not enough strips to form a cluster
   if( stripArray.nStrips < mMinStripsPerCluster )
      return kStOK;

   // ensure cleared
   clear();

   double floorParam1 = 0;
   double floorParam2 = 0;

   if( mSector == 0 || mSector == 3 || mSector == 6 || mSector == 9 ){
      if( mLayer == U_LAYER ){
         floorParam1=0.4;
         floorParam2=0.2;
      } else {
         floorParam1=0.2;
         floorParam2=0.1;
      };
   } else {
      if( mLayer == U_LAYER ){
         floorParam1=0.2;
         floorParam2=0.1;
      } else {
         floorParam1=0.4;
         floorParam2=0.2;
      };
   };

   // Note: for MC data, Weihong used fixed values for all layers and sectors
   //    floorParam1=0.2;
   //    floorParam2=0.1;

   // flag that will need to clear again after this function call
   mNeedsToBeCleared = 1;

   // container for seeds
   // important that this contains pointers, not copies
   vector< const EEmcElement_t* > seedStrips;

   // set all hit strip points

   // iterate over strips to find seeds

   // Note: only iterates between 3 and 283, as per Weihong's code.   He stated this was removing the "corners" but it only removes 2 of the 4 corners.
   // It should also depend on the sector and u/v layer.
   Int_t index = 3;
   for( const EEmcElement_t *strip = &stripArray.strip[index]; strip != &stripArray.strip[283]; ++strip, ++index ){

      // determine energy threshold as the sum of the floor plus the
      // overall threshold
      Float_t threshold = mSeedEnergyThres[ mLayer ];
      if( mSeedFloor )
         threshold += mSeedFloor*mSeedFloorArray[ index ];
      //LOG_INFO << "------> threshold = " << threshold << endm;

      // check cuts
      if( !strip->fail && strip->energy > threshold ){
         // strip passes as a seed, so add to the list
         seedStrips.push_back( strip );

         //LOG_INFO << "-----> seed strip index = " << hitStripIter->index() << ", energy = " << hitStripIter->energy() << " <-----" << endm;

         // also increase floor for other strips
         if( mUseNaiveFloorShape ){
            // the not "LOOSE_CUTS" version

            for ( Int_t i = 0; i < 288; ++i ){
               Int_t delta = (i > index ? i-index : index-i);

               /// Within +/- 2 strips, find no other seeds
               if( delta < 3 )
                  mSeedFloorArray[i] += strip->energy;

               /// Within +/- 4 strips, Floor is 20 or 40% of seed
               if( delta >= 3 && delta < 5 )
                  mSeedFloorArray[i] += floorParam1*strip->energy;

               /// Within +/- 10 strips, Floor is 10 or 20% of seed
               if( delta >= 5 && delta < 11 )
                  mSeedFloorArray[i] += floorParam2*strip->energy;

               /// Within +/- 20 strips, floor is 5% of seed
               if( delta >= 11 && delta < 21 )
                  mSeedFloorArray[i] += 0.05*strip->energy;
            };

         } else {
            // the "LOOSE_CUTS" version

            Int_t imin = ( index -6  < 0              ? 0              : index - 6 );
            Int_t imax = ( index + 6 > kEEmcNumStrips ? kEEmcNumStrips : index + 6 );

            for( Int_t i = imin; i <= imax; ++i ){
               // "Within +/- 6 strips, Floor is (at least) 5% of seed"
               // Note: if threshold already above, then don't add to it
               if ( TMath::Abs(index-i) < 7 ) 
                  if ( 0.05*strip->energy > mSeedFloorArray[i] )
                     mSeedFloorArray[i] += 0.05*strip->energy;
            };

         };
         // end if statement for whether to keep
      };
      // end of iteration over strips
   };

   // note: since mMaxExtent is fixed, then any one given strip can be
   // in at most two clusters.  Also, a cluster cannot overlap with
   // more than one other cluster on each side.

   //LOG_INFO << "-----> Iterating over seeds (" << seedStrips.size() << ") to find clusters <-----" << endm;

   // sort seeds in decending order by energy
   std::sort( seedStrips.begin(), seedStrips.end(), energyGreaterThan );

   // iterate over seeds to define clusters.
   vector< const EEmcElement_t* >::iterator seedStripIter = seedStrips.begin();
   for( ; seedStripIter != seedStrips.end(); ++seedStripIter ){
      Int_t index = static_cast< Int_t >( (*seedStripIter) - stripArray.strip );

      Bool_t ok_seed = 1;

      //LOG_INFO << "VVV index " << index << " already in a cluster.  Closest is " <<  mClosestClusterIDForEachStrip[ index ] << endm;

      // if seed already in a different strip, make sure it is at least
      // so many strips apart.
      if( mClosestClusterIDForEachStrip[ index ] > -1 ){

         // find the cluster to get its seed
         Bool_t found = 0;
         ok_seed = 0;
         Int_t i = -1;

         while( i < (Int_t)clusterVec.size() - 1 && !found ){
            found = (clusterVec[++i].getID() == mClosestClusterIDForEachStrip[ index ] );
            //LOG_INFO << "vvv a) " << clusterVec[i].getID() << ' ' << clusterVec[i].getSeedMember() << endm;
         };

         // sanity check
         if( !found ){
            LOG_WARN << "a) Error finding cluster with ID " << mClosestClusterIDForEachStrip[ index ] << endm;
         } else {
            Int_t delta = TMath::Abs( index - clusterVec[i].getSeedMember() );
            ok_seed = ( delta > mMinSeedDistance );
            //LOG_INFO << "Seed " << index << " already in cluster " << i << ", delta = |" << index << " - " << clusterVec[i].getSeedMember() << "| = " << delta << endm;
            //LOG_INFO << clusterVec[i] << endm;

            if( clusterVec[i].getSeedIdx() > 10 ){
               LOG_FATAL << "Something wierd is going on" << endm;
               return kStFatal;
            };
         };
      };

      // passes critereon
      if( ok_seed ){

         //LOG_INFO << "-----> clus w/ seed index = " << (*seedStripIter)->index() << ", energy = " << (*seedStripIter)->energy() << " <-----" << endm;

         // range of strips to include
         Int_t max = index + mMaxExtent;
         Int_t min = index - mMaxExtent;

         // check range
         if( min < 0 )
            min = 0;
         if( max > kEEmcNumStrips )
            max = kEEmcNumStrips;

         // number of strips in the cluster
         Int_t nStripsInClus = 0;

         // count how many strips actually in the cluster
         for( Int_t i = min; i <= max; ++i )
            if( stripArray.strip[i].energy && !stripArray.strip[i].fail )
               ++nStripsInClus;

         // check if enough
         if( nStripsInClus >= mMinStripsPerCluster ){

            // make a new cluster
            clusterVec.push_back( StSimpleCluster_t( ++mLastClusterID ) );

            //LOG_INFO << "vvv new " << clusterVec.back().getID() << ' ' << clusterVec.back().getSeedMember() << endm;

            // get reference to the cluster.  Note: important to use
            // reference not copy constuctor in all the following places
            // references occur
            StSimpleCluster_t &cluster = clusterVec.back();

            // nominal value for the moment
            cluster.setEnergy( -999 );

            // get reference to the arrays and set the size
            TArrayS &member = cluster.getMemberArray();
            TArrayF &weight = cluster.getWeightArray();
            member.Set( nStripsInClus );
            weight.Set( nStripsInClus );

            // keep track of position in member
            Int_t member_idx = -1;

            // add the seed as the first strip (needed for splitting)
            member[ ++member_idx ] = index;
            weight[ member_idx ] = 1;
            cluster.setSeedIdx( 0 );  // member_idx == 0 at this point

            //LOG_INFO << "-----> new cluster: " << cluster << endm;

            // iterate over strips in the cluster.
            // save the index, but don't include the seed twice.
            // also add to mClosestClusterIDForEachStrip
            for( Int_t i = min; i <= max; ++i ){
               // check that index doesn't equal that for the seed.
               // Also check that the strip was really hit
               if( i != index && stripArray.strip[i].energy > 0 && !stripArray.strip[i].fail ){

                  //LOG_INFO << "www " << member_idx + 1 << ' ' << nStripsInClus << ' ' << member.GetSize() << endm;

                   member[ ++member_idx ] = i;
                   weight[ member_idx ] = 1;

                   //LOG_INFO << "www" << endm;

                   //LOG_INFO << "first cluster for index " << i << " is " << mFirstClusterIDForEachStrip[ i ] << endm;

                   if( mFirstClusterIDForEachStrip[ i ] < 0 ){
                      // this is the first one, so set is also the closest
                      mFirstClusterIDForEachStrip[ i ] = cluster.getID();
                      mClosestClusterIDForEachStrip[ i ] = cluster.getID();
                   } else {
                      // Not the first one--check to see if this clusters is "closer" than the previous closest cluster.
                      // Note: any strip can be in at most two clusters if min seed distance == max extent

                      // find other cluster
                      Bool_t found = 0;
                      Int_t j = -1;
                      while( j < (Int_t)clusterVec.size() - 1 && !found )
                         found = (clusterVec[++j].getID() == mClosestClusterIDForEachStrip[ i ] );

                      // sanity check
                      if( !found ){
                         LOG_WARN << "b) Error finding cluster with ID " << mClosestClusterIDForEachStrip[ i ] << endm;

                         // default to using the new one, since the previous cluster not found
                         mClosestClusterIDForEachStrip[ i ] = cluster.getID();
                      } else {
                         Int_t delta_other = TMath::Abs( i - clusterVec[j].getSeedMember() );
                         Int_t delta_this = TMath::Abs( i - cluster.getSeedMember() );

                         if( delta_this < delta_other ){
                            // switch to this one
                            mClosestClusterIDForEachStrip[ i ] = cluster.getID();
                         };
                      };
                   };

                   //LOG_INFO << "vvv closest cluster for strip " << i << " ID = " << mClosestClusterIDForEachStrip[ i ] << endm;
               };
            };
            //LOG_INFO << "VVV a)" << endm;
         };
         // end check that seed is far enough away from other seeds
         //LOG_INFO << "VVV b)" << endm;
      };
      // end loop over all possible seeds
      //LOG_INFO << "VVV c)" << endm;
   };

   //LOG_INFO << "VVV d)" << endm;

   if( mApplyClusterSplitting ){

      //LOG_INFO << "-----> Applying cluster splitting <-----" << endm;

      // iterate over clusters
      StSimpleClusterVec_t::iterator clusIter1 = clusterVec.begin();
      StSimpleClusterVec_t::iterator clusIter2;

      for( clusIter1 = clusterVec.begin(); clusIter1 != clusterVec.end(); ++clusIter1 ){
         for( clusIter2 = clusterVec.begin(); clusIter2 < clusIter1; ++clusIter2 ){

            // make some pointers for the two clusters to make the
            // code simpler.
            StSimpleCluster_t *rightClusPtr = &(*clusIter1);
            StSimpleCluster_t *leftClusPtr = &(*clusIter2);

            // set it so that the index of left is less than the index of right
            if( leftClusPtr->getSeedMember() > rightClusPtr->getSeedMember() ){
               // switch
               rightClusPtr = &(*clusIter2);
               leftClusPtr = &(*clusIter1); 
            };

            // check to see how close they are
            if( rightClusPtr->getSeedMember() - leftClusPtr->getSeedMember() <= 2*mMaxExtent ){
               // are overlapping, do energy sharing

               // make copies of the indices for the seeds
               Int_t leftSeedIdx = leftClusPtr->getSeedMember();
               Int_t rightSeedIdx = rightClusPtr->getSeedMember();

               // Energy on the outside halves of both strips.  Note:
               // Weihong calls leftEnergy and rightEnergy,
               // respectively, EI and EII.  Both are intialized to
               // the seed energy
               Float_t leftEnergy = stripArray.strip[ leftSeedIdx ].energy;
               Float_t rightEnergy = stripArray.strip[ rightSeedIdx ].energy;

               //LOG_INFO << "VVV e)" << endm;

               // Compute energy on the left hand side of the left cluster.
               // Note: start at 1 so don't count seed twice.
               // todo: could do a sanity check on temp_index
               {
                  TArrayS leftMember = leftClusPtr->getMemberArray();
                  Int_t i = 1;
                  for( Int_t tempIdx = leftMember[ i ]; tempIdx < leftSeedIdx && i < leftMember.GetSize()-1; tempIdx = leftMember[++i] )
                     leftEnergy += stripArray.strip[ tempIdx ].energy;
               };

               //LOG_INFO << "VVV f)" << endm;

               // Compute energy on the right hand side of the right cluster.
               {
                  TArrayS rightMember = rightClusPtr->getMemberArray();
                  Int_t i =  rightMember.GetSize()-1;
                  for( Int_t tempIdx = rightMember[ i ]; tempIdx > rightSeedIdx && i >-1; tempIdx = rightMember[--i] )
                     rightEnergy += stripArray.strip[ tempIdx ].energy;
               };

               //LOG_INFO << "VVV g)" << endm;


               //LOG_INFO << "fff " << leftEnergy << ' ' << rightEnergy << endm;

               // now that we know the energies, need to adjust
               // weights for each of the strips that are in both
               // clusters

               // for simplicity, now want pointers to the cluster
               // according to which energy of the seeds is higher or lower
               StSimpleCluster_t *lowerClusPtr = &(*clusIter1);
               StSimpleCluster_t *higherClusPtr = &(*clusIter2);

               // this probably just a sanity check, as the seeds were
               // ordered according to energy
               if( stripArray.strip[ lowerClusPtr->getSeedMember() ].energy > 
                   stripArray.strip[ higherClusPtr->getSeedMember() ].energy ){

                  // switch
                  higherClusPtr = &(*clusIter2);
                  lowerClusPtr = &(*clusIter1); 
               };

               // get references to (not copies of) arrays.
               TArrayS &lowerMemberArray = lowerClusPtr->getMemberArray();
               TArrayF &lowerWeightArray = lowerClusPtr->getWeightArray();
               TArrayS &higherMemberArray = higherClusPtr->getMemberArray();
               TArrayF &higherWeightArray = higherClusPtr->getWeightArray();

               // precompute weights
               Float_t sumOfEnergy = leftEnergy + rightEnergy;
               Float_t lowerWeight =  ( lowerClusPtr == leftClusPtr ?  leftEnergy : rightEnergy ) / sumOfEnergy;
               Float_t higherWeight = ( lowerClusPtr == leftClusPtr ? rightEnergy :  leftEnergy ) / sumOfEnergy;

               //LOG_INFO << "VVV f)" << endm;

               // iterate over strips in the cluster with lower seed energy.
               for( Int_t i=0; i<lowerMemberArray.GetSize(); ++i ){
                  //LOG_INFO << "Lower member " << i << ", index " << lowerMemberArray[i] << endm;

                  // check if shares with the other cluster
                  if( mFirstClusterIDForEachStrip[ lowerMemberArray[i] ] == higherClusPtr->getID() ){
                     // adjust weight
                     lowerWeightArray[i] = lowerWeight;

                     // reset the "first" cluster for the strip to be this cluster
                     // as a flag for the other cluster
                     mFirstClusterIDForEachStrip[ lowerMemberArray[i] ] = lowerClusPtr->getID();
                  };

                  //LOG_INFO << "fff --> L index " << lowerMemberArray[i] << " setting weight to " << lowerWeightArray[i] << endm;
               };

               //LOG_INFO << "VVV g)" << endm;

               // iterate over strips in the cluster with the higher seed energy
               for( Int_t i=0; i<higherMemberArray.GetSize(); ++i ){
                  //LOG_INFO << "Higher member " << i << ", index " << lowerMemberArray[i] << endm;

                  // check if shares with the other cluster
                  if( mFirstClusterIDForEachStrip[ higherMemberArray[i] ] == lowerClusPtr->getID() ){
                     // adjust weight
                     higherWeightArray[i] = higherWeight;

                     // reset the "first" cluster for the strip back to the real "first"
                     mFirstClusterIDForEachStrip[ higherMemberArray[i] ] = higherClusPtr->getID();
                  };

                  //LOG_INFO << "fff --> H index " << higherMemberArray[i] << " setting weight to " << higherWeightArray[i] << endm;

               };
               // end check whether seed strips are close enough to cause clusters to overlap

               //LOG_INFO << "VVV f)" << endm;

            };
            // end inner loop over clusters
         };
         // end outer loop over clusters
      };
      // end check whether to apply energy splitting
   };

   //LOG_INFO << "VVV g)" << endm;


   //LOG_INFO << "-----> Computing SMD energy per cluster <-----" << endm;

   // compute energy and mean per cluster
   StSimpleClusterVec_t::iterator clusIter = clusterVec.begin();

   Int_t ierr = kStOK;
   for( clusIter = clusterVec.begin(); clusIter != clusterVec.end() && !ierr; ++clusIter ){

      Float_t E = 0;
      Float_t mean = 0;

      TArrayS &memberArray = clusIter->getMemberArray();
      TArrayF &weightArray = clusIter->getWeightArray();

      if( memberArray.GetSize() != weightArray.GetSize() ){
         LOG_FATAL << "SANITY CHECK FAILURE IN StEEmcStripClusterFinderIU.cxx, line " << __LINE__ << endm;
         ierr = kStFatal;
      };

      //LOG_INFO << "ggg Cluster with seed " << clusIter->getSeedMember() << endm;

      if( !ierr )
         for( Int_t i = 0; i < memberArray.GetSize(); ++i ){
            Float_t stripE = stripArray.strip[ memberArray[ i ] ].energy;
            if( stripE > 0 && !stripArray.strip[ memberArray[ i ] ].fail ){

               Float_t thisE = weightArray[i]*stripE;
               E += thisE;
               mean += memberArray[i] * thisE;

               //LOG_INFO << "ggg " <<  mStripPtrArray[ memberArray[i ] ]->index()
               //         << " E += " << weightArray[i] << " * " << mStripPtrArray[ memberArray[ i ] ]->energy()
               //         << " = " << weightArray[i]*mStripPtrArray[ memberArray[ i ] ]->energy() << endm;
            };
         };
      clusIter->setEnergy( E );
      clusIter->setMeanX( E > 0 ? mean/E : memberArray[ 0 ] + 0.5 );

      //LOG_INFO << "-----> layer = " << mLayer << ", SMD cluster: " << *clusIter << endm;
   };

   return kStOK;
};

Bool_t StEEmcStripClusterFinderIU_t::energyGreaterThan( const EEmcElement_t *s1, const EEmcElement_t *s2 ){
   return s1->energy > s2->energy;
};

ClassImp( StEEmcStripClusterFinderIU_t );

/*
 * $Id: StEEmcStripClusterFinderIU.cxx,v 1.1 2012/11/26 19:05:55 sgliske Exp $ 
 * $Log: StEEmcStripClusterFinderIU.cxx,v $
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
