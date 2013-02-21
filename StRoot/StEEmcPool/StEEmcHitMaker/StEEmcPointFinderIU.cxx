/*!
 * \class StEEmcPointFinderIU_t
 * \author Stephen Gliske <sgliske@anl.gov>
 *
 * Implementation of the point finder algrothim from the PhD
 * dissertation of Weihong He.  See further description in header
 * file.
*/

#include <TMath.h>

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"
#include "StRoot/StEEmcPool/StEEmcGeoId/StEEmcGeoId.h"

#include "StEEmcPointFinderIU.h"
#include "StFinderAlg.h"
#include "StSimpleCluster.h"
#include "StEEmcHit.h"
#include "StEEmcStripClusterFinder.h"
#include "StESMDClustersPerSector.h"

StEEmcPointFinderIU_t::StEEmcPointFinderIU_t() : StEEmcPointFinder_t(),
                                                 mEEmcSmdGeom( 0 ),
                                                 mEEmcSmdMap( 0 ),
                                                 mMinUVratio( 0 ),
                                                 mZratioThres( 0.2 ),
                                                 mTtestThres( 9.e9 ),
                                                 mUseBugFixes( 1 ),
                                                 mTowerThreshold( 0.0 ) 
{
   mEEmcSmdGeom = EEmcSmdGeom::instance();
   mEEmcSmdMap = EEmcSmdMap::instance(); 

   if( !mEEmcSmdGeom ){
      LOG_FATAL << "StEEmcPointFinderIU_t(...) CANNOT GET POINTER TO EEmcSmdGeom" << endm;
      return;
   };

   if( !mEEmcSmdMap ){
      LOG_FATAL << "StEEmcPointFinderIU_t(...) CANNOT GET POINTER TO EEmcSmdMap" << endm;
      return;
   };

   mIsReady = 1;
};

/// find some some points.  Note: input is towerClusterVec and
/// stripClusterVec results are stored in mHit, and the return Int_t
/// is an error code or kStOK.  Note: this algorithm does not use the
/// tower clusters at all, but just checks the energy of individual
/// towers.
Int_t StEEmcPointFinderIU_t::find( const EEmcEnergy_t& eemcEnergy,
                                   const StSimpleClusterVec_t &towerClusterVec,
                                   const StESMDClustersVec_t &smdClusterVec,
                                   const Double_t* smdEuEvRatio,                  // not used
                                   StEEmcHitVec_t& hitVec ){

   //LOG_INFO << "********** StEEmcPointFinderIU_t::find(...) **********" << endm;

   Int_t ierr = kStOK;

   // Want to make a pool of cluster pointers.  The pool needs to
   // be a linked list, so that clusters can be efficiently
   // removed.
   StClusterPool_t uClusterPool, vClusterPool;

   // define some iterators
   StESMDClustersVec_t::const_iterator smdClusterVecIter;
   StSimpleClusterVec_t::const_iterator clusterIter;

   // last hit ID
   mLastSMDhitID = -1;

   // iterate over sectors with at least one cluster
   for( smdClusterVecIter = smdClusterVec.begin(); smdClusterVecIter != smdClusterVec.end(); ++smdClusterVecIter ){
      mSector = smdClusterVecIter->getSector();

      //LOG_INFO << "mSector = " << mSector << " (" << (Int_t)(smdClusterVecIter-smdClusterVec.begin()) << " of " << smdClusterVec.size() << ")" << endm;

      // clear the pools
      uClusterPool.clear();
      vClusterPool.clear();

      // some references to make code cleaner
      const StSimpleClusterVec_t &uClusVec = smdClusterVecIter->getClusterVecU();
      const StSimpleClusterVec_t &vClusVec = smdClusterVecIter->getClusterVecV();

      //LOG_INFO << "--> Num clusters = " << uClusVec.size() << ' ' << vClusVec.size() << endm;

      // u layer
      for( clusterIter = uClusVec.begin(); clusterIter != uClusVec.end(); ++clusterIter )
         uClusterPool.insert( &(*clusterIter) );

      // v layer
      for( clusterIter = vClusVec.begin(); clusterIter != vClusVec.end(); ++clusterIter )
         vClusterPool.insert( &(*clusterIter) );

      // No need to sort, since the StClusterPool_t is a set (automatically ordered)
      //std::sort( uClusterPool.begin(), uClusterPool.end(), &StEEmcPointFinderIU_t::clusterLessThan );
      //std::sort( vClusterPool.begin(), vClusterPool.end(), &StEEmcPointFinderIU_t::clusterLessThan );

      // find the points, i.e. hits (a recursive algorithm)
      ierr = findPoints( eemcEnergy, uClusterPool, vClusterPool, hitVec );

      //LOG_INFO << "--> Number of hits " << hitVec.size() << endm;
   };


   // return the error code
   return ierr;
};

// need to make u and v cluster vectors are not constant?
Int_t StEEmcPointFinderIU_t::findPoints( const EEmcEnergy_t& eemcEnergy,
                                         StClusterPool_t &uClusterPool,
                                         StClusterPool_t &vClusterPool,
                                         StEEmcHitVec_t& hitVec ) {

   Int_t ierr = kStOK;

   // --------------- build SMD points ---------------

   // First try finding all candidate hits.  Any pair of u & v
   // clusters under an "active" tower are considered candidates, with
   // "active" being defined below

   // containter for all possible hits to consider
   // Call these the stage 0 hits
   StEEmcHitVec_t hitsSMDstage0;

   // Container for hits still available after stage 1 Note: can store
   // the index value.  Since stage 0 is sorted by ttest^2, sorting by
   // index value will also sort by ttest^2.
   std::vector< Int_t > hitIdxStage1;

   // container for hits still available after stage 2
   std::vector< Int_t > hitIdxStage2;

   //LOG_INFO << "Building candidate points" << endm;

   // iterators
   StClusterPool_t::const_iterator uClusIter;
   StClusterPool_t::const_iterator vClusIter;


   //LOG_INFO << "U Pool " << endm;
//    for( uClusIter = uClusterPool.begin(); uClusIter != uClusterPool.end(); ++uClusIter ){
//       //LOG_INFO << *uClusIter << endm;
//    };
//    //LOG_INFO << "V Pool " << endm;
//    for( vClusIter = vClusterPool.begin(); vClusIter != vClusterPool.end(); ++vClusIter ){
//       //LOG_INFO << *vClusIter << endm;
//    };

   //LOG_INFO << " iii " << mSector << ": pool sizes " << uClusterPool.size() << ' ' << vClusterPool.size() << endm;

   // iterate over u and v clusters to make pairs
   for( uClusIter = uClusterPool.begin(); uClusIter != uClusterPool.end() && !ierr; ++uClusIter ){
      for( vClusIter = vClusterPool.begin(); vClusIter != vClusterPool.end() && !ierr; ++vClusIter ){

         // get the index of the seed strips of the clusters.  index
         // is with reference to the indexing of the strips in the
         // sector, not the strips within the cluster.
         //Int_t uSeedIdx = (*uClusIter)->getSeedMember();
         //Int_t vSeedIdx = (*vClusIter)->getSeedMember();
         Float_t uSeedMean = (*uClusIter)->getMeanX();
         Float_t vSeedMean = (*vClusIter)->getMeanX();

         assert( uSeedMean >= 0 && vSeedMean >= 0 && uSeedMean < kEEmcNumStrips && vSeedMean < kEEmcNumStrips ); // fix the clustering code
         //LOG_INFO << "seed indices " << uSeedMean << ' ' << vSeedMean << ", sector is " << mSector << endm;

         // determine which tower this SMD point/hit is under
         Bool_t isValid = 0;

         TVector3 direct = mEEmcSmdGeom->getIntersection( mSector, uSeedMean, vSeedMean );
         Int_t sec,sub,eta;
         if( mEEmcGeomSimple.getTower(direct,sec,sub,eta) ){
            // is valid tower, check sectors agree
            isValid = ( sec == mSector );
         };

         //LOG_INFO << " rrr " << getEventNum() << ' ' << mSector << ' ' << uSeedIdx << ' ' << vSeedIdx << ' ' << uSeedMean << ' ' << vSeedMean << ' '
         //         << direct.X() << ' ' << direct.Y() << ' ' << direct.Z() << endm;


         //LOG_INFO << "zzz " << sec << ' ' << mSector << endm;

         Int_t hitTowerIdx = StEEmcGeoId_t::encodeTow( sec, sub, eta );
         const EEmcElement_t& tower = eemcEnergy.eTow.getByIdx( hitTowerIdx );

         Float_t uE = 0;
         Float_t vE = 0;

         isValid = 0;

         // the following is what is meant by "active":
         /// "to form a valid smd point, we require a struck tower,
         /// or a "fail" bit to be set and one of the other tower
         /// detectors to fire (pre/postshower)."
         if ( tower.energy > mTowerThreshold ) {
            isValid = 1;
         } else if ( tower.fail ) {
            isValid = ( eemcEnergy.ePre1.getByIdx( hitTowerIdx ).energy > 0 ||
                        eemcEnergy.ePre2.getByIdx( hitTowerIdx ).energy > 0 ||
                        eemcEnergy.ePost.getByIdx( hitTowerIdx ).energy > 0 );
         };
         //LOG_INFO << "Active OK? " << isValid << endm;

         if( isValid ){
            // compute the energy of the two clusters
            uE = (*uClusIter)->getEnergy();
            vE = (*vClusIter)->getEnergy();

            /// "furthermore, we may require a degree of energy
            /// matching between the smd hits"
            isValid &= uE > mMinUVratio*vE;
            isValid &= vE > mMinUVratio*uE;
         };

         //LOG_INFO << "E matching OK? " << isValid << ' ' << mMinUVratio << ' ' << uE << ' ' << vE << endm;

         if( isValid ) {
            //LOG_INFO << "Valid point with seeds " << (*uClusIter)->getID() << ' ' << (*vClusIter)->getID() << endm;

            // push back a potential hit
            hitsSMDstage0.push_back( StEEmcHit_t(++mLastSMDhitID) );

            // get reference
            StEEmcHit_t &hit = hitsSMDstage0.back();

            // set UV info
            hit.setEnergyU( uE );
            hit.setEnergyV( vE );
            hit.setClusIDu( (*uClusIter)->getID() );
            hit.setClusIDv( (*vClusIter)->getID() );
            hit.setTowerIdx( hitTowerIdx );
            hit.setSector( mSector );

            // pre-compute ttest value
            hit.computeTtest2();

            // determine position (actually already did earlier, and
            // called it direct)
            //TVector3 position = mEEmcSmdGeom->getIntersection( mSector, uSeedMean, vSeedMean );
            hit.setX( direct.X() );
            hit.setY( direct.Y() );
            hit.setZ( direct.Z() );

            //LOG_INFO << "position = " << direct.X() << ' ' << direct.Y() << ' ' << direct.Z() << endm;

            //LOG_INFO << "Potential hit: " << hit << endm;
            //LOG_INFO << *(*uClusIter) << endm;
            //LOG_INFO << *(*vClusIter) << endm;

            //LOG_INFO << ", clus IDs " << hit.getClusIDu() << ' ' << hit.getClusIDv() << ", tow " << hit.getTowerIdx() << endm;

            if( direct.Z() < -998 ){
               // bad pairing, remove the candidate hit
               hitsSMDstage0.pop_back();
            };

            //LOG_INFO << "stage 0 candidate: " << mLastSMDhitID << ' ' << (*uClusIter)->getSeedMember() << ' ' << (*vClusIter)->getSeedMember() << endm;

         }; // end if( isValid )
      };
      // end loop over v clusters
   };
   // end loop over u v clusters

   //LOG_INFO << "\tstage 0 candidates " << hitsSMDstage0.size() << endm;


   //LOG_INFO << "At stage 0, exists " << hitsSMDstage0.size() << " candidates" << endm;

   // ---------------- prepare for stage 1 and 2 ---------------

   // if not enough candidate hits, then give up trying (or if there was an error)
   // Note: this is not an error, but a stopping criteria
   if( hitsSMDstage0.size() < 1 || ierr )
      return ierr;

   // sort SMD candidate hits according to ttest, which is currently
   // stored in the energy field of the hit
   std::sort( hitsSMDstage0.begin(), hitsSMDstage0.end(), hitTtest2LessThan );

   /// "create associative arrays matching smd clusters to hits,
   /// or in this case index into the smdhits array.  NOTE-- from
   /// here on, it is important that smdhits do not get sorted."
   std::map< Int_t, std::vector<Int_t> > uClusID2hitIdx, vClusID2hitIdx;

   // also create associative array between cluster indices and pointers to the clusters
   std::map< Int_t, const StSimpleCluster_t* > uClusID2ptr, vClusID2ptr;
   for( uClusIter = uClusterPool.begin(); uClusIter != uClusterPool.end(); ++uClusIter )
      uClusID2ptr[ (*uClusIter)->getID() ] = (*uClusIter);

   for( vClusIter = vClusterPool.begin(); vClusIter != vClusterPool.end(); ++vClusIter )
      vClusID2ptr[ (*vClusIter)->getID() ] = (*vClusIter);


   for( UInt_t i=0; i < hitsSMDstage0.size(); i++ ) {
//       {
//          //LOG_INFO << "Stage0 hit " << i << ' ' << hitsSMDstage0[i].getID() << ", ttest " << hitsSMDstage0[i].getTtest2() << ", clus seeds ";
//          //LOG_INFO << uClusID2ptr[ hitsSMDstage0[i].getClusIDu() ]->getSeedMember() << ", ";
//          //LOG_INFO << vClusID2ptr[ hitsSMDstage0[i].getClusIDv() ]->getSeedMember() << endm;
//          const StSimpleCluster_t &uClus = *uClusID2ptr[ hitsSMDstage0[i].getClusIDu() ];
//          const StSimpleCluster_t &vClus = *vClusID2ptr[ hitsSMDstage0[i].getClusIDv() ];
//          //LOG_INFO << "-----> layer U, SMD cluster: " << uClus << endm;
//          //LOG_INFO << "-----> layer V, SMD cluster: " << vClus << endm;
//          Float_t ttest = (uClus.getEnergy()-vClus.getEnergy())/(uClus.getEnergy()+vClus.getEnergy());
//          //LOG_INFO << "----------> T-test = " << ttest << ", T-test^2 = " << ttest*ttest << endm;
//       };
      // renumber hits, just to make sure ordering is consistant

      //LOG_INFO << "Hit ID " << hitsSMDstage0[i].getID() << " -> " << i << endm;

      //hitsSMDstage0[i].setID( i );

      //LOG_INFO << "Adding hit " << i << ", " << hitsSMDstage0[i].getID() << " with u, v clus idx " << hitsSMDstage0[i].getClusIDu() << ", " << hitsSMDstage0[i].getClusIDv() << endm;

      // note: hit index is the index in the hitsSMDstage0 array
      uClusID2hitIdx[ hitsSMDstage0[i].getClusIDu() ].push_back( i );
      vClusID2hitIdx[ hitsSMDstage0[i].getClusIDv() ].push_back( i );

      //uClusID2hitIdx[ hitsSMDstage0[i].getClusIDu() ].push_back( hitsSMDstage0[i].getID() );
      //vClusID2hitIdx[ hitsSMDstage0[i].getClusIDv() ].push_back( hitsSMDstage0[i].getID() );
   };



   // not in original IU code: remove clusters from the pool that have no stage 0 points
   for( uClusIter = uClusterPool.begin(); uClusIter != uClusterPool.end();  ){
      const StSimpleCluster_t* clusPtr = (*uClusIter);

      if( uClusID2hitIdx[ clusPtr->getID() ].empty() ){
         // move the iterator first
         ++uClusIter;

         // now delete from the set
         uClusterPool.erase( clusPtr );
      } else {
         ++uClusIter;
      };
   };

   for( vClusIter = vClusterPool.begin(); vClusIter != vClusterPool.end();  ){
      const StSimpleCluster_t* clusPtr = (*vClusIter);

      if( vClusID2hitIdx[ clusPtr->getID() ].empty() ){
         // move the iterator first
         ++vClusIter;

         // now delete from the set
         vClusterPool.erase( clusPtr );
      } else {
         ++vClusIter;
      };
   };



   //LOG_INFO << "stage 1: stage0 candiates = " << hitsSMDstage0.size() << endm;


   // ---------------- STAGE 1 ---------------

   // iterate over clusters, saving those clusters which only appear in a single hit
   // about line 330 of StEEmcIUPointMaker.cxx

   //LOG_INFO << "looping over " << uClusterPool.size() << " u clusters" << endm;

   for( uClusIter = uClusterPool.begin(); uClusIter != uClusterPool.end(); ++uClusIter ){
      // get reference to array of associated hit indices

      //LOG_INFO << "U cluster ID = " << (*uClusIter)->getID() << endm;

      std::vector<Int_t>& hitIdxVec = uClusID2hitIdx[ (*uClusIter)->getID() ];

      //LOG_INFO << "Number of hits associated with this cluster " << hitIdxVec.size() << endm;

      // check how many possible hits are associated with the given
      // U cluster
      if( !hitIdxVec.empty() ){
         if( hitIdxVec.size() == 1 ){
            // only one, so add it to the stage 1 list of possible hits/points
            hitIdxStage1.push_back( hitIdxVec.front() );
            //LOG_INFO << "Pushing back stage 1: " << hitIdxVec.front() << endm;
         };
         
         //LOG_INFO << "Deciding whether to push back into stage 2: " << hitIdxVec.front() << " or " << hitIdxVec.back() << endm;

         // state two includes the best of the options (the one in front)
         hitIdxStage2.push_back( hitIdxVec.front() );
      };
   };

   //LOG_INFO << "looping over " << vClusterPool.size() << " v clusters" << endm;

   for( vClusIter = vClusterPool.begin(); vClusIter != vClusterPool.end(); ++vClusIter ){
      // get reference to array of associated hit indices

      //LOG_INFO << "V cluster ID = " << (*vClusIter)->getID() << endm;

      std::vector<Int_t>& hitIdxVec = vClusID2hitIdx[ (*vClusIter)->getID() ];

      //LOG_INFO << "Number of hits associated with this cluster " << hitIdxVec.size() << endm;

      // check how many possible hits are associated with the given
      // V cluster
      if( !hitIdxVec.empty() ){
         if( hitIdxVec.size() == 1 ){
            // only one, so add it to the stage 1 list of possible hits/points
            hitIdxStage1.push_back( hitIdxVec.front() );

            //LOG_INFO << "Pushing back stage 1: " << hitIdxVec.front() << endm;
         };

         //LOG_INFO << "Deciding whether to push back into stage 2: " << hitIdxVec.front() << " or " << hitIdxVec.back() << endm;

         // state two includes the best of the options (the one in front)
         hitIdxStage2.push_back( hitIdxVec.front() );
      };
   };

   // Note: in the case of only one u and only one v cluster, the
   // cluster will appear twice in the hitIdxStage1 vector.  Bug fixes
   // can be turned on or off with the mUseBugFixes variable.

   // since stage 0 is sorted, is sufficient to sort the indices in the stage1 vectors
   std::sort( hitIdxStage1.begin(), hitIdxStage1.end() );

   //LOG_INFO << "Number of stage 1 = " << hitIdxStage1.size() << ", stage 2 = " << hitIdxStage2.size() << endm;

//    //LOG_INFO << "STAGE 1" << endm;
//    for( UInt_t i=0; i<hitIdxStage1.size(); ++i ){
//       StEEmcHit_t &hit = hitsSMDstage0[ hitIdxStage1[i] ];
//       //LOG_INFO << "Stage1[" << i << "] = " << hit.getTtest2() << endm;
//       //LOG_INFO << "\t" << *uClusID2ptr[ hit.getClusIDu() ] << endm;
//       //LOG_INFO << "\t" << *vClusID2ptr[ hit.getClusIDv() ] << endm;
//    };

//    //LOG_INFO << "STAGE 2" << endm;
//    for( UInt_t i=0; i<hitIdxStage2.size(); ++i ){
//       StEEmcHit_t &hit = hitsSMDstage0[ hitIdxStage2[i] ];
//       //LOG_INFO << "Stage2[" << i << "] = " << hit.getTtest2() << endm;
//       //LOG_INFO << "\t" << *uClusID2ptr[ hit.getClusIDu() ] << endm;
//       //LOG_INFO << "\t" << *vClusID2ptr[ hit.getClusIDv() ] << endm;
//    };

   //LOG_INFO << "checking if splitting needed" << endm;

   // check if splitting needed
   if( hitIdxStage1.size() > 1 ){
      std::vector< Int_t >::iterator hitIter_1, hitIter_2;

      for( hitIter_1 = hitIdxStage1.begin(); hitIter_1 != hitIdxStage1.end(); ++hitIter_1 ){
         StEEmcHit_t &hit1 = hitsSMDstage0[ *hitIter_1 ];
         const StSimpleCluster_t *Uclus1 = uClusID2ptr[ hit1.getClusIDu() ];
         const StSimpleCluster_t *Vclus1 = vClusID2ptr[ hit1.getClusIDv() ];

         hitIter_2 = hitIter_1;

         ++hitIter_2;
         if( mUseBugFixes ){
            // move the second iter until it no longer points to the same hit
            while( (*hitIter_2) == (*hitIter_1) && hitIter_2 != hitIdxStage1.end())
               ++hitIter_2;
         };

         for( ; hitIter_2 != hitIdxStage1.end(); ++hitIter_2 ){
            //LOG_INFO << "hitIters point at " << *hitIter_1 << ' ' << *hitIter_2 << endm;
            //LOG_INFO << "stage 0 hits of size " << hitsSMDstage0.size() << endm;

            StEEmcHit_t &hit2 = hitsSMDstage0[ *hitIter_2 ];
            const StSimpleCluster_t *Uclus2 = uClusID2ptr[ hit2.getClusIDu() ];
            const StSimpleCluster_t *Vclus2 = vClusID2ptr[ hit2.getClusIDv() ];

            // now just work with Uclus1, Uclus2, Vclus1, Vclus2

            // Note: it is unclear why there are 'aa' and 'bb'
            // variables used in the older implementation, as they are
            // only incremented after the return statements, and thus
            // are always zero.

            Bool_t oneU = ( Uclus1 == Uclus2 );
            Bool_t oneV = ( Vclus1 == Vclus2 );

            if( oneU ^ oneV ){
               // either 2x1 or 1x2

               Bool_t isBelowThres = 0;
               Float_t uZratio, vZratio, uWeight1, uWeight2, vWeight1, vWeight2;

               //LOG_INFO << "a2a2a2 E = "
               //         << Uclus1->getEnergy()*hit1.getWeightU() << ' ' << Vclus1->getEnergy()*hit1.getWeightV() << ' '
               //         << Uclus2->getEnergy()*hit2.getWeightU() << ' ' << Vclus2->getEnergy()*hit2.getWeightV() << ' '
               //         << endm;

               if( oneU ){
                  // Weihong calls this uzr, presumably for the u Z-ratio
                  uZratio = TMath::Abs(
                                       ( Uclus1->getEnergy() - Vclus1->getEnergy() - Vclus2->getEnergy() )/ 
                                       ( Uclus1->getEnergy() + Vclus1->getEnergy() + Vclus2->getEnergy() )   );
                  vZratio = 1;

                  // Weihong hard codes mZratioThres = 0.2
                  isBelowThres = ( uZratio < mZratioThres );

                  uWeight1 = Vclus1->getEnergy()/(Vclus1->getEnergy()+Vclus2->getEnergy());
                  uWeight2 = 1 - uWeight1; //  == Vclus2->getEnergy()/(Vclus1->getEnergy()+Vclus2->getEnergy());
                  vWeight1 = 1;
                  vWeight2 = 1;

                  //LOG_INFO << "a2a2a2a " << uZratio << " vs " << mZratioThres << "\t" << uWeight1 << ' ' << uWeight2 << endm;
               } else {
                  uZratio = 1;
                  vZratio = TMath::Abs(
                                       ( Vclus1->getEnergy() - Uclus1->getEnergy() - Uclus2->getEnergy() )/ 
                                       ( Vclus1->getEnergy() + Uclus1->getEnergy() + Uclus2->getEnergy() )  );

                  // Weihong hard codes mZratioThres = 0.2
                  isBelowThres = ( vZratio < mZratioThres );
                  //LOG_INFO << "b) " << vZratio << " vs " << mZratioThres << endm;

                  uWeight1 = 1;
                  uWeight2 = 1;
                  vWeight1 = Uclus1->getEnergy()/(Uclus1->getEnergy()+Uclus2->getEnergy());
                  vWeight2 = 1 - vWeight1; //  == Uclus2->getEnergy()/(Uclus1->getEnergy()+Uclus2->getEnergy());

                  //LOG_INFO << "a2a2a2b " << vZratio << " vs " << mZratioThres << "\t" << vWeight1 << ' ' << vWeight2 << endm;
               };

               //LOG_INFO << "checking if is below threshold" << endm;

               if( isBelowThres ){
                  //LOG_INFO << "it is" << endm;

                  // make two new hits by copying the current two hits
                  hitVec.push_back( hit1 );
                  StEEmcHit_t &saved_hit1 = hitVec.back();

                  hitVec.push_back( hit2 );
                  StEEmcHit_t &saved_hit2 = hitVec.back();

                  // need to set new weights
                  saved_hit1.setWeightU( uWeight1 );
                  saved_hit1.setWeightV( vWeight1 );
                  saved_hit2.setWeightU( uWeight2 );
                  saved_hit2.setWeightV( vWeight2 );

                  // Note: not adjusting keys as done in Weihong's code

                  // remove clusters
                  uClusterPool.erase( Uclus1 );
                  vClusterPool.erase( Vclus1 );
                  if( !oneU )
                     uClusterPool.erase( Uclus2 );
                  if( !oneV )
                     vClusterPool.erase( Vclus2 );

                  //LOG_INFO << "aaa " << getEventNum() << ' ' << mSector << " 2x1 " <<
                  //   Uclus1->getSeedMember() << ' ' << Vclus1->getSeedMember() << ' ' <<
                  //   Uclus2->getSeedMember() << ' ' << Vclus2->getSeedMember() << ' ' <<
                  //   Uclus1->getEnergy() << ' ' << Vclus1->getEnergy() << ' ' <<
                  //   Uclus2->getEnergy() << ' ' << Vclus2->getEnergy() << ' ' <<
                  //   " | " << saved_hit1.getID() << ' ' << saved_hit2.getID() << endm;
                  //LOG_INFO << "aaa, Sector " << mSector << "\t";
                  //LOG_INFO << "2x1 found with seed indices " << Uclus1->getSeedMember() << ' ' << Vclus1->getSeedMember() << ' ' << Uclus2->getSeedMember() << ' ' << Vclus2->getSeedMember() << endm;

                  // recursive algo (high memory cost!!!)			  
                  return findPoints( eemcEnergy, uClusterPool, vClusterPool, hitVec );
               };
            };

            //if( oneU && oneV ){
            // Weihong's code calls this case 3

            // Note: in this case (and in several other places)
            // Weihong's code copies all hits to a new container.
            // This is not really needed.
            //};

            // Note: Weihong's code does no do anything yet for cases
            // except except 2x1 and 1x2.  This is enforced
            // earlier also, since only hits which had at least a
            // unique u or v is considered in stage one.
         };
      };
   };
   
   // Haven't found a good pair yet, but have only tried 2x1 and 1x2
   // configurations.  Now look for a good "1x1" configuration, or
   // actually best of that which is left, as Weihong coded it.

   // no need to iterate to find "the best", it is the last one in the list,
   // as the list is sorted.

   //LOG_INFO << "checking for 1x1 config" << endm;

   // check if there even is a 1x1 option
   if( !hitIdxStage1.empty() ){
      //LOG_INFO << "Yep!" << endm;

//       for( UInt_t i=0; i<hitIdxStage1.size(); ++i ){
//          StEEmcHit_t &hit = hitsSMDstage0[ hitIdxStage1[i] ];
//          //LOG_INFO << "Stage1[" << i << "] = " << hit.getTtest2() << endm;
//          //LOG_INFO << "\t" << *uClusID2ptr[ hit.getClusIDu() ] << endm;
//          //LOG_INFO << "\t" << *vClusID2ptr[ hit.getClusIDv() ] << endm;
//       };

      StEEmcHit_t &hit = hitsSMDstage0[ hitIdxStage1.front() ];

      //LOG_INFO << "Got stage0[" << hitIdxStage1.front() << "] of " << hitsSMDstage0.size() << ", hit: " << hit << endm;

      const StSimpleCluster_t *uClus = uClusID2ptr[ hit.getClusIDu() ];
      const StSimpleCluster_t *vClus = vClusID2ptr[ hit.getClusIDv() ];

     //LOG_INFO << "Checking ttest^2: " << hit.getTtest2() << " <? " << mTtestThres << endm;

      // Wiehong hardcodes mTtestThres to be 9.0e9
      if( hit.getTtest2() < mTtestThres ){
         // save a copy of this hit
         hitVec.push_back( hit );

         // remove the clusters
         uClusterPool.erase( uClus );
         vClusterPool.erase( vClus );

         //LOG_INFO << "erased " << uClus << ' ' << vClus << endm;

         //LOG_INFO << "__--> 1x1 found, recursing, but now pools of size " << uClusterPool.size() << ' ' << vClusterPool.size() << endm;

         //LOG_INFO << "aaa, Sector " << mSector << "\t";
         //LOG_INFO << "1x1 found with seed indices " << uClus->getSeedMember() << ' ' << vClus->getSeedMember() << endm;
         //LOG_INFO << "aaa " << getEventNum() << ' ' << mSector << " 1x1 " <<
         //   uClus->getSeedMember() << ' ' << vClus->getSeedMember() << ' ' <<
         //   uClus->getEnergy() << ' ' << vClus->getEnergy() << 
         //   " | " << hit.getID() << endm;

         // recurse
         return findPoints( eemcEnergy, uClusterPool, vClusterPool, hitVec );
      };
   };

   //LOG_INFO << "stage 2" << endm;

   // ---------------- STAGE 2 ---------------

   // no luck finding matches yet.

   // Weihong does a fair amount of work to determine what the hit
   // with the best t-test is.  It seems this is due misperceptions
   // regarding scoping and recursion.  It seems the code of Weihong
   // tries to find the point with the smallest asymmetry in the
   // cluster energies (what he calls chi^2), but only search amoung
   // points where either the u or v cluster belongs to only one
   // candidate hit.  The largest element of the hitIdxState2 array is
   // already the element that Weihong seems to be after.

   // make sure there is at least one option
   if( !hitIdxStage2.empty() ){
      // find the largest element
      // since put u and v in seperately, stage2 is not sorted.
      Int_t idx = *(std::min_element( hitIdxStage2.begin(), hitIdxStage2.end() ));

//       //LOG_INFO << "Stage 2:" << endm;
//       for( UInt_t i = 0; i<hitIdxStage2.size(); ++i ){
//          StEEmcHit_t &hit = hitsSMDstage0[ hitIdxStage2[i] ];
//          const StSimpleCluster_t *uClus = uClusID2ptr[ hit.getClusIDu() ];
//          const StSimpleCluster_t *vClus = vClusID2ptr[ hit.getClusIDv() ];

//          //LOG_INFO << i << ' ' << hit.getID() << ", ttest " << hit.getTtest2() << ", seed indices " << uClus->getSeedMember() << ' ' << vClus->getSeedMember();
//          //LOG_INFO << ' ' << uClus->getEnergy() << ' ' << vClus->getEnergy() << endm;
//       };

      StEEmcHit_t &hit = hitsSMDstage0[ idx ];
      const StSimpleCluster_t *uClus = uClusID2ptr[ hit.getClusIDu() ];
      const StSimpleCluster_t *vClus = vClusID2ptr[ hit.getClusIDv() ];

      // Wiehong hardcodes mTtestThres to be 9.0e9
      if( hit.getTtest2() < mTtestThres ){
         // save a copy of this hit
         hitVec.push_back( hit );

         // remove the clusters
         uClusterPool.erase( uClus );
         vClusterPool.erase( vClus );

         //LOG_INFO << "aaa " << getEventNum() << ' ' << mSector << " last " <<
         //   uClus->getSeedMember() << ' ' << vClus->getSeedMember() << ' ' <<
         //   uClus->getEnergy() << ' ' << vClus->getEnergy() << 
         //   " | " << hit.getID() << endm;

         //LOG_INFO << "aaa, Sector " << mSector << "\t";
         //LOG_INFO << "last case " << idx << ' ' << hit.getID() << " with seed indices " << uClus->getSeedMember() << ' ' << vClus->getSeedMember() << endm;

         // recurse
         return findPoints( eemcEnergy, uClusterPool, vClusterPool, hitVec );
      };
   };

   // will probably never get here
   return kStOk;
};

ClassImp( StEEmcPointFinderIU_t );

/*
 * $Id: StEEmcPointFinderIU.cxx,v 1.2 2013/02/21 22:00:44 sgliske Exp $ 
 * $Log: StEEmcPointFinderIU.cxx,v $
 * Revision 1.2  2013/02/21 22:00:44  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:05:55  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcHitMaker to StRoot/StEEmcPool/StEEmcHitMaker
 *
 * 
 */
