/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header
 *
 */

#include <Rtypes.h>
#include <TChain.h>
#include <TTree.h>
#include <TFile.h>

#include "StEEmcTreeMaker.h"
#include "StEEmcEnergyMaker.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "StRoot/StEEmcPool/EEmcTreeContainers/EEmcEnergy.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/EEmcSmdCluster.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/EEmcHit.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/EEmcParticleCandidate.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/EEmc2ParticleCandidate.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/TrigSet.h"

#include "StRoot/StEEmcPool/StEEmcTreeMaker/StSpinInfoMaker.h"
#include "StRoot/StEEmcPool/StEEmcTreeMaker/StTrigCounter.h"
#include "StRoot/StEEmcPool/EEmcTreeContainers/StSpinInfo.h"
#include "StRoot/StEEmcPool/StEEmcHitMaker/StEEmcHitMaker.h"
#include "StRoot/StEEmcPool/StEEmcHitMaker/StESMDClustersPerSector.h"

#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

StEEmcTreeMaker_t::StEEmcTreeMaker_t( const Char_t *myName ) : StMaker( myName ), mNumEvents(0), mNumPart1EventsWritten(0), mMaxNumEvents(-1),
                                                               mSpinInfoIO(1), mEventHddrIO(1),
                                                               mDoMakePairs(1), mNumTowers( 0 ), mHTthres( 2 ), mTPthres( 4 ),
                                                               mEventHddr(0), mSpinInfo(0), mEEmcEnergy(0), 
                                                               mBbcOnlineTimeDiff(0), mVertexRank(-999), mVertex(0), mHrdTrigSet(0), mSftTrigSet(0), mClusArr(0), mHitArr(0),
                                                               mParticleArr1(0), mParticleArr2(0), mET0ht(0), mET0tp(0),
                                                               mSpinInfoMkr(0), mEnMkr(0), mHitMkr(0) {
   for( Int_t i=0; i<NUM_TREE_PARTS; ++i ){
      mIOStat[i] = IGNORE;
      mFile[i] = 0;
      mTree[i] = 0;
      mChain[i] = 0;
   };

   mVertex = new TVector3();
   mVertex->SetXYZ( -999, -999, -999 );

   mHrdTrigSet = new TrigSet();
   mSftTrigSet = new TrigSet();

   // get pointer to the eta bin ranges
   mEta = EEmcGeomSimple::Instance().getEtaBinRangeArray();
};


/// deconstructor
StEEmcTreeMaker_t::~StEEmcTreeMaker_t(){
   if( mClusArr )
      delete mClusArr;

   if( mHitArr && mIOStat[ PART_2 ] != IGNORE )
      delete mHitArr;

   if( mParticleArr1 )
      delete mParticleArr1;

   if( mParticleArr2 )
      delete mParticleArr2;

   if( mET0ht )
      delete mET0ht;

   if( mET0tp )
      delete mET0tp;

   if( mHrdTrigSet )
      delete mHrdTrigSet;

   if( mSftTrigSet )
      delete mSftTrigSet;

   for( Int_t i=0; i<NUM_TREE_PARTS; ++i ){
      if( mChain[i] )
         delete mChain[i];

      if( mFile[i] )
         delete mFile[i];

      // no need to delete the TTree after deleting the TFile
   };

   delete mVertex;
};

/// Initialize
Int_t StEEmcTreeMaker_t::Init(){
   Int_t ierr = kStOk;

   for( Int_t i=0; i<NUM_TREE_PARTS; ++i ){
      LOG_INFO << GetName() << ": Part " << i+1 << " set to "
               << ( mIOStat[i] == IGNORE ? "IGNORE" : ( mIOStat[i] == READ ? "READ" : "WRITE" ) )
               << endm;
   };

   if( !ierr ){
      if( mIOStat[ PART_1 ] == READ ){
         ierr = openForRead( PART_1 );

         if( !ierr ){
            // set the branch addresses of the branches that must be there
            mEEmcEnergy = 0;
            mTree[PART_1]->SetBranchAddress( "eemcEnergy", &mEEmcEnergy );
            mTree[PART_1]->SetBranchAddress( "vertex",     &mVertex );
            mTree[PART_1]->SetBranchAddress( "vertexRank",         &mVertexRank );
            mTree[PART_1]->SetBranchAddress( "bbcOnlineTimeDiff",  &mBbcOnlineTimeDiff );
            mTree[PART_1]->SetBranchAddress( "hardwareTriggers",   &mHrdTrigSet );
            mTree[PART_1]->SetBranchAddress( "softwareTriggers",   &mSftTrigSet );

            // for searching for branches
            TObjArray *branchArray = mTree[PART_1]->GetListOfBranches();

            // deal with evtHddr branch
            TObject *objPtr = branchArray->FindObject("evtHddr");
            if( objPtr ){
               mTree[PART_1]->SetBranchStatus( "evtHddr", 0 );

               if( mEventHddrIO ){
                  mTree[PART_1]->SetBranchStatus( "evtHddr", 1 );
                  mEventHddr = 0;
                  mTree[PART_1]->SetBranchAddress( "evtHddr", &mEventHddr );
               };
            } else if ( mEventHddrIO ){
               LOG_ERROR << "Requested EventHeaderIO, but event header branch not present in tree from file '" << mFilename[PART_1] << "'" << endm;
               ierr = kStFatal;
            };

            // deal with spinInfo branch
            objPtr = branchArray->FindObject("spinInfo");
            if( objPtr ){
               mTree[PART_1]->SetBranchStatus( "spinInfo", 0 );

               if( mSpinInfoIO ){
                  mTree[PART_1]->SetBranchStatus( "spinInfo", 1 );
                  mSpinInfo = 0;
                  mTree[PART_1]->SetBranchAddress( "spinInfo", &mSpinInfo );
               };
            } else if ( mSpinInfoIO ){
               LOG_ERROR << "Requested SpinInfoIO, but spin info branch not present in tree from file '" << mFilename[PART_1] << "'" << endm;
               ierr = kStFatal;
            };
         };
      } else if( mIOStat[ PART_1 ] == WRITE ){
         ierr = openForWrite( PART_1, "EEmcTreePart1" );
         if( !ierr ){
            mEventHddr = 0;
            if( mEventHddrIO )
               mTree[PART_1]->Branch( "evtHddr", &mEventHddr );

            mSpinInfo = 0;
            if( mSpinInfoIO )
               mTree[PART_1]->Branch( "spinInfo", &mSpinInfo );

            mTree[PART_1]->Branch( "vertex",     &mVertex );
            mTree[PART_1]->Branch( "vertexRank", &mVertexRank );

            mTree[PART_1]->Branch( "hardwareTriggers", &mHrdTrigSet, 32000, 1 );
            mTree[PART_1]->Branch( "softwareTriggers", &mSftTrigSet, 32000, 1 );

            mTree[PART_1]->Branch( "bbcOnlineTimeDiff", &mBbcOnlineTimeDiff );

            mEEmcEnergy = 0;
            mTree[PART_1]->Branch( "eemcEnergy", &mEEmcEnergy );
         };
      } else if( mIOStat[ PART_1 ] == IGNORE ){
         if( !mTreeRdr && !mEnMkr ){
            LOG_FATAL << "If this StEEmcTreeMaker is ignoring Part 1, then one must set a pointer to a StEEmcTreeMaker with a valid EEmcEnergy_t or" << endm;
            LOG_FATAL << "by setting a pointer to an EEmcEnergyMaker_t.  This is done via calling StEEmcTreeMaker_t::setEEmcTreeReader(...) or " << endm;
            LOG_FATAL << "StEEmcTreeMaker_t::setEEmcEnergyMaker(...) on this class before ::Init is called." << endm;
            ierr = kStFatal;
         };
      };
   };

   if( !ierr ){
      if( mIOStat[ PART_2 ] != IGNORE ){
         mClusArr = new TClonesArray( "EEmcSmdCluster_t", 100 );
         mHitArr  = new TClonesArray( "EEmcHit_t", 50 );

         if( mIOStat[ PART_2 ] == READ ){
            ierr = openForRead( PART_2 );

            if( !ierr ){
               mTree[ PART_2 ]->SetBranchStatus( "eemcClusArr", 1 );
               mTree[ PART_2 ]->SetBranchStatus( "eemcHitArr", 1 );

               mTree[ PART_2 ]->SetBranchAddress( "eemcClusArr", &mClusArr );
               mTree[ PART_2 ]->SetBranchAddress( "eemcHitArr",  &mHitArr );
            };
         } else {
            ierr = openForWrite( PART_2, "EEmcTreePart2" );
            if( !ierr ){
               mTree[ PART_2 ]->Branch( "eemcClusArr", &mClusArr );
               mTree[ PART_2 ]->Branch( "eemcHitArr",  &mHitArr );
            };
         };
      };
   };

   if( !ierr ){
      if( mIOStat[ PART_3 ] != IGNORE ){
         mParticleArr1 = new TClonesArray( "EEmcParticleCandidate_t", 10 );
         mParticleArr2 = new TClonesArray( "EEmc2ParticleCandidate_t", 10 );

         if( mIOStat[ PART_3 ] == READ ){
            ierr = openForRead( PART_3 );

            if( !ierr ){
               mTree[ PART_3 ]->SetBranchStatus( "photon", 1 );
               mTree[ PART_3 ]->SetBranchStatus( "pi0", 1 );
               mTree[ PART_3 ]->SetBranchAddress( "photon", &mParticleArr1 );
               mTree[ PART_3 ]->SetBranchAddress( "pi0", &mParticleArr2 );
            };
         } else {
            ierr = openForWrite( PART_3, "EEmcTreePart3" );
            if( !ierr ){
               mTree[ PART_3 ]->Branch( "photon", &mParticleArr1 );
               mTree[ PART_3 ]->Branch( "pi0", &mParticleArr2 );
            };
         };
      };
   };

   if( !ierr && mIOStat[ PART_4 ] != IGNORE ){
      mET0ht = new TArrayF(5);
      mET0tp = new TArrayF(5);

      if( mIOStat[ PART_4 ] == READ ){
         ierr = openForRead( PART_4 );

         if( !ierr ){
            // set the branch addresses of the branches that must be there
            mTree[ PART_4 ]->SetBranchAddress( "htET0", &mET0ht );
            mTree[ PART_4 ]->SetBranchAddress( "tpET0", &mET0tp );
         };
      } else { 
         ierr = openForWrite( PART_4, "EEmcTreePart4" );
         if( !ierr ){
            mTree[ PART_4 ]->Branch( "htET0", &mET0ht );
            mTree[ PART_4 ]->Branch( "tpET0", &mET0tp );
         };
      };
   };

   Int_t treeMax = -1111;
   if( mIOStat[ PART_1 ] == READ && mChain[ PART_1 ]->GetEntries() > treeMax )
      treeMax = mChain[ PART_1 ]->GetEntries();

   if( mIOStat[ PART_2 ] == READ && mChain[ PART_2 ]->GetEntries() > treeMax )
      treeMax = mChain[ PART_2 ]->GetEntries();

   if( mIOStat[ PART_3 ] == READ && mChain[ PART_3 ]->GetEntries() > treeMax )
      treeMax = mChain[ PART_3 ]->GetEntries();

   if( mIOStat[ PART_4 ] == READ && mChain[ PART_4 ]->GetEntries() > treeMax )
      treeMax = mChain[ PART_4 ]->GetEntries();

   if( mMaxNumEvents < 0 || mMaxNumEvents > treeMax )
      mMaxNumEvents = treeMax;

   return ierr;
};


/// Build an event
Int_t StEEmcTreeMaker_t::Make(){
   Int_t ierr = kStOk;

   // if reading, throw error if read past the end
   if( ( mIOStat[ PART_1 ] == READ || mIOStat[ PART_2 ] == READ || mIOStat[ PART_3 ] == READ ) && mNumEvents >= mMaxNumEvents )
      ierr = kStEOF;

   // otherwise, only do stuff if not past the end
   if( !ierr && ( mNumEvents < mMaxNumEvents || mMaxNumEvents < 0 ) ){
      if( mIOStat[ PART_1 ] == READ ){
         mChain[ PART_1 ]->GetEntry( mNumEvents );
      } else if( mIOStat[ PART_1 ] == WRITE ){
         ierr = fillPart1();
      } else if( mIOStat[ PART_1 ] == IGNORE ){
         if( mTreeRdr ){
            mEEmcEnergy = mTreeRdr->getEEmcEnergy();
         } else if ( mEnMkr ){
            mEEmcEnergy = mEnMkr->getEEmcEnergyPtr();
         } else {
            LOG_FATAL << "Cannot find valid pointer for EEmcEnergy_t" << endm;
            ierr = kStFatal;
         };
      };

      if( !ierr && mIOStat[ PART_2 ] == READ ){
         mChain[ PART_2 ]->GetEntry( mNumEvents );
      } else if( mIOStat[ PART_2 ] == IGNORE && mIOStat[ PART_3 ] != IGNORE ){
         if( mTreeRdr ){
            mHitArr = mTreeRdr->getHitArray();
            assert( mHitArr ); // fix your chain
         } else {
            LOG_FATAL << "Cannot find valid pointer for the hit array" << endm;
            ierr = kStFatal;
         };
      } else if( mIOStat[ PART_2 ] == WRITE ){
         ierr = fillPart2();
      };

      if( !ierr && mIOStat[ PART_3 ] == READ ){
         mChain[ PART_3 ]->GetEntry( mNumEvents );
      } else if( mIOStat[ PART_3 ] == WRITE ){
         ierr = fillPart3();
      };

      if( !ierr && mIOStat[ PART_4 ] == READ ){
         mChain[ PART_4 ]->GetEntry( mNumEvents );
      } else if( mIOStat[ PART_4 ] == WRITE ){
         ierr = fillPart4();
      };

   };

   // must increment after all the checks and processing
   ++mNumEvents;
   return ierr;
};

Int_t StEEmcTreeMaker_t::fillPart1(){
   Int_t ierr = kStOk;

   if( !mEnMkr ){
      LOG_ERROR << "Must set energy maker pointer to write out Part 1" << endm;
      ierr = kStFatal;
   };

   mEEmcEnergy = mEnMkr->getEEmcEnergyPtr();
   assert( mEEmcEnergy );

   // check if passes the simple filter
   if( mEEmcEnergy->nTowers > mNumTowers ){

      // fill the hardware trigger
      const StMuDst* muDst = (const StMuDst*)GetInputDS( "MuDst" );
      if( muDst ){
         StMuEvent *event = muDst->event();

         if( event )
            mHrdTrigSet->insert( event->triggerIdCollection().nominal().triggerIds() );  // l1 -> nominal, Sept 18th
      };

      // fill the software trigger
      StTriggerSimuMaker *trgSimMkr= (StTriggerSimuMaker*) StMaker::GetChain()->GetMaker("StarTrigSimu");
      if( trgSimMkr ){
         // only copy Ids not vetoed
         std::vector< UInt_t > trigIds;
         const std::vector< int >& trigL0 = trgSimMkr->triggerIds();

         Int_t id = 0;
         for( UInt_t i=0; i<trigL0.size(); ++i ){
            //cout << mNumEvents << " L0 trig " << trigL0[i] << " passed L2?" << trgSimMkr->isTrigger( trigL0[i] ) << endl;
            if( trgSimMkr->isTrigger( ( id = trigL0[i] ) ) )
               trigIds.push_back( static_cast< UInt_t >( id ) );
         };

         mSftTrigSet->insert( trigIds );
      };

      if( mEventHddrIO )
         mEventHddr = static_cast< StEvtHddr* >( GetEvtHddr()->Clone("eventHeader") );

      if( mSpinInfoIO ){
         if( !mSpinInfoMkr ){
            LOG_ERROR << "Requested to write SpinInfo, but pointer to maker not provided" << endm;
            ierr = kStFatal;
         } else {
            mSpinInfo = mSpinInfoMkr->getSpinInfoPtr();
         };
      };

      if( !ierr ){
         const StMuDst* muDst = (const StMuDst*)GetInputDS( "MuDst" );
         mVertex->SetXYZ( -999, -999, -999 );
         mVertexRank = -999;

         if( muDst ){
            StMuEvent *event = muDst->event();

            if( event ){
               const StThreeVectorF& v = event->primaryVertexPosition();
               mVertex->SetXYZ( v.x(), v.y(), v.z() );
               if( muDst->primaryVertex() )
                  mVertexRank = muDst->primaryVertex()->ranking();

#ifdef DEBUG
               cout << "vertex at " << v.x() << ' ' << v.y() << ' ' << v.z() << " | " << mVertex->X() << ' ' << mVertex->Y() << ' ' << mVertex->Z() << endl;
#endif

               mBbcOnlineTimeDiff = event->bbcTriggerDetector().onlineTimeDifference();
            };
         };

         mTree[ PART_1 ]->Fill();
         ++mNumPart1EventsWritten;
      };
   };

   return ierr;
};


Int_t StEEmcTreeMaker_t::fillPart2(){
   Int_t ierr = kStOk;

   if( !mHitMkr ){
      LOG_ERROR << "Cannot fill Part 2, as no StEEmcHitMaker pointer was provided." << endm;
      ierr = kStFatal;
   };

   if( !ierr ){
      // Clear.  May not be needed, but just to be safe.
      // Note, cluster and hit array must always be cleared with option "C".
      mClusArr->Clear("C");
      mHitArr->Clear("C");

      // convert from clus::ID to index in TClonesArray
      std::map< Int_t, Int_t > clusMapU, clusMapV;

      if( mHitMkr->getIfClusteredSMD() ){
//          Int_t nClus = mHitMkr->getNumSMDClusters();
//          if( mClusArr->GetEntries() < nClus )
//             mClusArr->Expand( nClus );

         const StESMDClustersVec_t& clusVec = mHitMkr->getESMDClustersVec();

         Int_t clusIdx = -1;
         for( UInt_t i = 0; i<clusVec.size(); ++i ){
            Short_t sector = clusVec[i].getSector();

            const StSimpleClusterVec_t& clusVecU = clusVec[i].getClusterVecU();
            const StSimpleClusterVec_t& clusVecV = clusVec[i].getClusterVecV();

            Float_t *blah  = new Float_t[12];
            delete[] blah;
            blah = 0;

            for( UInt_t j=0; j<clusVecU.size(); ++j ){
               new( (*mClusArr)[++clusIdx] ) EEmcSmdCluster_t();
               copySimpleClusterToSmdCluster( *mEEmcEnergy, sector, 0, clusVecU[j], *static_cast< EEmcSmdCluster_t* >( mClusArr->UncheckedAt( clusIdx ) ) );
               clusMapU[ clusVecU[j].getID() ] = clusIdx;
            };

            for( UInt_t j=0; j<clusVecV.size(); ++j ){
               new( (*mClusArr)[++clusIdx] ) EEmcSmdCluster_t();
               copySimpleClusterToSmdCluster( *mEEmcEnergy, sector, 1, clusVecV[j], *static_cast< EEmcSmdCluster_t* >( mClusArr->UncheckedAt( clusIdx ) ) );
               clusMapV[ clusVecV[j].getID() ] = clusIdx;
            };
         };
      };

      // copy hits
      const StEEmcHitVec_t& hitVec = mHitMkr->getHitVec();
      if( mHitArr->GetEntries() < (Int_t)hitVec.size() )
         mHitArr->Expand( hitVec.size() );

      Int_t hitIdx = -1;
      for( UInt_t i=0; i<hitVec.size(); ++i ){
         if( hitVec[i].mUsedTowerIndices.GetSize() && hitVec[i].mUsedTowerWeights.GetSize() ){
            new( (*mHitArr)[++hitIdx] ) EEmcHit_t();
            copyStEEmcHitToEEmcHit( *mEEmcEnergy, clusMapU[ hitVec[i].getClusIDu() ], clusMapV[ hitVec[i].getClusIDv() ], hitVec[i], 
                                    *static_cast< EEmcHit_t* >( mHitArr->UncheckedAt( hitIdx ) ) );
         };
      };
   };

#ifdef DEBUG3
   cout << "Filling part 2 with " << mClusArr->GetEntriesFast() << " clusters and " << mHitArr->GetEntriesFast() << " hits." << endl;
#endif
   mTree[ PART_2 ]->Fill();

   return ierr;
};

Int_t StEEmcTreeMaker_t::fillPart3(){
   Int_t ierr = kStOk;

   // Clear.  May not be needed, but just to be safe.
   mParticleArr1->Clear();
   mParticleArr2->Clear();

   assert( mHitArr ); // fix your chain

   Int_t numHits = mHitArr->GetEntriesFast();

   if( numHits ){
      if( mParticleArr1->GetSize() < numHits )
         mParticleArr1->Expand( numHits );

      Int_t numPairs = 0;
      if( mDoMakePairs ){
         numPairs = TMath::Binomial( numHits, 2 );
         if( mParticleArr2->GetSize() < numPairs )
            mParticleArr2->Expand( numPairs );
      };

      TIter hitIter1 = mHitArr;
      assert( mTreeRdr );   // update this later to use the energy maker???
      TVector3* vertexPtr = mTreeRdr->getVertex();
      assert( vertexPtr ); // fix your chain
      mBbcOnlineTimeDiff = mTreeRdr->getBbcOnlineTimeDiff();

      if( vertexPtr->Z() < -900 && mBbcOnlineTimeDiff ){
         // not vertex defined yet--use the BBC time diff.

         UInt_t bbcTimeBin = mBbcOnlineTimeDiff/32;

         vertexPtr->SetX( 0 );
         vertexPtr->SetY( 0 );

         if( bbcTimeBin <= 2 )
            vertexPtr->SetZ( 40.1377 );
         else if( bbcTimeBin == 3 )
            vertexPtr->SetZ( 54.1733 );
         else if( bbcTimeBin == 4 )
            vertexPtr->SetZ( 75.2597 );
         else if( bbcTimeBin == 5 )
            vertexPtr->SetZ( 66.9572 );
         else if( bbcTimeBin == 6 )
            vertexPtr->SetZ( 38.324 );
         else if( bbcTimeBin == 7 )
            vertexPtr->SetZ( 5.83516 );
         else if( bbcTimeBin == 8 )
            vertexPtr->SetZ( -27.6028 );
         else if( bbcTimeBin == 9 )
            vertexPtr->SetZ( -60.6408 );
         else if( bbcTimeBin == 10 )
            vertexPtr->SetZ( -91.4053 );
         else if( bbcTimeBin == 11 )
            vertexPtr->SetZ( -105.579 );
         else if( bbcTimeBin == 12 )
            vertexPtr->SetZ( -95.7182 );
         else if( bbcTimeBin == 13 )
            vertexPtr->SetZ( -85.9305 );
         else if( bbcTimeBin >= 14 )
            vertexPtr->SetZ( -87.5347 );
      };

      EEmcHit_t *hitPtr1 = 0;
      Int_t hitIdx = -1;
      Int_t pairIdx = -1;
      while(( hitPtr1 = static_cast< EEmcHit_t* >(hitIter1.Next()) )){
         ++hitIdx;
         new( (*mParticleArr1)[ hitIdx ] ) EEmcParticleCandidate_t( hitIdx, *hitPtr1, *vertexPtr );

         if( mDoMakePairs ){
            const EEmcParticleCandidate_t *p1 = static_cast< const EEmcParticleCandidate_t* >( (*mParticleArr1)[ hitIdx ] );
            EEmcParticleCandidate_t *p2 = 0;
            TIter pIter = mParticleArr1;

            while( (p2 = static_cast< EEmcParticleCandidate_t* >(pIter.Next())) && p2->hitIdx1 != p1->hitIdx1 )
               new( (*mParticleArr2)[ ++pairIdx ] ) EEmc2ParticleCandidate_t( *p1, *p2 );
         };
      };
   };

#ifdef DEBUG3
   cout << "Filling part 3 with " << mParticleArr1->GetEntriesFast() << " + " << mParticleArr2->GetEntriesFast() << " particle candidates." << endl;
#endif
   mTree[ PART_3 ]->Fill();

   return ierr;
};

Int_t StEEmcTreeMaker_t::fillPart4(){
   assert( mEEmcEnergy );

   std::vector< std::pair< Float_t, Float_t > > httpVec;

   const ETowEnergy_t& eTow = mEEmcEnergy->eTow;

   std::vector< Int_t > neighbors;
   neighbors.reserve(9);

   // find a tower above threshold
   for( Int_t idx=0; idx<720; ++idx ){
      const EEmcElement_t& elem = eTow.getByIdx( idx );

      Int_t etabin = idx%12;
      Double_t cosHeta = TMath::CosH( 0.5*( mEta[etabin]+mEta[etabin+1] ) );

      // check if passes ht thres
      Double_t htET = elem.energy/cosHeta;
      if( htET > mHTthres && !elem.fail ){

         // now compute trigger patch energy

         Int_t phiBin = idx/12;
         Int_t phiBinLow = ( phiBin > 0 ? phiBin-1 : 59 );
         Int_t phiBinHigh = ( phiBin < 59 ? phiBin+1 : 0 );

         neighbors.clear();

         neighbors.push_back(idx);
         neighbors.push_back(etabin+12*phiBinLow);
         neighbors.push_back(etabin+12*phiBinHigh);

         if( etabin > 0 ){
            neighbors.push_back(idx-1);
            neighbors.push_back(etabin+12*phiBinLow-1);
            neighbors.push_back(etabin+12*phiBinHigh-1);
         };

         if( etabin < 11 ){
            neighbors.push_back(idx+1);
            neighbors.push_back(etabin+12*phiBinLow+1);
            neighbors.push_back(etabin+12*phiBinHigh+1);
         };

         Double_t tpET = 0;
         for( UInt_t i = 0; i<neighbors.size(); ++i ){
            Int_t idx2 = neighbors[i];
            const EEmcElement_t& elem = eTow.getByIdx( idx2 );
            if( !elem.fail )
               tpET += elem.energy / TMath::CosH( 0.5*( mEta[idx2%12]+mEta[idx2%12+1] ) );
         };

         //cout << mNumEvents << ' ' << htET << ' ' << tpET << ' ' << phiBin << ' ' << etabin << endl;

         if( tpET > mTPthres )
            httpVec.push_back( std::make_pair( htET, tpET ) );
      };
   };


   mET0ht->Set( httpVec.size() );
   mET0tp->Set( httpVec.size() );

   for( UInt_t i=0; i<httpVec.size(); ++i ){
      const std::pair< Float_t, Float_t >& http = httpVec[i];

      // add at is really set at
      mET0ht->AddAt( http.first,  i);
      mET0tp->AddAt( http.second, i);
   };

   mTree[ PART_4 ]->Fill();

   return 0;
};

/// Clear for next event
void StEEmcTreeMaker_t::Clear(Option_t *opts ){
   if( mIOStat[ PART_1 ] != IGNORE ){
      if( mEventHddr )
         mEventHddr->Clear();
      if( mSpinInfo )
         mSpinInfo->Clear();
      if( mEEmcEnergy )
         mEEmcEnergy->Clear();
      if( mVertex )
         mVertex->SetXYZ( -999, -999, -999 );
      mVertexRank = -999;
      mBbcOnlineTimeDiff = 0;
   };

   if( mIOStat[ PART_2 ] != IGNORE ){
      mClusArr->Clear("C");
      mHitArr->Clear("C");
   };

   if( mIOStat[ PART_3 ] != IGNORE ){
      mParticleArr1->Clear();
      mParticleArr2->Clear();
   };

};

/// Write everything to file
Int_t StEEmcTreeMaker_t::Finish(){

   // write out the trigger count information

   if( mIOStat[ PART_1 ] == WRITE ){
      StTrigCounter* trigCounter = (StTrigCounter*)StMaker::GetChain()->GetMaker("trigCounter");
      if( !trigCounter ){
         LOG_WARN << "ERROR cannot find the trigger counter" << endl;
      } else {
         TArrayI trigArray(3);
         trigArray[0] = trigCounter->getTrigID();
         trigArray[1] = trigCounter->getNumEventsTot();
         trigArray[2] = trigCounter->getNumEventsInCut();

         cout << "Writing trigger counts " << trigArray[0] << ' ' << trigArray[1] << ' ' << trigArray[2] << endl;
         mFile[ PART_1 ]->WriteObject( &trigArray, "trigCounts" );
      };
   };

   for( Int_t ipart=0; ipart<NUM_TREE_PARTS; ++ipart ){
      if( mIOStat[ ipart ] == WRITE ){
         mFile[ ipart ]->Write();
         mFile[ ipart ]->Close();
      };
   };

   return kStOk;
};

/// modifiers
void StEEmcTreeMaker_t::setTreeStatus( treeTypeEnum_t type, iostatus_t iostatus, const Char_t* fileName ){
   mIOStat[ type ] = iostatus;
   mFilename[ type ] = fileName;
};

void StEEmcTreeMaker_t::setMaxNumEvents( Int_t maxNum ){
   mMaxNumEvents = maxNum;
};

void StEEmcTreeMaker_t::setSpinInfoMkr( StSpinInfoMaker_t* spinInfoMkr ){
   mSpinInfoMkr = spinInfoMkr;
};

void StEEmcTreeMaker_t::setEEmcEnergyMkr( StEEmcEnergyMaker_t* eMkr ){
   mEnMkr = eMkr;
};

void StEEmcTreeMaker_t::setEEmcHitMkr( StEEmcHitMaker_t* hMkr ){
   mHitMkr = hMkr;
};

void StEEmcTreeMaker_t::setEEmcTreeReader( StEEmcTreeMaker_t* treeRdr ){
   mTreeRdr = treeRdr;
};

void StEEmcTreeMaker_t::doSpinInfoIO( Bool_t doIt ){ mSpinInfoIO = doIt; };
void StEEmcTreeMaker_t::doEvtHddrIO( Bool_t doIt ){ mEventHddrIO = doIt; };

Int_t StEEmcTreeMaker_t::openForRead( treeTypeEnum_t type ){
   Int_t ierr = kStOk;

   mChain[type] = new TChain("tree");
   Int_t nFiles = mChain[type]->Add( mFilename[type].data(), 1 );

   if( nFiles ){
      LOG_INFO << "Opened " << nFiles << " based on '" << mFilename[type] << endm;
   } else {
      LOG_ERROR << "Opened " << nFiles << " based on '" << mFilename[type] << endm;
      ierr = kStFatal;
   };
   mTree[type] = mChain[type];

   return ierr;
};

Int_t StEEmcTreeMaker_t::openForWrite( treeTypeEnum_t type, const Char_t *name ){
   Int_t ierr = kStOk;

   mFile[type] = new TFile( mFilename[type].data(), "RECREATE" );
   if( mFile[type]->IsOpen() ){
      LOG_INFO << "Opened file '" << mFilename[type] << "' for writing" << endm;
   } else {
      LOG_FATAL << "Error opening file '" << mFilename[type] << "'" << endm;
      ierr = kStFatal;
   };

   if( !ierr )
      mTree[type] = new TTree( "tree", name );

   return ierr;
};

void StEEmcTreeMaker_t::copySimpleClusterToSmdCluster( const EEmcEnergy_t& eemcEnergy, Short_t sector, Bool_t inLayerV, const StSimpleCluster_t& other, EEmcSmdCluster_t& clus ){
   clus.sector = sector;
   clus.inLayerV = inLayerV;
   clus.seedStripIdx = other.getSeedMember();
   clus.energy = 0;
   clus.meanPos = 0;
   clus.width = 0;

   // make a reference, so that one doesn't have to type such a long name
   const TArrayS& iArr = other.getMemberArray();
   const TArrayF& wArr = other.getWeightArray();

   clus.numUsedStrips = wArr.GetSize();
   if( iArr.GetSize() < wArr.GetSize() )
      clus.numUsedStrips = iArr.GetSize();

   if( clus.numUsedStrips > EEmcSmdCluster_t::kMaxClusterSize ) // cluster is too wide
      clus.numUsedStrips = EEmcSmdCluster_t::kMaxClusterSize;

   // loop over used strips
   for( Int_t i=0; i<clus.numUsedStrips; ++i ){
      // copy the values
      Int_t   idx    = clus.usedStripIdx[i]    = iArr[i];
      Float_t weight = clus.usedStripWeight[i] = wArr[i];

      // recompute mean, width, and energy
      const EEmcElement_t& element = eemcEnergy.eSmd.sec[sector].layer[inLayerV].strip[ idx ];
      Float_t energy = ( element.fail ? 0 : element.energy );

      energy *= weight;
      clus.energy += energy;
      clus.meanPos += energy*idx;
      clus.width += energy*idx*idx;
   };
   if( clus.energy ){
      clus.meanPos /= clus.energy;
      clus.width /= clus.energy;
      clus.width = sqrt( clus.width - clus.meanPos*clus.meanPos );
   };
};

void StEEmcTreeMaker_t::copyStEEmcHitToEEmcHit( const EEmcEnergy_t& eemcEnergy, Int_t uClusIdx, Int_t vClusIdx, const StEEmcHit_t& other, EEmcHit_t& hit ){
   hit.uClusIdx = uClusIdx;
   hit.vClusIdx = vClusIdx;

   const TVector3& position = other.getPosition();
   hit.x = position.X();
   hit.y = position.Y();
   hit.eta = position.Eta();
   hit.phi = position.Phi();

   hit.centralTowerIdx = other.getTowerIdx();

   // make a reference, so that one doesn't have to type such a long name
   const TArrayS& iArr = other.mUsedTowerIndices;
   const TArrayF& wArr = other.mUsedTowerWeights;

   // determine number of used towers
   hit.numUsedTowers = wArr.GetSize();
   if( hit.numUsedTowers > iArr.GetSize() )
      hit.numUsedTowers = iArr.GetSize();

   if( hit.numUsedTowers > EEmcHit_t::kMaxNumTowers ) // too many towers to save them all
      hit.numUsedTowers = EEmcHit_t::kMaxNumTowers;

   // clear
   hit.eTow = hit.ePre1 = hit.ePre2 = hit.ePost = 0;

   // to hold onto after loop
   Float_t centralTowerWeight = 0;

   //cout << "hit " << other.getID() << " central tower " << hit.centralTowerIdx << " num used " << hit.numUsedTowers << endl;

   // loop over towers
   for( Int_t i = 0; i < hit.numUsedTowers; ++i ){
      // copy the values
      Short_t idx    = hit.usedTowerIdx[i]    = iArr[i];
      Float_t weight = hit.usedTowerWeight[i] = wArr[i];

      const EEmcElement_t& towElement = eemcEnergy.eTow.getByIdx( idx );
      const EEmcElement_t& pstElement = eemcEnergy.ePost.getByIdx( idx );

      hit.eTow  += ( towElement.fail ? 0 : towElement.energy )*weight;
      hit.ePost += ( pstElement.fail ? 0 : pstElement.energy )*weight;

      //cout << " \t used tower " << idx << ' ' << weight << endl;

      if( idx == hit.centralTowerIdx )
         centralTowerWeight = weight;
   };

   // if the central tower is not in the array of used towers, perhaps
   // it was a failed tower, but the preshowers and post showers are
   // still OK.  Assume a weight of 1.
   if( !centralTowerWeight )
      centralTowerWeight = 1;

   // assert( centralTowerWeight ); // some code didn't include the central tower!

   // get the energy in the preshowers
   const EEmcElement_t& pr1Element = eemcEnergy.ePre1.getByIdx( hit.centralTowerIdx );
   const EEmcElement_t& pr2Element = eemcEnergy.ePre2.getByIdx( hit.centralTowerIdx );

   // set to negative 1 if failed.
   hit.ePre1 = ( pr1Element.fail ? -1 : pr1Element.energy )*centralTowerWeight;
   hit.ePre2 = ( pr2Element.fail ? -1 : pr2Element.energy )*centralTowerWeight;
};

ClassImp( StEEmcTreeMaker_t );

/*
 * $Id: StEEmcTreeMaker.cxx,v 1.5 2013/02/28 23:37:18 sgliske Exp $
 * $Log: StEEmcTreeMaker.cxx,v $
 * Revision 1.5  2013/02/28 23:37:18  sgliske
 * Updated so result of StTrigCounter gets saved in EEmcTree Part1
 * rather than just being output to the console (log file)
 *
 * Revision 1.4  2013/02/28 02:34:48  sgliske
 * bug fix for copying vertex rank
 *
 * Revision 1.3  2013/02/21 21:57:12  sgliske
 * updated values of vertexZ for BBC time bins (affects part 3)
 *
 * Revision 1.2  2013/02/21 21:28:50  sgliske
 * added vertex rank
 *
 * Revision 1.1  2012/11/26 19:06:10  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
