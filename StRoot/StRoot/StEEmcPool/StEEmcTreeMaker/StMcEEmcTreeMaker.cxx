/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header
 *
 */

//#define DEBUG

#include <Rtypes.h>
#include <TChain.h>
#include <TTree.h>
#include <TFile.h>

#include "StMcEEmcTreeMaker.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcContainers.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"
#include "StRoot/StEEmcPool/./EEmcTreeContainers/McParticle.h"
#include "StRoot/StEEmcPool/StEEmcPointMap/StEEmcPointMap.h"

StMcEEmcTreeMaker_t::StMcEEmcTreeMaker_t( const Char_t *myName ) : StMaker( myName ), mIOStat( UNSET ), mFile(0), mTree(0), mChain(0), 
                                                                   mNumEvents(0), mMaxNumEvents(-1), mTowEnergyThres( 0.01 ), mBjX1(0), mBjX2(0) {
   mEEmcEnergyArr       = new TClonesArray("EEmcEnergy_t", 5);
   mAncestorParticleArr = new TClonesArray("McParticle_t", 10);
   mIncidentParticleArr = new TClonesArray("McParticle_t", 5);
   mVertexArr           = new TClonesArray("TVector3",     5);
};

/// deconstructor
StMcEEmcTreeMaker_t::~StMcEEmcTreeMaker_t(){
   delete mEEmcEnergyArr;
   delete mAncestorParticleArr;
   delete mIncidentParticleArr;
   delete mVertexArr;

   if( mChain )
      delete mChain;

   if( mFile )
      delete mFile;

   // no need to delete the TTree after deleting the TFile
};

/// Initialize
Int_t StMcEEmcTreeMaker_t::Init(){
   Int_t ierr = kStOk;

   if( mIOStat == UNSET || mFilename.empty() ){
      LOG_ERROR << "Must set IO status to READ or WRITE and provide a filename" << endm;
      ierr = kStFatal;
   };

   // check trigger set
   if( mTriggerSet.empty() && mIOStat == WRITE ){
      LOG_FATAL << "ERROR: trigger set is empty." << endm;
      LOG_FATAL << "If you really want to process all triggers, enter a single trigger ID of -999." << endm;
      ierr = kStFatal;
   } else if (mTriggerSet.size() == 1 && mTriggerSet.count( -999 ) ) {
      // user has flagged to process all triggers
      mTriggerSet.clear();
   };

   if( !ierr ){
      LOG_INFO << GetName() << ": Set to "
               << ( mIOStat == READ ? "READ" : "WRITE" )
               << " the file '" << mFilename
               << endm;

      if( mIOStat == READ ){
         mChain = new TChain("tree");
         Int_t nFiles = mChain->Add( mFilename.data(), 1 );

         if( nFiles ){
            LOG_INFO << "Opened " << nFiles << " based on '" << mFilename << endm;
         } else {
            LOG_ERROR << "Opened " << nFiles << " based on '" << mFilename << endm;
            ierr = kStFatal;
         };
         mTree = mChain;

         if( !ierr ){
            // set the branch addresses of the branches that must be there
            mTree->SetBranchAddress( "eemcEnergyArr",       &mEEmcEnergyArr );
            mTree->SetBranchAddress( "incidentParticleArr", &mIncidentParticleArr );
            mTree->SetBranchAddress( "ancestorParticleArr", &mAncestorParticleArr );
            mTree->SetBranchAddress( "vertexArr",           &mVertexArr );
            mTree->SetBranchAddress( "xBj1",                &mBjX1 );
            mTree->SetBranchAddress( "xBj2",                &mBjX2 );
         };

      } else if( mIOStat == WRITE ){
         mFile = new TFile( mFilename.data(), "RECREATE" );
         if( mFile->IsOpen() ){
            LOG_INFO << "Opened file '" << mFilename << "' for writing" << endm;
         } else {
            LOG_FATAL << "Error opening file '" << mFilename << "'" << endm;
            ierr = kStFatal;
         };

         if( !ierr ){
            mTree = new TTree( "tree", "McEEmcTree" );
            mTree->Branch( "eemcEnergyArr",       &mEEmcEnergyArr );
            mTree->Branch( "incidentParticleArr", &mIncidentParticleArr );
            mTree->Branch( "ancestorParticleArr", &mAncestorParticleArr );
            mTree->Branch( "vertexArr",           &mVertexArr );
            mTree->Branch( "xBj1",                &mBjX1 );
            mTree->Branch( "xBj2",                &mBjX2 );
         };
      };
   };

   Int_t treeMax = -1111;
   if( mIOStat == READ && mChain->GetEntries() > treeMax )
      treeMax = mChain->GetEntries();

   if( mMaxNumEvents < 0 || mMaxNumEvents > treeMax )
      mMaxNumEvents = treeMax;

   return ierr;
};


/// Build an event
Int_t StMcEEmcTreeMaker_t::Make(){
   Int_t ierr = kStOk;

   if( mIOStat == READ && mNumEvents >= mMaxNumEvents )
      ierr = kStEOF;

   if( !ierr && ( mNumEvents < mMaxNumEvents || mMaxNumEvents < 0 ) ){
      if( mIOStat == READ ){
         mChain->GetEntry( mNumEvents );
      } else {
         Bool_t accept = 1;
         // check if event passes trigger
         if( !mTriggerSet.empty() ){
            accept = 0;

            const StMuDst* muDst = (const StMuDst*)GetInputDS( "MuDst" );
            if( muDst ){
               StMuEvent *event = muDst->event();

               if( event ){
                  const StTriggerId& l1trig = event->triggerIdCollection().l1();

                  std::set< Int_t >::iterator iter = mTriggerSet.begin();
                  for( ; iter != mTriggerSet.end() && !accept; ++iter )
                     accept = l1trig.isTrigger( (*iter) );
               };
            };
         };

         if( accept )
            ierr = fill();
      };
   };

   ++mNumEvents;
   return ierr;
};

//
Int_t StMcEEmcTreeMaker_t::fill(){
   Int_t ierr = kStOk;

   // get the McEvent
   StMcEvent* mcEventPtr = 0;
   mcEventPtr = static_cast< StMcEvent* >( StMaker::GetChain()->GetDataSet("StMcEvent") );
   if( !mcEventPtr ){
      LOG_FATAL << "ERROR finding StMcEvent" << endm;
      ierr = kStFatal;
   };

   // for bjorken-x from Pythia
   {
      //GET GEANT EVENT
      TDataSet *Event = GetDataSet("geant"); //Event->ls(3);

      //GET PYTHIA RECORD from particleTable
      TDataSetIter geantDstI(Event);

      //CHECK IF EXISTS PARTICLE TABLE
      if( geantDstI("particle") ){
         St_g2t_pythia *Pg2t_pythia = (St_g2t_pythia*)geantDstI("g2t_pythia");
         g2t_pythia_st *g2t_pythia1 = Pg2t_pythia->GetTable();

         mBjX1 = g2t_pythia1->bjor_1;
         mBjX2 = g2t_pythia1->bjor_2;
      };
   };

   // index in mIncidentParticleArr
   Int_t incPartIdx = -1;

   if( !ierr ){

      // prepare to iterate over the tracks and gather those indicident on the endcap.
      const StSPtrVecMcTrack& tracks = mcEventPtr->tracks();
      StSPtrVecMcTrack::const_iterator trackIter;

      // in the following, hit is defined as the energy response of a
      // given element (tower, strip, etc.) of the EEmc, to be
      // consistent with definitions in StEvent.
      StSPtrVecMcCalorimeterHit::const_iterator hitIter;

      for( trackIter = tracks.begin(); trackIter != tracks.end(); ++trackIter ){
         const StPtrVecMcCalorimeterHit& etowHits = (*trackIter)->eemcHits();

         Bool_t pass = !etowHits.empty();

         if( pass ){
            const StMcVertex *stop = (*trackIter)->stopVertex();
            pass = ( !stop || stop->position().z() > kEEmcZPRE1 );

         };

         // fill tower energy and determine if above thres
         Float_t eTowEnergy = 0;
         EEmcEnergy_t *eemcEnergy = 0;
         if( pass ){
            // determine the total energy deposited in the towers from this particle
            for( hitIter = etowHits.begin(); hitIter != etowHits.end(); ++hitIter )
               eTowEnergy += (*hitIter)->dE();

            pass &= ( eTowEnergy > mTowEnergyThres );
         };

         if( pass ){
            // increment the counter
            ++incPartIdx;

            // add new object to the array
            eemcEnergy = static_cast< EEmcEnergy_t* >( new( (*mEEmcEnergyArr)[ incPartIdx ] ) EEmcEnergy_t() );

            // fill towers
            Float_t ePre1 = 0, ePre2 = 0, ePost = 0, eTow = 0;
            for( hitIter = etowHits.begin(); hitIter != etowHits.end(); ++hitIter ){
               Int_t sec = (*hitIter)->module() - 1;
               Int_t sub = (*hitIter)->sub() - 1;
               Int_t eta = (*hitIter)->eta() - 1;

               eemcEnergy->eTow.getByBin( sec, sub, eta ).energy = (*hitIter)->dE();
               ++(eemcEnergy->nTowers);
               eTow += (*hitIter)->dE();
            };

            // now fill pre and post shower layers
            {
               const StPtrVecMcCalorimeterHit& eprsHits = (*trackIter)->eprsHits();
               for( hitIter = eprsHits.begin(); hitIter != eprsHits.end(); ++hitIter ){
                  Int_t sec = (*hitIter)->module() - 1;
                  Int_t sub = ((*hitIter)->sub() - 1)%5;
                  Int_t eta = (*hitIter)->eta() - 1;
                  Int_t offset = ((*hitIter)->sub() - 1)/5;

                  ( offset ? ( offset == 2 ? eemcEnergy->ePost : eemcEnergy->ePre2 ) : eemcEnergy->ePre1 ).getByBin( sec, sub, eta ).energy = (*hitIter)->dE();
                  ( offset ? ( offset == 2 ? ePost : ePre2 ) : ePre1 ) += (*hitIter)->dE();
               };
            };

            // now fill smd layers
            std::vector< Double_t > meanU(kEEmcNumSectors,0), eU(kEEmcNumSectors,0), meanV(kEEmcNumSectors,0), eV(kEEmcNumSectors,0);

            {
               const StPtrVecMcCalorimeterHit& esmduHits = (*trackIter)->esmduHits();
               for( hitIter = esmduHits.begin(); hitIter != esmduHits.end(); ++hitIter ){
                  Int_t sec = (*hitIter)->module() - 1;
                  Int_t strip = (*hitIter)->eta() - 1;
                  eemcEnergy->eSmd.sec[sec].layer[0].strip[strip].energy = (*hitIter)->dE();
                  meanU[sec] += strip*(*hitIter)->dE();
                  eU[sec] += (*hitIter)->dE();
                  ++(eemcEnergy->nStrips);
               };
            };
            {
               const StPtrVecMcCalorimeterHit& esmdvHits = (*trackIter)->esmdvHits();
               for( hitIter = esmdvHits.begin(); hitIter != esmdvHits.end(); ++hitIter ){
                  Int_t sec = (*hitIter)->module() - 1;
                  Int_t strip = (*hitIter)->eta() - 1;
                  eemcEnergy->eSmd.sec[sec].layer[1].strip[strip].energy = (*hitIter)->dE();
                  meanV[sec] += strip*(*hitIter)->dE();
                  eV[sec] += (*hitIter)->dE();
                  ++(eemcEnergy->nStrips);
               };
            };

            Float_t uPos = -1, vPos = -1;
            Float_t maxE = 0, maxEu = 0, maxEv = 0;
            Short_t sector = 0;
            for( Int_t i=0; i<kEEmcNumSectors; ++i ){
               Float_t smdSum = eU[i]+eV[i];
               if( maxE < smdSum && eU[i] && eV[i] ){
                  maxE = smdSum;
                  maxEu = eU[i];
                  maxEv = eV[i];
                  uPos = static_cast< Float_t >( meanU[i] / eU[i] );
                  vPos = static_cast< Float_t >( meanV[i] / eV[i] );
                  sector = i;
               };
            };


            // Note: uPos and vPos are the energy weighted average
            // positions in the SMD in the sector with the most energy
            // deposited in the SMD.  Next, convert uPos, vPos to an x,y
            // position
            Float_t xPos = 0, yPos = 0;
            if( uPos > 0 && vPos > 0 )
               StEEmcPointMap_t::convertStripUVtoXY( sector, uPos, vPos, xPos, yPos );

            // Add McParticle_t to the TClonesArray
            new( (*mIncidentParticleArr)[ incPartIdx ] ) McParticle_t( getAncestorIdx( (*trackIter)->parent() ),
                                                                       getVertexIdx( (*trackIter)->startVertex() ),
                                                                       getVertexIdx( (*trackIter)->stopVertex() ),
                                                                       (*trackIter)->geantId(),
                                                                       (*trackIter)->pdgId(),
                                                                       (*trackIter)->energy(),
                                                                       TVector3( (*trackIter)->momentum().xyz() ),
                                                                       sector, uPos, vPos,
                                                                       maxEu, maxEv,
                                                                       TVector3( xPos, yPos, kEEmcZSMD ),
                                                                       eTow, ePre1, ePre2, ePost );
         } else {
            // It didn't pass as an incident particle.  However, if it
            // was still aimed somewhat towards the EEMC, add it to the list of
            // ancestors.

            // copy the momentum
            TVector3 mom( (*trackIter)->momentum().xyz() );
            Double_t eta = mom.Eta();

            // give a wide physics eta range
            if( eta > 0.5 && eta < 2.5 )
               getAncestorIdx( *trackIter ); // adds to the map if it is not there
         };
      };
   };

   if( !ierr && incPartIdx > -1 ){
      // first make a copy of the map
      std::map< const StMcTrack*, Int_t  > ancestorMapCopy( mAncestorMap );

      // an iterator
      std::map< const StMcTrack*, Int_t  >::iterator ancestorMapIter;

      // next, gather all the ancestors into the main map
      for( ancestorMapIter = ancestorMapCopy.begin(); ancestorMapIter != ancestorMapCopy.end(); ++ancestorMapIter ){
         UInt_t mapSize = mAncestorMap.size();
         const StMcTrack *parent = ancestorMapIter->first->parent();

         // keep adding parents of parents, until connecting with an
         // existing line or until there are no more parents.
         if( parent ){
            do {
               mapSize = mAncestorMap.size();  // size before possible increase
               getAncestorIdx( parent );       // adds to the map if it is not there
               parent = parent->parent();      // update the pointer for the (possible) next loop
            } while( parent && mapSize != mAncestorMap.size() );
         };
      };

      // now that the ancestors are gathered, lets fill the TClonesArray
      for( ancestorMapIter = mAncestorMap.begin(); ancestorMapIter != mAncestorMap.end(); ++ancestorMapIter ){
         Int_t           idx    = ancestorMapIter->second;
         const StMcTrack *track = ancestorMapIter->first;

         if( idx > -1 && track )
            new( (*mAncestorParticleArr)[ idx ] ) McParticle_t( getAncestorIdx( track->parent() ),
                                                                getVertexIdx( track->startVertex() ),
                                                                getVertexIdx( track->stopVertex() ),
                                                                track->geantId(),
                                                                track->pdgId(),
                                                                track->energy(),
                                                                TVector3( track->momentum().xyz() ),
                                                                0, 0, 0, 0, 0, TVector3(),
                                                                0, 0, 0, 0 );
      };

      // lastly, we fill the TClonesArray of vertices
      for( mVertexMapIter = mVertexMap.begin(); mVertexMapIter != mVertexMap.end(); ++mVertexMapIter ){
         Int_t             idx    = mVertexMapIter->second;
         const StMcVertex *vertex = mVertexMapIter->first;

         if( idx > -1 && vertex )
            new( (*mVertexArr)[ idx ] ) TVector3( vertex->position().xyz() );
      };

//       cout << "Event " << GetEventNumber() << " arrays of size "
//            << mEEmcEnergyArr->GetEntriesFast() << ' '
//            << mIncidentParticleArr->GetEntriesFast() << ' '
//            << mAncestorParticleArr->GetEntriesFast() << ' '
//            << mVertexArr->GetEntriesFast() << endl;
   };

   // fill in any case, so that the entry number corresponds, even if
   // errors earlier
   mTree->Fill();

   return ierr;
};

/// Clear for next event
void StMcEEmcTreeMaker_t::Clear(Option_t *opts ){
   mEEmcEnergyArr->Clear("C");
   mAncestorParticleArr->Clear("C");
   mIncidentParticleArr->Clear("C");
   mVertexArr->Clear("C");

   mAncestorMap.clear();
   mVertexMap.clear();
};

/// Write everything to file
Int_t StMcEEmcTreeMaker_t::Finish(){
   if( mIOStat == WRITE ){
      mFile->Write();
      mFile->Close();
   };

   return kStOk;
};

/// modifiers
void StMcEEmcTreeMaker_t::setTreeStatus( iostatus_t iostatus, const Char_t* fileName ){
   mIOStat = iostatus;
   mFilename = fileName;
};

void StMcEEmcTreeMaker_t::setMaxNumEvents( Int_t maxNum ){
   mMaxNumEvents = maxNum;
};

// particles leaving less energy than this value in the ETOW are ignored.
void StMcEEmcTreeMaker_t::setEnergyThreshold( Float_t val ){
   mTowEnergyThres = val;
};


Int_t StMcEEmcTreeMaker_t::getVertexIdx( const StMcVertex* vertex ){
   Int_t idx = -1;

   if( vertex && vertex->position().z() > -900 ){
      mVertexMapIter = mVertexMap.find( vertex );
      if( mVertexMapIter != mVertexMap.end() ) {
         idx = mVertexMapIter->second;
      } else {
         idx = mVertexMap.size();
         mVertexMap[ vertex ] = idx;
      };
   };

   return idx;
};

Int_t StMcEEmcTreeMaker_t::getAncestorIdx( const StMcTrack* track ){
   Int_t idx = -1;

   if( track ){
      mAncestorMapIter = mAncestorMap.find( track );
      if( mAncestorMapIter != mAncestorMap.end() ) {
         idx = mAncestorMapIter->second;
      } else {
         idx = mAncestorMap.size();
         mAncestorMap[ track ] = idx;
      };
   };

   return idx;
};

ClassImp( StMcEEmcTreeMaker_t );

/*
 * $Id: StMcEEmcTreeMaker.cxx,v 1.2 2013/03/19 18:49:08 sgliske Exp $
 * $Log: StMcEEmcTreeMaker.cxx,v $
 * Revision 1.2  2013/03/19 18:49:08  sgliske
 * added Bjorken x1 and x2
 *
 * Revision 1.1  2012/11/26 19:06:10  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */
