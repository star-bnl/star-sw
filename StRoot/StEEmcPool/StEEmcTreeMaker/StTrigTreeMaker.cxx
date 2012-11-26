/*
 * Created by S. Gliske, Aug 2012
 *
 * Description: see header
 *
 */


//#define DEBUG

#include <Rtypes.h>
#include <TTree.h>
#include <TFile.h>

#include <set>
#include <vector>

#include "StTrigTreeMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include "StRoot/StEEmcPool/./EEmcTreeContainers/TrigSet.h"

StTrigTreeMaker_t::StTrigTreeMaker_t( const Char_t *myName, const Char_t* filenameOut, Bool_t isMC, Bool_t useSimu )
   : StMaker( myName ), mIsMC( isMC ), mUseSimuTrg( useSimu ), mFilename( filenameOut), mFile( 0 ), mTree( 0 ), mTrigSet( 0 ) { /* */ };

/// deconstructor
StTrigTreeMaker_t::~StTrigTreeMaker_t(){
   if( mTrigSet )
      delete mTrigSet;
   if( mFile )
      delete mFile;
   // no need to delete the TTree after deleting the TFile
};

/// Initialize
Int_t StTrigTreeMaker_t::Init(){

#ifdef DEBUG
   cout << "--------------------------------------------------> StTrigTreeMaker_t::Init() <--------------------------------------------------" << endl;
#endif

   Int_t ierr = kStOk;

   // check trigger set
   if( mTriggerSet.empty() ){
      LOG_FATAL << "ERROR: trigger set is empty." << endm;
      LOG_FATAL << "If you really want to process all triggers, enter a single trigger ID of -999." << endm;
      ierr = kStFatal;
   } else if (mTriggerSet.size() == 1 && mTriggerSet.count( -999 ) ) {
      // user has flagged to process all triggers
      mTriggerSet.clear();
   };

   if( !ierr ){
      mFile = TFile::Open( mFilename.data(), "RECREATE" );
      assert( mFile && mFile->IsOpen() );
      if( mFile->IsOpen() ){
         LOG_INFO << "Opened file '" << mFilename << "' for writing" << endm;
      } else {
         LOG_FATAL << "Error opening file '" << mFilename << "'" << endm;
         ierr = kStFatal;
      };
   };

   if( !ierr ){
      if( mIsMC )
         mTree = new TTree( "tree", "StMcTrigTree" );
      else
         mTree = new TTree( "tree", "StTrigTree" );

      mTrigSet = new TrigSet();
      mTree->Branch( "trigSet", &mTrigSet, 32000, 1 );
   };

#ifdef DEBUG
   cout << "--------------------------------------------------> end of StTrigTreeMaker_t::Init() <--------------------------------------------------" << endl;
#endif

   return ierr;
};


/// Build an event
Int_t StTrigTreeMaker_t::Make(){

#ifdef DEBUG
   cout << "--------------------------------------------------> StTrigTreeMaker_t::Make() <--------------------------------------------------" << endl;
#endif

   mTrigSet->Clear();
   Bool_t passesCuts = 1;

   if( mIsMC || mUseSimuTrg ){
      StTriggerSimuMaker *trgSimMkr= (StTriggerSimuMaker*) StMaker::GetChain()->GetMaker("StarTrigSimu");
      assert(trgSimMkr);

      if( !mTriggerSet.empty() ){
         passesCuts = 0;

         std::set< UInt_t >::iterator iter = mTriggerSet.begin();
         for( ; iter != mTriggerSet.end() && !passesCuts; ++iter )
            passesCuts = trgSimMkr->isTrigger( (*iter) );
      };

      if( passesCuts ){
         // only copy Ids not vetoed
         std::vector< UInt_t > trigIds;
         const std::vector< int >& trigL0 = trgSimMkr->triggerIds();

         Int_t id = 0;
         for( UInt_t i=0; i<trigL0.size(); ++i )
            if( trgSimMkr->isTrigger( ( id = trigL0[i] ) ) )
               trigIds.push_back( static_cast< UInt_t >( id ) );

         mTrigSet->insert( trigIds );
      };

   } else {

      const StMuDst* muDst = (const StMuDst*)GetInputDS( "MuDst" );
      if( muDst ){
         StMuEvent *event = muDst->event();

         if( event ){
            const StTriggerId& l1trig = event->triggerIdCollection().l1();

            if( !mTriggerSet.empty() ){
               passesCuts = 0;

               std::set< UInt_t >::iterator iter = mTriggerSet.begin();
               for( ; iter != mTriggerSet.end() && !passesCuts; ++iter )
                  passesCuts = l1trig.isTrigger( (*iter) );
            };

            if( passesCuts ){
               const std::vector< UInt_t > &trigIds = l1trig.triggerIds();
               mTrigSet->insert( trigIds );
            };
         };
      };
   };

#ifdef DEBUG
   cout << "--------------------------------------------------> fill (" << passesCuts << ") <--------------------------------------------------" << endl;
#endif

   if( passesCuts )
      mTree->Fill();

#ifdef DEBUG
   cout << "--------------------------------------------------> end of StTrigTreeMaker_t::Make() <--------------------------------------------------" << endl;
#endif

   return kStOK;
};

/// Clear for next event
void StTrigTreeMaker_t::Clear(Option_t *opts ){
#ifdef DEBUG
   cout << "--------------------------------------------------> StTrigTreeMaker_t::Clear() <--------------------------------------------------" << endl;
#endif

   if( mTrigSet )
      mTrigSet->Clear();

#ifdef DEBUG
   cout << "--------------------------------------------------> end of StTrigTreeMaker_t::Clear() <--------------------------------------------------" << endl;
#endif

};

/// Write everything to file
Int_t StTrigTreeMaker_t::Finish(){

#ifdef DEBUG
   cout << "--------------------------------------------------> StTrigTreeMaker_t::Finish() <--------------------------------------------------" << endl;
#endif

   mFile->Write();
   mFile->Close();

#ifdef DEBUG
   cout << "--------------------------------------------------> end of StTrigTreeMaker_t::Finish() <--------------------------------------------------" << endl;
#endif

   return kStOk;
};


ClassImp( StTrigTreeMaker_t );

/*
 * $Id: StTrigTreeMaker.cxx,v 1.1 2012/11/26 19:06:11 sgliske Exp $
 * $Log: StTrigTreeMaker.cxx,v $
 * Revision 1.1  2012/11/26 19:06:11  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 *
 */
