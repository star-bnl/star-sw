/*
 * Created by S. Gliske, May 2012
 *
 * Description: see header
 *
 */

#include <Rtypes.h>

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include "StTrigCounter.h"

StTrigCounter::StTrigCounter( const Char_t *myName, Int_t trigID ) : StMaker( myName ), mTrigID( trigID ), mNumEvents(0), mNumEventsInCut(0) {
   // nothing else
};


/// deconstructor
StTrigCounter::~StTrigCounter(){
   // nothing else
};

/// Initialize
Int_t StTrigCounter::Init(){
   Int_t ierr = kStOk;

   // nothing else

   return ierr;
};


/// Build an event
Int_t StTrigCounter::Make(){
   Int_t ierr = kStOk;

   // check trigger
   const StMuDst* muDst = (const StMuDst*)GetInputDS( "MuDst" );
   if( muDst ){
      StMuEvent *event = muDst->event();

      if( event && event->triggerIdCollection().nominal().isTrigger( mTrigID ) ){
         ++mNumEvents;

         UInt_t bbcOnlineTimeDiff = event->bbcTriggerDetector().onlineTimeDifference()/32;
         if( bbcOnlineTimeDiff >= 6 && bbcOnlineTimeDiff <= 10 )
            ++mNumEventsInCut;
      };
   };

   return ierr;
};

/// Clear for next event
void StTrigCounter::Clear(Option_t *opts ){
   // nothing to do
};

void StTrigCounter::printStatus(){
   cout << "status: event " << GetEventNumber() << ", counts for trigger ID " << mTrigID << " are " << mNumEventsInCut << " and " << mNumEvents << endl;
};

/// Output to console
Int_t StTrigCounter::Finish(){
   cout << "RESULT: Counts for trigger ID " << mTrigID << " are " << mNumEventsInCut << " and " << mNumEvents << endl;

   return kStOk;
};


/// get values
UInt_t StTrigCounter::getTrigID() const {
   return mTrigID;
};

Int_t StTrigCounter::getNumEventsTot() const {
   return mNumEvents;
};

Int_t StTrigCounter::getNumEventsInCut() const {
   return mNumEventsInCut;
};

ClassImp( StTrigCounter );

/*
 * $Id: StTrigCounter.cxx,v 1.2 2013/02/28 23:37:18 sgliske Exp $
 * $Log: StTrigCounter.cxx,v $
 * Revision 1.2  2013/02/28 23:37:18  sgliske
 * Updated so result of StTrigCounter gets saved in EEmcTree Part1
 * rather than just being output to the console (log file)
 *
 * Revision 1.1  2012/12/17 20:58:56  sgliske
 * first added to official StEEmcPool
 *
 *
 */
