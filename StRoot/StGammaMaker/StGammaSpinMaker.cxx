#include "StGammaSpinMaker.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StGammaEventMaker.h"
#include   "StGammaEvent.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

ClassImp(StGammaSpinMaker);

// ---------------------------------------------------------------------
StGammaSpinMaker::StGammaSpinMaker(const Char_t *name):StMaker(name)
{
  mSpinDb=0;
}

// ---------------------------------------------------------------------
Int_t StGammaSpinMaker::Init()
{
  return StMaker::Init();
}

Int_t StGammaSpinMaker::Finish()
{
  return kStOK;
}

// ---------------------------------------------------------------------

Int_t StGammaSpinMaker::Make()
{

  StMuDst *mudst = (StMuDst*)GetDataSet("MuDst");
  if ( !mudst )
    {
      LOG_DEBUG<<" +++++ MuDst is missing from the chain +++++" << endm;
      return kStFatal;
    }

  StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMakerInheritsFrom("StGammaEventMaker");
  if ( !gemaker ) 
    {
      LOG_DEBUG<<" +++++ gamme event maker is missing from the chain +++++" << endm;
      return kStFatal;
    }

  StGammaEvent *gevent = gemaker->event();
  if ( !gevent )
    {
      LOG_DEBUG<<" ++++ something is screwy, gamma event maker but no gamma event? ++++" << endm;
      return kStFatal;
    }

  //
  // BBC timebin information
  //
  gevent->SetDsmVertex(  mudst -> event() -> bbcTriggerDetector().onlineTimeDifference() );

  mSpinDb = (StSpinDbMaker *)GetMakerInheritsFrom("StSpinDbMaker");
  if ( !mSpinDb )
    {
      LOG_WARN<<" +++++ spindb maker not in the chain, so why am I in the chain? +++++" << endm;
      return kStOK;
    }

  Bool_t valid = mSpinDb->isValid();
  if ( !valid )
    {
      LOG_WARN<<Form(" ++++ spindb reports invalid for run=%i event=%i",GetRunNumber(),GetEventNumber())<<endm;
    }


  //
  // Obtain spin information and populate gamma tree w/ it
  //
  
  StMuEvent   *event = mudst -> event();
  StL0Trigger *trig  =&(event->l0Trigger());

  StMuTriggerIdCollection tic = event -> triggerIdCollection();
  StTriggerId l1trig = tic.l1();

  UShort_t bx48   = (UShort_t)trig->bunchCrossingId();
  UShort_t bx7    = (UShort_t)trig->bunchCrossingId7bit( event->runNumber() );
  UShort_t bxStar = (UShort_t)mSpinDb->BXyellowUsingBX48(bx48);

  if ( mSpinDb -> isMaskedUsingBX48(bx48) ) // bunch crossing is masked out this event/run
    return kStOK;

  if ( mSpinDb->offsetBX48minusBX7(bx48,bx7)!=0 ) {
    LOG_WARN<<" ++++ spindb indicates 7bit and 48bit bunch crossings are inconsistent... event invalid ++++" <<endm;
    return kStOK; // returns and leaves spin info in an "invalidated" state
  }

  Int_t spin4 = mSpinDb->spin4usingBX48(bx48);

  //
  // If we got here, set the spin information
  //
  gevent -> SetValidDb(true);
  gevent -> SetSpin4( (UShort_t)spin4 );
  gevent -> SetBunchCrossing7bit( (UShort_t)bx7 );
  gevent -> SetBunchCrossing48bit( (UShort_t)bx48 );
  gevent -> SetBunchCrossingStar( (UShort_t)bxStar );

  if ( mSpinDb->isPolDirLong() ) 
    gevent -> SetPolarizationType( StGammaEvent::kLongLong );
  if ( mSpinDb->isPolDirTrans() )
    gevent -> SetPolarizationType( StGammaEvent::kTransTrans );

  return kStOK;

}

// ---------------------------------------------------------------------
void StGammaSpinMaker::Clear(Option_t *opts)
{
}
