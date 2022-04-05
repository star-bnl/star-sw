#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StGammaEventMaker.h"
#include "StGammaEvent.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

#include "StGammaSpinMaker.h"

ClassImp(StGammaSpinMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaSpinMaker::StGammaSpinMaker(const Char_t *name): StMaker(name), mSpinDb(0)
{}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaSpinMaker::~StGammaSpinMaker()
{}

//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaSpinMaker::Make()
{

    StMuDst *mudst = (StMuDst*)GetDataSet("MuDst");
    if(!mudst)
    {
        LOG_DEBUG << "Make() - No MuDst found!" << endm;
        return kStFatal;
    }

    StGammaEventMaker *mGammaEventMaker = dynamic_cast<StGammaEventMaker*>(GetMakerInheritsFrom("StGammaEventMaker"));
    if(!mGammaEventMaker) 
    {
        LOG_DEBUG << "Make() - No StGammaEventMaker found!" << endm;
        return kStFatal;
    }

    StGammaEvent *mGammaEvent = mGammaEventMaker->event();
    if(!mGammaEvent)
    {
        LOG_DEBUG << "Make() - StGammaEventMaker contains no StGammaEvent!" << endm;
        return kStFatal;
    }
    
    // Retrieve BBC timebin information
    mGammaEvent->SetDsmVertex( mudst->event()->bbcTriggerDetector().onlineTimeDifference() );
    
    mSpinDb = dynamic_cast<StSpinDbMaker*>(GetMakerInheritsFrom("StSpinDbMaker"));
    if (!mSpinDb)
    {
      LOG_WARN << "Make() - No StSpinDbMaker found!  No spin information will be stored." << endm;
      return kStOK;
    }
    
    if(!mSpinDb->isValid())
    {
        LOG_WARN << Form("++++ spindb reports invalid for run=%i event=%i", GetRunNumber(), GetEventNumber()) << endm;
    }


    // Populate StGammaTree with spin information
    StMuEvent   *event = mudst -> event();
    StL0Trigger *trig  =&(event->l0Trigger());
    
    StMuTriggerIdCollection tic = event -> triggerIdCollection();
    StTriggerId l1trig = tic.l1();
    
    UShort_t bx48   = (UShort_t)trig->bunchCrossingId();
    UShort_t bx7    = (UShort_t)trig->bunchCrossingId7bit( event->runNumber() );
    UShort_t bxStar = (UShort_t)mSpinDb->BXyellowUsingBX48(bx48);

    // If bunch crossing is masked out skip event
    if( mSpinDb->isMaskedUsingBX48(bx48) ) return kStOK;

    if( mSpinDb->offsetBX48minusBX7(bx48,bx7) != 0 ) 
    {
        LOG_WARN << " ++++ spindb indicates 7bit and 48bit bunch crossings are inconsistent... event invalid ++++" << endm;
        return kStOK; // returns and leaves spin info in an "invalidated" state
    }

    Int_t spin4 = mSpinDb->spin4usingBX48(bx48);

    // Finanlly, store the spin information
    mGammaEvent->SetValidDb(true);
    mGammaEvent->SetSpin4( (UShort_t)spin4 );
    mGammaEvent->SetBunchCrossing7bit( (UShort_t)bx7 );
    mGammaEvent->SetBunchCrossing48bit( (UShort_t)bx48 );
    mGammaEvent->SetBunchCrossingStar( (UShort_t)bxStar );
    
    if( mSpinDb->isPolDirLong() )  mGammaEvent->SetPolarizationType( StGammaEvent::kLongLong );
    if( mSpinDb->isPolDirTrans() ) mGammaEvent->SetPolarizationType( StGammaEvent::kTransTrans );
    
    return kStOK;

}

