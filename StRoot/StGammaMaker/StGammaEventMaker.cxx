#include "TChain.h"

#include "StGammaPythiaEvent.h"
#include "StGammaPythiaEventMaker.h"
#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StGammaScheduleMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

ClassImp(StGammaEventMaker);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaEventMaker::StGammaEventMaker(const char *name): StMaker(name)
{}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaEventMaker::~StGammaEventMaker()
{}

//////////////////////////////////////////////////
//                 Maker Init                   //
//////////////////////////////////////////////////
Int_t StGammaEventMaker::Init()
{

    // Instantaite a new StGammaEvent
    mGammaEvent = new StGammaEvent();
    
    // Retrieve StGammaPythiaEventMaker from the chain
    mPythia = 0;
    mPythiaMaker = dynamic_cast<StGammaPythiaEventMaker*>(GetMakerInheritsFrom("StGammaPythiaEventMaker"));
    if(mPythiaMaker) 
    {
        mPythia = new StGammaPythiaEvent;
        mPythiaMaker->SetPythia(mPythia);
        mGammaEvent->SetPythia(mPythia);
    }
    
    AddObj(mGammaEvent, ".data"); // ok, but what can I do with this?
    
    // Retrieve StMuDstMaker from the chain
    muDstMaker  = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
    assert(muDstMaker);
    
    return StMaker::Init();
}

//////////////////////////////////////////////////
//                 Maker Clear                  //
//////////////////////////////////////////////////
void StGammaEventMaker::Clear(Option_t *opts)
{
    mGammaEvent->Clear(opts);
    StMaker::Clear(opts);
}


//////////////////////////////////////////////////
//                  Maker Make                  //
//////////////////////////////////////////////////
Int_t StGammaEventMaker::Make()
{

    // Retrieve MuDst
    if(!GetDataSet("MuDst")) 
    {
        LOG_WARN << "No MuDst" << endm;
        return kStFatal;
    }

    // Retrieve the primary vertex, or set
    // vertex to zero if none is found
    StMuPrimaryVertex *pv = StMuDst::primaryVertex();
    if(pv)
    {
        mGammaEvent->SetVertex(TVector3(pv->position().xyz()));
        mGammaEvent->mFlags |= TPC_VERTEX;
    }
    else
    {
        mGammaEvent->SetVertex(TVector3(0.,0.,0.));
        mGammaEvent->mFlags |= !(TPC_VERTEX);
    }

    mGammaEvent->SetRunNumber( StMuDst::event()->runNumber() );
    mGammaEvent->SetEventNumber( StMuDst::event()->eventNumber() );
    mGammaEvent->SetMudstFileName( muDstMaker->chain()->GetFile()->GetName() );
    mGammaEvent->SetMagneticField( StMuDst::event()->magneticField() );
    mGammaEvent->SetTriggerIds( StMuDst::event()->triggerIdCollection().nominal().triggerIds() );
    
    // Store timestamp index in place of run number in simulation
    StGammaScheduleMaker *scheduler = dynamic_cast<StGammaScheduleMaker*>(GetMakerInheritsFrom("StGammaScheduleMaker"));
    if(scheduler) mGammaEvent->SetRunNumber(scheduler->index());
    
    return kStOK;
  
}
