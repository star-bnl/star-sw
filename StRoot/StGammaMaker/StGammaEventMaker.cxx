#include "TChain.h"

#include "StGammaPythiaEvent.h"
#include "StGammaPythiaEventMaker.h"
#include "StGammaEventMaker.h"
#include "StGammaEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

ClassImp(StGammaEventMaker);

// -------------------------------------------------------------------
StGammaEventMaker::StGammaEventMaker(const Char_t *name):StMaker(name) { /* nada */ }

// -------------------------------------------------------------------
Int_t StGammaEventMaker::Init()
{
  mGammaEvent = new StGammaEvent;
  mPythia = 0;
  mPythiaMaker = (StGammaPythiaEventMaker*)GetMakerInheritsFrom("StGammaPythiaEventMaker");
  if (mPythiaMaker) {
    mPythia = new StGammaPythiaEvent;
    mPythiaMaker->SetPythia(mPythia);
    mGammaEvent->SetPythia(mPythia);
  }
  AddObj(mGammaEvent,".data"); // ok, but what can I do with this?
 
  // Instantiate mDustMaker
  muDstMaker  = dynamic_cast<StMuDstMaker*>(GetMakerInheritsFrom("StMuDstMaker"));
  assert(muDstMaker);

  return StMaker::Init();
}

// -------------------------------------------------------------------
Int_t StGammaEventMaker::Make()
{

  if ( !GetDataSet("MuDst") )
    {
      LOG_DEBUG<<" +++++ MuDst is missing from the chain +++++" << endm;
      return kStFatal;
    }

  mGammaEvent -> mFlags |= StMuDst::numberOfPrimaryVertices() ? TPC_VERTEX : !TPC_VERTEX;
  mGammaEvent -> SetVertex( StMuDst::event()->primaryVertexPosition().xyz() );
  mGammaEvent -> SetRunNumber( StMuDst::event()->runNumber() );
  mGammaEvent -> SetEventNumber( StMuDst::event()->eventNumber() );
  mGammaEvent -> SetMudstFileName( muDstMaker->chain()->GetFile()->GetName() );
  mGammaEvent -> SetMagneticField( StMuDst::event()->magneticField() );
  mGammaEvent -> SetTriggerIds( StMuDst::event()->triggerIdCollection().nominal().triggerIds() );

  return kStOK;
}

// -------------------------------------------------------------------
void StGammaEventMaker::Clear(Option_t *opts)
{
  mGammaEvent->Clear(opts);
  StMaker::Clear(opts);
}
