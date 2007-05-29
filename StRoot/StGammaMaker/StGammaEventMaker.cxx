
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
  mGammaEvent=new StGammaEvent();
  AddObj(mGammaEvent,".data"); // ok, but what can I do with this?
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

  StMuPrimaryVertex *pv = StMuDst::primaryVertex();
  if ( pv )
    {
      mGammaEvent->SetVertex(TVector3(pv->position().x(),pv->position().y(),pv->position().z()));
      mGammaEvent->mFlags |= TPC_VERTEX;
    }
  else
    {
      mGammaEvent->SetVertex(TVector3(0.,0.,0.));
      mGammaEvent->mFlags |= !(TPC_VERTEX);
    }

  mGammaEvent -> SetRunNumber ( StMuDst::event()->runNumber() );
  mGammaEvent -> SetEventNumber( StMuDst::event()->eventNumber() );

  return kStOK;
}
// -------------------------------------------------------------------
void StGammaEventMaker::Clear(Option_t *opts)
{
  mGammaEvent->Clear();
}

