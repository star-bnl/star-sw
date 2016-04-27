#include "StChain.h"

#include "TFile.h"
#include "TTree.h"
#include "StSpinPool/StJetSkimEvent/StPythiaEvent.h"

#include "StNNPDFPythiaEventMaker.h"
#include "StNNPDFAsymMaker.h"

ClassImp(StNNPDFPythiaEventMaker);

void StNNPDFPythiaEventMaker::Clear(Option_t* option)
{
  mPythiaEvent->Clear(option);
}

int StNNPDFPythiaEventMaker::Init()
{
  assert(!mFileName.IsNull());
  mFile = TFile::Open(mFileName,"recreate");
  assert(mFile);
  StNNPDFAsymMaker *asymMaker = dynamic_cast<StNNPDFAsymMaker*>(GetMaker("NNPDFAsym"));
  assert(asymMaker);
  if(asymMaker) mPythiaEvent = asymMaker->pythiaEvent();
  //LOG_INFO<<mPythiaEvent<<endm;
  mTree = new TTree("PythiaTree","Pythia Record");
  mTree->Branch("PythiaBranch","StPythiaEvent",&mPythiaEvent);
  return kStOk;
}

int StNNPDFPythiaEventMaker::Make()
{
  mTree->Fill();

  return kStOk;
}

int StNNPDFPythiaEventMaker::Finish()
{
  mFile->Write();
  mFile->Close();
  return kStOk;
}
