#include "StGammaTreeMaker.h"

#include <TFile.h>
#include <TTree.h>

#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StGammaEventMaker.h"
#include  "StGammaEvent.h"

#include "StGammaRawMaker.h"



ClassImp(StGammaTreeMaker);

// ---------------------------------------------------------------------
StGammaTreeMaker::StGammaTreeMaker(const Char_t *name):StMaker(name)
{
  mFilename   = "gammas.gtree.root";
  mGammaFile  = 0;
  mGammaTree  = 0;
  mGammaEvent = 0;
}

// ---------------------------------------------------------------------
Int_t StGammaTreeMaker::Init()
{
  if ( !mGammaFile )
    mGammaFile=new TFile(mFilename,"RECREATE");
  if ( !mGammaTree ) {
    mGammaTree=new TTree("gammas","Gamma TTree $Id: StGammaTreeMaker.cxx,v 1.7 2008/06/11 20:49:35 pibero Exp $ built "__DATE__" "__TIME__);    
    mGammaTree->SetDirectory(mGammaFile);
  }

  StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMakerInheritsFrom("StGammaEventMaker");
  assert(gemaker);

  mGammaEvent = gemaker->event();
  mGammaTree->Branch("GammaEvent",&mGammaEvent,32000,99);
  mGammaTree->BranchRef(); // to ensure reference table is saved

  assert(mGammaFile);
  assert(mGammaTree);

  return StMaker::Init();

}

Int_t StGammaTreeMaker::Finish()
{
  mGammaFile->cd();
  mGammaTree->Write();
  return kStOK;
}

// ---------------------------------------------------------------------



Int_t StGammaTreeMaker::Make()
{
  // write out the tree
  mGammaTree->Fill();
  return kStOK;

}

// ---------------------------------------------------------------------
void StGammaTreeMaker::Clear(Option_t *opts)
{
  mGammaEvent->Clear();
  StMaker::Clear(opts);
}
