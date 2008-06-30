#include "StGammaTreeMaker.h"

#include <TFile.h>
#include <TTree.h>

#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StGammaEventMaker.h"
#include  "StGammaEvent.h"

#include "StGammaRawMaker.h"
#include "TObjString.h"

ClassImp(StGammaTreeMaker);
ClassImp(StGammaTreeVersion);

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
    mGammaTree=new TTree("gammas","Gamma TTree $Id: StGammaTreeMaker.cxx,v 1.9 2008/06/30 16:34:13 jwebb Exp $ built "__DATE__" "__TIME__);    
    mGammaTree->SetDirectory(mGammaFile);
  }

  StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMakerInheritsFrom("StGammaEventMaker");
  assert(gemaker);

  mGammaEvent = gemaker->event();
  mGammaTree->Branch("GammaEvent",&mGammaEvent,32000,99);
  mGammaTree->BranchRef();  // to ensure reference table is saved

  // Generate versioning information for each type of storage container
  mVersion.mStorageTags.push_back( TString(mGammaEvent->GetCVS()) );
  mVersion.mStorageTags.push_back( TString(mGammaEvent->newCandidate()->GetCVS()) );
  mVersion.mStorageTags.push_back( TString(mGammaEvent->newTower()->GetCVS()) );
  mVersion.mStorageTags.push_back( TString(mGammaEvent->newTrack()->GetCVS()) );
  mVersion.mStorageTags.push_back( TString(mGammaEvent->newStrip()->GetCVS()) );
  mVersion.mStorageTags.push_back( TString(mGammaEvent->candidate(0)->smdFit().GetCVS() ) ); 
  // Clear the current event
  mGammaEvent->Clear();

  assert(mGammaFile);
  assert(mGammaTree);

  return StMaker::Init();

}

Int_t StGammaTreeMaker::Finish()
{

  // Write QA information to log files AND append information
  // to an object which we will save w/in the tree file

  TString cvstag=GetCVS();
  //  LOG_QA << "[StGamma] " << cvstag.Data() << endm;
  TIter next( GetParentChain()->GetMakeList());
  mVersion.mMakerTags.push_back( cvstag );
  StMaker *maker;
  while ((maker = (StMaker* )next())) {
    cvstag=maker->GetCVS();
    //    LOG_QA << "[StGamma] " << cvstag.Data() << endm;
    mVersion.mMakerTags.push_back( cvstag );
  }
  LOG_QA<<" -- StGammaMaker versioning information follows --" << endm;
  mVersion.print();
  mGammaFile->cd();
  mVersion.Write();
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

// ---------------------------------------------------------------------
void StGammaTreeVersion::print()
{

  std::cout << "-- List of makers --" << std::endl << std ::endl;
  for ( UInt_t i=0;i<mMakerTags.size();i++ )
    {
      std::cout << Form("[%i]: %s",(int)i,mMakerTags[i].Data()) << std::endl;
    }

  std::cout << std::endl;
  std::cout << "-- List of containers --" << std::endl << std ::endl;
  for ( UInt_t i=0;i<mStorageTags.size();i++ )
    {
      std::cout << Form("[%i]: %s",(int)i,mStorageTags[i].Data()) << std::endl;
    }

}
