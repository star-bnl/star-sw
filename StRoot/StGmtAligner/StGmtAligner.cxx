#include "StGmtAligner.h"
#include <stdlib.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TFile.h"
#include "TKey.h"
#include "TRandom.h"
#include "TTree.h"
#include "TBranch.h"
#include "TStopwatch.h"
#include "StThreeVectorF.hh"
#include "StMatrixF.hh"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TMath.h"
#include "TVector3.h"
#include "TProcessID.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StBFChain.h"
#include "TGeoMatrix.h"
#include "EventT.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/StGmtSurveyC.h"
//________________________________________________________________________________
StGmtAligner::StGmtAligner(const Char_t *name) : StMaker(name),fFile(0), fTree(0), fEvent(0) {
  SetMinNoHits();
  SetpCut();
  SetOut();
}
//________________________________________________________________________________
Int_t StGmtAligner::Init() {
  SetTree();
  return kStOK;
}
//________________________________________________________________________________
Int_t StGmtAligner::Finish() {
  if (fFile) {
    fFile = fTree->GetCurrentFile(); //just in case we switched to a new file
    fFile->Write();
    fTree->Print();
  }
  return kStOK;
}

//________________________________________________________________________________
void StGmtAligner::SetTree() {
  StBFChain *chain = (StBFChain *) StMaker::GetChain();
  if (! chain) return;
  // root.exe 
  Int_t split  = 99;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  Int_t bufsize;
  //Authorize Trees up to 2 Terabytes (if the system can do it)
  TTree::SetMaxTreeSize(1000*Long64_t(2000000000));
  TFile *f = GetTFile();
  if (f) f->cd();
  else {
    TString FName(fOut);
    if (fMinNoHits > 0) FName += Form("_%i_%f2.1_",fMinNoHits,fpCut);
    FName += gSystem->BaseName(chain->GetFileIn().Data());
    FName.ReplaceAll("st_physics","");
    FName.ReplaceAll(".event","");
    FName.ReplaceAll(".daq",".root");
    fFile = new TFile(FName,"RECREATE","TTree with SVT + SSD hits and tracks");
    fFile->SetCompressionLevel(comp);
  }
  // Create a ROOT Tree and one superbranch
  fTree = new TTree("T","TTree with SVT + SSD hits and tracks");
  fTree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  bufsize = 64000;
  if (split)  bufsize /= 4;
  fEvent = new EventT();
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = fTree->Branch("EventT", &fEvent, bufsize,split);
  branch->SetAutoDelete(kFALSE);
}
//________________________________________________________________________________
Int_t StGmtAligner::Make() {
  if (! EventT::RotMatrices()) MakeListOfRotations();
  StEvent* pEvent = (StEvent*) GetInputDS("StEvent");
  if (pEvent && !fEvent->Build(pEvent,fpCut)) fTree->Fill();  //fill the tree
  return kStOK;
}
//________________________________________________________________________________
void StGmtAligner::Print(Option_t *opt) const {
  if (! EventT::RotMatrices()) return;
  TIter next(EventT::RotMatrices());
  TGeoHMatrix *comb = 0;
  while ((comb = (TGeoHMatrix *) next())) {
    Int_t Id;
    sscanf(comb->GetName()+1,"%04i",&Id);
    Int_t Ladder = Id%100;
    Int_t Layer  = Id/1000; if (Layer > 7) Layer = 7;
    Int_t Wafer  = (Id - 1000*Layer)/100;
    cout << comb->GetName() << "\tLayer/Ladder/Wafer = " << Layer << "/" << Ladder << "/" << Wafer << endl;
    comb->Print();
    cout << "=================================" << endl;
  }
}
//________________________________________________________________________________
void StGmtAligner::MakeListOfRotations() {
  if (EventT::RotMatrices()) return;
  THashList *rotMHash = new THashList(100,0);
  EventT::SetRotMatrices(rotMHash);
  //  THashList *hash = 0;
  const TGeoHMatrix& tpc2Glob = gStTpcDb->Tpc2GlobalMatrix();
  for(int module=0;module<  kGmtNumModules;module++)    {
    TGeoHMatrix *WL            = new TGeoHMatrix(StGmtOnModule::instance()->GetMatrix(module));
    WL->SetName(Form("WL%i",module));
    rotMHash->Add(WL);
    TGeoHMatrix GmtOnGlob      = tpc2Glob * StGmtOnTpc::instance()->GetMatrix(module) * (*WL);
    TGeoHMatrix *R             = new TGeoHMatrix(GmtOnGlob);
    R->SetName(Form("R%i",module));
    rotMHash->Add(R);
  }
  TIter next(rotMHash);
  TGeoHMatrix *comb;
  Int_t fail = 0;
  while ((comb = (TGeoHMatrix *) next())) {
    TString Name(comb->GetName());
    if (Name.BeginsWith("R")) {
      TGeoHMatrix *WL = (TGeoHMatrix *) rotMHash->FindObject(Form("WL%s",Name.Data()+1));
      if (! WL) {
	cout << Form("WL%s",Name.Data()+1) << " has not been found" << endl;
	fail++;
      }
    }
  }
  assert(! fail);
#if 0
  if (fFile) {
    TDirectory *g = 0;
    if (gDirectory != fFile) {
      g = gDirectory; 
      fFile->cd(); 
    }
    rotMHash->Write();
    if (g) g->cd();
  }
#endif
}
