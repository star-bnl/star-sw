#ifndef __CINT__
#include <iostream.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "TClassTable.h"
#include "TMinuit.h"
#include "TCanvas.h"
#include "TInterpreter.h"
#include "StFlowMaker/StFlowPicoEvent.h" 
#include "StFlowMaker/StFlowPicoTrack.h" 
#else
class StFlowPicoEvent;
class StFlowPicoTrack;
class StuFraction;
class TMinuit;
class TF1;
class TH1F;
class TH3F;
class TH2D;
class TChain;
class TCanvas;
#endif
TFile  *outfile = 0;
TChain *fgChain = 0;
const Int_t NHYPS = 5;
Char_t *Names[5] = {"p",
		    "K",
		    "pi",
		    "e",
		    "Unknown"
		    //			"mu",
		    //			"d"
};
Double_t Masses[4] = {0.93827231,
		      0.493677,
		      0.13956995,
		      0.51099907e-3
		      //			  0.1056583568,
		      //			  1.87561339
};
#ifndef __NOCINT__
//________________________________________________________________________________
void Load() {
  gROOT->LoadMacro("bfc.C");
  gSystem->Load("libPhysics");
  bfc(-2,"NoDefault flow",0,0,0);
  gSystem->Load("StFraction");

}
#endif
//________________________________________________________________________________ 
void flow(Int_t nevents=2000, 
	  const Char_t *macro="plot",
	  const char* TreeName = "FlowTree") 
{
  //	  TString topDir = "/star/u/fisyak/FlowP01gl"){
  //	  TString topDir = "/star/rcf/pwg/spectra/fisyak/FlowP01i/"){
  if (gClassTable->GetID("StuFraction") < 0) Load();
  fgChain = new TChain(TreeName);
  StFlowPicoEvent *pPicoEvent = 0; //new StFlowPicoEvent();
  fgChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  TIter next(gROOT->GetListOfFiles());
  TFile *f = 0;
  Int_t Ntotal = 0;
  while ((f = (TFile*) next())) {
    TChain *tree = (TChain *) f->Get(TreeName);
    UInt_t nEvents = tree->GetEntries();
    Ntotal += nEvents;
    Char_t *name = f->GetName();
    cout << "Add file: " << name << " with " << nEvents 
	 << " events, Total = " << Ntotal << endl;
    fgChain->Add(name);
    f->Close();
    if (nevents > 0 && Ntotal >= nevents) break;
  }
  if (fgChain) {
    cout << "Use Chain " << fgChain->GetName() << " with " << fgChain->GetEntries() << " events." << endl;
    TString cmd(".X ");
    //    TString cmd(".L ");
    cmd += macro;
    cmd += ".C++";
    cmd += Form("(%i,fgChain)",nevents);
    cout << "execute " << cmd.Data() << endl;
    gInterpreter->ProcessLine(cmd.Data());
  }
}
