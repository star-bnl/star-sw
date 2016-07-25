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
#include "StFlowMaker/StFlowPicoEvent.h"
#include "StFlowMaker/StFlowPicoTrack.h" 
#include "BetheBloch.h"
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
const Int_t NHYPS = 4;
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
//________________________________________________________________________________
void BBG(Int_t nevents=10000, TChain* fChain=0)
{
  if (!fChain) return;
  TString OutFile(Form("BBG%iK.root",nevents/1000));
  outfile = new TFile(OutFile.Data(),"recreate");
  TH2F *histz[NHYPS][2];
  TH2F *hist70[NHYPS][2];
  TProfile *histB[NHYPS][2];
  //  const Int_t npT = 10;
  Int_t s, hyp;
  TString Name, title;
  TH3F *TPoints  = new TH3F("TPoints","dEdx(fit) versus no. of measured points and length", 
		      50,0,50., 140,20.,160., 200,-1.,1.);
  TH3F *TPoints70  = new TH3F("TPoints70","dEdx(70) versus no. of measured points and length", 
		      50,0,50., 140,20.,160., 200,-1.,1.);
  for (s = 0; s < 2; s++) {
    for (hyp = 0; hyp < NHYPS; hyp++) {
	Name = Names[hyp];
	if (s == 0) Name += "N";
	else        Name += "P";
	Name += "z";
	title = "zFit - log(I(";
	title += Names[hyp];
	title += ")) versus log10(p_glob/m)";
	histz[hyp][s] = new TH2F(Name.Data(),title.Data(),200,-1.,4.,1000,-1.,4.);
	Name += "B";
	title = "log(I_{Sirrf}(";
	title += Names[hyp];
	title += ")) versus log10(p_glob/m)";
	histB[hyp][s] = new TProfile(Name.Data(),title.Data(),200,-1.,4.);
	Name = Names[hyp];
	if (s == 0) Name += "N";
	else        Name += "P";
	Name += "70";
	title = "I70 - log(I(";
	title += Names[hyp];
	title += ")) versus log10(p_glob/m)";
	hist70[hyp][s] = new TH2F(Name.Data(),title.Data(),200,-1.,4.,1000,-1.,4.);
      }
    }
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  Int_t nbytes = 0, nb = 0;
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    //    if (jentry==82725) continue;
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    for (Int_t kTrack=0; kTrack < pPicoEvent->GetNtrack(); kTrack++) {
      StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(kTrack);
      Double_t TrackLength =  pPicoTrack->PidFitdEdxLength();
#if 0
      cout << "Ev:\t" << jentry << "\tTrk:\t" << kTrack << "\tpT:\t" 
	   << pPicoTrack->Charge()* pPicoTrack->Pt() 
	   << "\tEta:" << pPicoTrack->Eta() << endl;
#endif
      s = 0;
      Double_t dEdxL = TMath::Log(pPicoTrack->PidFitdEdx());
      Double_t dEdx70L = TMath::Log(pPicoTrack->Dedx());
      Double_t p     = TMath::Abs(pPicoTrack->PtGlobal())*TMath::CosH(pPicoTrack->EtaGlobal());
      Double_t pL    = TMath::Log10(p);
      Int_t NdEdx    = pPicoTrack->PidFitNdEdx();
      if (pPicoTrack->Charge() > 0) s = 1;
      for (hyp = 0; hyp < NHYPS; hyp++) {
	Int_t k = 0;
	if (hyp == 3) {
	  if (s == 1) k = 1;
	  else        k = 2;
	}
	Double_t Pred = TMath::Log(1.e-6*BetheBloch::Sirrf(p/Masses[hyp],TrackLength,k));
	Double_t pLM = pL - TMath::Log10(Masses[hyp]);
	if (TrackLength > 60.) {
	  histz[hyp][s]->Fill(pLM,dEdxL - Pred);
	  hist70[hyp][s]->Fill(pLM,dEdx70L - Pred);
	  histB[hyp][s]->Fill(pLM,Pred);
	}
	if (hyp == 2) {
	  TPoints->Fill(NdEdx,TrackLength,dEdxL - Pred);
	  TPoints70->Fill(NdEdx,TrackLength,dEdx70L - Pred);
	}
      }
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}
