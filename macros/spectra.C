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
#include "StPiD/mudifi/StuFraction.h"
#include "BetheBloch.h"
#include "TLorentzVector.h"
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
void spectra(Int_t nevents=10000, TChain* fChain=0)
{
  if (!fChain) return;
  StuFraction *frac = new StuFraction();
  Char_t line[80];
  sprintf(line,"spectra%iK.root",nevents/1000);
  TString OutFile(line);
  outfile = new TFile(OutFile.Data(),"recreate");
  const Int_t    nEta   =   16;
  const Double_t EtaMin = -1.6;
  const Double_t EtaMax =  1.6;
  const Int_t    npT    =  190;
  const Double_t pTmin  = 0.10;
  const Double_t pTmax  = 2.00;
  const Int_t    L      = 3;
  const Int_t    M      = 11; // multiplicity
  TH2F *Total[2], *TotalCut[2];
  TH2F *hists[kNoHypo][2][L][M];
  Int_t s, k, l, m;
  TString Name, title;
  for (s = 0; s < 2; s++) {
    Name = "Total";
    if (s == 0) Name += "N";
    else        Name += "P";
    title = "#frac{d^{2}N}{p_{T} #times dp_{T} #times d#eta} positive with no Cut";
    Total[s] = new TH2F(Name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax);
    Total[s]->SetXTitle("Rapidity");
    Total[s]->SetYTitle("p_{T} (GeV/c)");
    Total[s]->SetZTitle("frac{d^{2}N}{p_{T} #times dp_{T} #times dy}");
    Name += "Cut";
    title = "frac{d^{2}N}{p_{T} #times dp_{T} #times d#eta} dEdx track length Cut";
    TotalCut[s] = new TH2F(Name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax);
    TotalCut[s]->SetXTitle("Rapidity");
    TotalCut[s]->SetYTitle("p_{T} (GeV/c)");
    TotalCut[s]->SetZTitle("frac{d^{2}N}{p_{T} #times dp_{T} #times dy}");
    for (k = 0; k < kNoHypo; k ++) {
      for (l = 0; l < L; l++) {
	for (m = 0; m < M; m++) {
	  Name = frac->GetName(k);
	  if (s == 0) Name += "N";
	  else        Name += "P";
	  title = "d^{2}N/(p_{T} dp_{T} dy) for ";
	  title +=  Name;
	  if (m < M - 1) {
	    Name += Form("%02i",m);
	    title += Form(", Centrality = %i",m);
	  }
	  if (l == 0) {Name += "w", title += " weighted";}
	  if (l == 1) {Name += "c", title += " cut > 20%";}
	  if (l == 2) {Name += "m", title += " Max. prob.";}
	  hists[k][s][l][m] = 
	    new TH2F(Name.Data(),title.Data(),nEta,EtaMin,EtaMax,npT,pTmin,pTmax);
	  hists[k][s][l][m]->SetXTitle("Rapidity");
	  hists[k][s][l][m]->SetYTitle("p_{T} (GeV/c)");
	  hists[k][s][l][m]->SetZTitle("frac{d^{2}N}{p_{T} #times dp_{T} #times dy}");
	  //	  cout << "Make " << Name.Data() << "/" << title.Data() << endl;
	}
      }
    }
  }
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  Int_t nbytes = 0, nb = 0, ierr = 0, nevt = 0;
  Int_t nTracks = 0;
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    m = pPicoEvent->Centrality();
    for (Int_t kTrack=0; kTrack < pPicoEvent->GetNtrack(); kTrack++) {
      StFlowPicoTrack* pPicoTrack = 
	(StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(kTrack);
      Double_t Eta = pPicoTrack->Eta();
      Double_t pT  = pPicoTrack->Pt();
      Double_t Phi = pPicoTrack->Phi();
      if (pT < pTmin) continue;
      Double_t pTinv = 1./pT;
      s = 1;
      if (pPicoTrack->Charge() < 0) s = 0;
      Total[s]->Fill(Eta,pT,pTinv);
      if (pPicoTrack->PidFitdEdxLength() < 40.) continue;
      TotalCut[s]->Fill(Eta,pT,pTinv);
      Int_t ok = frac->Set(pPicoEvent,pPicoTrack);
      Double_t *prob = frac->Probs();
      Int_t kMax = -1; 
      Double_t probMax = 0;
      TVector3 trk3(pT*TMath::Cos(Phi),pT*TMath::Sin(Phi),pT*TMath::SinH(Eta));
      Double_t pmom2= trk3.Mag2();
      Double_t y[kNoHypo];
      for (k = 0; k < kNoHypo; k ++) {
	Double_t mass = frac->GetMass(k);
	Double_t energy = TMath::Sqrt(mass*mass + pmom2);
	TLorentzVector p4(trk3,energy);
	y[k] = p4.Rapidity();
	if (prob[k] > 1.e-5) {
	  hists[k][s][0][m]->Fill(y[k],pT,prob[k]*pTinv);
	  hists[k][s][0][10]->Fill(y[k],pT,prob[k]*pTinv);
	}
	if (prob[k] > 0.2)   {
	  hists[k][s][1][m]->Fill(y[k],pT,pTinv);
	  hists[k][s][1][10]->Fill(y[k],pT,pTinv);
	}
	if (prob[k] > probMax) {
	  probMax = prob[k]; kMax = k;
	}
      }
      if (kMax > -1) {
	hists[kMax][s][2][m]->Fill(y[kMax],pT,pTinv);
	hists[kMax][s][2][10]->Fill(y[kMax],pT,pTinv);
      }
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}




