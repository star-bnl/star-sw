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
#include "StPiD/StFraction/StuFraction.h"
#include "BetheBloch.h"
#include "TLorentzVector.h"
#else
class StuFraction;
class StFlowPicoEvent;
class StFlowPicoTrack;
class TMinuit;
class TF1;
class TH1F;
class TH3F;
class TH2D;
class TChain;
class TCanvas;
#endif
TFile  *outfile = 0;
void phi90(Int_t nevents=10000, TChain* fChain=0)
{
  if (!fChain) return;
  StuFraction *frac = new StuFraction();
  Char_t line[80];
  sprintf(line,"phi90%iK.root",nevents/1000);
  TString OutFile(line);
  outfile = new TFile(OutFile.Data(),"recreate");
  const Int_t    nEta   =   16;
  const Double_t EtaMin = -1.6;
  const Double_t EtaMax =  1.6;
  const Int_t    npT    =   19;
  const Double_t pTmin  = 0.10;
  const Double_t pTmax  = 2.00;
  const Int_t    L      = 3;
  const Int_t    M      = 11; // multiplicity
  TH3F *hists[2][M];
  Int_t s, k, l, m;
  TString Name, title;
  for (s = 0; s < 2; s++) {
    for (m = 0; m < M; m++) {
      //      Name = frac->GetName(k);
      Name = "KK";
      title = "K^{+}K^{-} eff. mass";
      if (s == 0) {Name += "Tot"; title += " Total";}
      else        {Name += "Cut"; title += " Cut";}
      if (m < M - 1) {
	Name += Form("%02i",m);
	title += Form(", Centrality = %i",m);
      }
      hists[s][m] = new TH3F(Name.Data(),title.Data(),
			     nEta,EtaMin,EtaMax,npT,pTmin,pTmax,
			     150,0.980,1.280);
      hists[s][m]->SetXTitle("Rapidity");
      hists[s][m]->SetYTitle("p_{T} (GeV/c)");
      hists[s][m]->SetZTitle("K^{+}K^{-} eff. mass");
    }
  }
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  Int_t nbytes = 0, nb = 0, ierr = 0, nevt = 0;
  Int_t nTracks = 0;
  Double_t mass = frac->GetMass(kKaon);
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    m = pPicoEvent->Centrality();
    for (Int_t k=0; k < pPicoEvent->GetNtrack(); k++) {
      StFlowPicoTrack* pTrack = 
	(StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(k);
      Double_t Eta = pTrack->Eta();
      Double_t pT  = pTrack->Pt();
      Double_t Phi = pTrack->Phi();
      TVector3 trk3(pT*TMath::Cos(Phi),pT*TMath::Sin(Phi),pT*TMath::SinH(Eta));
      Double_t pmom2= trk3.Mag2();
      Double_t energy = TMath::Sqrt(mass*mass + pmom2);
      TLorentzVector pk4(trk3,energy);
      Int_t ok1 = frac->Set(pPicoEvent,pTrack);
      for (Int_t l=0; l < k; l++) {
	StFlowPicoTrack* lTrack = 
	  (StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(l);
 	if (pTrack->Charge()*lTrack->Charge() >= 0) continue;
	Eta = lTrack->Eta();
	pT  = lTrack->Pt();
	Phi = lTrack->Phi();
	TVector3 trl3(pT*TMath::Cos(Phi),pT*TMath::Sin(Phi),pT*TMath::SinH(Eta));
	pmom2= trl3.Mag2();
	energy = TMath::Sqrt(mass*mass + pmom2);
	TLorentzVector pl4(trl3,energy);
	pl4 += pk4;
	Double_t Mass = pl4.M();
	Double_t y = pl4.Rapidity();
	pT = pl4.Pt();
	hists[0][m]->Fill(y,pT,Mass);
	hists[0][10]->Fill(y,pT,Mass);
	if (pTrack->PidFitdEdxLength() < 60. ||
	    lTrack->PidFitdEdxLength() < 60.
	    ) continue;
	if (pTrack->PidKaon() < 90.0) continue; 
	Int_t ok2 = frac->Set(pPicoEvent,lTrack);
	if (lTrack->PidKaon() < 90.0) continue;
	hists[1][m]->Fill(y,pT,Mass);
	hists[1][10]->Fill(y,pT,Mass);
      } 
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}








