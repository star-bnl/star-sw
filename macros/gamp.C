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
void gamp(Int_t nevents=10000, TChain* fChain=0)
{// take gamma from primary tracks
  if (!fChain) return;
  StuFraction *frac = new StuFraction();
  Char_t line[80];
  sprintf(line,"gamp%iK.root",nevents/1000);
  TString OutFile(line);
  outfile = new TFile(OutFile.Data(),"recreate");
  TH1D *effm = new TH1D("effm","e^{+}e^{-} eff. mass with no cuts",200,0,0.400);
  TH1D *effc = new TH1D("effc","e^{+}e^{-} eff. mass with Theta cut",200,0,0.400);
  TH1D *dTheta = new TH1D("dTheta","e^{+}e^{-} dTheta",200,-0.100,0.100);
  TH1D *dPhi   = new TH1D("dPhi","e^{+}e^{-} dTheta",200,-0.100,0.100);
  TH2F *ePzA  = new TH2F("ePzA","zFit - log(I(e)) versus log10(p/m)",200,-1.,4.,1000,-1.,4.);
  TH2F *eNzA  = new TH2F("eNzA","zFit - log(I(e)) versus log10(p/m)",200,-1.,4.,1000,-1.,4.);
  TH2F *ePz  = new TH2F("ePz","zFit - log(I(e)) versus log10(p/m)",200,-1.,4.,1000,-1.,4.);
  TH2F *eNz  = new TH2F("eNz","zFit - log(I(e)) versus log10(p/m)",200,-1.,4.,1000,-1.,4.);
  TProfile *ePzAB = new TProfile("ePzAB","log(I_{Sirrf}(e)) versus log10(p/m)",200,-1.,4.);
  TProfile *eNzAB = new TProfile("eNzAB","log(I_{Sirrf}(e)) versus log10(p/m)",200,-1.,4.);
  TProfile *ePzB = new TProfile("ePzB","log(I_{Sirrf}(e)) versus log10(p/m)",200,-1.,4.);
  TProfile *eNzB = new TProfile("eNzB","log(I_{Sirrf}(e)) versus log10(p/m)",200,-1.,4.);
  //_____________________
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  Int_t nbytes = 0, nb = 0, ierr = 0, nevt = 0;
  Int_t nTracks = 0;
  Double_t mass = frac->GetMass(kElectron);
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    nTracks = pPicoEvent->GetNtrack();
    for (Int_t k=0; k < nTracks; k++) {
      StFlowPicoTrack* pTrack = 
	(StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(k);
      if (pTrack->Charge() < 0) continue; // positive tracks
      Double_t TrackLength_k =  pTrack->PidFitdEdxLength();
      if (TrackLength_k < 60.) continue;
      Double_t Eta = pTrack->Eta();
      Double_t pT  = pTrack->Pt();
      Double_t Phi = pTrack->Phi();
      TVector3 trk3(pT*TMath::Cos(Phi),pT*TMath::Sin(Phi),pT*TMath::SinH(Eta));
      Double_t pmom2= trk3.Mag2();
      Double_t energy = TMath::Sqrt(mass*mass + pmom2);
      TLorentzVector pk4(trk3,energy);
      Double_t bgk   = TMath::Sqrt(pmom2)/mass;
      Double_t Predk = TMath::Log(1.e-6*BetheBloch::Sirrf(bgk,TrackLength_k,1));
      Double_t zk    = TMath::Log(pTrack->PidFitdEdx()) - Predk;
      //      Int_t ok1 = frac->Set(pPicoEvent,pTrack);
      for (Int_t l=0; l < nTracks; l++) {
	StFlowPicoTrack* lTrack = 
	  (StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(l);
 	if (lTrack->Charge() >= 0) continue; // negative tracks
	Double_t TrackLength_l =  lTrack->PidFitdEdxLength();
	if (TrackLength_l < 60.) continue;
	Eta = lTrack->Eta();
	pT  = lTrack->Pt();
	Phi = lTrack->Phi();
	TVector3 trl3(pT*TMath::Cos(Phi),pT*TMath::Sin(Phi),pT*TMath::SinH(Eta));
	pmom2= trl3.Mag2();
	energy = TMath::Sqrt(mass*mass + pmom2);
	TLorentzVector pl4(trl3,energy);
	Double_t bgl   = TMath::Sqrt(pmom2)/mass;
	Double_t Predl = TMath::Log(1.e-6*BetheBloch::Sirrf(bgl,TrackLength_l,1));
	Double_t zl    = TMath::Log(lTrack->PidFitdEdx()) - Predl;
	pl4 += pk4;
	Double_t M = pl4.M();
	Double_t y = pl4.Rapidity();
	//	Double_t dT = (pk4.Rapidity() - pl4.Rapidity())/TMath::CosH(y);
	Double_t dT = (pk4.Theta() - pl4.Theta());
	Double_t dP = pTrack->Phi() - lTrack->Phi();
	pT = pl4.Pt();
	effm->Fill(M);
	dTheta->Fill(dT);
	dPhi->Fill(dP);
	ePzA->Fill(TMath::Log10(bgk),zk);
	eNzA->Fill(TMath::Log10(bgl),zl);
	ePzAB->Fill(TMath::Log10(bgk),Predk);
	eNzAB->Fill(TMath::Log10(bgl),Predl);
	if (TMath::Abs(dT) > 0.020 || TMath::Abs(dP) > 0.020) continue;
	effc->Fill(M);
	if (M > 0.050) continue;
	ePz->Fill(TMath::Log10(bgk),zk);
	eNz->Fill(TMath::Log10(bgl),zl);
	ePzB->Fill(TMath::Log10(bgk),Predk);
	eNzB->Fill(TMath::Log10(bgl),Predl);
      } 
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}








