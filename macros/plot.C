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
TChain *fChain = 0;
const Int_t NHYPS = 4;
const Int_t NHyps = NHYPS + 1;
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
void plot(Int_t nevents=10000, TChain* fChain=0)
{
  if (!fChain) return;
  Char_t line[80];
  sprintf(line,"dEdx%iK.root",nevents/1000);
  TString OutFile(line);
  outfile = new TFile(OutFile.Data(),"recreate");
  TH2F *hists[NHyps][2][4];
  TProfile *histB[NHYPS][2];
  TH2D *histz[NHYPS][2];
  const Int_t npT = 10;
  const Double_t pTmin = 0.25;
  const Double_t dpT   = 0.05;
  TH1F *pbarp[npT][2]; 
  Int_t sCharge, hyp, k;
  TString Name, title;
  for (sCharge = 0; sCharge < 2; sCharge++) {
    for (hyp = 0; hyp < NHyps; hyp++) {
      for (k = 0; k < 5; k ++) {
	Name = Names[hyp];
	if (sCharge == 0) Name += "N";
	else              Name += "P";
	title = "dEdx versus momentum for ";
	title +=  Name;
	if (k == 0) {Name += "90"; title += " 90% cut";}
	if (k == 1) {Name += "50"; title += " 50% cut";}
	if (k == 2) {Name += "20"; title += " 20% cut";}
	if (k == 3) {Name += "MX"; title += " max prob. cut";}
	if (hyp == NHyps-1) title += ". No hyps survived this cut";
	if (hyp == NHyps-1 && k == 3) title = "dEdx versus momentum with NO selection";
	hists[hyp][sCharge][k] = new TH2F(Name.Data(),title.Data(),160,-1.,3.,240,-1.5,4.5);
	hists[hyp][sCharge][k]->SetMarkerColor(hyp+1);
	hists[hyp][sCharge][k]->SetYTitle("log(dE/dx(keV))");
	hists[hyp][sCharge][k]->SetXTitle("log_{10} p (GeV/c)");
      }
    }
    for (k = 0; k < npT; k++) {
      if (sCharge == 0) Name = "pbar";
      else              Name = "p";
      sprintf(line,"%02i",k);
      Name += line;
      Double_t pT1 = pTmin+dpT*k;
      Double_t pT2 = pT1 + dpT;
      title = "log(dEdx/dEdx)_{BBp}  for |#eta| < 0.1 and pT range ";
      sprintf(line,"[%4.2f,%4.2f] GeV/c",pT1,pT2);
      title += line;
      pbarp[k][sCharge] = new TH1F(Name.Data(),title.Data(),400,-5.,5.);
    }
  }
  for (hyp=0; hyp<NHYPS;hyp++) {
    for (sCharge = 0; sCharge < 2; sCharge++) {
      TString nameP = Names[hyp];
      if (sCharge == 0) nameP += "P";
      else        nameP += "N";
      TString name = nameP;
      name += "z";
      title = "zFit - log(I(";
      title += nameP;
      title += ")) versus log10(p/m)";
      histz[hyp][sCharge] = new TH2D(name.Data(),title.Data(),100,-1.,4.,200,-1.,1.);
      name = nameP;
      name += "B";
      title = "log(I_{Sirrf}(";
      title += nameP;
      title += ")) versus log10(p/m)";
      histB[hyp][sCharge] = new TProfile(name.Data(),title.Data(),100,-1.,4.);
    }
  }
 
  StFlowPicoEvent *pPicoEvent = new StFlowPicoEvent();
  fChain->SetBranchAddress("pPicoEvent", &pPicoEvent);
  StuFraction *frac = new StuFraction();
  Int_t nbytes = 0, nb = 0, ierr = 0, nevt = 0;
  Int_t nTracks = 0;
  Int_t nentries = Int_t(fChain->GetEntries());
  if (nevents > 0 && nentries > nevents) nentries = nevents;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    //in case of a TChain, ientry is the entry number in the current file
    fChain->LoadTree(jentry); 
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    for (Int_t kTrack=0; kTrack < pPicoEvent->GetNtrack(); kTrack++) {
      StFlowPicoTrack* pPicoTrack = (StFlowPicoTrack*)pPicoEvent->Tracks()->UncheckedAt(kTrack);
      if (pPicoTrack->PidFitdEdxLength() < 40.) continue;
      Int_t ok = frac->Set(pPicoEvent,pPicoTrack);
      Double_t *prob = frac->Probs();
#if 0
      cout << "Ev:\t" << jentry << "\tTrk:\t" << kTrack << "\tpT:\t" 
	   << pPicoTrack->Charge()* pPicoTrack->Pt() << "\tok:" << ok 
	   << "\tEta:" << pPicoTrack->Eta()   
	   << "\tp:" << prob[0] // << "/" << frac->ProtonProb()
	   << "\tK:" << prob[1] // << "/" << frac->KaonProb()
	   << "\tpi:" << prob[2] // << "/" << frac->PionProb();
	   << "\te:" << prob[3] // << "/" <<  frac->ElectronProb()
	   << "\tsum:" << frac->Sum() << endl;
#endif
      sCharge = 0;
      Double_t dEdxL = TMath::Log(1.e6*pPicoTrack->PidFitdEdx());
      Double_t p     = TMath::Abs(pPicoTrack->Pt())*TMath::CosH(pPicoTrack->Eta());
      Double_t pL    = TMath::Log10(p);
      if (pPicoTrack->Charge() > 0) sCharge = 1;
      Int_t hmax = -1;
      Double_t probMax = 0;
      Int_t n90 = 0, n50 = 0, n20 = 0;
      for (h = 0; h < NHyps; h++) {
	if (ok < 0) {
	}
	else {
	  if (prob[h] > 0.9) {hists[h][sCharge][0]->Fill(pL,dEdxL); n90++;}
	  if (prob[h] > 0.5) {hists[h][sCharge][1]->Fill(pL,dEdxL); n50++;}
	  if (prob[h] > 0.2) {hists[h][sCharge][2]->Fill(pL,dEdxL); n20++;}
	  if (prob[h] > probMax) {
	    probMax = prob[h];
	    hmax = h;
	  }
	}
      }
      if (hmax >= 0) hists[hmax][sCharge][3]->Fill(pL,dEdxL);
      if (n90 == 0)  hists[NHyps-1][sCharge][0]->Fill(pL,dEdxL);
      if (n50 == 0)  hists[NHyps-1][sCharge][1]->Fill(pL,dEdxL);
      if (n20 == 0)  hists[NHyps-1][sCharge][2]->Fill(pL,dEdxL);
                     hists[NHyps-1][sCharge][0]->Fill(pL,dEdxL);
		     //		     cout << "\tEta:" << pPicoTrack->Eta() << endl;
		     
      if (TMath::Abs(pPicoTrack->Eta()) < 0.1) {
	k = (Int_t) (pPicoTrack->Pt() - pTmin)/dpT;
	//	cout << "\tEta:" << pPicoTrack->Eta() << "\t:pT" << pPicoTrack->Pt() << "\tk:" << k <<endl;
	if (k >= 0 && k < npT) {
	  Double_t poverm = p/Masses[0];
	  Double_t pred = 1.e-6*BetheBloch::Sirrf(poverm,pPicoTrack->PidFitdEdxLength());
	  Double_t dEdxN = TMath::Log(pPicoTrack->PidFitdEdx()/pred);
	  pbarp[k][sCharge]->Fill(dEdxN);
	  
	}
      }
      Double_t Pred[NHYPS];
      Double_t TrackLength = pPicoTrack->PidFitdEdxLength();
      for (hyp = 0; hyp < NHYPS; hyp++) {
	Pred[hyp] = 1.e-6*BetheBloch::Sirrf(p/Masses[hyp],TrackLength,hyp==3); 
	histB[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),TMath::Log(Pred[hyp]));
	Double_t dEdxN = TMath::Log(pPicoTrack->PidFitdEdx()/Pred[hyp]);
	histz[hyp][sCharge]->Fill(TMath::Log10(p/Masses[hyp]),dEdxN);
      }
    }
    if (jentry%1000 == 1) cout << "Read event no. " << jentry << endl;
  }
  outfile->Write();
}
