#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#endif
//________________________________________________________________________________
void MuCov(Char_t *files = "*.MuDst.root",const Char_t *out = "") {
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("MuDst");
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  if (! NFiles) return;
#define __MuCov__
#include "MuDst.h"
#undef __MuCov__
  // Book Histograms
  struct Name_t {
    const Char_t *Name;
    const Char_t *Title;
    const Char_t *Axis;
  };
  const Int_t nT = 2;
  const Name_t PG[2] = {
    {"Pr","Primary tracks ", ""},
    {"Gl","Global tracks ", ""}
  };
  const Int_t nV = 3;
  const Name_t V[3] = {
    {"dpT","dpT / pT ","#sigma_{pT}/pT"},
    {"dT","d(Tan) "   ,"#sigma_{Tan}"},
    {"dP","dPhi "     ,"#sigma_{#phi}"}
  };
  const Int_t nW = 5;
  const Name_t W[5] = {
    {"pTI"," versus 1/pT","1/pT"},
    {"NF" ," versus No. fit points","No.fit points"},
    {"Eta"," versus Eta","#{eta}"},
    {"Phi"," versus Phi","#{phi}"},
    {"pT" ," versus pT","pT"} 
  };
  TString Out(out);
  if (Out == "") Out = "MuCov.root";
  TFile *fOut = new TFile(Out,"recreate");
  TH2D *hist[2][3][5];
  memset(hist, 0, sizeof(hist));
  for (Int_t i = 0; i < nT; i++) 
    for (Int_t j = 0; j < nV; j++) 
      for (Int_t m = 0; m < nW; m++) {
	TString name(PG[i].Name); name += V[j].Name; name += W[m].Name;
	TString title(PG[i].Title); title += V[j].Title; title += W[m].Title;
	Int_t nx = 100;
	Double_t xmin = 0;
	Double_t xmax = 10;                   // pT
	if (m == 1) {nx = 50; xmax = 50;}              //No. fit points
	if (m == 2) {xmin = -5.; xmax = 5.;}  // Eta
	if (m == 3) {xmax = TMath::Pi(); xmin = - xmax;} // Phi
	Double_t ymax = 0.10;       // dpT/pT
	if (j == 1) ymax = 0.020;   // dTan
	if (j == 2) ymax = 0.020;   // dPhi
	hist[i][j][m] = new TH2D(name,title,nx,xmin,xmax,100,0,ymax);
	hist[i][j][m]->SetXTitle(W[m].Axis);
	hist[i][j][m]->SetYTitle(V[j].Axis);
      }
  // Now iterations
  while (iter.Next()) {
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	if (PrimaryTracks_mVertexIndex[k] != l) continue;
	Int_t kpc = PrimaryTracks_mIndex2Cov[k];
	Int_t kg = PrimaryTracks_mIndex2Global[k];
	if (kg < 0 || kg > NoGlobalTracks) continue;
	Int_t kgc = GlobalTracks_mIndex2Cov[kg];
	//	if (GlobalTracks_mNHitsFit[kg] < 15) continue;
	Double_t VV[2][3] = { // nV
	  {TMath::Sqrt(CovPrimTrack_mPtiPti[kpc])*PrimaryTracks_mPt[k], TMath::Sqrt( CovPrimTrack_mTanTan[kpc]), TMath::Sqrt(CovPrimTrack_mPsiPsi[kpc])},
	  {TMath::Sqrt(CovGlobTrack_mPtiPti[kgc])/CovGlobTrack_mPti[kgc], TMath::Sqrt(CovGlobTrack_mTanTan[kgc]), TMath::Sqrt(CovGlobTrack_mPsiPsi[kgc])}
	};
	Double_t WW[2][5] = {// nW
	  {1./PrimaryTracks_mPt[k], PrimaryTracks_mNHitsFit[k], PrimaryTracks_mEta[k], PrimaryTracks_mPhi[k], PrimaryTracks_mPt[k]},
	  {1./GlobalTracks_mPt[kg], GlobalTracks_mNHitsFit[kg], GlobalTracks_mEta[kg], GlobalTracks_mPhi[kg], GlobalTracks_mPt[kg]}
	};
	for (Int_t i = 0; i < nT; i++)
	  for (Int_t j = 0; j < nV; j++)
	    for (Int_t m = 0; m < nW; m++) {
	      if (m != 1 && WW[i][1] < 15) continue;
	      hist[i][j][m]->Fill(WW[i][m], VV[i][j]);
	    } 
      }
    }
  }
  fOut->Write();
}
