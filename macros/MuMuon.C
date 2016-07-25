#define __MuDst__
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
void MuMuon(const Char_t *files = "./*MuDst.root",
	   const Char_t *Out="MuDst.root") { 
  static const Float_t pimass = 0.13956995;
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
#include "MuDst.h"
  //         Now iterations
  struct Flag_t {
    Short_t x;
    const Char_t *Name;
  };
  // mFlag=xyy, where x
  const Int_t NFlags = 10;
  Flag_t Flags[10] = {
    {0, "All tracks"},
    {1, "TPC only, w/o flag = 111"},
    {2, "TPC only, w   flag = 111"},
    {3, "TPC       + primary vertex"},
    {4, ""},
    {5, "SVT + TPC"},
    {6, "SVT + TPC + primary vertex"},
    {7, "FTPC only"},
    {8, "FTPC      + primary"},
    {9, "TPC beam background tracks"}
  };
  const Int_t MaxRank = 5;
  const Char_t *RankName[5] = {"All","Rank 1","Rank 2","Rank 3","Rank > 4"};
  TFile *fOut = new TFile(Out,"recreate");
  TH2D *PrVsGl = new TH2D("PrVsGl","No. of Primary tracks versus No. of Globals",
			    100,0,5000., 100,0.,2500.);
  TH1D *K0mass = new TH1D("K0mass","mass of K0",100,.250,.750);
  const Int_t Ntypes = 4;
  struct Name_t {
    Char_t *Name;
    Char_t *Title;
  };
  Name_t Names[4] = {
    {"eta","pseudo rapidity"},
    {"y","rapidity"},
    {"pT","p_{T}"},
    {"Phi","#phi"}
  };
  const Int_t Ncut = 3;
  Name_t Cuts[3] = {
    {"NfpLT15","no. fit point <  15"},
    {"NfpGE15","no. fit point >= 15 && no. fit point < 25"},
    {"NfpGE25","no. fit point >= 25"} 
  };
  TH1D *****hists = new TH1D****[Ntypes]; memset (hists, 0, sizeof(hists));
  for (Int_t t = 0; t < Ntypes; t++) {
    hists[t] = new TH1D***[NFlags]; memset(hists[t], 0, sizeof(hists[t]));
    for (Int_t i = 0; i < NFlags; i++) {
      hists[t][i] = new TH1D**[MaxRank]; memset(hists[t][i],0,sizeof(hists[t][i]));
      Int_t N = 1;
      if (i == 4) continue;
      if (i == 0 || i == 2 || i == 3 || i == 6 || i == 8) N = MaxRank;
      for (Int_t j = 0; j < N; j++) {
	hists[t][i][j] = new TH1D*[Ncut]; memset(hists[t][i][j],0,sizeof(hists[t][i][j]));
	for (Int_t c = 0; c < Ncut; c ++) {
	  Int_t nx = 1000;
	  Double_t xmin = -5;
	  Double_t xmax =  5;
	  switch (t) {
	  case 2: 
	    nx   = 100;
	    xmin = 0;
	    xmax = 5;
	    break;
	  case 3:
	    nx = 12*12;
	    xmin = -TMath::Pi();
	    xmax =  TMath::Pi();
	  default:
	    break;
	  }
	  hists[t][i][j][c] = new TH1D(Form("%sF%iR%i%s",Names[t].Name,i,j,Cuts[c].Name),
				       Form("%s for %s and Rank %s with %s",Names[t].Title,Flags[i].Name,RankName[j],Cuts[c].Title),
				       nx, xmin, xmax);
	}
      }
    }
  }
  static TLorentzVector p4;
  while (iter.Next()) {
#ifdef DEBUG
    cout << "NoTracks\t" << (int) NoGlobalTracks << "\t" <<  (int) NoPrimaryTracks << endl;
#endif
    PrVsGl->Fill(NoGlobalTracks,NoPrimaryTracks);
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      if (GlobalTracks_mFlag[kg] < 100) continue;
      if (TMath::Abs(GlobalTracks_mEta[kg]) > 5) continue;
      Int_t c = 1;
      if (GlobalTracks_mNHitsFit[kg] < 15) c = 0;
      Int_t i = (GlobalTracks_mFlag[kg]/100)%10;
      if (GlobalTracks_mFlag[kg] == 111) i = 2;
      p4.SetPtEtaPhiM(GlobalTracks_mPt[kg],GlobalTracks_mEta[kg],GlobalTracks_mPhi[kg],pimass);
      hists[0][0][0][c]->Fill(GlobalTracks_mEta[kg]);
      hists[1][0][0][c]->Fill(p4.Rapidity());
      hists[2][0][0][c]->Fill(GlobalTracks_mPt[kg]);
      hists[3][0][0][c]->Fill(p4.Phi());
      hists[0][i][0][c]->Fill(GlobalTracks_mEta[kg]);
      hists[1][i][0][c]->Fill(p4.Rapidity());
      hists[2][i][0][c]->Fill(GlobalTracks_mPt[kg]);
      hists[3][i][0][c]->Fill(p4.Phi());
    }
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	if (l != PrimaryTracks_mVertexIndex[k]) continue;
	Int_t kg = PrimaryTracks_mIndex2Global[k];
	if (GlobalTracks_mFlag[kg] < 100) continue;
	if (TMath::Abs(GlobalTracks_mEta[kg]) > 5) continue;
	Int_t c = 1;
	if (GlobalTracks_mNHitsFit[kg] < 15) c = 0;
	if (GlobalTracks_mNHitsFit[kg] >=25) c = 2;
	Int_t i = PrimaryTracks_mFlag[k]/100;
	if (GlobalTracks_mFlag[kg] == 111) i = 2;
	p4.SetPtEtaPhiM(GlobalTracks_mPt[kg],GlobalTracks_mEta[kg],GlobalTracks_mPhi[kg],pimass);
	hists[0][0][0][c]->Fill(GlobalTracks_mEta[kg]);
	hists[1][0][0][c]->Fill(p4.Rapidity());
	hists[2][0][0][c]->Fill(GlobalTracks_mPt[kg]);
	hists[3][0][0][c]->Fill(p4.Phi());
	hists[0][i][0][c]->Fill(GlobalTracks_mEta[kg]);
	hists[1][i][0][c]->Fill(p4.Rapidity());
	hists[2][i][0][c]->Fill(GlobalTracks_mPt[kg]);
	hists[3][i][0][c]->Fill(p4.Phi());
	Int_t j = l + 1;
	if (j >= MaxRank) j = MaxRank - 1;
 	hists[0][0][j][c]->Fill(GlobalTracks_mEta[kg]);
	hists[1][0][j][c]->Fill(p4.Rapidity());
	hists[2][0][j][c]->Fill(GlobalTracks_mPt[kg]);
	hists[3][0][j][c]->Fill(p4.Phi());
	hists[0][i][j][c]->Fill(GlobalTracks_mEta[kg]);
	hists[1][i][j][c]->Fill(p4.Rapidity());
	hists[2][i][j][c]->Fill(GlobalTracks_mPt[kg]);
	hists[3][i][j][c]->Fill(p4.Phi());
      }
    }
    for (Int_t k = 0; k < NoV0; k++) {
      TVector3 p(V0_mMomPosX[k],V0_mMomPosY[k],V0_mMomPosZ[k]);
      Double_t e = pimass*pimass + p.Mag2();
      e = TMath::Sqrt(e);
      TLorentzVector P(p,e);
      TVector3 n(V0_mMomNegX[k],V0_mMomNegY[k],V0_mMomNegZ[k]);
      e = pimass*pimass + n.Mag2();
      e = TMath::Sqrt(e);
      TLorentzVector N(n,e);
      P += N;
      K0mass->Fill(P.M());
    }
  }
  fOut->Write();
}
