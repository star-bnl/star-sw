#define __MuDst__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
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
void EmbeddingNFitPoints(const Char_t *files = "./*MuDst.root",const Char_t *Out="EmbeddingNFitPoints.root") { 
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
#ifdef DEBUG
  const Int_t&       NoPrimaryTracks                           = iter("PrimaryTracks");
#endif
  const Int_t&       NoGlobalTracks                           = iter("GlobalTracks");
  const Short_t*&    GlobalTracks_mFlag                       = iter("GlobalTracks.mFlag");
  const UChar_t*&    GlobalTracks_mNHitsFit                   = iter("GlobalTracks.mNHitsFit");
  const Float_t*&    GlobalTracks_mPt                         = iter("GlobalTracks.mPt");
  const Short_t*&    GlobalTracks_mHelix_mQ                   = iter("GlobalTracks.mHelix.mQ");
  const Float_t*&    GlobalTracks_mEta                        = iter("GlobalTracks.mEta");
  const UShort_t*&   GlobalTracks_mIdTruth                    = iter("GlobalTracks.mIdTruth");
  const UInt_t*&     GlobalTracks_mTopologyMap_mMap0          = iter("GlobalTracks.mTopologyMap.mMap0");
  const UInt_t*&     GlobalTracks_mTopologyMap_mMap1          = iter("GlobalTracks.mTopologyMap.mMap1");
  // Book Histograms
  TFile *fOut = new TFile(Out,"recreate");
  TH3F *nfitp[2];
  nfitp[0] = new TH3F("Nfit","no. Nfit points versus q/pT and eta",40,-20,20,40,-1.0,1.0,45,0.5,45.5);
  nfitp[1] = new TH3F("NfitI","no. Nfit points with IdTruth versus q/pT and eta",40,-20,20,40,-1.0,1.0,45,0.5,45.5);
  TH2F *nfitN[2];
  nfitN[0] = new TH2F("NfitN","q/pT and eta for no. Nfit points > 42",40,-20,20,40,-1.0,1.0);
  nfitN[1] = new TH2F("NfitNI","q/pT and eta for no. Nfit points > 42 with IdTruth",40,-20,20,40,-1.0,1.0);
  TH1F *topo[2][2];
  topo[0][0] = new TH1F("ETopo","fired rows  -0.5 < eta < 0 && 1 < pT < 4",45,0.5,45.5);
  topo[1][0] = new TH1F("ETopoI","fired rows -0.5 < eta < 0 && 1 < pT < 4 with Idtruth",45,0.5,45.5);
  topo[0][1] = new TH1F("WTopo","fired rows  0 < eta < 0.5 && 1 < pT < 4",45,0.5,45.5);
  topo[1][1] = new TH1F("WTopoI","fired rows 0 < eta < 0.5 && 1 < pT < 4 with Idtruth",45,0.5,45.5);
  while (iter.Next()) {
#ifdef DEBUG
    cout << "NoTracks\t" << (int) NoGlobalTracks << "\t" <<  (int) NoPrimaryTracks << endl;
#endif
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      if (GlobalTracks_mFlag[kg]     <   0) continue;     // Bad fit
      if (GlobalTracks_mFlag[kg]     > 700) continue;     // FTPC
      if (GlobalTracks_mFlag[kg]%100 == 11) continue;     // Short track pointing to EEMC
      Float_t qpTInv = GlobalTracks_mHelix_mQ[kg]/GlobalTracks_mPt[kg];
      nfitp[0]->Fill(qpTInv, GlobalTracks_mEta[kg], GlobalTracks_mNHitsFit[kg]);
      if (GlobalTracks_mIdTruth[kg] > 0) 
      nfitp[1]->Fill(qpTInv, GlobalTracks_mEta[kg], GlobalTracks_mNHitsFit[kg]);
      if (GlobalTracks_mNHitsFit[kg] > 42) {
	nfitN[0]->Fill(qpTInv, GlobalTracks_mEta[kg]);
	if (GlobalTracks_mIdTruth[kg] > 0) nfitN[1]->Fill(qpTInv, GlobalTracks_mEta[kg]);
      }
      if (TMath::Abs(GlobalTracks_mEta[kg]) > 0.5) continue;
      Int_t iwe = 0;
      if (GlobalTracks_mEta[kg] > 0) iwe = 1;
      if (GlobalTracks_mPt[kg] < 1 || GlobalTracks_mPt[kg] > 4) continue;
      for (Int_t row = 1; row <= 45; row++) {
	Int_t i = 7+row;
	Int_t inRow = i>31 ? (GlobalTracks_mTopologyMap_mMap1[kg]>>(i-32) & 1U) : (GlobalTracks_mTopologyMap_mMap0[kg]>>i & 1U);
	if (inRow) {
	  topo[0][iwe]->Fill(row);
	  if (GlobalTracks_mIdTruth[kg] > 0) topo[1][iwe]->Fill(row);
	}
      }
    }
  }
  fOut->Write();
}
