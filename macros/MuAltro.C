/*
  root.exe lMuDst.C MuAltro.C+
 */
//#define DEBUG
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
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
#include "TNtuple.h"
#include "THnSparse.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "StBichsel/Bichsel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TRandom.h"
#include "TFractionFitter.h"
#include "TLegend.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
#include "Ask.h"
StMuDstMaker* maker = 0;
static Int_t _debug = 0;
Bichsel *m_Bichsel = 0;
#include "Names.h"
//#include "FitP_t.h"
#ifndef __FitP_t_h__
#define __FitP_t_h__
#include "TObject.h"
#include "Names.h"
#include <string.h>
#endif

TFile *fOut = 0;
TCanvas *c1 = 0;
//________________________________________________________________________________
Int_t Debug() {return _debug;}
//________________________________________________________________________________
void MuAltro(const Char_t *files ="./*.MuDst.root",
	     const Char_t *Out = "MuAltro1.root"){
  //  static const Double_t sigmaB[2] = {6.26273e-01, -5.80915e-01}; // Global Tracks, wrt Bichsel
  fOut = new TFile(Out,"recreate");
  TH1F *GoodPrTracks = new TH1F("GoodPrTracks","No. of Good Tracks at the best primary vertex",1000,-0.5,999.5);
  TH1F *NhitsOnTrack  = new TH1F("NhitsOnTrack","No. of Hits On good primary Track",50,-0.5,49.5);
  TH1F *NdEdxhitsOnTrack  = new TH1F("NdEdxhitsOnTrack","No. of dEdx Hits On good primary Track",50,-0.5,49.5);
 
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",files,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent"
    ,"PrimaryVertices"
    ,"PrimaryTracks"
    ,"GlobalTracks"
#if 0
    ,"CovPrimTrack"
    ,"CovGlobTrack"
    ,"StStMuMcVertex"
    ,"StStMuMcTrack"
    ,"KFTracks"
    ,"KFVertices"
#endif
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  //         Now iterations
  Int_t nev = 0;
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  Long64_t nevent = 999999;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  if (nentries <= 0) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,0)
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
#endif
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
#if 0
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
    TClonesArray *KFVertices       = mu->KFVertices(); Int_t NoKFVertices = KFVertices->GetEntriesFast();if (Debug()) {cout << "\tNoKFVertices " << NoKFVertices;}
    TClonesArray *KFTracks         = mu->KFTracks();   Int_t NoKFTracks = KFTracks->GetEntriesFast(); if (Debug()) {cout << "\tNoKFTracks " << NoKFTracks;}
#endif
    if (Debug())                                                               {cout << endl;}
    StMuPrimaryVertex *RcVx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(0);
    if (! RcVx) continue; 
    Int_t noGoodPrimTracks = 0;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (  pTrack->vertexIndex() != 0) continue;
      Int_t kg = pTrack->index2Global();
      if (kg < 0) continue;
      //      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (Debug())
      cout << k << "\tNHits " << (int) pTrack->nHits()
	   << "\tNHitsDedx " << (int) pTrack->nHitsDedx()
	   << "\tpT " << pTrack->pt() << "\tEta " << pTrack->eta() 
	   << "\tpx " << pTrack->p().x()
	   << "\tpy " << pTrack->p().y()
	   << "\tpz " << pTrack->p().z()
	   << "\tdEdx " << 1e6*pTrack->probPidTraits().dEdxTruncated() << "\tQ " << (int) pTrack->charge() << endl;
      noGoodPrimTracks++;
      NhitsOnTrack->Fill(pTrack->nHits());
      NdEdxhitsOnTrack->Fill(pTrack->nHitsDedx());
    }
    GoodPrTracks->Fill(noGoodPrimTracks);
    if (nev % 1000 == 1) cout << "read " << nev << " event so far" << endl;
  }
  //    iter.Reset(); //ready for next loop                                 
  if (fOut) fOut->Write();
}
/*
TFile *_file0 = TFile::Open("16131028.root"); // 16500 events -> Normal run 
TFile *_file1 = TFile::Open("16131032.root"); // 14640 events -> Cuts: Altro threshould 3 =>4, time buckets 415 => 370
gStyle->SetOptStat(0);  
TLegend *l = new TLegend(0.5,0.5,0.8,0.7);
_file0->cd(); GoodPrTracks->SetNormFactor(1.); GoodPrTracks->GetXaxis()->SetRange(1,100); GoodPrTracks->Draw(); l->AddEntry(GoodPrTracks,Form("Run 16131028: <x> = %7.2f",GoodPrTracks->GetMean()));
_file1->cd(); GoodPrTracks->SetNormFactor(1.); GoodPrTracks->SetLineColor(2); GoodPrTracks->Draw("sames"); l->AddEntry(GoodPrTracks,Form("Run 16131032: <x> = %7.2f",GoodPrTracks->GetMean()));
l->Draw();

TLegend *l1 = new TLegend(0.5,0.5,0.8,0.7);
_file0->cd(); NhitsOnTrack->SetNormFactor(1.); NhitsOnTrack->Draw(); l1->AddEntry(NhitsOnTrack,Form("Run 16131028: <x> = %7.2f",NhitsOnTrack->GetMean()));
_file1->cd(); NhitsOnTrack->SetNormFactor(1.); NhitsOnTrack->SetLineColor(2); NhitsOnTrack->Draw("sames"); l1->AddEntry(NhitsOnTrack,Form("Run 16131032: <x> = %7.2f",NhitsOnTrack->GetMean()));
l1->Draw();

TLegend *l2 = new TLegend(0.5,0.5,0.8,0.7);
_file0->cd(); NdEdxhitsOnTrack->SetNormFactor(1.); NdEdxhitsOnTrack->Draw(); l2->AddEntry(NdEdxhitsOnTrack,Form("Run 16131028: <x> = %7.2f",NdEdxhitsOnTrack->GetMean()));
_file1->cd(); NdEdxhitsOnTrack->SetNormFactor(1.); NdEdxhitsOnTrack->SetLineColor(2); NdEdxhitsOnTrack->Draw("sames"); l2->AddEntry(NdEdxhitsOnTrack,Form("Run 16131032: <x> = %7.2f",NdEdxhitsOnTrack->GetMean()));
l2->Draw();


 */
