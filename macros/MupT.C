/* 
   root.exe lMuDst.C MupT.C+
ToF Mass^2
   gStyle->SetOptStat(0);
   M2Pos->GetXaxis()->SetRange(1,95);
   M2Pos->GetYaxis()->SetRange(61,120);
   M2Pos->FitSlicesY();
   M2Pos_1->GetXaxis()->SetRange(1,95);
   M2Neg->GetXaxis()->SetRange(1,95);
   M2Neg->GetYaxis()->SetRange(61,120);
   M2Neg->FitSlicesY();
   M2Neg_1->GetXaxis()->SetRange(1,95);
   M2Neg_1->SetMarkerColor(2);
   M2Pos_1->SetTitle("Mass^{2} versus p_{T}");
   M2Pos_1->SetXTitle("p_{T}");
   M2Pos_1->SetYTitle("Mass^{2}");
   M2Pos_1->Draw();
   TLegend *l = new TLegend(0.3,0.2,0.5,0.5);
   l->AddEntry(M2Pos_1,"p");
   M2Neg_1->Draw("same");
   l->AddEntry(M2Neg_1,"pbar");
   l->Draw();
pT +/-
  
*/
#define __Y2014__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#undef  StMessMgr
#undef ClassStMessMgr
#else
#ifndef __MAKECINT__
#define BIT(n)       (1 << (n))
#define SETBIT(n,i)  ((n) |= (1 << i))
#define CLRBIT(n,i)  ((n) &= ~(1 << i))
#define TESTBIT(n,i) ((Bool_t)(((n) & (1 << i)) != 0))
class StMuDstMaker;
#endif
#endif
StMuDstMaker* maker = 0;
enum TrackMcType {kNotDefined, kMatched, kLost, kGost, kClone, kTotalMcType};
//static const Char_t *NameTrackMcName[kTotalMcType] = {"NotDefined","Matched","Lost","Ghost","Clone"};
enum TrackMatchType {kPositive, kNegative, kTotalSigns,                                     // switch between charges
		     kGl = 0, kPr, kTotalT,                                                 // switch between global and primary tracks
		     kMc = 0, kMcHit, kMcRec, kMcClone, kRcGhost, kMcLost, kTotalMatchType, // match type
		     kTotalQAPr = 10,                                                       // no. of plots for Primary tracks
		     kTotalQAGl = 13                                                        // no. of plots for Global  tracks
};
struct PlotName_t {
  TrackMatchType    k;
  const Char_t *Name;
  const Char_t *Title;
};
struct VarName_t {
  const Char_t *Name;
  const Char_t *Title;
  Int_t nx;
  Double_t xmin, xmax;
  Int_t ny;
  Double_t ymin, ymax;
  Int_t nz;
  Double_t zmin, zmax;
  Double_t  min,  max; // min and max for plots
};
//         g/p+/-
TH2D *hists[2][2];
TH2D *Mass2[2]; // +/-
const Char_t *NameCharge[kTotalSigns] = {"Pos", "Neg"};
const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};
const Char_t *NameTrType[kTotalSigns] = {"Gl", "Pr"};
const Char_t *TitleTrType[kTotalSigns] = {"Global Tracks", "Primary Tracks"};
TFile *fOut = 0;
//________________________________________________________________________________
void Init(const Char_t *outfName="Out.root") {
  Int_t opt = 1;
  if (outfName) {// recreate
    fOut = new TFile(outfName,"recreate");
    opt = 0;
  }
  Int_t    npT    = 101;
  //  Double_t pTMax =   10;
  const Double_t ptBins[102] = {
    0.07, 0.08, 0.11, 0.14, 0.16, 0.17, 0.19, 0.21, 0.22, 0.23,
    0.24, 0.26, 0.27, 0.28, 0.29, 0.30, 0.31, 0.32, 0.33, 0.34,
    0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41, 0.42, 0.43, 0.44,
    0.45, 0.46, 0.47, 0.48, 0.49, 0.50, 0.51, 0.53, 0.54, 0.55,
    0.56, 0.57, 0.58, 0.60, 0.61, 0.62, 0.63, 0.65, 0.66, 0.67,
    0.69, 0.70, 0.72, 0.73, 0.75, 0.76, 0.78, 0.80, 0.81, 0.83,
    0.85, 0.87, 0.89, 0.91, 0.93, 0.96, 0.98, 1.01, 1.03, 1.06,
    1.09, 1.12, 1.16, 1.19, 1.23, 1.27, 1.31, 1.35, 1.40, 1.45,
    1.51, 1.57, 1.64, 1.71, 1.80, 1.89, 2.00, 2.11, 2.24, 2.39,
    2.57, 2.78, 3.05, 3.38, 3.80, 4.28, 4.96, 5.88, 7.25, 8.55,
   10.00, 25.0
  };
  for (Int_t s = kPositive; s < kTotalSigns; s++) {
    for (Int_t k = kGl; k < kTotalT; k++) {
      hists[k][s] = new TH2D(Form("%s%s",NameCharge[s],NameTrType[k]), 
				    Form("%s %s",TitleCharge[s],TitleTrType[k]),
			     npT, ptBins, 4, -1., 1.);
      hists[k][s]->GetXaxis()->SetTitle("pT (GeV/c)");   
      hists[k][s]->SetMarkerColor(s+1);       
      hists[k][s]->SetMarkerStyle(20);       
    }
    Mass2[s] = new TH2D(Form("M2%s",NameCharge[s]), 
			Form("ToF M^2 %s Primary",TitleCharge[s]),
			     npT, ptBins, 160, -0.05, 1.55);
  }
}
//________________________________________________________________________________
void Draw() {// -/+ ratio
  for (Int_t k = kGl; k < kTotalT; k++) {
    TH2D *ratio = new TH2D(*hists[k][kPositive]);
    TString Name(ratio->GetName());
    Name.ReplaceAll(NameCharge[kPositive],"N2Pratio");
    TString Title(ratio->GetTitle());
    Title.ReplaceAll(TitleCharge[kPositive],"Ratio (-/+) ");
    ratio->SetName(Name); ratio->SetTitle(Title);
    ratio->Reset();
    TAxis *x = ratio->GetXaxis();
    TAxis *y = ratio->GetYaxis();
    Int_t nx = x->GetNbins();
    Int_t ny = y->GetNbins();
    for (Int_t i = 1; i <= nx; i++) {
      Double_t dx = x->GetBinWidth(i);
      for (Int_t j = 1; j <= ny; j++) {
	Double_t dy = y->GetBinWidth(i);
	Double_t N1 = hists[k][kNegative]->GetBinContent(i,j);
	Double_t N2 = hists[k][kPositive]->GetBinContent(i,j);
	if (N2 > 0) {
	  Double_t R = N1/N2;
	  Double_t dR = TMath::Sqrt(R*(1+R)/N2);
	  ratio->SetBinContent(i,j,R);
	  ratio->SetBinError(i,j,dR);
	}
	hists[k][kNegative]->SetBinContent(i,j,N1/(dx*dy));
	hists[k][kPositive]->SetBinContent(i,j,N2/(dx*dy));
      }
    }
  }
}
//________________________________________________________________________________
Bool_t Accept(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  //  if (! gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  //  if (  gTrack->nHitsFit() < 10) return kFALSE;
  if (  gTrack->nHitsFit() < 15) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  if (  gTrack->nHitsFit() < 0.52*gTrack->nHitsPoss()) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
void MupT(const char* file="*.MuDst.root",
	  const  char* outFile="MupTCut.root") {
  StMuTimer timer;
  Init(outFile);
  timer.start();
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",file,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent",
    "PrimaryVertices",
    "PrimaryTracks",
    "GlobalTracks",
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  timer.reset();
  timer.start();
  TChain *tree = maker->chain();
  if (! tree) {
    cout << "No tree found" << endl;
    return;
  }
  Long64_t nentries = tree->GetEntries();
  cout << nentries << " events in chain " << endl;
  if (nentries < 100) return;
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,30,0)  
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nentries);
#endif
  TChain *chainT = maker->chain();
  TFile *oldfile = 0;
  for (Long64_t ev = 0; ev < nentries; ev++) {
    if (maker->Make()) break;
    if (oldfile != chainT->GetFile()) {
      oldfile = chainT->GetFile();
      cout << "New file " << oldfile->GetName() << " has been opened" << endl;
    }
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    StMuEvent* evt = mu->event();
    if (! evt) continue;
    const StTriggerId &triggerId = evt->triggerIdCollection().nominal();
#if 0
#if defined(__Y2011__)
    // Y2011 vpd-zdc-mb-protected from http://www.star.bnl.gov/protected/common/common2011/trigger2011/streams.html
    if (! (triggerId.isTrigger(    22) ||
	   triggerId.isTrigger(350013) ||
	   triggerId.isTrigger(350023) ||
	   triggerId.isTrigger(350033) ||
	   triggerId.isTrigger(350043))) continue;
#elif defined(__Y2014__)
    // Y2014 AuAu15, BBC_mb http://www.star.bnl.gov/protected/common/common2014/trigger2014/streams.html
    if (! (triggerId.isTrigger(     8) ||
	   triggerId.isTrigger(440005) ||
	   triggerId.isTrigger(440015))) continue;
#else
    cout << "Trigger set is not defined" << endl;
    return;
#endif
#endif
    //    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    // cout << " #" << ev;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  // cout << "\tPrimaryVertices " << NoPrimaryVertices;
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();  // cout << "\tPrimaryTracks " << NoPrimaryTracks;
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();  // cout << "\tGlobalTracks " << NoGlobalTracks;
    // cout << endl;
    for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! Accept(gTrack)) continue;
      TrackMatchType s = kPositive;
      if (gTrack->charge() < 0) s = kNegative;
      hists[kGl][s]->Fill(gTrack->pt(),gTrack->eta());
    }
    Int_t lBestVx = -1;
    Float_t BestRank = -1e10;
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (Vtx->ranking() > BestRank) {BestRank = Vtx->ranking(); lBestVx = l;}
    }
    if (lBestVx < 0 || BestRank < 0) continue;
    StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(lBestVx);
    if (TMath::Abs(Vtx->position().z()) > 50) continue;
    for (Int_t l = lBestVx; l <= lBestVx; l++) {
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	if (! pTrack) continue;
        if (pTrack->vertexIndex() != l) continue;
	if (! Accept(pTrack)) continue;
	TrackMatchType s = kPositive;
	if (pTrack->charge() < 0) s = kNegative;
	hists[kPr][s]->Fill(pTrack->pt(),pTrack->eta());
	Double_t p2 = pTrack->p().mag2();
	const StMuBTofPidTraits &btof = pTrack->btofPidTraits();
	Double_t beta = btof.beta();
	if (beta <= 0) continue;
	Double_t beta2 = beta*beta;
	Double_t mass2 = p2*(1./beta2 - 1);
	Mass2[s]->Fill(pTrack->pt(),mass2);
      }
    }
    if (ev%1000 == 0) cout << "Done with event\t" << ev << endl;
  }
  Draw();
  if (fOut) fOut->Write();
}
//________________________________________________________________________________
void DrawM2() {
   gStyle->SetOptStat(0);
   TH2D *M2Pos = (TH2D *) gDirectory->Get("M2Pos");
   TH2D *M2Neg = (TH2D *) gDirectory->Get("M2Neg");
   if (! M2Pos || ! M2Neg) return;
   M2Pos->GetXaxis()->SetRange(1,95);
   M2Pos->GetYaxis()->SetRange(61,120);
   M2Pos->FitSlicesY();
   TH1D *M2Pos_1 = (TH1D *) gDirectory->Get("M2Pos_1");
   M2Pos_1->GetXaxis()->SetRange(1,95);
   
   M2Neg->GetXaxis()->SetRange(1,95);
   M2Neg->GetYaxis()->SetRange(61,120);
   M2Neg->FitSlicesY();
   TH1D *M2Neg_1 = (TH1D *) gDirectory->Get("M2Neg_1");
   M2Neg_1->GetXaxis()->SetRange(1,95);
   M2Neg_1->SetMarkerColor(2);
   M2Pos_1->SetTitle("Mass^{2} versus p_{T}");
   M2Pos_1->SetXTitle("p_{T}");
   M2Pos_1->SetYTitle("Mass^{2}");
   M2Pos_1->Draw();
   TLegend *l = new TLegend(0.3,0.2,0.5,0.5);
   l->AddEntry(M2Pos_1,"p");
   M2Neg_1->Draw("same");
   l->AddEntry(M2Neg_1,"pbar");
   l->Draw();
}
//________________________________________________________________________________
void DrawPrpTRatio() {
   gStyle->SetOptStat(0);
   TH2D *PosPr = (TH2D *) gDirectory->Get("PosPr");
   TH2D *NegPr = (TH2D *) gDirectory->Get("NegPr");
   if (! PosPr || ! NegPr) return;
   PosPr->GetXaxis()->SetRange(1,95);
   PosPr->GetYaxis()->SetRange(2,3);
   TH1D *PosPr_px = PosPr->ProjectionX();
   PosPr_px->GetXaxis()->SetRange(1,95);
   
   NegPr->GetXaxis()->SetRange(1,95);
   NegPr->GetYaxis()->SetRange(2,3);
   NegPr->FitSlicesY();
   TH1D *NegPr_px = NegPr->ProjectionX();
   TH1D *ratio = new TH1D(*PosPr_px);
   ratio->Reset();
   ratio->SetName("RPosNeg");
   ratio->SetTitle("Ratio (+)/(-)");
   Int_t nx = ratio->GetNbinsX();
   for (Int_t bin = 1; bin <= nx; bin++) {
     Double_t v1 = PosPr_px->GetBinContent(bin);
     Double_t e1 = PosPr_px->GetBinError(bin);
     Double_t v2 = NegPr_px->GetBinContent(bin);
     Double_t e2 = NegPr_px->GetBinError(bin);
     Double_t r  = 0;
     if (v2 > 0) r = v1/v2;
     Double_t e = 0;
     if (v1 > 0) e += (e1/v1)*(v1/v1);
     if (v2 > 0) e += (e2/v2)*(v2/v2);
     e = TMath::Sqrt(e);
     ratio->SetBinContent(bin,r);
     ratio->SetBinError(bin,e);
   }
   ratio->GetXaxis()->SetRange(1,95);
   ratio->SetXTitle("p_{T}");
   ratio->SetYTitle("(+)/(-)");
   ratio->Draw();
}
