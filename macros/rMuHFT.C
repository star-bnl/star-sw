/* 
   root.exe -q -b -x  'lMuDst.C(0,"*.MuDst.root")' rMuHFT.C+ >& rMuHFT.log &
*/
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "SystemOfUnits.h"
#include "StBFChain/StBFChain.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StarRoot/TPolynomial.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "Names.h"
#include "StBichsel/Bichsel.h"
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
enum TrackMatchType {kPositive, kNegative, kTotalSigns};                                     // switch between charges
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
enum {kRC  = 0, kMC = 1, kRCMC = 2};
enum {kSel = 5, kVarX = 3, kVarY = 4};
const Char_t *NameRcMc[2] = {"RC","MC"};
const Char_t *NameCharge[kTotalSigns] = {"Pos", "Neg"};
const Char_t *TitleCharge[kTotalSigns] = {"(+)", "(-)"};
const Double_t zcut = 5;
const Char_t *sel[kSel] = {"Pxl+Ist","Pxl+Sst","Pxl+Ist+Sst","No HFT","All"}; // i
const Char_t *pTp[kVarX] = {"1/pT","1/p","Phi"}; // j
const Char_t *pTpN[kVarX] = {"InvpT","Invp","Phi"}; // jj
const Char_t *varN[kVarY] = {"dcaXY","pullXY","dcaZ","pullZ"}; // k
TFile **gFitFiles = 0;
TH2F ******gHists = 0;
Int_t gNF = 0;
//________________________________________________________________________________
static Int_t _debug = 0;
void SetDebug(Int_t k) {_debug = k;}
Int_t Debug() {return _debug;}
//________________________________________________________________________________
Bool_t Accept(const StMuTrack *gTrack = 0) {
  if (! gTrack)            return kFALSE;
  //  if (! gTrack->idTruth()) return kFALSE;
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < 15) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t AcceptVX(const StMuPrimaryVertex *Vtx = 0) {
  if (! Vtx) return kFALSE;
  StThreeVectorF V = Vtx->position();
  if (TMath::Abs(V.z()) > zcut) return kFALSE;
  if (! (-0.3 < V.x() && V.x() < 0.1 && -0.27 < V.y() && V.y() < - 0.13)) return kFALSE;
  //  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
void Fill(TH2F *hist[kVarX][kVarY], Double_t VarX[kVarX], Double_t VarY[kVarY]) {
  for (Int_t j = 0; j < kVarX; j++) 
    for (Int_t k = 0; k < kVarY; k++)
      hist[j][k]->Fill(VarX[j],VarY[k]);
}
//________________________________________________________________________________
void rMuHFT(Long64_t nevent = 9999999, const  char* outFile="rMuHFTCut6Vx.root") {
  TFile *fOut   = new TFile(outFile,"recreate");
  TH1F *VxZ     = new TH1F("VxZ","Vertex Z",20,-10.,10.);
  TH2F *Vxy     = new TH2F("Vxy","Vertex XY",200,-2.,2.,200,-2.,2.);
  TH2F *EtapT   = new TH2F("EtapT","track #eta versus pT",100,-1,1,100,0.,5.);
  TString zCut(Form(" vs no. of Possible ones for primary tracks with primary vertex |Z| < %f cm", zcut));
  TString Name;
  TString Title;
  TH2F *hists[kRCMC][kSel][kVarX][4];
  TH2F *NoHftHits[2] = {0};
  TH2F *HftTpcHits[2] = {0};
  for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
    for (Int_t i = 0; i < kSel; i++) {
      for (Int_t j = 0; j < kVarX; j++) {
	for (Int_t k = 0; k < kVarY; k++) {
	  Name = Form("%s%s%s%i",NameRcMc[iRM],varN[k],pTpN[j],i); Title = Form("%s versus %s for %s %s",varN[k],pTpN[j],sel[i],NameRcMc[iRM]);
	  Int_t    nx   = 100;
	  Double_t xmin =   0;
	  Double_t xmax =  10;
	  if (j == 2) {nx = 90; xmin = -180; xmax = 180;}
	  Int_t    ny   = 2000;
	  Double_t ymin =  -0.1;
	  Double_t ymax =   0.1;
	  if (i > 2) {ymin = -1; ymax = 1;}
	  if (k == 1 || k == 3) {ymin = -20; ymax = 20;}
	  hists[iRM][i][j][k] = new TH2F(Name,Title,nx,xmin,xmax,ny,ymin,ymax);
	}
      }
    }
    Name = NameRcMc[iRM]; Name += "NoHftHits"; 
    Title = NameRcMc[iRM]; Title += "No.of fitted HFT hits versus no. of possible ones"; Title += zCut;
    NoHftHits[iRM] = new TH2F(Name, Title,12,-.5,11.5,12,-.5,11.5); 
    Name = NameRcMc[iRM]; Name += "HftTpcHits";
    Title =  NameRcMc[iRM]; Title += ": No. of HFT hits versus no. of TPC hits";
    HftTpcHits[iRM] = new TH2F(Name,Title,32,14.5,46.5,16,-0.5,15.5);
  }
  StBFChain *chain = (StBFChain *) StMaker::GetTopChain();
  StMuDebug::setLevel(0);  
  StMuDstMaker* maker = (StMuDstMaker *) chain->Maker("MuDst");
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {"MuEvent"
				    ,"PrimaryVertices"
				    ,"PrimaryTracks"
				    ,"GlobalTracks"
				    ,"CovGlobTrack"
#if 0
				    ,"StStMuMcVertex"
				    ,"StStMuMcTrack"
#endif
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
  StMuDebug::setLevel(0);  
  TChain *tree = maker->chain();
  if (! tree) return;
  Long64_t nentries = tree->GetEntries();
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  if (nentries < 100) return;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);

  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (chain->MakeEvent()) break;
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    // cout << " #" << ev;
    //    Int_t referenceMultiplicity = muEvent->refMult(); // get the reference multiplicity
    // cout << " refMult= "<< referenceMultiplicity;
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  // cout << "\tPrimaryVertices " << NoPrimaryVertices;
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();  // cout << "\tPrimaryTracks " << NoPrimaryTracks;
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          
    Int_t NoDca = CovGlobTrack->GetEntriesFast(); if (Debug()) {cout << "\tCovGlobTrack " << NoDca;}
    for (Int_t l = 0; l < NoPrimaryVertices; l++) {
      StMuPrimaryVertex *Vtx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(l);
      if (l) continue;
      VxZ->Fill(Vtx->position().z());
      Vxy->Fill(Vtx->position().x(),Vtx->position().y());
      if (! AcceptVX(Vtx)) continue; //
      //      cout << *Vtx << endl;
#ifdef  DEBUG
      cout << "Primary l = " << l 
	   << " x " << PrimVertexX[l] << " +/- " << PrimVerSigX[l]
	   << " y " << PrimVertexY[l] << " +/- " << PrimVerSigY[l]
	   << " z " << PrimVertexZ[l] << " +/- " << PrimVerSigZ[l] << endl;
#endif
      Double_t vtx[3] = {Vtx->position().x(),Vtx->position().y(),Vtx->position().z()};
      Int_t noGoodPrimaryTracks = 0;
      for (Int_t k = 0; k < NoPrimaryTracks; k++) {
	StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
	if (! pTrack) continue;
        if (pTrack->vertexIndex() != l) continue;
	if (! Accept(pTrack)) continue;
	Int_t iRM = kRC;
	if (pTrack->idTruth()) iRM = kMC;
	//	cout << *pTrack << endl;
	EtapT->Fill(pTrack->eta(), pTrack->pt());
	Int_t NoPxlFits = pTrack->nHitsFit(kPxlId); // 0-3 
	Int_t NoIstFits = pTrack->nHitsFit(kIstId); // 0-2
	Int_t NoSstFits = pTrack->nHitsFit(kSstId); // 0-2
	Int_t NoHftFits = TMath::Min(2,NoPxlFits) + 3*(TMath::Min(1,NoIstFits) + 2*TMath::Min(1,NoSstFits));
	Int_t NoTpcFits = pTrack->nHitsFit(kTpcId); // 0-45
	Int_t iok = 3;
	if (NoPxlFits && NoIstFits) iok = 0;
	if (NoPxlFits && NoSstFits) iok = 1;
	if (NoPxlFits && NoIstFits && NoSstFits) iok = 2;
	Int_t NoPPxlHits = pTrack->nHitsPoss(kPxlId); // 0-3 
	Int_t NoPIstHits = pTrack->nHitsPoss(kIstId); // 0-2
	Int_t NoPSstHits = pTrack->nHitsPoss(kSstId); // 0-2
	Int_t NoPHftHits = TMath::Min(2,NoPPxlHits) + 3*(TMath::Min(1,NoPIstHits) + 2*TMath::Min(1,NoPSstHits));
	Int_t NoPTpcHits = pTrack->nHitsPoss(kTpcId); // 0-45
	HftTpcHits[iRM]->Fill(NoTpcFits,NoHftFits);
	NoHftHits[iRM]->Fill(NoPHftHits,NoHftFits);
	// Cut 
	if (NoPPxlHits < 2 || ! NoPIstHits || ! NoPSstHits) continue;
	Double_t phi = TMath::RadToDeg()*pTrack->p().phi();
	Double_t pT  = pTrack->p().perp();
	Double_t p   = pTrack->p().mag();
	Int_t kg = pTrack->index2Global();
	const StMuTrack *gTrack = pTrack->globalTrack();
	if (! gTrack) continue;
	const StDcaGeometry* dcaG = gTrack->dcaGeom();
	if (! dcaG) continue;
        THelixTrack     thelix =  dcaG->thelix();
	Double_t path = thelix.Path(vtx);
        thelix.Move(path);
	Double_t dcaXY = dcaG->impact();
	Double_t dcaZ  = dcaG->z();
	Double_t dcaEmx[3];
	thelix.Dca(vtx,dcaXY,dcaZ,dcaEmx,2);
	Double_t sigmaXY   = 0;
	Double_t sigmaZ    = 0;
        if (dcaEmx[0] > 0) sigmaXY = TMath::Sqrt(dcaEmx[0]);
        if (dcaEmx[2] > 0) sigmaZ  = TMath::Sqrt(dcaEmx[2]);
	if (sigmaXY <= 0 || sigmaZ > 1 || sigmaZ <= 0 || sigmaZ > 1) {
#ifdef DEBUG
	  cout << "First Point x/y/z = " 
	       << GlobalTracks_mFirstPoint_mX1[kg] << "/" << GlobalTracks_mFirstPoint_mX2[kg] << "/" << GlobalTracks_mFirstPoint_mX3[kg] << endl;
	  cout << "Last Point x/y/z = " 
	       << GlobalTracks_mLastPoint_mX1[kg] << "/" << GlobalTracks_mLastPoint_mX2[kg] << "/" << GlobalTracks_mLastPoint_mX3[kg] << endl;
#endif
	  continue;
	}
	Double_t pullXY = dcaXY/sigmaXY;
	Double_t pullZ  = dcaZ /sigmaZ;
	//	Double_t pullXYVx = dcaXY/sigmaXYVx;
	//	Double_t pullZVx  = dcaZ /sigmaZVx;
	//	Double_t pull = TMath::Sqrt(pullXY*pullXY + pullZ*pullZ);
	Double_t Varx[kVarX] = {1./pT, 1./p, phi};
	Double_t Vary[kVarY] = {dcaXY, pullXY, dcaZ, pullZ};
	Fill (hists[iRM][iok], Varx, Vary);
	Fill (hists[iRM][4], Varx, Vary);
      }
    }
  }
  if (fOut) fOut->Write();
}
//________________________________________________________________________________
void Init() {
  if (gNF) return;
  TH1::SetDefaultSumw2();
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  gFitFiles = new TFile *[nn];
  TIter next(files);
  gHists = new TH2F*****[nn];
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    gHists[gNF] = new TH2F****[kRCMC];
    gFitFiles[gNF] = f;
    for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
      gHists[gNF][iRM] = new TH2F***[kSel];
      for (Int_t i = 0; i < kSel; i++) {
	gHists[gNF][iRM][i] =  new TH2F**[kVarX];
	for (Int_t j = 0; j < kVarX; j++) {
	  gHists[gNF][iRM][i][j] =  new TH2F*[kVarY];
	  for (Int_t k = 0; k < kVarY; k++) {
	    gHists[gNF][iRM][i][j][k] = (TH2F*) f->Get(Form("%s%s%s%i",NameRcMc[iRM],varN[k],pTpN[j],i));
#if 0
	  if (gHists[gNF][iRM][i][j][k]) cout << "Found";
	  else                    cout << "Not found";
	  cout << " plot " << Form("%s%s%i",varN[k],pTpN[j],i) << " in file " << f->GetName() << endl;
#endif
	  }
	}
      }
    }
    gNF++;
  }
}
//________________________________________________________________________________
void Plots(Int_t x = 1, Int_t y = 0) {
  if (x < 0 || x >= kVarX) return;
  if (y < 0 || y >= kVarY) return;
  Init(); 
// sigma_dca_XY versus 1/pT
  //  Int_t x = 0; // InvpT
  //  Int_t x = 1; // Invp
  //  Int_t y = 0; // dcaXY
  for (Int_t i = kSel - 2; i >= -1; i--) {
    TString cName;
    cName += (Form("%s_%s_%s",sel[i],pTp[x],varN[y]));
    TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(cName);
    if (c) c->Clear();
    else   c = new TCanvas(cName,cName);
    Double_t xmin = 0;
    Double_t xmax = 5;
    if (x >= 2) {xmin = -180; xmax = 180;}
    Double_t ymax = 0.02e4;
    if (i == kSel - 2) ymax = 0.6e4;
    Double_t ymin = -0.1*ymax;
    if (y == 1 || y == 3) {
      ymin = -1; ymax = 5;
    }
    TH1F *frame = c->DrawFrame(xmin,ymin,xmax, ymax);
    if (i >= 0) frame->SetTitle(sel[i]);
    else        frame->SetTitle("Hft");
    if (x < 2) 
      frame->SetXTitle(Form("%s (GeV/c)",pTp[x]));
    else 
      frame->SetXTitle(Form("%s (^{o})",pTp[x]));
    frame->SetYTitle(Form("%s (#mum)",varN[y]));
    TLegend *l = new TLegend(0.5,0.2,0.7,0.4);
    l->Draw();
    for (Int_t m = 0; m < gNF; m++) {
      TString NameF(gFitFiles[m]->GetName());
      NameF.ReplaceAll("P.root","+");
      NameF.ReplaceAll("N.root","-");
      NameF.ReplaceAll(".root","");
      NameF.ReplaceAll("rMuHFT","");
      NameF.ReplaceAll("pi","#pi");
      gFitFiles[m]->cd();
      for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
	TString NameP(NameF);
	if (! iRM) {NameP.ReplaceAll("#pi",""); NameP.ReplaceAll("+",""); NameP.ReplaceAll("-",""); }
	TH2F *cHist = 0;
	if (i >= 0) {cHist = gHists[m][iRM][i][x][y];}
	else {
	  cHist = new TH2F(*gHists[m][iRM][0][x][y]);
	  cHist->SetName(Form("%s%s%s",NameRcMc[iRM],varN[y],pTpN[x])); 
	  cHist->Add(gHists[m][iRM][1][x][y]);
	  cHist->Add(gHists[m][iRM][2][x][y]);
	}
	cHist->FitSlicesY();
	TH1D *mu = (TH1D *) gFitFiles[m]->Get(Form("%s_1",cHist->GetName()));
	if (! mu) continue;
	if (mu->GetEntries() < 1) continue;
	if (y != 1 && y !=3) mu->Scale(1e4);
	Int_t marker = 21;
	if (! iRM) marker = 25;
	mu->SetMarkerStyle(marker);
	mu->SetMarkerColor(m+1);
	mu->SetLineColor(m+1);
	mu->Draw("same");
	l->AddEntry(mu,Form("%s #mu: %s",NameRcMc[iRM],NameP.Data()));
	TH1D *sigma = (TH1D *) gFitFiles[m]->Get(Form("%s_2",cHist->GetName()));
	if (! sigma) continue;
	if (sigma->GetEntries() < 1) continue;
	if (y != 1 && y !=3)  sigma->Scale(1e4);
	marker = 20;
	if (! iRM) marker = 24;
	sigma->SetMarkerStyle(marker);
	sigma->SetMarkerColor(m+1);
	sigma->SetLineColor(m+1);
	sigma->Draw("same");
	l->AddEntry(sigma,Form("%s #sigma: %s",NameRcMc[iRM],NameP.Data()));
	c->Update();
      }
    }
  }
}
//________________________________________________________________________________
void PlotEff(Int_t x = 1, Int_t y = 0) {
  if (x < 0 || x >= kVarX) return;
  if (y < 0 || y >= kVarY) return;
  Init(); 
  // Efficiency
  Int_t ref = kSel - 1;
  for (Int_t m = 0; m < gNF; m++) {
    TString NameF(gFitFiles[m]->GetName());
    NameF.ReplaceAll("P.root","+");
    NameF.ReplaceAll("N.root","-");
    NameF.ReplaceAll(".root","");
    NameF.ReplaceAll("rMuHFT","");
    NameF.ReplaceAll("pi","#pi");
    if (NameF.Contains("RC")) {NameF.ReplaceAll("#pi",""); NameF.ReplaceAll("+",""); NameF.ReplaceAll("-",""); }
    gFitFiles[m]->cd();
    for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
      TString cName(NameRcMc[iRM]);
      cName += NameF;
      TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(cName);
      if (c) c->Clear();
      else   c = new TCanvas(cName,cName);
      TH1F *frame = 0;
      if (x < 2) frame = c->DrawFrame(0,0,5, 100);
      else       frame = c->DrawFrame(-180,0,180, 100);
      frame->SetTitle(cName);
      if (x < 2) 
	frame->SetXTitle(Form("%s (GeV/c)",pTp[x]));
      else 
	frame->SetXTitle(Form("%s (^{o})",pTp[x]));
      frame->SetYTitle("(%)");
      TLegend *l = new TLegend(0.5,0.2,0.7,0.4);
      l->Draw();
      TH1D *refH = gHists[m][iRM][ref][x][y]->ProjectionX();
      for (Int_t i = kSel - 2; i >= 0; i--) {
	TH1D *proj = gHists[m][iRM][i][x][y]->ProjectionX();
	TH1D *eff  = new TH1D(*proj); eff->SetName(Form("Eff_%s",proj->GetName()));
	eff->Divide(proj,refH,1,1,"b");
	eff->Scale(100.);
	eff->SetMarkerColor(iRM+1);
	eff->SetLineColor(iRM+1);
	eff->SetMarkerStyle(20+i);
	if (x < 2) 
	  eff->Fit("pol0","er","samep",0.5,2.5);
	else 
	  eff->Fit("pol0","e","samep");
	TF1 *pol0 = (TF1 *) eff->GetListOfFunctions()->FindObject("pol0");
	if (pol0) 
	  l->AddEntry(eff,Form("%s: %s : %7.2f #pm %7.2f",NameRcMc[iRM],sel[i],pol0->GetParameter(0),pol0->GetParError(0)));
	else 
	  l->AddEntry(eff,Form("%s: %s",NameRcMc[iRM],sel[i]));
	//	eff->Draw("samep");
      }
    }
  }
}
//________________________________________________________________________________
void PlotEff2(Int_t x = 1, Int_t y = 0) {
  if (x < 0 || x >= kVarX) return;
  if (y < 0 || y >= kVarY) return;
  Init(); 
  // Efficiency
  Int_t ref = kSel - 1;
  Int_t kIS = 2;
  for (Int_t m = 0; m < gNF; m++) {
    TString NameF(gFitFiles[m]->GetName());
    NameF.ReplaceAll("P.root","+");
    NameF.ReplaceAll("N.root","-");
    NameF.ReplaceAll(".root","");
    NameF.ReplaceAll("rMuHFT","");
    NameF.ReplaceAll("pi","#pi");
    if (NameF.Contains("RC")) {NameF.ReplaceAll("#pi",""); NameF.ReplaceAll("+",""); NameF.ReplaceAll("-",""); }
    gFitFiles[m]->cd();
    for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
      TString cName(NameRcMc[iRM]);
      cName += NameF;
      TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(cName);
      if (c) c->Clear();
      else   c = new TCanvas(cName,cName);
      TH1F *frame = 0;
      if (x < 2) frame = c->DrawFrame(0,0,5, 100);
      else       frame = c->DrawFrame(-180,0,180, 100);
      frame->SetTitle(cName);
      if (x < 2) 
	frame->SetXTitle(Form("%s (GeV/c)",pTp[x]));
      else 
	frame->SetXTitle(Form("%s (^{o})",pTp[x]));
      frame->SetYTitle("(%)");
      TLegend *l = new TLegend(0.5,0.2,0.7,0.4);
      l->Draw();
      TH1D *refH = gHists[m][iRM][ref][x][y]->ProjectionX();
      TH1D *proj = gHists[m][iRM][kIS][x][y]->ProjectionX();
      TH1D *effIS = new TH1D(*proj); effIS->SetName(Form("Eff_%s",proj->GetName()));
      effIS->Divide(proj,refH,1,1,"b");
      effIS->Scale(100.);
      //      for (Int_t i = kSel - 2; i >= 0; i--) {
      for (Int_t i = 0; i < 2; i++) {
	TH1D *proj = gHists[m][iRM][i][x][y]->ProjectionX();
	TH1D *eff  = new TH1D(*proj); eff->SetName(Form("Eff_%s",proj->GetName()));
	eff->Divide(proj,refH,1,1,"b");
	eff->Scale(100.);
	eff->Add(effIS);
#if 0
	eff->SetMarkerColor(iRM+1);
	eff->SetLineColor(iRM+1);
#else
	eff->SetMarkerColor(i+1);
	eff->SetLineColor(i+1);
#endif
	eff->SetMarkerStyle(20+i);
	if (x < 2) 
	  eff->Fit("pol0","er","samep",0.5,2.5);
	else 
	  eff->Fit("pol0","e","samep");
	TF1 *pol0 = (TF1 *) eff->GetListOfFunctions()->FindObject("pol0");
	if (pol0) 
	  l->AddEntry(eff,Form("%s: %s : %7.2f #pm %7.2f",NameRcMc[iRM],sel[i],pol0->GetParameter(0),pol0->GetParError(0)));
	else 
	  l->AddEntry(eff,Form("%s: %s",NameRcMc[iRM],sel[i]));
	//	eff->Draw("samep");
      }
    }
  }
}

