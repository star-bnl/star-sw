/* 
   root.exe -q -b -x  'lMuDst.C(0,"*.MuDst.root")' rMuHFT.C+
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
  if (TMath::Abs(Vtx->position().z()) > zcut) return kFALSE;
  //  if (! Vtx->idTruth())  return kFALSE;
  //  if (  Vtx->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
void Fill(TH2D *hist[kVarX][kVarY], Double_t VarX[kVarX], Double_t VarY[kVarY]) {
  for (Int_t j = 0; j < kVarX; j++) 
    for (Int_t k = 0; k < kVarY; k++)
      hist[j][k]->Fill(VarX[j],VarY[k]);
}
//________________________________________________________________________________
void rMuHFT(Long64_t nevent = 9999999, const  char* outFile="rMuHFT.root") {
  TFile *fOut   = new TFile(outFile,"recreate");
  TH1F *VxZ     = new TH1F("VxZ","Vertex Z",20,-10.,10.);
  TH2F *EtapT   = new TH2F("EtapT","track #eta versus pT",100,-1,1,100,0.,5.);
  TH2F *HftHits = new TH2F("HftHist","No. of HFT hits versus no. of TPC hits",32,14.5,46.5,16,-0.5,15.5);
  TString zCut(Form(" vs no. of Possible ones for primary tracks with primary vertex |Z| < %f cm", zcut));
  TString Name;
  TString Title;
  Name = "NoPxlHits"; Title = "No.of fitted PXL hits"; Title += zCut;
  TH2D *NoPxlHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoIstHits"; Title = "No.of fitted IST hits"; Title += zCut;
  TH2D *NoIstHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoSstHits"; Title = "No.of fitted SST hits"; Title += zCut;
  TH2D *NoSstHits = new TH2D(Name, Title,10,0,10,10,0,10); 
  Name = "NoPxlIstSstHits"; Title = "No.of fitted Pxl,Ist and SST hits"; Title += zCut;
  TH2D *hists[kRCMC][kSel][kVarX][4];
  for (Int_t iRM = 0; iRM < kRCMC; iRM++) {
    for (Int_t i = 0; i < kSel; i++) {
      for (Int_t j = 0; j < kVarX; j++) {
	for (Int_t k = 0; k < kVarY; k++) {
	  Name = Form("%s%s%s%i",NameRcMc[iRM],varN[k],pTpN[j],i); Title = Form("%s versus %s for %s %s",varN[k],pTpN[j],sel[i],NameRcMc[iRM]);
	  Int_t    nx   = 100;
	  Double_t xmin =   0;
	  Double_t xmax =  10;
	  if (j == 2) {nx = 90; xmin = -180; xmax = 180;}
	  Int_t    ny   = 500;
	  Double_t ymin =  -0.1;
	  Double_t ymax =   0.1;
	  if (i > 2) {ymin = -1; ymax = 1;}
	  if (k == 1 || k == 3) {ymin = -20; ymax = 20;}
	  hists[iRM][i][j][k] = new TH2D(Name,Title,nx,xmin,xmax,ny,ymin,ymax);
	}
      }
    }
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
      if (! AcceptVX(Vtx)) continue; //
      VxZ->Fill(Vtx->position().z());
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
	StTrackTopologyMap topologyMap = pTrack->topologyMap();
	UInt_t noPxlHits = topologyMap.numberOfHits(kPxlId); // 0-3 
	UInt_t noIstHits = topologyMap.numberOfHits(kIstId); // 0-2
	UInt_t noSstHits = topologyMap.numberOfHits(kSstId); // 0-2
	UInt_t noHftHits = noPxlHits + 3*(noIstHits + 2*noSstHits);
	UInt_t noTpcHits = topologyMap.numberOfHits(kTpcId); // 0-45
	Int_t iok = 3;
	if (noPxlHits && noIstHits) iok = 0;
	if (noPxlHits && noSstHits) iok = 1;
	if (noPxlHits && noIstHits && noSstHits) iok = 2;
	
// 	HftHits->Fill(noTpcHits,noHftHits);
// 	Int_t NoFPxlHits =  (mNHitsFitInner[k] & 0x7);
// 	Int_t NoFSstHits = ((mNHitsFitInner[k] & 0x18) >> 3);
// 	Int_t NoFPxlIstSstHits = NoFPxlHits +  NoFSstHits;
// 	Int_t NoPPxlHits =  (mNHitsPossInner[k] & 0x7);
// 	Int_t NoPSstHits = ((mNHitsPossInner[k] & 0x18) >> 3);
// 	Int_t NoPPxlIstSstHits = NoPPxlHits + NoPSstHits;
//	NoPxlHits->Fill(NoPPxlHits,NoFPxlHits);
//	NoSstHits->Fill(NoPSstHits,NoFSstHits);
//	NoPxlIstSstHits->Fill(NoPPxlIstSstHits,NoFPxlIstSstHits);
	Double_t phi = TMath::RadToDeg()*pTrack->p().phi();
	Double_t pT  = pTrack->p().perp();
	Double_t p   = pTrack->p().mag();
	Int_t N = 0;
	//	N = NoFPxlIstSstHits;
	Int_t kg = pTrack->index2Global();
	const StMuTrack *gTrack = pTrack->globalTrack();
	if (! gTrack) continue;
	const StDcaGeometry* dcaG = gTrack->dcaGeom();
	if (! dcaG) continue;
        THelixTrack     thelix =  dcaG->thelix();
        thelix.Move(thelix.Path(vtx));
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
void Plots() {
  TH1::SetDefaultSumw2();
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TH2D *****hists = new TH2D****[nn];
  Int_t NF = 0;
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    hists[NF] = new TH2D***[kSel];
    FitFiles[NF] = f;
    for (Int_t i = 0; i < kSel; i++) {
      hists[NF][i] =  new TH2D**[kVarX];
      for (Int_t j = 0; j < kVarX; j++) {
	hists[NF][i][j] =  new TH2D*[kVarY];
	for (Int_t k = 0; k < kVarY; k++) {
	  hists[NF][i][j][k] = (TH2D*) f->Get(Form("%s%s%i",varN[k],pTpN[j],i));
#if 0
	  if (hists[NF][i][j][k]) cout << "Found";
	  else                    cout << "Not found";
	  cout << " plot " << Form("%s%s%i",varN[k],pTpN[j],i) << " in file " << f->GetName() << endl;
#endif
	}
      }
    }
    NF++;
  }
#if 1
  {
    // sigma_dca_XY versus 1/pT
    Int_t x = 0; // InvpT
    Int_t y = 0; // dcaXY
    for (Int_t i = kSel - 2; i >= 0; i--) {
      TString cName(Form("%s_%s_%s",sel[i],pTp[x],varN[y]));
      TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(cName);
      if (c) c->Clear();
      else   c = new TCanvas(cName,cName);
      Double_t ymax = 0.02;
      if (i == kSel - 2) ymax = 0.6;
      TH1F *frame = c->DrawFrame(0,0,5, ymax);
      frame->SetTitle(sel[i]);
      frame->SetXTitle(pTp[x]);
      frame->SetYTitle(Form("#sigma %s",varN[y]));
      TLegend *l = new TLegend(0.5,0.2,0.7,0.4);
      l->Draw();
      for (Int_t m = 0; m < NF; m++) {
	TString NameF(FitFiles[m]->GetName());
	NameF.ReplaceAll("P.root","+");
	NameF.ReplaceAll("N.root","-");
	NameF.ReplaceAll(".root","");
	NameF.ReplaceAll("rMuHFT","");
	NameF.ReplaceAll("pi","#pi");
	FitFiles[m]->cd();
	hists[m][i][x][y]->FitSlicesY();
	TH1D *sigma = (TH1D *) FitFiles[m]->Get(Form("%s_2",hists[m][i][x][y]->GetName()));
	if (! sigma) continue;
	sigma->SetMarkerColor(m+1);
	sigma->SetLineColor(m+1);
	sigma->Draw("same");
	l->AddEntry(sigma,NameF);
	c->Update();
      }
    }
  }
#endif
  {
    // Efficiency
    TString cName("Efficiency");
    TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(cName);
    if (c) c->Clear();
    else   c = new TCanvas(cName,cName);
    TH1F *frame = c->DrawFrame(0,0,5,100.);
    Int_t x = 0; // InvpT
    Int_t y = 0; // dcaXY
    frame->SetTitle(cName);
    frame->SetXTitle(pTp[x]);
    frame->SetYTitle("(%)");
    Int_t ref = kSel - 1;
    TLegend *l = new TLegend(0.5,0.2,0.7,0.4);
    l->Draw();
    for (Int_t m = 0; m < NF; m++) {
      TString NameF(FitFiles[m]->GetName());
      NameF.ReplaceAll("P.root","+");
      NameF.ReplaceAll("N.root","-");
      NameF.ReplaceAll(".root","");
      NameF.ReplaceAll("rMuHFT","");
      NameF.ReplaceAll("pi","#pi");
      FitFiles[m]->cd();
      TH1D *refH = hists[m][ref][x][y]->ProjectionX();
      for (Int_t i = kSel - 2; i >= 0; i--) {
	TH1D *proj = hists[m][i][x][y]->ProjectionX();
	TH1D *eff  = new TH1D(*proj); eff->SetName(Form("Eff_%s",proj->GetName()));
	eff->Divide(proj,refH,1,1,"b");
	eff->Scale(100.);
	eff->SetMarkerColor(m+1);
	eff->SetLineColor(m+1);
	eff->SetMarkerStyle(20+i);
	l->AddEntry(eff,Form("%s %s",sel[i],NameF.Data()));
	eff->Draw("samep");
      }
    }
  }
}

