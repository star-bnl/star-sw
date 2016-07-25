//#define DEBUG
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
#include "TNtuple.h"
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
TFile *fOut = 0;
//________________________________________________________________________________
struct Tree_t {
  Char_t             mBeg[1];        //!
  const Int_t  *&    MuEvent_mEventInfo_mRunId;
  const Int_t  *&    MuEvent_mEventInfo_mId;
  const Int_t   &    NoPrimaryVertices;                        
  const Float_t*&    PrimaryVertices_mPosition_mX1;            
  const Float_t*&    PrimaryVertices_mPosition_mX2;            
  const Float_t*&    PrimaryVertices_mPosition_mX3;  
  const Float_t*&    PrimaryVertices_mRanking;
  Char_t             mEnd[1];        //!
  Tree_t (TTreeIter &iter) : 
    MuEvent_mEventInfo_mRunId(iter("MuEvent.mEventInfo.mRunId")),
    MuEvent_mEventInfo_mId(iter("MuEvent.mEventInfo.mId")),
    NoPrimaryVertices(iter("PrimaryVertices")),
    PrimaryVertices_mPosition_mX1(iter("PrimaryVertices.mPosition.mX1")),
    PrimaryVertices_mPosition_mX2(iter("PrimaryVertices.mPosition.mX2")),
    PrimaryVertices_mPosition_mX3(iter("PrimaryVertices.mPosition.mX3")), 
    PrimaryVertices_mRanking(iter("PrimaryVertices.mRanking"))
  {}
};
//________________________________________________________________________________
void Twist3(const Char_t *Out = "Twist3.root") {
  if (Out) {
    fOut = new TFile(Out,"RECREATE");
  }
  const Char_t *sidesN[3] = {"A","W","E"};
  const Char_t *sides[3]  = {"All","EastOff","WestOff"};
  TH2D *xz[3] = {0,0,0};  
  TH2D *yz[3] = {0,0,0};
  TDirIter *Dirs[3]  = {0, 0, 0};
  for (Int_t side = 0; side < 3; side++) {
    TString files(sides[side]);
    files += "/*.MuDst.root";
    Dirs[side] = new TDirIter(files);
    if (! Dirs[side]->NoFiles()) {
      delete Dirs[side]; Dirs[side] = 0; continue;
    }
    xz[side] = new TH2D(Form("XZ%s",sidesN[side]),Form("Vertex X versus Z %s",sides[side]),
			500,-250,250,200,-5,5);
    yz[side] = new TH2D(Form("YZ%s",sidesN[side]),Form("Vertex Y versus Z %s",sides[side]),
			500,-250,250,200,-5,5);
  }
  // Try to match file by file, assuming that the event order always the same
  TNamed dirN[3];
  const Char_t *files[3] = {0,0,0};
  TTreeIter *iters[3] = {0, 0, 0};
  while (Dirs[0] && Dirs[1] && Dirs[2]) {
    UInt_t fcase = 0;
    for (Int_t side = 0; side < 3; side++) {
      if (! files[side]) {
	dirN[side]->SetName("");
	if (Dirs[side]) files[side] = Dirs[side]->NextFile();
	if (! files[side]) {files[side] = ""; delete Dirs[side]; Dirs[side] = 0;}
	else {
	  dirN[size]->SetName(gSystem->BaseName(files[side]));
	  SETBIT(fcase,side);
	  iters[side] = new TTreeIter();
	  iters[side]->AddFile(files[side]);
	}
      }
    }
    cout << fcase << "|\t" << files[0] << "|\t" << files[1] << "|\t" << files[2] << endl; 
    if (dirN[0] != dirN[1] || dirN[0] != dirN[2]) {
      if (dirN[0] < dirN[1] || dirN[0] < dirN[2]) {files[0] = 0; delete iters[0]; iters[0] = 0;} 
      if (dirN[1] < dirN[0] || dirN[1] < dirN[2]) {files[1] = 0; delete iters[1]; iters[1] = 0;}
      if (dirN[2] < dirN[0] || dirN[2] < dirN[1]) {files[2] = 0; delete iters[2]; iters[2] = 0;}
    }
    if (! iters[0] || ! iters[1] || ! iters[2]) continue;
    Tree_t *V[3] = {0, 0, 0};
    for (Int_t side = 0; side < 3; side++) 
      if (iters[side]) V[side] = new Tree_t(*iters[side]);
    Int_t moved[3] = {0, 0, 0};
    while (iters[0] && iters[1] && iters[2]) {
      for (Int_t side = 0; side < 3; side++) {
	if (moved[side]) continue;
	if (! iters[side]) continue;
	if (! iters[side]->Next()) {delete iters[side]; iters[side] = 0; continue}
	moved[side] = 1;
      }
      if (V[0]->MuEvent_mEventInfo_mId != V[1]->MuEvent_mEventInfo_mId ||
	  V[0]->MuEvent_mEventInfo_mId != V[0]->MuEvent_mEventInfo_mId) {
	if (V[0]->MuEvent_mEventInfo_mId < V[1]->MuEvent_mEventInfo_mId || V[0]->MuEvent_mEventInfo_mId < V[2]->MuEvent_mEventInfo_mId) moved[0] = 0;
	if (V[1]->MuEvent_mEventInfo_mId < V[0]->MuEvent_mEventInfo_mId || V[1]->MuEvent_mEventInfo_mId < V[2]->MuEvent_mEventInfo_mId) moved[1] = 0;
	if (V[2]->MuEvent_mEventInfo_mId < V[0]->MuEvent_mEventInfo_mId || V[2]->MuEvent_mEventInfo_mId < V[1]->MuEvent_mEventInfo_mId) moved[2] = 0;
      }
      if (! moved[0] || ! moved[1] || ! moved[2]) continue;
      for (Int_t side = 0; side < 3; side++) {
	for (Int_t l = 0; l < V[side]->NoPrimaryVertices; l++) {
	  xz[side]->Fill(V[side]->PrimaryVertices_mPosition_mX3[l],V[side]->PrimaryVertices_mPosition_mX1[l]);
	  yz[side]->Fill(V[side]->PrimaryVertices_mPosition_mX3[l],V[side]->PrimaryVertices_mPosition_mX2[l]);
	}
      }
      moved[0] = moved[1] = moved[2] = 0;
    }
    delete V[0]; delete V[1]; delete V[2];
  }
}
//________________________________________________________________________________
void Draw() {
  const Char_t *side[3] = {"All","EastOff","WestOff"};
  const Char_t *S[3]    = {"",   "W",      "E"};
  const Char_t *type[2] = {"XZ","YZ"};
  const Char_t *T[3]    = {"x", "y"};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1 = new TCanvas("c1","c1",600,600);
  c1->Divide(1,2);
  c1->cd(1)->DrawFrame(-250,-0.5,250,0.5,"X versus Z");
  c1->cd(2)->DrawFrame(-250,-0.5,250,0.5,"Y versus Z");
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (c2) c2->Clear();
  else    c2 = new TCanvas("c2","c2",600,600);
  c2->Divide(2,3);
  for (Int_t s = 0; s < 3; s++) {
    TFile *f = new TFile(Form("%s/Twist.root",side[s]));
    if (! f) continue;
    Double_t xmin = -150;
    Double_t xmax =  150;
    if (s == 1) {xmin = 0;}
    if (s == 2) {xmax = 0;}
    for (Int_t t = 0; t < 2; t++) {
      TH2D *h2 = (TH2D *) f->Get(type[t]);
      if (! h2) continue;
      h2->FitSlicesY(0,0,-1,0,"QNRG3S");
      TH1D *h1 = (TH1D *) f->Get(Form("%s_1",type[t]));
      if (! h1) continue;
      h1->SetName(Form("%s_%s",side[s],type[t]));
      h1->SetMarkerColor(s+1);
      c2->cd(2*s+t+1);
      if (s) {
	cout << h1->GetName() << "\t" << h1->GetTitle() << endl;
	h1->Fit("pol1","er","",xmin,xmax);
	TF1 *pol1 = (TF1 *) h1->GetListOfFunctions()->FindObject("pol1");
	if (pol1) {
	  cout << Form("Double_t %1s_%1s = %f; // +/- %f",
		       T[t],S[s],pol1->GetParameter(0),pol1->GetParError(0)) << endl;
	  cout << Form("Double_t d%1s_%1sdZ = %f; // +/- %f",
		       T[t],S[s],pol1->GetParameter(1),pol1->GetParError(1)) << endl;
	}				     
      }	else   {h1->SetStats(0); h1->Draw();}
      c1->cd(t+1);
      h1->Draw("sames");
    }
  }
}
