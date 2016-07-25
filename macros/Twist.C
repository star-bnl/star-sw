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
struct Tree_t {
  const Int_t   &    NoPrimaryVertices;                        
  const Float_t*&    PrimaryVertices_mPosition_mX1;            
  const Float_t*&    PrimaryVertices_mPosition_mX2;            
  const Float_t*&    PrimaryVertices_mPosition_mX3;            
  Tree_t (TTreeIter &iter) : NoPrimaryVertices(iter("PrimaryVertices")),
			    PrimaryVertices_mPosition_mX1(iter("PrimaryVertices.mPosition.mX1")),
			    PrimaryVertices_mPosition_mX2(iter("PrimaryVertices.mPosition.mX2")),
			    PrimaryVertices_mPosition_mX3(iter("PrimaryVertices.mPosition.mX3")) {}

};
//________________________________________________________________________________
void Twist(const Char_t *files = "./*.MuDst.root",const Char_t *Out = "Twist.root") {
  if (Out) {
    fOut = new TFile(Out,"RECREATE");
  }
  TH2D *xz = new TH2D("XZ","Vertex X versus Z",500,-250,250,200,-5,5);
  TH2D *yz = new TH2D("YZ","Vertex Y versus Z",500,-250,250,200,-5,5);
   TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  if (! NFiles) return;
  // init of user variables
  Tree_t V(iter);
#if 0
  const Int_t&       NoPrimaryVertices                        = iter("PrimaryVertices");
  const Float_t*&    PrimaryVertices_mPosition_mX1            = iter("PrimaryVertices.mPosition.mX1");
  const Float_t*&    PrimaryVertices_mPosition_mX2            = iter("PrimaryVertices.mPosition.mX2");
  const Float_t*&    PrimaryVertices_mPosition_mX3            = iter("PrimaryVertices.mPosition.mX3");
#endif
  //         Now iterations
  while (iter.Next()) {
    for (Int_t l = 0; l < V.NoPrimaryVertices; l++) {
      xz->Fill(V.PrimaryVertices_mPosition_mX3[l],V.PrimaryVertices_mPosition_mX1[l]);
      yz->Fill(V.PrimaryVertices_mPosition_mX3[l],V.PrimaryVertices_mPosition_mX2[l]);
    }
  }
  //    iter.Reset(); //ready for next loop                                 
  if (fOut) fOut->Write();
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
