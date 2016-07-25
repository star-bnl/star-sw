#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//#define __StvPull__
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TClassTable.h"
#include "TClonesArray.h"
#include "TDirIter.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TROOT.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TLegend.h"
#ifndef __StvPull__
#include "StiUtilities/StiPullEvent.h"
#endif
#else
class TMinuit;
class TF1;
class TH1F;
class TH2F;
class TH3F;
class TProfile;
class TH2D;
class TCanvas;
class TSpectrum;
class TSystem;
class Bichsel;
#endif
UInt_t bits(UInt_t mHardwarePosition, UInt_t bit, UInt_t nbits) {
  return (mHardwarePosition>>bit) & ~(~0UL<<nbits);
}
TChain *tree = 0;
TBranch *branch = 0;
Int_t nentries = 0;
Int_t nevent = 0;
StiPullEvent *event = 0;
Int_t ev = 0;
Int_t nb = 0;
TCanvas *c1 = 0;
struct PlotName_t {
  const Char_t *Name;
  const Char_t *Title;
  const Char_t *xAxis;
  const Char_t *yAxis;
  Int_t   Detector;
  Int_t   nx;
  Double_t xmin, xmax;
  Int_t  ny;
  Double_t ymin, ymax;
};
struct Diff_t {
  Double_t dy, dz, ypull, zpull;
};
Int_t NzBin = 100;
Diff_t zMax = { 2., 2., 5., 5.};
enum eNumbers {NDiff = 4, NVar = 6, NGP = 2, NCharge=3, NIO = 3, NPL=16};
const Char_t *Diff_Name[NDiff] = {"dY", "dZ", "pullY", "pullZ"};
struct Var_t {
  Double_t Z, R, row, pTinv, Psi, Dip;
};
const Char_t *VarName[NVar] = {"Z","R","row","pTinv","Psi","Dip"};
Int_t nBin[NVar] = { 210, 200, 50, 200,        180,   90};
Var_t vMin       = {-210,   0, -5, -10,-TMath::Pi(),-1.5};
Var_t vMax       = { 210, 200, 45,  10, TMath::Pi(), 1.5};
struct Plot_t {
  Int_t z, x, y;
};
//________________________________________________________________________________
void StiTpcPulls() {
  const Char_t *GP[NGP] = {"G","P"};
  const Char_t *GPT[NGP] = {"Global","Primary"};
  const Char_t *Charge[NCharge] = {"A","P","N"};
  const Char_t *ChargeT[NCharge] = {"All","(+)","(-)"};
  const Char_t *InOut[NIO]    = {"","I","O"};
  const Char_t *InOutT[NIO]   = {"All","Inner","Outer"};
  Plot_t xyzplots[NPL] = {
    {0, 0, 1},  // dY vs Z and R
    {1, 0, 1},  // dZ vs Z and R
    {2, 0, 1},  // Ypull vs Z and R
    {3, 0, 1},  // Zpull vs Z and R

    {0, 2, 3},  // dY vs row and pTinv
    {1, 2, 3},  // dZ vs row and pTinv
    {2, 2, 3},  // Ypull vs row and pTinv
    {3, 2, 3},  // Zpull vs row and pTinv
    
    {0, 0, 4},  // dY vs Z and Psi
    {1, 0, 4},  // dZ vs Z and Psi
    {2, 0, 4},  // Ypull vs Z and Psi
    {3, 0, 4},  // Zpull vs Z and Psi

    {0, 0, 5},  // dY vs Z and Dip
    {1, 0, 5},  // dZ vs Z and Dip
    {2, 0, 5},  // Ypull vs Z and Dip
    {3, 0, 5}   // Zpull vs Z and Dip
  };
  TH3F *plots[NCharge][NGP][NIO][NPL]; memset (plots, 0, sizeof(plots));
  TString Out(gSystem->DirName(gDirectory->GetName()));
  Out.ReplaceAll("/","_");
  Out += "APulls.root";
  TFile *fOut = new TFile(Out,"recreate");
  for (Int_t s = 0; s < NCharge; s++) {
    for (Int_t l = 0; l < NGP; l++) {
      for (Int_t i = 0; i < NIO; i++) {
	for (Int_t t = 0; t < NPL; t++) {
	  Int_t iz = xyzplots[t].z;
	  Int_t ix = xyzplots[t].x;
	  Int_t iy = xyzplots[t].y;
	  TString Name(Diff_Name[iz]); Name += InOut[i]; Name += GP[l]; Name += Charge[s]; Name += "vs"; Name += VarName[ix];  Name += VarName[iy];
	  TString Title(Diff_Name[iz]); Title += " for ", Title += InOutT[i]; Title += " "; Title += GPT[l]; Title += ChargeT[s]; 
	  Title += " versus "; Title += VarName[ix]; Title += " and "; Title += VarName[iy];
	  Double_t *min = &vMin.Z;
	  Double_t *max = &vMax.Z;
	  Double_t *zmax = &zMax.dy;
	  plots[s][l][i][t] = new TH3F(Name,Title, 
				       nBin[ix], min[ix], max[ix],
				       nBin[iy], min[iy], max[iy],
				       NzBin, -zmax[iz], zmax[iz]);
	  plots[s][l][i][t]->SetMarkerStyle(20);
// 	  plots[s][l][i][t]->SetMarkerColor(s+1);
// 	  plots[s][l][i][t]->SetLineColor(s+1);
	  plots[s][l][i][t]->SetXTitle(VarName[ix]);
	  plots[s][l][i][t]->SetYTitle(VarName[iy]);
	  plots[s][l][i][t]->SetZTitle(Diff_Name[iz]);
	}
      }
    }
  }
  // Loop
  for (ev = 0; ev < nevent; ev++) {
     Long64_t centry = tree->LoadTree(ev);
    //    if (centry < 0) break;
    nb += tree->GetEntry(ev);        //read complete event in memory
#if 1
    cout << "Run/Event" << event->mRun << "/" << event->mEvt << endl;
    cout << "Vtx:\t" << event->mVtx[0] << "\t" << event->mVtx[1] <<"\t" << event->mVtx[2] << endl;
#endif
    TClonesArray *Hits = 0;
    for (Int_t l = 0; l < 2; l++) {
      if (l == 0) Hits = &(event->mHitsG);
      else        Hits = &(event->mHitsP);
      if (! Hits) continue;
      TClonesArray &HitsGP = *Hits; 
      Int_t nHit = HitsGP.GetEntriesFast();
      for (Int_t i = 0; i < nHit; i++) {
	StiPullHit *hit =  (StiPullHit *) HitsGP.UncheckedAt(i);
	if (! hit) continue;
	//	hit->Print();
	Int_t row = -10;
	Int_t index = 0;
	Int_t barrel = 0;
	switch (hit->mDetector) {
	case 0: row = -5; break; //vtx
	case 1: row = bits(hit->mHardwarePosition,9,6); break; //tpc
	case 2: 
	  index = bits(hit->mHardwarePosition,4,9); 
	  if (index >= 0) { 
	    barrel = 1;
	    if (index >= 64) barrel = 2; 
	    if (index >= 208) barrel = 3;
	    row = -5 + barrel;
	  }
	  break; // svt
	case 8: row = -1; break; // ssd
	default: row = -10; break;
	};
	Diff_t d = {hit->lYPul, hit->lZPul, hit->lYPul/hit->lYPulErr, hit->lZPul/hit->lZPulErr};
	Double_t *z = &d.dy;
	Double_t q = 1;
	if (hit->mCurv < 0) q = -1;
	Var_t  V = {hit->gZHit, hit->gRHit, row,  q/hit->mPt, hit->lPsi, hit->lDip};
	Double_t *v = &V.Z;
	Int_t s = 1;
	if (q < 0) s = 2;
	Int_t         i = 0;
	if (row >  0) i = 1;
	if (row > 13) i = 2;
	for (Int_t t = 0; t < NPL; t++) {
	  Int_t iz = xyzplots[t].z;
	  Int_t ix = xyzplots[t].x;
	  Int_t iy = xyzplots[t].y;
	  plots[0][l][i][t]->Fill(v[ix],v[iy],z[iz]);
	  plots[s][l][i][t]->Fill(v[ix],v[iy],z[iz]);
	  plots[0][l][0][t]->Fill(v[ix],v[iy],z[iz]);
	  plots[s][l][0][t]->Fill(v[ix],v[iy],z[iz]);
	}
      }
    }
  }
#if 0
  fOut->cd();
  for (Int_t l = 0; l < 2; l++) {
    for (Int_t t = 0; t < NPL2; t++) {
      for (Int_t s = 0; s < 3; s++) {
	if (! plots2D[s][l][t]) plots2D[s][l][t]->FitSlicesY();
	if (! pulls2D[s][l][t]) pulls2D[s][l][t]->FitSlicesY();
      }
    }
  }  
#endif
  fOut->Write();  
}
//________________________________________________________________________________
void StiSvtSsd() {
}
//________________________________________________________________________________
void Pulls(Int_t NoEvents = 99999999, const Char_t *files = "MuDst/*.tags.root", Int_t opt=0) {
  if (gClassTable->GetID("StiPullEvent") < 0) {gSystem->Load("StiUtilities");}
#if 0
  tree = (TTree *) gDirectory->Get("StiPulls");
#endif
  const Char_t *treeName = "StiPulls";  
  TDirIter Dir(files);
  tree = new TChain(treeName);
  Char_t *file = 0;
  Int_t NFiles = 0;
  ULong64_t nEvTot = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    cout << "File " << file << endl;
    TFile *f = new TFile(file);
    if (f) {
      TChain *T = (TChain *) f->Get(treeName);
      if (T) {
	ULong64_t nEvents = T->GetEntries();
	cout << "#\t" << NFiles << "\t" << f->GetName() << "\t" << nEvents << endl;
	if (nEvents) {
	  nEvTot += nEvents;
	  tree->Add(T);
	  NFiles++;
	}
      } else {
	cout << "Cannot find " << treeName << " tree in " << file << endl;
      }
    } else {
      cout << "Cannot open the file " << file << endl;
    }
    //    delete f;
  }
  branch = tree->GetBranch("event");
  if (! branch) return;
  branch->SetAddress(&event);
  Int_t nentries = (Int_t) tree->GetEntries();
  cout << "chained " << NFiles  << " files\t" 
       << "\twith total\t" << nentries << " events" << endl;
  if (! nentries) return;
  nevent = TMath::Min(NoEvents,nentries);
  cout << "It will be read " << nevent << " events." << endl;
  if (opt == 0) StiTpcPulls();
  else          StiSvtSsd();
}
//________________________________________________________________________________
void DrawPulls(const Char_t *histName="lYDifVsZAG", Double_t Ymin = -500, Double_t Ymax = 500) {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TH1 **Hist = new TH1 *[nn];
  TFile    **Files= new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  Double_t xmin = 9999, xmax = -9999;
  Double_t ymin = 9999, ymax = -9999;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("Pull") ) continue;
    TH1 *hist = (TH1 *) f->Get(histName);
    if (! hist) continue;
    //    cout << "Found " << hist->GetName() << "\t in " << f->GetName() << endl;
    if (! hist->IsA()->InheritsFrom( "TH2D" ) ) {
      Int_t nxbin = hist->GetNbinsX();
      Double_t y, dy;
      for (Int_t bin = 1; bin <= nxbin; bin++) {
	y = hist->GetBinContent(bin);
	dy = hist->GetBinError(bin);
	if (TMath::Abs(y) < 3*dy) continue;
	if (y + dy > ymax) ymax = y + dy;
	if (y - dy < ymin) ymin = y - dy;
      }
    } else {
      if (hist->GetYaxis()->GetXmax() > ymax) ymax = hist->GetYaxis()->GetXmax();
      if (hist->GetYaxis()->GetXmin() < ymin) ymin = hist->GetYaxis()->GetXmin();
    }
    if (hist->GetXaxis()->GetXmax() > xmax) xmax = hist->GetXaxis()->GetXmax();
    if (hist->GetXaxis()->GetXmin() < xmin) xmin = hist->GetXaxis()->GetXmin();
    Int_t color = NF+1;
    if (color >= 5) color++;
    hist->SetMarkerColor(color);
    Files[NF] = f; Hist[NF] = hist; NF++;
  }
  //  cout << "NF " << NF << endl;
  if (! NF) return;
  TString CName("C");
  CName += Hist[0]->GetName();
  if (c1) delete c1;
  c1 = new TCanvas(CName,Hist[0]->GetTitle(),400,400); c1->SetLeftMargin(0.14);
  c1->SetLogz(1);
  if (! Hist[0]->IsA()->InheritsFrom( "TH2D" ) ) {
    if (ymin < Ymin) ymin = Ymin;
    if (ymax > Ymax) ymax = Ymax;
  }
  TH1F *frame = c1->DrawFrame(xmin,ymin,xmax,ymax);
  frame->SetTitle(Hist[0]->GetTitle());
  frame->SetXTitle(Hist[0]->GetXaxis()->GetTitle());
  frame->SetYTitle(Hist[0]->GetYaxis()->GetTitle());
  TLegend *leg = new TLegend(0.65,0.75,1.05,0.90,"");
  //  leg->SetTextSize(0.033);
  for (int i = 0; i<NF; i++) {
    TString Title(Files[i]->GetName());
    Title.ReplaceAll("Pulls","");
    Title.ReplaceAll("Pull","");
    Title.ReplaceAll(".root","");
    Files[i]->cd();
    TH1 *hist = Hist[i];
    if (! Hist[0]->IsA()->InheritsFrom( "TH2D" ) ) {
      hist->Draw("same");
      leg->AddEntry(hist,Title);
    } else {
      TH2D *hist2 = (TH2D*) hist;
      if (i == 0) hist2->Draw("samecolz");
      hist2->FitSlicesY();
      TH1 *mu =  (TH1 *) Files[i]->Get(Form("%s_1",hist2->GetName()));
      TH1 *sigma =  (TH1 *) Files[i]->Get(Form("%s_2",hist2->GetName()));
      Int_t color = i+1;
      if (color >= 5) color++;
      mu->SetMarkerStyle(20); mu->SetMarkerColor(color); mu->Draw("same");
      sigma->SetMarkerStyle(21); sigma->SetMarkerColor(color); sigma->Draw("same");
      TString tTitle("#mu "); tTitle += Title;
      leg->AddEntry(mu,tTitle);
      TString sTitle("sigma "); sTitle += Title;
      leg->AddEntry(sigma,sTitle);
    }
  }
  leg->Draw();
}
//________________________________________________________________________________
void LoopOverPulls(Int_t opt = 0) {
  Char_t *names[] = {
      "lYDifVsZAG",      "lYDifVsZPG",      "lYDifVsZNG",
      "lZDifVsZAG",      "lZDifVsZPG",      "lZDifVsZNG",
      "lYDifVsRAG",      "lYDifVsRPG",      "lYDifVsRNG",
      "lZDifVsRAG",      "lZDifVsRPG",      "lZDifVsRNG",
      "lYDifVsRowAG",    "lYDifVsRowPG",    "lYDifVsRowNG",
      "lZDifVsRowAG",    "lZDifVsRowPG",    "lZDifVsRowNG",
      "lYDifVsZAP",      "lYDifVsZPP",      "lYDifVsZNP",
      "lZDifVsZAP",      "lZDifVsZPP",      "lZDifVsZNP",
      "lYDifVsRAP",      "lYDifVsRPP",      "lYDifVsRNP",
      "lZDifVsRAP",      "lZDifVsRPP",      "lZDifVsRNP",
      "lYDifVsRowAP",    "lYDifVsRowPP",    "lYDifVsRowNP",
      "lZDifVsRowAP",    "lZDifVsRowPP",    "lZDifVsRowNP"};
  Int_t N = sizeof(names)/sizeof(Char_t*);
  for(Int_t i = 0; i < N; i++) {
    TString Name(names[i]);
    if (opt == 0) {
      DrawPulls(Name);
      c1->SaveAs(".png");
    } else {
      Name.ReplaceAll("Dif","Pull");
      Name += "2D";
      Pulls(Name);
      c1->SaveAs(".png");
    }
  }
}

