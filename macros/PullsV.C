/* 
   root.exe lStv.C PullsV.C+
 */
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
#define __StvPull__
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
#else  /* __StvPull__ */
#include "StvUtil/StvPullEvent.h"
#endif /* __StvPull__ */
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
ULong64_t nentries = 0;
ULong64_t nevent = 0;
#ifndef __StvPull__
StiPullEvent *event = 0;
#else  /* __StvPull__ */
StvPullEvent *event = 0;
#endif /* __StvPull__ */
ULong64_t ev = 0;
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
Diff_t zMax = { 0.5, 0.5, 5., 5.};
enum eNumbers {NDiff = 4, NVar = 6, NGP = 2, NCharge=3, NIO = 3, NPL=16};
const Char_t *Diff_Name[NDiff] = {"dY", "dZ", "pullY", "pullZ"};
struct Var_t {
  Double_t Z, R, row, pT, Psi, Dip;
};
const Char_t *VarName[NVar] = {"Z","R","row","pT","Psi","Dip"};
Int_t nBin[NVar] = { 210, 200, 50, 100,        180,   90};
Var_t vMin       = {-210,   0, -5,   0,-TMath::Pi(),-1.5};
Var_t vMax       = { 210, 200, 45,  10, TMath::Pi(), 1.5};
struct Plot_t {
  Int_t z, x, y;
};
//________________________________________________________________________________
void TpcPulls() {
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
#if 1
    {0, 2, 3},  // dY vs row and pT
    {1, 2, 3},  // dZ vs row and pT
    {2, 2, 3},  // Ypull vs row and pT
    {3, 2, 3},  // Zpull vs row and pT
#endif    
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
  TString Out("PullsA.root");
  TFile *fOut = new TFile(Out,"recreate");
  Int_t    npT    = 98;
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
	  if (iy == 3) {plots[s][l][i][t]->GetYaxis()->Set(npT, ptBins);}
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
    if (centry < 0) break;
    nb += tree->GetEntry(centry);        //read complete event in memory
#if 1
    cout << ev << "\tRun/Event" << event->mRun << "/" << event->mEvt << endl;
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
#ifndef __StvPull__
	StiPullHit *hit =  (StiPullHit *) HitsGP.UncheckedAt(i);
#else  /* __StvPull__ */
	StvPullHit *hit =  (StvPullHit *) HitsGP.UncheckedAt(i);
#endif /* __StvPull__ */
	if (! hit) continue;
	//	hit->Print();
	Int_t row = -10;
#ifndef __StvPull__
	Int_t index = 0;
	Int_t barrel = 0;
#endif /* __StvPull__ */
	switch (hit->mDetector) {
	case 0: row = -5; break; //vtx
	case 1: 
#ifndef __StvPull__
	  row = bits(hit->mHardwarePosition,9,6); break; //tpc
#else  /* __StvPull__ */
	  row = 1;
#endif /* __StvPull__ */
	case 2: 
#ifndef __StvPull__
	  index = bits(hit->mHardwarePosition,4,9); 
	  if (index >= 0) { 
	    barrel = 1;
	    if (index >= 64) barrel = 2; 
	    if (index >= 208) barrel = 3;
	    row = -5 + barrel;
	  }
#else  /* __StvPull__ */
	  row = -4;
#endif /* __StvPull__ */
	  break; // svt
	case 8: row = -1; break; // ssd
	default: row = -10; break;
	};
#ifndef __StvPull__
	Diff_t d = {hit->lYPul, hit->lZPul, hit->lYPul/hit->lYPulErr, hit->lZPul/hit->lZPulErr};
#else  /* __StvPull__ */
	Diff_t d = {hit->lYHit, hit->lZHit, hit->lYPul, hit->lZPul};
#endif /* __StvPull__ */
	Double_t *z = &d.dy;
	Double_t q = 1;
	if (hit->mCurv < 0) q = -1;
	Var_t  V = {hit->gZHit, hit->gRHit, row, hit->mPt, 
#ifndef __StvPull__
		    hit->lPsi, 
#else  /* __StvPull__ */
		    hit->gPsi - TMath::Pi()/6*TMath::Nint(hit->gPsi/(TMath::Pi()/6)), 
#endif /* __StvPull__ */
		    hit->gDip
	};
	Double_t *v = &V.Z;
	Int_t s = 1;
	if (q < 0) s = 2;
	Int_t         io = 0;
	if (row >  0) io = 1;
	if (row > 13) io = 2;
	for (Int_t t = 0; t < NPL; t++) {
	  Int_t iz = xyzplots[t].z;
	  Int_t ix = xyzplots[t].x;
	  Int_t iy = xyzplots[t].y;
	  plots[0][l][io][t]->Fill(v[ix],v[iy],z[iz]);
	  plots[s][l][io][t]->Fill(v[ix],v[iy],z[iz]);
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
void SvtSsd() {
}
//________________________________________________________________________________
#ifndef __StvPull__
void Pulls(ULong64_t NoEvents = 99999999, const Char_t *files = "tags.root", Int_t opt=0) 
#else  /* __StvPull__ */
void PullsV(ULong64_t NoEvents = 99999999, const Char_t *files = "tags.root", Int_t opt=0) 
#endif /* __StvPull__ */
{
#ifndef __StvPull__
  const Char_t *treeName = "StiPulls";  
#else  /* __StvPull__ */
  const Char_t *treeName = "StvPulls";  
#endif /* __StvPull__ */
#if 0
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
	  tree->Add(file);
	  NFiles++;
	}
      } else {
	cout << "Cannot find " << treeName << " tree in " << file << endl;
      }
    } else {
      cout << "Cannot open the file " << file << endl;
    }
    delete f;
  }
#endif
  TFile *f = new TFile(files);
  if (! f) return;
  tree = (TChain *) f->Get(treeName);
  if (! tree) return;
  branch = tree->GetBranch("event");
  if (! branch) return;
  branch->SetAddress(&event);
  nentries = tree->GetEntries();
  //  cout << "chained " << NFiles  << " files\t" 
  //       << "\twith total\t" << nentries << " events" << endl;
  //  if (! nentries) return;
  //  nevent = TMath::Min(NoEvents,nentries);
  nevent = TMath::Min(NoEvents,nentries);
  cout << "It will be read " << nevent << " events." << endl;
  if (opt == 0) TpcPulls();
  else          SvtSsd();
}
//________________________________________________________________________________
void DrawPulls(const Char_t *histName="pullZPNvsZR", const Char_t *projection = "zx") {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (nn != 4) return;
  TIter next(files);
  TFile *f = 0;
  if (c1) delete c1;
  c1 = new TCanvas("c1","c1",200,10,800,600);
  c1->Divide(2,2);
  Int_t nf = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName());
    F.ReplaceAll(".root","");
    if      (F == "Sti"  ) nf = 1;
    else if (F == "StiCA") nf = 2;
    else if (F == "Stv"  ) nf = 3;
    else if (F == "StvCA") nf = 4;
    else {continue;}
    c1->cd(nf)->SetLogz(1);
#if 0
    if (! F.Contains("Pull") ) continue;
#endif
    TH3 *hist = (TH3 *) f->Get(histName);
    if (! hist) continue;
    cout << "Found " << hist->GetName() << "\t in " << f->GetName() << endl;
    TH2 *h2 = (TH2 *) hist->Project3D(projection);
    if (nf == 1) {c1->SetName(h2->GetName()); c1->SetTitle(h2->GetTitle());}
    h2->SetStats(0);
    h2->Draw("colz");
    TLegend *l = new TLegend(0.7,0.1,0.9,0.2);
    l->AddEntry(h2,F.Data());
    l->Draw();
    h2->FitSlicesY();
    TH1 *mu = (TH1 *) f->Get(Form("%s_1",h2->GetName()));
    if (mu) mu->Draw("same");
    TH1 *sigma = (TH1 *) f->Get(Form("%s_2",h2->GetName()));
    if (sigma) {
      sigma->SetMarkerStyle(21);
      sigma->Draw("same");
    }
  }
  c1->SaveAs(".png");
}
//________________________________________________________________________________
void LoopOverPulls() {
  struct Name_t {
    const Char_t *Name;
    const Char_t *Projection;
  };
  Name_t names[] = {
    {"pullZPAvsZR", "zx"}, // pull Z All Primaries All versus Z
    {"dZPAvsZR", "zx"},    //     dZ All Primaries All versus Z
    {"pullYPAvsZR", "zx"}, // pull Y All Primaries All versus Z
    {"dYPAvsZR", "zx"},    //     dY All Primaries All versus Z
    {"pullYPAvsZR","zy"},  // pull Y All Primaries All versus R
    {"dYPAvsZR","zy"},     //     dY All Primaries All versus R    
    {"pullZPAvsZR", "zy"}, // pull Z All Primaries All versus R
    {"dZPAvsZR", "zy"},    //     dZ All Primaries All versus R
    {"pullZPAvsZPsi","zy"}//                          versus Psi
  };
  Int_t N = sizeof(names)/sizeof(Name_t);
  for(Int_t i = 0; i < N; i++) {
    DrawPulls(names[i].Name,names[i].Projection);
  }
}

