/*
  root.exe 'bfc.C(-1)' 'StiPulls.C+(1e9,"./*tags.root")'

StiPulls->Draw("mHitsR.lZPul>>Z(100,-0.02,0.02)","mHitsR.mDetector==27","colz")
TChain *chain =  Chain.C+("*.tags.root","StiPulls")
chain->Draw("mHitsR.lZPul>>Z(100,-0.02,0.02)","mHitsR.mDetector==27","colz") // Pxl sigma =  77 um
chain->Draw("mHitsR.lZPul>>Z(100,-0.02,0.02)","mHitsR.mDetector==28","colz") // Ist sigma = 238 um
chain->Draw("mHitsR.lZPul>>Z(100,-0.02,0.02)","mHitsR.mDetector==36","colz") // Sst sigma = 990 um
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
//#define __StvPull__
#define __ITPC__
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
#include "TKey.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TClassTable.h"
#include "TClonesArray.h"
#include "TDirIter.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "TROOT.h"
#include "TString.h"
#include "TPRegexp.h"
#include "TLine.h"
#include "TText.h"
#include "TPaveText.h"
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
TFile *fOut = 0;
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
//enum eNumbers {NDiff = 4, NVar = 6, NGP = 2, NCharge=3, NIO = 3, NPL=16};
enum eNumbers {NDiff = 4, NVar = 6, NGP = 2, NCharge=1, NIO = 1, NPL=16};
const Char_t *Diff_Name[NDiff] = {"dY", "dZ", "pullY", "pullZ"};
struct Var_t {
  Double_t Z, R, row, pTinv, Psi, Dip;
};
const Char_t *VarName[NVar] = { "Z", "R","row", "pTinv",       "Psi","Dip"};
const Char_t *VarNamA[NVar] = { "Z", "R","row","q/p_{T}",     "#psi","#lambda"};
Int_t nBin[NVar] =            { 210, 200,   78,   200,         180,   90};
Var_t vMin       =            {-210,   0, -5.5,   -10,-TMath::Pi(), -1.5};
Var_t vMax       =            { 210, 200, 72.5,    10, TMath::Pi(),  1.5};
struct Plot_t {
  Int_t z, x, y;
};
//________________________________________________________________________________
void StiTpcPulls() {
  if (! tree) return;
  const Char_t *GP[NGP] = {"G","P"};
  const Char_t *GPT[NGP] = {"Global","Primary"};
  const Char_t *Charge[NCharge] = {"A"}; //,"P","N"};
  const Char_t *ChargeT[NCharge] = {"All"}; //,"(+)","(-)"};
  const Char_t *InOut[NIO]    = {""}; //,"I","O"};
  const Char_t *InOutT[NIO]   = {"All"}; //,"Inner","Outer"};
  Plot_t xyzplots[NPL] = {
    {0, 0, 1},  // dY vs Z and R
    {1, 0, 1},  // dZ vs Z and R
    {2, 0, 1},  // Ypull vs Z and R
    {3, 0, 1},  // Zpull vs Z and R

    {0, 2, 3},  // dY vs row and q/pT
    {1, 2, 3},  // dZ vs row and q/pT
    {2, 2, 3},  // Ypull vs row and q/pT
    {3, 2, 3},  // Zpull vs row and q/pT
    
    {0, 0, 4},  // dY vs Z and Psi
    {1, 0, 4},  // dZ vs Z and Psi
    {2, 0, 4},  // Ypull vs Z and Psi
    {3, 0, 4},  // Zpull vs Z and Psi

    {0, 0, 5},  // dY vs Z and Dip
    {1, 0, 5},  // dZ vs Z and Dip
    {2, 0, 5},  // Ypull vs Z and Dip
    {3, 0, 5}   // Zpull vs Z and Dip
  };
  static TH3F *plots[NCharge][NGP][NIO][NPL] = {0};
  TString tt(gSystem->BaseName(tree->GetFile()->GetName()));
  Int_t dot = tt.Index(".");
  if (dot < 1) {
    cout << "Bad input file " << tt << endl;
    return;
  }
  TString Out(tt,dot);
  Out += ".PullsH.root";
  if (! fOut) {
    fOut = new TFile(Out,"recreate");
    cout << "Open " << Out.Data() << endl;
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
	    plots[s][l][i][t]->SetXTitle(VarNamA[ix]);
	    plots[s][l][i][t]->SetYTitle(VarNamA[iy]);
	    plots[s][l][i][t]->SetZTitle(Diff_Name[iz]);
	  }
	}
      }
    }
  }
  // Loop
#if __PRINT_FILE_NAME__
  TString currentFileName;
#endif
  for (ev = 0; ev < nevent; ev++) {
     Long64_t centry = tree->LoadTree(ev);
    //    if (centry < 0) break;
    nb += tree->GetEntry(ev);        //read complete event in memory
#if __PRINT_FILE_NAME__
    if (currentFileName != TString(tree->GetFile()->GetName())) {
      currentFileName = tree->GetFile()->GetName();
      cout << "Open File " << currentFileName.Data() << endl;
    }
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
	case 1: row = bits(hit->mHardwarePosition,9,7); break; //tpc
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
	Var_t  V = {hit->gZHit, hit->gRHit, (Double_t) row,  q/hit->mPt, hit->lPsi, hit->lDip};
	Double_t *v = &V.Z;
	Int_t s = 1;
	if (q < 0) s = 2;
	Int_t         j = 0;
	if (row >  0) j = 1;
#ifndef __ITPC__
	if (row > 13) j = 2;
#else /* __ITPC__ */
	if (row > 40) j = 2;
#endif
	for (Int_t t = 0; t < NPL; t++) {
	  Int_t iz = xyzplots[t].z;
	  Int_t ix = xyzplots[t].x;
	  Int_t iy = xyzplots[t].y;
	  //	  plots[0][l][j][t]->Fill(v[ix],v[iy],z[iz]);
	  //	  plots[s][l][j][t]->Fill(v[ix],v[iy],z[iz]);
	  plots[0][l][0][t]->Fill(v[ix],v[iy],z[iz]);
	  //	  plots[s][l][0][t]->Fill(v[ix],v[iy],z[iz]);
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
void StiPulls(Int_t NoEvents = 99999999, const Char_t *files = "*.stipull.root") {
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
	  tree->Add(f->GetName());
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
  branch = tree->GetBranch("event");
  if (! branch) return;
  branch->SetAddress(&event);
  Int_t nentries = (Int_t) tree->GetEntries();
  cout << "chained " << NFiles  << " files\t" 
       << "\twith total\t" << nentries << " events" << endl;
  if (! nentries) return;
  nevent = TMath::Min(NoEvents,nentries);
  cout << "It will be read " << nevent << " events." << endl;
  StiTpcPulls();
}
//______________________________________________________________________________
Int_t FindFirstSignificantBin(TH1 *h, Double_t threshold=1, Int_t axis=1)
{
   //find first bin with content > threshold for axis (1=x, 2=y, 3=z)
   //if no bins with content > threshold is found the function returns -1.

   if (axis != 1) {
      Warning("FindFirstBinAbove","Invalid axis number : %d, axis x assumed\n",axis);
      axis = 1;
   }
   Int_t nbins = h->GetXaxis()->GetNbins();
   for (Int_t bin=1;bin<=nbins;bin++) {
     if (h->GetBinError(bin) > 0) {
       if (TMath::Abs(h->GetBinContent(bin)) > threshold*h->GetBinError(bin)) return bin;
     }
   }
   return -1;
}
//______________________________________________________________________________
Int_t FindLastSignificantBin(TH1 *h, Double_t threshold=1, Int_t axis=1)
{
   //find last bin with content > threshold for axis (1=x, 2=y, 3=z)
   //if no bins with content > threshold is found the function returns -1.

   if (axis != 1) {
      Warning("FindLastBinAbove","Invalid axis number : %d, axis x assumed\n",axis);
      axis = 1;
   }
   Int_t nbins = h->GetXaxis()->GetNbins();
   for (Int_t bin=nbins;bin>=1;bin--) {
     if (h->GetBinError(bin) > 0) {
       if (TMath::Abs(h->GetBinContent(bin)) > threshold*h->GetBinError(bin)) return bin;
     }
   }
   return -1;
}

//________________________________________________________________________________
void Draw3D(const Char_t *histName="lYDifVsZAG", Int_t NF = 0, TFile **Files = 0) {
  if (! NF) return;
  TFile *f = 0;
  TCanvas *c1 = 0;
  TH1F *frames[2] = {0};
  TLegend *l[2] = {0};
  for (Int_t xy = 0; xy < 2; xy++) {
    Double_t xmin = 9999, xmax = -9999;
    Double_t ymin = 9999, ymax = -9999;
    for (Int_t i = 0; i < NF; i++) {
      f = Files[i];
      if (! f) {
	cout << "Fules[" << i << "} = 0" << endl;
	continue;
      }
      TString FT(f->GetName());
      FT.ReplaceAll(".PullsH.root","");
      f->cd();
      TH3F *h3 = (TH3F *) f->Get(histName);
      if (! h3) continue;
      //    cout << "Found " << h3->GetName() << "\t in " << f->GetName() << endl;
      if ( h3->GetDimension() != 3) continue;
      TH2D *h2;
      if (xy == 0) h2 = (TH2D *) h3->Project3D("zx");
      else         h2 = (TH2D *) h3->Project3D("zy");
      TObjArray* arr =  new TObjArray(4);
      h2->FitSlicesY(0, 0, -1, 0, "QNR", arr);
      TH1D *mu = (TH1D *) (*arr)[1]; mu->SetMarkerColor(3*i+1); mu->SetMarkerStyle(20);
      TH1D *sigma = (TH1D *) (*arr)[2]; sigma->SetMarkerColor(3*i+1); sigma->SetMarkerStyle(21);
      if (! c1) {
	Double_t w = 600*2;
	Double_t h = 400*(NF+1);
	TString CN("C");
	CN += h3->GetName();
	c1 = new TCanvas(CN,CN, w, h);
	c1->SetWindowSize(w + (w - c1->GetWw()), h + (h - c1->GetWh()));
	c1->Divide(2,NF+1);
      }
      if (! frames[xy]) { 
	TVirtualPad *pad = c1->cd(2*NF + xy + 1);  
	Int_t ixminM = FindFirstSignificantBin(mu,3);
	Int_t ixminS = FindFirstSignificantBin(sigma,3);
	Int_t ixmin  = ixminS; //TMath::Max(ixminM, ixminS);

	Int_t ixmaxM = FindLastSignificantBin(mu,3);
	Int_t ixmaxS = FindLastSignificantBin(sigma,3);
	Int_t ixmax  = ixmaxS; // TMath::Min(ixmaxM, ixmaxS);
	if (ixmin < ixmax) { 
	  xmin = mu->GetXaxis()->GetBinLowEdge(ixmin);
	  xmax = mu->GetXaxis()->GetBinUpEdge(ixmax);
	  Double_t y, dy, s, ds;
	  for (Int_t i = ixmin; i <= ixmax; i++) {
	    y = mu->GetBinContent(i);
	    dy = mu->GetBinError(i);
	    s = sigma->GetBinContent(i);
	    ds = sigma->GetBinError(i);
	    if (ymin > y - dy) ymin = y - dy;
            if (ymax < s + ds) ymax = s + ds;
	  }
	  frames[xy] = pad->DrawFrame(xmin,1.5*ymin,xmax,1.2*ymax);
	  l[xy] = new TLegend(0.7,0.3,0.8,0.5);
	}
      }
      c1->cd(2*i+xy+1)->SetLogz(1);
      h2->Draw("colz");
      mu->Draw("same");
      sigma->Draw("same");
      TPaveText *pt = new TPaveText(.1,.8,.2,.9, "brNDC");
      pt->SetTextSize(0.10);
      pt->SetTextFont(22);
      pt->SetTextColor(1);
      pt->SetFillColor(0);
      pt->SetBorderSize(0);
      pt->AddText(FT.Data());
      pt->Draw();
      c1->Update();
      if (frames[xy]) {
	c1->cd(2*NF + xy + 1);  
	TString Line("#mu:");
	Line += " "; Line += FT;
	mu->Draw("same");
	l[xy]->AddEntry(mu,Line);
	sigma->Draw("same");
	Line.ReplaceAll("mu","sigma");
	l[xy]->AddEntry(mu,Line);
	l[xy]->Draw();
	c1->Update();
      }
    }
    if (c1) c1->Update();
  }
  if (c1) c1->SaveAs(".png");
  return;
}
//________________________________________________________________________________
void LoopOverHistograms( const Char_t *pattern = "dYIPNvsrowpTinv") {
  TPRegexp Pattern(pattern);
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **Files = new TFile *[nn];
  Int_t NF = 0;
  TFile *f = 0;
  TIter next(files);
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    if (! F.Contains("PullsH.root")) continue;
    Files[NF] = f;
    NF++;
  }
  f = Files[0];
  if (! f) return;
  TList *keys = f->GetListOfKeys();
  if (! keys) return;
  keys->Sort();
  TIter nextk(keys);
  TKey *key = 0;
  while ((key = (TKey *) nextk())) {
    TString F(key->GetName());
    if (Pattern.GetPattern() != ""  && !  F.Contains(Pattern)) continue;
    Draw3D(F, NF, Files);
  }
}
