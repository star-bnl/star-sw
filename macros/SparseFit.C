#if !defined(__CINT__) && !defined(__CLING__) && ! defined(__MAKECINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) && !defined(__CLING__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//    ROOT5
#if defined(__CINT__) && !defined(__MAKECINT__)
// source code being actually interpreted by cint
#elif defined(__MAKECINT__)
// source code seen by rootcint only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//    ROOT6 
#if defined(__CLING__) && !defined(__ROOTCLING__)
// source code being actually interpreted by Cling
#elif defined(__ROOTCLING__)
// source code seen by rootcling only
#elif defined(__ACLIC__)
// source code being actually compiled by ACLiC
#else
// source code suitable for a standalone executable
#endif
//#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,18)
//#define __USE_ROOFIT__
//#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TCanvas.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TDataSet.h"
#include "TClassTable.h"
//#include "DeDxTree.C"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "TKey.h"
#include "TLegend.h"
#include "TArrayI.h"
#include "TArrayD.h"
#ifdef __USE_ROOFIT__
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooFFTConvPdf.h"
#include "RooPlot.h"
#include "RooCFunction1Binding.h" 
#include "RooCFunction3Binding.h"
#include "RooTFnBinding.h" 
#include "RooDataHist.h"
#include "RooAbsPdf.h"
#include "RooRealProxy.h"
#include "RooFit.h"
#include "RooRandom.h"
#include "RooFitResult.h"
#include "RooWorkspace.h"
using namespace RooFit ;
#endif /* __USE_ROOFIT__ */
#include "TObjectTable.h"
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
// Refer to a class implemented in libRooFit to force its loading
// via the autoloader.
#ifdef __USE_ROOFIT__
class Roo2DKeysPdf;
#endif /* __USE_ROOFIT__ */
#endif
#include "Ask.h"
TCanvas *c1 = 0;
//______________________________________________________________________________
TTree* toTree(THnSparse* h)
{
   // Creates a TTree and fills it with the coordinates of all
   // filled bins. The tree will have one branch for each dimension,
   // and one for the bin content.

   Int_t dim = h->GetNdimensions();
   TString name(h->GetName()); name += "_tree";
   TString title(h->GetTitle()); title += " tree";

   TTree* tree = new TTree(name, title);
   Double_t* x = new Double_t[dim + 1];
   memset(x, 0, sizeof(Double_t) * (dim + 1));

   TString branchname;
   for (Int_t d = 0; d < dim; ++d) {
      if (branchname.Length())
         branchname += ":";
      TAxis* axis = h->GetAxis(d);
      branchname += axis->GetName();
      branchname += "/D";
   }
   tree->Branch("coord", x, branchname);
   tree->Branch("bincontent", &x[dim], "bincontent/D");

   Int_t *bins = new Int_t[dim];
   for (Long64_t i = 0; i < h->GetNbins(); ++i) {
      x[dim] = h->GetBinContent(i, bins);
      for (Int_t d = 0; d < dim; ++d) {
         x[d] = h->GetAxis(d)->GetBinCenter(bins[d]);
      }

      tree->Fill();
   }

   delete [] bins;
   //delete [] x;
   return tree;
}
//________________________________________________________________________________
Int_t Bins2Index(Int_t *bins, Int_t *Nbins, Int_t dim, Int_t *step = 0) {
  Int_t indx = 0;
  for (Int_t d = 0; d < dim - 1; d++) {
    indx *= Nbins[d];
    Int_t i = bins[d] - 1;
    if (step && step[d] > 1) {
      i /= step[d];
    }
    indx += i;
  }
  return indx;
}
//________________________________________________________________________________
void Index2Bins(Int_t indx, Int_t *Nbins, Int_t dim, Int_t *bins) {
   for (Int_t d = dim - 2; d >= 0; d--) {
     bins[d] = indx%Nbins[d] + 1;
     indx /= Nbins[d];
  }
}
//________________________________________________________________________________
TTree *SparseFit() {
  THnSparse *h = (THnSparse *) gDirectory->Get("Sparse");
  if (! h) return 0;
#ifdef __TOTREE__
  TFile *f = new TFile("Sparse2Tree.root","recreate");
  TTree *tree = toTree(h);
#else /* ! __TOTREE__ */
  Int_t dim = h->GetNdimensions();
  TArrayI Bins(dim); Int_t *bins = Bins.GetArray();
  TArrayI NBins(dim); Int_t *Nbins = NBins.GetArray();
  TArrayD X(dim+11); Double_t *x = X.GetArray();
  TAxis **axises = new TAxis*[dim];
  memset(axises, 0, sizeof(TAxis *) * dim);
  if (! gROOT->IsBatch()) {
    c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
    if (c1) c1->Clear();
    else    c1 = new TCanvas();
  }
  TFile *f = new TFile("SparseFit.root","recreate");
  
  TTree* tree = new TTree("FitP", "Fit Results");
  TString branchname;
  Int_t NbinsT = 1;
  for (Int_t d = 0; d < dim; ++d) {
    if (branchname.Length()) branchname += ":";
    axises[d] =  h->GetAxis(d);
    Nbins[d]  = axises[d]->GetNbins();
    if (d < dim - 1) NbinsT *= Nbins[d];
    branchname += axises[d]->GetName();
    branchname += "/D";
  }
  tree->Branch("coord", x, branchname);
  tree->Branch("fit", &x[dim], "entries/D:mean/D:rms/D:peak/D:dpeak/D:mu/D:dmu/D:sigma/D:dsimga/D:chisq/D:prob/D");
#ifndef __PROJECTION__
  //  Int_t step[6] = {12, 40, 1, 1, 1, 1};
  Int_t *step = 0;
  Int_t binsM[6] = {0};
  TH1F **hist = new TH1F*[NbinsT];
  memset (hist, 0, sizeof(TH1D *)*NbinsT);
  TH1F *proj = 0;
  
  for (Long64_t i = 0; i < h->GetNbins(); ++i) {
    x[dim] = h->GetBinContent(i, bins);
    Bool_t fail = kFALSE;
    for (Int_t d = 0; d < dim-1; ++d) {
      if (bins[d] > Nbins[d]) {fail = kTRUE; break;}
    }
    if (fail) continue;
    for (Int_t d = 0; d < dim-1; ++d) {

      x[d] = h->GetAxis(d)->GetBinCenter(bins[d]);
    }
    Int_t index = Bins2Index(bins,Nbins,dim,step);
    if (! hist[index]) {
      TString Name("P");
      TString Title;
      Index2Bins(index, Nbins, dim, binsM);
      for (Int_t d = 0; d < dim-1; ++d) {
	Name += Form("_%i",binsM[d]);
	if      (d < 2) Title += Form(" %s=%i",axises[d]->GetName(),binsM[d]);
        else if (d < 4) Title += Form(" %s=%7.1f",axises[d]->GetName(),x[d]);
        else 	        Title += Form(" %s=%7.3f",axises[d]->GetName(),x[d]);
      }
      hist[index] = new TH1F(Name,Title, axises[dim-1]->GetNbins(), axises[dim-1]->GetXmin(), axises[dim-1]->GetXmax());
    }
    hist[index]->SetBinContent(bins[dim-1], x[dim]);
  }
  for (Int_t index = 0; index < NbinsT; index++) {
    if (! hist[index]) continue;
    if (hist[index]->GetSumOfWeights() < 100) {
      delete hist[index]; hist[index] = 0;
      continue;
    }
    Index2Bins(index, Nbins, dim, bins);
    for (Int_t d = 0; d < dim; ++d) {
      x[d] = h->GetAxis(d)->GetBinCenter(bins[d]);
    }
    proj = hist[index];
    proj->Fit("gaus");
    TF1 *gaus = (TF1 *) proj->GetListOfFunctions()->FindObject("gaus");
    x[dim  ] = proj->GetSumOfWeights();
    x[dim+1] = proj->GetMean();
    x[dim+2] = proj->GetRMS();
    for (Int_t p = 0; p < 3; p++) {
      x[dim+3+2*p] = gaus->GetParameter(p);
      x[dim+4+2*p] = gaus->GetParError(p);
    }
    x[dim+9] = gaus->GetChisquare();
    x[dim+10] = gaus->GetProb();
    tree->Fill();
    proj->Write();
    if (! gROOT->IsBatch() ) {
      if (c1) c1->Update();
      Ask();
    }
  }
#else /* __PROJECTION__ */
  TH1D *proj = 0;
  for (Int_t index = 0; index < NbinsT; index++) {
    Index2Bins(index, Nbins, dim, bins);
    for (Int_t d = 0; d < dim - 1; d++) {
      axises[d]->SetRange(bins[d],bins[d]);
    }
    proj = h->Projection(dim-1);
    if (proj->GetEntries() > 100) {
      TString name("P");
      for (Int_t d = 0; d < dim - 1; d++) {
	name += "_"; name += bins[d];
      }
      proj->SetName(name);
      proj->Fit("gaus");
      TF1 *gaus = (TF1 *) proj->GetListOfFunctions()->FindObject("gaus");
      x[dim  ] = proj->GetEntries();
      x[dim+1] = proj->GetMean();
      x[dim+2] = proj->GetRMS();
      for (Int_t p = 0; p < 3; p++) {
	x[dim+3+2*p] = gaus->GetParameter(0);
	x[dim+4+2*p] = gaus->GetParError(0);
      }
      x[dim+9] = gaus->GetChisquare();
      x[dim+10] = gaus->GetProb();
      tree->Fill();
      proj->Write();
      if (! gROOT->IsBatch() ) {
	if (c1) c1->Update();
	Ask();
      }
    }
    delete proj; proj = 0;
  }
#endif /* __TOTREE__ */
#endif /* __TOTREE__ */
  return tree;
}
