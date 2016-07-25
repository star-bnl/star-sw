//*********************************************************************
// Convert a THnSparse to a TTree using efficient iteration through the THnSparse
//   and draw a THnSparse using TParallelCoord. 
//  The plot will contain one line for each filled bin,
//  with the bin's coordinates on each axis, and the bin's content on
//  the rightmost axis.
// 
//  Run as
//    .L $ROOTSYS/tutorials/tree/drawsparse.C+
// 
//  Axel.Naumann@cern.ch (2007-09-14)
// ********************************************************************

#include "TParallelCoord.h"
#include "TParallelCoordVar.h"
#include "TROOT.h"
#include "TTree.h"
#include "TLeaf.h"
#include "THnSparse.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TRandom.h"
#include "TFile.h"
#include "TH3.h"
#include "Riostream.h"
TTree* tree = 0;
struct T_t {
  Double_t        refMult;
  Double_t        cpT;
  Double_t        eta;
  Double_t        hyp;
  Double_t        z;
  Double_t        content;
};
//______________________________________________________________________________
void THn2Tree(THnSparse* h) {
   // Creates a TTree and fills it with the coordinates of all
   // filled bins. The tree will have one branch for each dimension,
   // and one for the bin content.

   Int_t dim = h->GetNdimensions();
   Double_t* x = new Double_t[dim + 1];
   memset(x, 0, sizeof(Double_t) * (dim + 1));
   if (! tree) {
     TString name(h->GetName()); name += "_tree";
     TString title(h->GetTitle()); title += " tree";
     tree = new TTree(name, title);
     
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
   }
   Int_t *bins = new Int_t[dim];
   for (Long64_t i = 0; i < h->GetNbins(); ++i) {
      x[dim] = h->GetBinContent(i, bins);
      for (Int_t d = 0; d < dim; ++d) {
         x[d] = h->GetAxis(d)->GetBinCenter(bins[d]);
      }
      tree->Fill();
   }

   delete [] bins;
   delete [] x;
}
//________________________________________________________________________________
void toTree(const Char_t *file="0_Sparse_pT100_eta24.root", const Char_t *name = "sZdEdx") {
  TFile *f = new TFile(file);
  if (! f) return;
  THnSparse *h = (THnSparse *) f->Get(name);
  if (! h) return;
  TString Out(file); Out.ReplaceAll(".root","_"); Out += name; Out += "_Tree.root";
  TFile *fOut = new TFile(Out,"recreate");
  THn2Tree(h);
  fOut->Write();
  delete fOut;
}
//________________________________________________________________________________
void toSparse(const Char_t *file="0_Sparse_pT100_eta24.root", 
	      const Char_t *name = "sZdEdx",
	      const Char_t *treeFName = "Sparse_pT100_eta24_sZdEdx_Tree.root") {
  TFile *f = new TFile(file);
  if (! f) return;
  THnSparseF *h = (THnSparseF *) f->Get(name);
  if (! h) return;
  Int_t NoDim = h->GetNdimensions();
  Int_t *Nbins = new Int_t[NoDim];
  for (Int_t d = 0; d < NoDim; d++) {
    Nbins[d] = h->GetAxis(d)->GetNbins();
  }
  THnSparseF *hNew = new THnSparseF(h->GetName(), h->GetTitle(), NoDim, Nbins, 0, 0);
  delete [] Nbins;
  for (Int_t d = 0; d < NoDim; d++) {
    TAxis *a = h->GetAxis(d);
    if (a->IsVariableBinSize()) {
      hNew->SetBinEdges(d,a->GetXbins()->GetArray());
    } else {
      hNew->GetAxis(d)->Set(a->GetNbins(),a->GetXmin(),a->GetXmax());
    }
  }
  delete f;
  TFile *fileT = new TFile(treeFName);
  if (! fileT) return;
  T_t T;
  TTree *tree = (TTree *) fileT->Get(Form("%s_tree",name));
  if (! tree ) return;
  tree->SetMakeClass(1);
  TBranch        *b_coord;   //!
  TBranch        *b_bincontent;   //!
  tree->SetBranchAddress("coord", &T.refMult, &b_coord);
  tree->SetBranchAddress("bincontent", &T.content, &b_bincontent);
  
  Long64_t nentries = tree->GetEntriesFast();
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = tree->LoadTree(jentry);
    if (ientry < 0) break;
    tree->GetEntry(jentry);  
    hNew->Fill(&T.refMult, T.content);
  }
  TString Out(treeFName); Out.ReplaceAll("_Tree.root",""); Out += "new.root";
  TFile *fOut = new TFile(Out,"recreate");
  hNew->Write();
  delete fOut;
}
