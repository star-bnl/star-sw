// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  root.exe -q -b xyPad3qBG*.root Chain.C 'MakeTpcPadCorrectionMDCFitP.C+(tChain)' >& MakeTpcPadCorrectionMDC.log
   root.exe xyPad3qBG*.root Chain.C 'MakeTpcPadCorrectionMDCFitP.C+(tChain,1,0,1)'
   root.exe xyPad3qBG*.root Chain.C 'MakeTpcPadCorrectionMDCFitP.C+(tChain,1,1,1)'
 */
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#include "TString.h"
#include "Ask.h"
#include "TCanvas.h"
#else
class TMultiDimFit;
#endif
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
TMultiDimFit* fit = 0;
ofstream out;
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Tue Aug 30 16:10:50 2022 by ROOT version 5.34/39
// from TChain FitP/
//////////////////////////////////////////////////////////

#ifndef FitP_h
#define FitP_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class FitP {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
   Float_t         i;
   Float_t         j;
   Float_t         x;
   Float_t         y;
   Float_t         mean;
   Float_t         rms;
   Float_t         peak;
   Float_t         dpeak;
   Float_t         entries;
   Float_t         chisq;
   Float_t         prob;
   Float_t         Npar;
   Float_t         muJ;
   Float_t         dmuJ;
   Float_t         NormL;
   Float_t         mu;
   Float_t         sigma;
   Float_t         p3;
   Float_t         p4;
   Float_t         p5;
   Float_t         p6;
   Float_t         p7;
   Float_t         p8;
   Float_t         p9;
   Float_t         p10;
   Float_t         p11;
   Float_t         p12;
   Float_t         p13;
   Float_t         p14;
   Float_t         p15;
   Float_t         p16;
   Float_t         p17;
   Float_t         dNormL;
   Float_t         dmu;
   Float_t         dsigma;
   Float_t         dp3;
   Float_t         dp4;
   Float_t         dp5;
   Float_t         dp6;
   Float_t         dp7;
   Float_t         dp8;
   Float_t         dp9;
   Float_t         dp10;
   Float_t         dp11;
   Float_t         dp12;
   Float_t         dp13;
   Float_t         dp14;
   Float_t         dp15;
   Float_t         dp16;
   Float_t         dp17;

   // List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_mean;   //!
   TBranch        *b_rms;   //!
   TBranch        *b_peak;   //!
   TBranch        *b_dpeak;   //!
   TBranch        *b_entries;   //!
   TBranch        *b_chisq;   //!
   TBranch        *b_prob;   //!
   TBranch        *b_Npar;   //!
   TBranch        *b_muJ;   //!
   TBranch        *b_dmuJ;   //!
   TBranch        *b_NormL;   //!
   TBranch        *b_mu;   //!
   TBranch        *b_sigma;   //!
   TBranch        *b_p3;   //!
   TBranch        *b_p4;   //!
   TBranch        *b_p5;   //!
   TBranch        *b_p6;   //!
   TBranch        *b_p7;   //!
   TBranch        *b_p8;   //!
   TBranch        *b_p9;   //!
   TBranch        *b_p10;   //!
   TBranch        *b_p11;   //!
   TBranch        *b_p12;   //!
   TBranch        *b_p13;   //!
   TBranch        *b_p14;   //!
   TBranch        *b_p15;   //!
   TBranch        *b_p16;   //!
   TBranch        *b_p17;   //!
   TBranch        *b_dNormL;   //!
   TBranch        *b_dmu;   //!
   TBranch        *b_dsigma;   //!
   TBranch        *b_dp3;   //!
   TBranch        *b_dp4;   //!
   TBranch        *b_dp5;   //!
   TBranch        *b_dp6;   //!
   TBranch        *b_dp7;   //!
   TBranch        *b_dp8;   //!
   TBranch        *b_dp9;   //!
   TBranch        *b_dp10;   //!
   TBranch        *b_dp11;   //!
   TBranch        *b_dp12;   //!
   TBranch        *b_dp13;   //!
   TBranch        *b_dp14;   //!
   TBranch        *b_dp15;   //!
   TBranch        *b_dp16;   //!
   TBranch        *b_dp17;   //!

   FitP(TTree *tree=0);
   virtual ~FitP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
  virtual void     Mdf(Int_t sector, Int_t qB, Int_t io, Int_t max=7, Int_t maxTerm = 20, Int_t nrows = 96);
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif
#define FitP_cxx
#include <TH2.h>
#include <TStyle.h>
#include <TCanvas.h>

void FitP::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L FitP.C
//      Root > FitP t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
   }
}

#ifdef FitP_cxx
FitP::FitP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {

#ifdef SINGLE_TREE
      // The following code should be used if you want this class to access
      // a single tree instead of a chain
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("xyPad3C+xyPad3PCG4EYps_OO_200GeV_2021.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("xyPad3C+xyPad3PCG4EYps_OO_200GeV_2021.root");
      }
      f->GetObject("FitP",tree);

#else // SINGLE_TREE

      // The following code should be used if you want this class to access a chain
      // of trees.
      TChain * chain = new TChain("FitP","");
      chain->Add("xyPad3CG4EY11p5GeV_2020.root/FitP");
      chain->Add("xyPad3CG4EY13p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY14p5GeV_2019.root/FitP");
      chain->Add("xyPad3CG4EY17p3GeV_2021.root/FitP");
      chain->Add("xyPad3CG4EY19GeV_2019.root/FitP");
      chain->Add("xyPad3CG4EY19p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY26p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY26p5GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3CG4EY31GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3CG4EY31p2GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY3p85GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3CG4EY44p5GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3CG4EY4p59GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3CG4EY5p75GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY70GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3CG4EY7.3GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3CG4EY7p3GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3CG4EY7p7GeV_2019.root/FitP");
      chain->Add("xyPad3CG4EY7p7GeV_2020.root/FitP");
      chain->Add("xyPad3CG4EY7p7GeV_2021.root/FitP");
      chain->Add("xyPad3CG4EY9p2GeV_2019.root/FitP");
      chain->Add("xyPad3CG4EYAuAu200GeV_2019.root/FitP");
      chain->Add("xyPad3CG4EYCOLGeV_2019.root/FitP");
      chain->Add("xyPad3CG4EYCOLGeV_2020.root/FitP");
      chain->Add("xyPad3CG4EYCOLGeV_2021.root/FitP");
      chain->Add("xyPad3CG4EYFXT_2019.root/FitP");
      chain->Add("xyPad3CG4EYFXT_2020.root/FitP");
      chain->Add("xyPad3CG4EYFXT_2021.root/FitP");
      chain->Add("xyPad3CG4EYOO_200GeV_2021.root/FitP");
      chain->Add("xyPad3CG4EYpp500GeV_2022.root/FitP");
      chain->Add("xyPad3CG4EYps_OO_200GeV_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY11p5GeV_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY13p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY14p5GeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY17p3GeV_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY19GeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY19p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY26p5GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY26p5GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY31GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY31p2GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY3p85GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY44p5GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY4p59GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY5p75GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY70GeV_fixedTarget_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY7.3GeV_fixedTarget_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY7p3GeV_fixedTarget_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY7p7GeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY7p7GeV_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY7p7GeV_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EY9p2GeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYAuAu200GeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYCOLGeV_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYCOLGeV_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYCOLGeV_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYFXT_2019.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYFXT_2020.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYFXT_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYOO_200GeV_2021.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYpp500GeV_2022.root/FitP");
      chain->Add("xyPad3C+xyPad3PCG4EYps_OO_200GeV_2021.root/FitP");
      tree = chain;
#endif // SINGLE_TREE

   }
   Init(tree);
}

FitP::~FitP()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t FitP::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t FitP::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void FitP::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("i", &i, &b_i);
   fChain->SetBranchAddress("j", &j, &b_j);
   fChain->SetBranchAddress("x", &x, &b_x);
   fChain->SetBranchAddress("y", &y, &b_y);
   fChain->SetBranchAddress("mean", &mean, &b_mean);
   fChain->SetBranchAddress("rms", &rms, &b_rms);
   fChain->SetBranchAddress("peak", &peak, &b_peak);
   fChain->SetBranchAddress("dpeak", &dpeak, &b_dpeak);
   fChain->SetBranchAddress("entries", &entries, &b_entries);
   fChain->SetBranchAddress("chisq", &chisq, &b_chisq);
   fChain->SetBranchAddress("prob", &prob, &b_prob);
   fChain->SetBranchAddress("Npar", &Npar, &b_Npar);
   fChain->SetBranchAddress("muJ", &muJ, &b_muJ);
   fChain->SetBranchAddress("dmuJ", &dmuJ, &b_dmuJ);
   fChain->SetBranchAddress("NormL", &NormL, &b_NormL);
   fChain->SetBranchAddress("mu", &mu, &b_mu);
   fChain->SetBranchAddress("sigma", &sigma, &b_sigma);
   fChain->SetBranchAddress("p3", &p3, &b_p3);
   fChain->SetBranchAddress("p4", &p4, &b_p4);
   fChain->SetBranchAddress("p5", &p5, &b_p5);
   fChain->SetBranchAddress("p6", &p6, &b_p6);
   fChain->SetBranchAddress("p7", &p7, &b_p7);
   fChain->SetBranchAddress("p8", &p8, &b_p8);
   fChain->SetBranchAddress("p9", &p9, &b_p9);
   fChain->SetBranchAddress("p10", &p10, &b_p10);
   fChain->SetBranchAddress("p11", &p11, &b_p11);
   fChain->SetBranchAddress("p12", &p12, &b_p12);
   fChain->SetBranchAddress("p13", &p13, &b_p13);
   fChain->SetBranchAddress("p14", &p14, &b_p14);
   fChain->SetBranchAddress("p15", &p15, &b_p15);
   fChain->SetBranchAddress("p16", &p16, &b_p16);
   fChain->SetBranchAddress("p17", &p17, &b_p17);
   fChain->SetBranchAddress("dNormL", &dNormL, &b_dNormL);
   fChain->SetBranchAddress("dmu", &dmu, &b_dmu);
   fChain->SetBranchAddress("dsigma", &dsigma, &b_dsigma);
   fChain->SetBranchAddress("dp3", &dp3, &b_dp3);
   fChain->SetBranchAddress("dp4", &dp4, &b_dp4);
   fChain->SetBranchAddress("dp5", &dp5, &b_dp5);
   fChain->SetBranchAddress("dp6", &dp6, &b_dp6);
   fChain->SetBranchAddress("dp7", &dp7, &b_dp7);
   fChain->SetBranchAddress("dp8", &dp8, &b_dp8);
   fChain->SetBranchAddress("dp9", &dp9, &b_dp9);
   fChain->SetBranchAddress("dp10", &dp10, &b_dp10);
   fChain->SetBranchAddress("dp11", &dp11, &b_dp11);
   fChain->SetBranchAddress("dp12", &dp12, &b_dp12);
   fChain->SetBranchAddress("dp13", &dp13, &b_dp13);
   fChain->SetBranchAddress("dp14", &dp14, &b_dp14);
   fChain->SetBranchAddress("dp15", &dp15, &b_dp15);
   fChain->SetBranchAddress("dp16", &dp16, &b_dp16);
   fChain->SetBranchAddress("dp17", &dp17, &b_dp17);
   Notify();
}

Bool_t FitP::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void FitP::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t FitP::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef FitP_cxx

//using namespace std;
void FitP::Mdf(Int_t sector, Int_t qB, Int_t IO, Int_t max, Int_t maxTerm, Int_t nrows)
{
//   In a ROOT session, you can do:
//      Root > .L FitP.C
//      Root > FitP t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
  if (fChain == 0) return;
  TString dirName(Form("MDC_s%02i_io%i_qB%i",sector,IO,qB));
  Int_t nVars       = 2;
  gDirectory->cd("/");
  TDirectory *dir = gDirectory->mkdir(dirName);
  dir->cd();
  // make fit object and set parameters on it. 
  if (fit) delete fit;
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kLegendre,"vk");
  fit->SetName(dirName);
  gDirectory->Append(fit);
  Int_t nx = 10;
  Int_t ny = 32;
  fit->SetBinVarX(nx);
  fit->SetBinVarY(ny);
  Double_t xS   = 24*qB + sector;
  Double_t xmin = xS;
  Double_t xmax = xS + 0.5;
  if (IO) {
    xmin = xS - 0.5;
    xmax = xS;
  }
  TH2F *hists[2] = {new TH2F("val_x_0","input values 0", nx, xmin, xmax, 200, -0.5, 0.5), new TH2F("val_x_1","input values 1", ny, -1, 1, 200, -0.5, 0.5)};
  //  Int_t mPowers[]   = {max , max};
  Int_t mPowers[]   = {3 , max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
#if 1
  //  fit->SetPowerLimit(max);
  fit->SetPowerLimit(1);
  fit->SetMinAngle(); //10);
  fit->SetMaxAngle(1); //10);
  fit->SetMinRelativeError(0.01);
#endif
  
  Long64_t nentries = fChain->GetEntriesFast();
  Long64_t nbytes = 0, nb = 0;
  Long64_t nev = 0;
  Int_t ix1, ix2;
  Double_t xx[2];
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // tChain->Draw("mu:0.5*y+TMath::Nint(x)>>xy2P(768,0.5,24.5,200,-0.2,0.2)","dmu<0.1&&dsigma<0.05&&chisq<2e2&&mu>-0.8","prof")
    Int_t ix = this->i;
    Int_t jy = this->j;
    if (ix == 0 || jy == 0)   continue;
    if (x < xmin || x > xmax) continue;
    if (dmu    > 0.15) continue;
    if (dsigma > 0.05) continue;
    if (chisq >  200.) continue;
    if (mu    <= -0.8) continue;
    /*
      
      static Hists3D xyPad3 ## SIGN ("xyPad3" MakeString(SIGN) ,"log(dEdx/Pion)" MakeString(NEGPOS) ,"sector+yrow[-0.5,0.5] and xpad [-1,1]"," xpad",numberOfSectors*20, 32,-1,1, 200, -5., 5., 0.5, 24.5); \
	CdEdx[NdEdx].xpad = 2*(CdEdx[NdEdx].pad - 0.5)/NoPadsInRow - 1.0;
	CdEdx[NdEdx].yrow = sector + 0.5*((row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/St_tpcPadConfigC::instance()->innerPadRows(sector) : 
					  (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/(St_tpcPadConfigC::instance()->numberOfRows(sector) - St_tpcPadConfigC::instance()->innerPadRows(sector)));
    */
#if 1 /* 2D */
    xx[0] = x;
    xx[1] = y;
#else
    xx[0] = y;
#endif
    hists[0]->Fill(x,mu);
    hists[1]->Fill(y,mu);
    fit->AddRow(xx, mu, dmu*dmu);
    nev++;
  }

  fit->Print("p");
  // Print out the statistics
  fit->Print("s");
  cout << "SampleSize = " << fit->GetSampleSize() << "\tSumSqQuantity = " << fit->GetSumSqQuantity() << "\tSumSqAvgQuantity = " << fit->GetSumSqQuantity()/fit->GetSampleSize() << endl;
  out << "// SampleSize = " << fit->GetSampleSize() << "\tSumSqQuantity = " << fit->GetSumSqQuantity() << "\tSumSqAvgQuantity = " << fit->GetSumSqQuantity()/fit->GetSampleSize() << endl;
  //  if (fit->GetSumSqQuantity()/fit->GetSampleSize() < 5e-5) return;
  // Book histograms 
  //  fit->SetBinVarX(1000);
  //  fit->SetBinVarY(1000);
  fit->MakeHistograms();
  TAxis *ax = 0;
  TH1 *d_orig = (TH1 *)fit->GetHistograms()->FindObject("d_orig");
  ax = d_orig->GetXaxis();
  d_orig->SetBins(100, ax->GetXmin(), ax->GetXmax()); 
  TH1 *res_train = (TH1*) fit->GetHistograms()->FindObject("res_train");
  ax = res_train->GetXaxis();
  res_train->SetBins(100, ax->GetXmin(), ax->GetXmax()); 
  //  ((TH1 *)fit->GetHistograms()->FindObject("res_train"))->SetNbinx(100);
  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
  //
  Int_t i, j;
 // Assignment to coefficients vector.
  Int_t idx = IO + 2*(sector-1) + 48*qB;
  out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
  out << "  row.nrows = " << nrows << "; //" << gDirectory->GetName() << endl;
  out << "  row.idx   = " << Form("%2i", idx+1) << ";" << endl;
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  out << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  out << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  out << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
    out << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;  out << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
    out << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;  out << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
      out << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;    out << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  out << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    out << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) {cout << endl; out << endl;}
  }
  if (fit->GetNCoefficients()%2) {cout << endl; out << endl;}
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    out << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) {cout << endl; out << endl;}
  }
  if (fit->GetNCoefficients()%2) {cout << endl; out << endl;}
	out << "  tableSet->AddAt(&row," << idx << ");" << endl;
}//____________________________________________________________________________
void MakeTpcPadCorrectionMDCFitP(TChain *tChain, Int_t sec = -1, Int_t qb = -1, Int_t IO = -1, Int_t max=5, Int_t maxTerm = 20, Int_t date = 20190225, Int_t time = 202320){
  if (! tChain) return;
  FitP t(tChain);
  TFile *fOut = new TFile("MakeTpcPadCorrectionMDCFitP.root","recreate");
  TString cOut =  Form("TpcPadCorrectionMDC.%8i.%06i.C",date,time);
  cout << "Create " << cOut << endl;
  out.open(cOut.Data());
  Int_t nrows = 96;
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcPadCorrectionMDC\"," << nrows << "); //" << gDirectory->GetName() << endl;
  Int_t sec1 = 1, sec2 = 24;
  Int_t qB1 = 0, qB2 = 1;
  Int_t io1 = 0, io2 = 1;
  if (sec > 0) {sec1 = sec2 = sec;}
  if (qb >= 0) { qB1 = qB2 = qb;}
  if (IO >= 0) {io1 = io2 = IO;}
  for (Int_t qB = qB1; qB <= qB2; qB++) {
    for (Int_t sector = sec1; sector <= sec2; sector++) {
      for (Int_t io = io1; io <= io2; io++) {// io == 0 : Outer, io = 1 : Inner
	t.Mdf(sector, qB, io, max, maxTerm, nrows);
	if (Ask()) goto ENDL;
	else if (! gROOT->IsBatch()) {
	  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
	  if (! c1 ) c1 = new TCanvas("c1","c1",1200,1600);
	  else c1->Clear();
	  c1->SetTitle(gDirectory->GetName());
	  c1->Divide(2,3);
	  c1->cd(2);
	  cout << gDirectory->GetName() << endl;
	  TH1 *res_train = (TH1*) gDirectory->Get("res_train");
	  if (res_train) {
	    res_train->Draw();
	    TH1 *d_orig = (TH1*) gDirectory->Get("d_orig");
	    if (d_orig) {
	      c1->cd(1);
	      d_orig->SetLineColor(2);
	      d_orig->Draw();
	    }
	    c1->Update();
	  }
	  c1->cd (3);
          TH2 *val_x_0 = (TH2 *) gDirectory->Get("val_x_0");
	  if (val_x_0) val_x_0->Draw("colz");
	  c1->cd(4);
	  TH2* res_x_0 = (TH2 *) gDirectory->Get("res_x_0");
	  if (res_x_0) res_x_0->Draw("colz");
	  c1->cd (5);
          TH2 *val_x_1 = (TH2 *) gDirectory->Get("val_x_1");
	  if (val_x_1) val_x_1->Draw("colz");
	  c1->cd(6);
	  TH2* res_x_1 = (TH2 *) gDirectory->Get("res_x_1");
	  if (res_x_1) res_x_1->Draw("colz");
	  c1->Update();
	}
      }
    }
  }
 ENDL:
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close();
  fOut->Write();
}
//____________________________________________________________________________
