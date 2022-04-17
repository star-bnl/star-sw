/*
  root.exe 'lDb.C("sdt20190226",0)' FitP.root 'mdf4ADCM.C+(1)' // Inner
  root.exe 'lDb.C("sdt20190226",0)' FitP.root 'mdf4ADCM.C+(2)' // Outer
  root.exe 'lDb.C("sdt20190226",0)' FitP.root  mdf4ADCM.C+     // Test
*/
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
#include "TF2.h"
#include "TProfile.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TBrowser.h"
#include "TCanvas.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrectionBC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection4MDF.h"
#include "StDetectorDbMaker/St_TpcAdcCorrection5MDF.h"
#if 0
class TMultiDimFit;
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
#endif
enum {kTPC = 2, kVar = 4};
TProfile *prof[kTPC][kVar] = {0};
TProfile *profC[kTPC][kVar] = {0};
TProfile *profD[kTPC][kVar] = {0};
TH2F     *h2[kTPC][kVar] = {0};     
//--------------------------------------------------------------------------------
Double_t mdf4(Int_t k, Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  return St_TpcAdcCorrection4MDF::instance()->Eval(k,z0,z1,z2,z3);
}
//--------------------------------------------------------------------------------
Double_t mdf5(Int_t k, Double_t z0, Double_t z1, Double_t z2, Double_t z3) {
  return St_TpcAdcCorrection5MDF::instance()->Eval(k,z0,z1,z2,z3);
}
//--------------------------------------------------------------------------------
Double_t adcB(Int_t k, Double_t ADC, Double_t zG) {
  Float_t gasGain = 1;
  Float_t gainNominal = 0;
   St_tss_tssparC *tsspar = St_tss_tssparC::instance();
  if (k == 1 ) {
    // kTpcOutIn = kTpcInner;
    gainNominal = tsspar->gain_in()*tsspar->wire_coupling_in();
    gasGain = tsspar->gain_in(1,1) *tsspar->wire_coupling_in();
  } else {
    // kTpcOutIn = kTpcOuter;
    gainNominal = tsspar->gain_out()*tsspar->wire_coupling_out();
    gasGain = tsspar->gain_out(1,41)*tsspar->wire_coupling_out();
  }
  Double_t Adc2GeVReal = tsspar->ave_ion_pot() * tsspar->scale()/gasGain;
  Double_t adc = St_TpcAdcCorrectionBC::instance()->CalcCorrection(k,ADC,TMath::Abs(zG));
  Double_t dE = Adc2GeVReal*adc;
  adc *= TMath::Exp(St_TpcAdcCorrectionBC::instance()->a(k)[0]); //TMath::Exp(-cor->a[0]);
  return adc;
}
TMultiDimFit* fit = 0;
Int_t    fgIO = 0;
Int_t    fgNP = 0;
// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class FitPS {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
   Float_t         i;
   Float_t         j;
   Float_t         k0;
   Float_t         k1;
   Float_t         k2;
   Float_t         k3;
   Float_t         k4;
   Float_t         k5;
   Float_t         k6;
  Float_t         x; // sector
  Float_t         y; // rowIO
  Float_t         z0;// Ntmbks
  Float_t         z1;// Npads
  Float_t         z2;// z
  Float_t         z3;// AdcL
   Float_t         z4;
   Float_t         z5;
   Float_t         z6;
   Float_t         mean;
   Float_t         rms;
   Float_t         peak;
   Float_t         mu;
   Float_t         sigma;
   Float_t         entries;
   Float_t         chisq;
   Float_t         prob;
   Float_t         a0;
   Float_t         a1;
   Float_t         a2;
   Float_t         a3;
   Float_t         a4;
   Float_t         a5;
   Float_t         a6;
   Float_t         Npar;
   Float_t         dpeak;
   Float_t         dmu;
   Float_t         dsigma;
   Float_t         da0;
   Float_t         da1;
   Float_t         da2;
   Float_t         da3;
   Float_t         da4;
   Float_t         da5;
   Float_t         da6;
   Float_t         muJ;
   Float_t         dmuJ;

   // List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_k0;   //!
   TBranch        *b_k1;   //!
   TBranch        *b_k2;   //!
   TBranch        *b_k3;   //!
   TBranch        *b_k4;   //!
   TBranch        *b_k5;   //!
   TBranch        *b_k6;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_z0;   //!
   TBranch        *b_z1;   //!
   TBranch        *b_z2;   //!
   TBranch        *b_z3;   //!
   TBranch        *b_z4;   //!
   TBranch        *b_z5;   //!
   TBranch        *b_z6;   //!
   TBranch        *b_mean;   //!
   TBranch        *b_rms;   //!
   TBranch        *b_peak;   //!
   TBranch        *b_mu;   //!
   TBranch        *b_sigma;   //!
   TBranch        *b_entries;   //!
   TBranch        *b_chisq;   //!
   TBranch        *b_prob;   //!
   TBranch        *b_a0;   //!
   TBranch        *b_a1;   //!
   TBranch        *b_a2;   //!
   TBranch        *b_a3;   //!
   TBranch        *b_a4;   //!
   TBranch        *b_a5;   //!
   TBranch        *b_a6;   //!
   TBranch        *b_Npar;   //!
   TBranch        *b_dpeak;   //!
   TBranch        *b_dmu;   //!
   TBranch        *b_dsigma;   //!
   TBranch        *b_da0;   //!
   TBranch        *b_da1;   //!
   TBranch        *b_da2;   //!
   TBranch        *b_da3;   //!
   TBranch        *b_da4;   //!
   TBranch        *b_da5;   //!
   TBranch        *b_da6;   //!
   TBranch        *b_muJ;   //!
   TBranch        *b_dmuJ;   //!

   FitPS(TTree *tree=0);
   virtual ~FitPS();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual void     Loop2();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

FitPS::FitPS(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("FitP.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("FitP.root");
      }
      f->GetObject("FitP",tree);

   }
   Init(tree);
}

FitPS::~FitPS()
{
   if (!fChain) return;
   //   delete fChain->GetCurrentFile();
}

Int_t FitPS::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t FitPS::LoadTree(Long64_t entry)
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

void FitPS::Init(TTree *tree)
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
   fChain->SetBranchAddress("k0", &k0, &b_k0);
   fChain->SetBranchAddress("k1", &k1, &b_k1);
   fChain->SetBranchAddress("k2", &k2, &b_k2);
   fChain->SetBranchAddress("k3", &k3, &b_k3);
   fChain->SetBranchAddress("k4", &k4, &b_k4);
   fChain->SetBranchAddress("k5", &k5, &b_k5);
   fChain->SetBranchAddress("k6", &k6, &b_k6);
   fChain->SetBranchAddress("x", &x, &b_x);
   fChain->SetBranchAddress("y", &y, &b_y);
   fChain->SetBranchAddress("z0", &z0, &b_z0);
   fChain->SetBranchAddress("z1", &z1, &b_z1);
   fChain->SetBranchAddress("z2", &z2, &b_z2);
   fChain->SetBranchAddress("z3", &z3, &b_z3);
   fChain->SetBranchAddress("z4", &z4, &b_z4);
   fChain->SetBranchAddress("z5", &z5, &b_z5);
   fChain->SetBranchAddress("z6", &z6, &b_z6);
   fChain->SetBranchAddress("mean", &mean, &b_mean);
   fChain->SetBranchAddress("rms", &rms, &b_rms);
   fChain->SetBranchAddress("peak", &peak, &b_peak);
   fChain->SetBranchAddress("mu", &mu, &b_mu);
   fChain->SetBranchAddress("sigma", &sigma, &b_sigma);
   fChain->SetBranchAddress("entries", &entries, &b_entries);
   fChain->SetBranchAddress("chisq", &chisq, &b_chisq);
   fChain->SetBranchAddress("prob", &prob, &b_prob);
   fChain->SetBranchAddress("a0", &a0, &b_a0);
   fChain->SetBranchAddress("a1", &a1, &b_a1);
   fChain->SetBranchAddress("a2", &a2, &b_a2);
   fChain->SetBranchAddress("a3", &a3, &b_a3);
   fChain->SetBranchAddress("a4", &a4, &b_a4);
   fChain->SetBranchAddress("a5", &a5, &b_a5);
   fChain->SetBranchAddress("a6", &a6, &b_a6);
   fChain->SetBranchAddress("Npar", &Npar, &b_Npar);
   fChain->SetBranchAddress("dpeak", &dpeak, &b_dpeak);
   fChain->SetBranchAddress("dmu", &dmu, &b_dmu);
   fChain->SetBranchAddress("dsigma", &dsigma, &b_dsigma);
   fChain->SetBranchAddress("da0", &da0, &b_da0);
   fChain->SetBranchAddress("da1", &da1, &b_da1);
   fChain->SetBranchAddress("da2", &da2, &b_da2);
   fChain->SetBranchAddress("da3", &da3, &b_da3);
   fChain->SetBranchAddress("da4", &da4, &b_da4);
   fChain->SetBranchAddress("da5", &da5, &b_da5);
   fChain->SetBranchAddress("da6", &da6, &b_da6);
   fChain->SetBranchAddress("muJ", &muJ, &b_muJ);
   fChain->SetBranchAddress("dmuJ", &dmuJ, &b_dmuJ);
   Notify();
}

Bool_t FitPS::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void FitPS::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t FitPS::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}

void FitPS::Loop2()
{
//   In a ROOT session, you can do:
//      Root > .L FitPS.C
//      Root > FitPS t
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
  TFile *fOut = new TFile("Test.root","recreate");
  struct Plot_t {
    const Char_t *vName;
    Int_t nx;
    Double_t xmin;
    Double_t xmax;
  };
  Plot_t Plots[kVar] = {
    {"tmbks",     25, 2.5, 27.5},
    {"pads",      15, 1.5, 16.5},
    {"Z",        110, -5., 215.},
    {"AdcL",      70,  3.,  10.}
  };
  static Double_t Adjust[2] = {
    0.4815 - (1.0 - (-1.03976e-01)), // Outer
    0.4948 - (1.0 - ( 1.47851e-01))  // Inner
  };
  const Char_t *IO[kTPC] = {"I","O"};
  for (Int_t io = 0; io < 2; io++)
    for (Int_t j = 0; j < kVar; j++) {
      prof[io][j]  = new TProfile(Form("%s%s",Plots[j].vName,IO[io]),"mu-0.4",Plots[j].nx,Plots[j].xmin,Plots[j].xmax,"s");
      profC[io][j] = new TProfile(Form("%s%sC",Plots[j].vName,IO[io]),"predicted mu - 0.4",Plots[j].nx,Plots[j].xmin,Plots[j].xmax,"s");
      profC[io][j]->SetMarkerColor(2); profC[io][j]->SetLineColor(2); 
      profD[io][j] = new TProfile(Form("%s%sD",Plots[j].vName,IO[io]),"mu - predicted",Plots[j].nx,Plots[j].xmin,Plots[j].xmax,"s");
      profD[io][j]->SetMarkerColor(4); profD[io][j]->SetLineColor(4); 
      h2[io][j] = new TH2F(Form("%s%s2D",Plots[j].vName,IO[io]),"dmu",Plots[j].nx,Plots[j].xmin,Plots[j].xmax,100,-0.25,0.25);
    }
	 Long64_t nentries = fChain->GetEntriesFast();
  Double_t xx[4], yy[4];
  Long64_t nbytes = 0, nb = 0;
  
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    Int_t io = j - 1;
    if (io < 0 || io > 1)    continue;
    if (chisq >  200) continue;
    if (dmu   > 0.01) continue;
    xx[0] = z0;// Ntmbks
    xx[1] = z1;// Npads
    xx[2] = z2;// z
    xx[3] = z3;// AdcL
    //    Double_t pred = mdf4(io,z0,z1,z2,z3) + Adjust[io];
    //    Double_t pred = mdf4(io,z0,z1,z2,z3) + Adjust[io];
    Int_t ioT = 1 - io;
    //    Double_t pred = TMath::Log(adcB(ioT, TMath::Exp(z3), TMath::Abs(207.707 - xx[2]))) - z3;
    Double_t pred = TMath::Log(adcB(ioT, TMath::Exp(z3), TMath::Abs(207.707 - xx[2]))) - z3 + mdf5(ioT,z0,z1,z2,z3);
    Double_t dmu = mu - pred;
    for (Int_t j = 0; j < kVar; j++) {
      prof[io][j]->Fill(xx[j],mu - 0.4);
      profC[io][j]->Fill(xx[j],pred - 0.4);
      profD[io][j]->Fill(xx[j],dmu);
      h2[io][j]->Fill(xx[j],dmu);
    }
  }
  for (Int_t io = 0; io < 2; io++) {
    TCanvas *c1 = new TCanvas(Form("c%s",IO[io]),IO[io], 800, 800);
    c1->Divide(2,2);
    for (Int_t j = 0; j < kVar; j++) {
      c1->cd(j+1);
      prof[io][j]->SetMinimum(-0.1);
      prof[io][j]->Draw();
      profC[io][j]->Draw("same");
      profD[io][j]->Draw("same");
    }
    c1->Update();
  }
  fOut->Write();
}
//________________________________________________________________________________
void FitPS::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L FitPS.C
//      Root > FitPS t
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
  Double_t xx[4], yy[4];
  const Double_t DDmin =  22.0; // Drift distance min
  const Double_t DDmax = 210.0; // -"-            max
  const Double_t zMax  = 207.707 - DDmin;
  const Double_t zMin  = 207.707 - DDmax;
  Long64_t nbytes = 0, nb = 0;
  Long64_t nev = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if (j != fgIO)    continue; // j = 1 Inner, j = 2 Outer; fgIO = 1 Inner, = 2 Outer,  
    if (chisq >  200) continue;
    if (dmu   > 0.01) continue;
    xx[0] = z0;// Ntmbks
    xx[1] = z1;// Npads
    xx[2] = z2;// z
    xx[3] = z3;// AdcL
    Int_t k = 2 - fgIO;
    Double_t ADC = TMath::Exp(z3);
    Double_t adc = adcB(k, ADC, TMath::Abs(207.707 - xx[2]));
    Double_t shift = TMath::Log(adc/ADC);
    fit->AddRow(xx, mu-shift, dmu*dmu);
    nev++;
  }
}

//using namespace std;
//________________________________________________________________________________
Double_t funcMDF(Double_t *x, Double_t *p=0) {
  if (! fit ) return 0;
  return fit->Eval(x, p);
}
//________________________________________________________________________________
void mdf4ADCM(Int_t io) {
  // Global data parameters 
  Int_t nVars      =  4;
  if (fgNP > 0) nVars = fgNP;
  Int_t maxTerm    = 20;
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  //  TMultiDimFit::EMDFPolyType type = (TMultiDimFit::EMDFPolyType) 0;
  TMultiDimFit::EMDFPolyType type = (TMultiDimFit::EMDFPolyType) 1; // Chebyshev
  fit = new TMultiDimFit(nVars, type,"vk");
  Int_t max = 3;
  Int_t mPowers[]   = { max, max, max, max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(10000);
  fit->SetMaxStudy(10000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");
  // Fill data  
  TTree *tree = (TTree *) gDirectory->Get("FitP");
  if (! tree ) return;
  if (! (io == 1 || io == 2) ) return;
  fgIO = io;
  FitPS t(tree);
  t.Loop();
  // Print out the statistics
  fit->Print("s");
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
#if 0
  //
  // Now for the data
  //
  TF2 *mdfP = new TF2("mdfP", funcMDF,xa->GetXmin(), xa->GetXmax(),ya->GetXmin(), ya->GetXmax());
  new TCanvas("mdfPar","mdfPar");
  mdfP->Draw();
#endif
  Int_t i, j;
  // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
}
//________________________________________________________________________________
void mdf4ADCM() {
  TTree *tree = (TTree *) gDirectory->Get("FitP");
  if (! tree ) return;
  FitPS t(tree);
  t.Loop2();
}

