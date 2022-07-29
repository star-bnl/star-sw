/*
  root.exe *GEX*.root 'Chain.C("FitP")' 'mdf2NeNpTCut.C+(tChain)'
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
#include "TF1.h"
#include "TF2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TChain.h"
//#include "MDFCorrection.h"
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
class MDFCorrection : public TObject {
public:
  UChar_t idx; 
  UChar_t nrows; 
  UChar_t PolyType; 
  UChar_t NVariables; 
  UChar_t NCoefficients; 
  UChar_t Power[100]; 
  Double_t DMean; 
  Double_t XMin[2]; 
  Double_t XMax[2]; 
  Double_t Coefficients[50]; 
  Double_t CoefficientsRMS[50]; 
  Double_t MDFunc(Double_t *x);
  Double_t MDFunc2(Double_t x, Double_t y);
  Double_t EvalFactor(Int_t p, Double_t x) const;
private:
  ClassDef(MDFCorrection,1)
};
//____________________________________________________________________
Double_t MDFCorrection::EvalFactor(Int_t p, Double_t x) const {
  // Evaluate function with power p at variable value x
  Int_t    i   = 0;
  Double_t p1  = 1;
  Double_t p2  = 0;
  Double_t p3  = 0;
  Double_t r   = 0;

  switch(p) {
  case 1:
    r = 1;
    break;
  case 2:
    r =  x;
    break;
  default:
    p2 = x;
    for (i = 3; i <= p; i++) {
      p3 = p2 * x;
      if (PolyType == kLegendre)
	p3 = ((2 * i - 3) * p2 * x - (i - 2) * p1) / (i - 1);
      else if (PolyType == kChebyshev)
	p3 = 2 * x * p2 - p1;
      p1 = p2;
      p2 = p3;
    }
    r = p3;
  }
  return r;
}
//________________________________________________________________________________
Double_t MDFCorrection::MDFunc(Double_t *x) {
  Double_t returnValue = DMean;
  Double_t term        = 0;
  UChar_t    i, j;
  for (i = 0; i < NCoefficients; i++) {
    // Evaluate the ith term in the expansion
    term = Coefficients[i];
    for (j = 0; j < NVariables; j++) {
      // Evaluate the factor (polynomial) in the j-th variable.
      Int_t    p  =  Power[i * NVariables + j];
      Double_t y  =  1 + 2. / (XMax[j] - XMin[j])
	* (x[j] - XMax[j]);
      term        *= EvalFactor(p,y);
    }
    // Add this term to the final result
    returnValue += term;
  }
  return returnValue;
}
//________________________________________________________________________________
Double_t MDFCorrection::MDFunc2(Double_t x, Double_t y) {
  Double_t xx[2] = {x, y};
  return MDFunc(xx);
}
//________________________________________________________________________________
TMultiDimFit* fit = 0;
MDFCorrection **rows = 0;
//________________________________________________________________________________
MDFCorrection *MDFcor(Int_t i = 0) {
  if (! rows) {
    rows = new MDFCorrection*[3];
    Int_t k = 0;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 1;
    // mu
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   22;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  1;  rows[k]->Power[ 3] =  2;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  3;
    rows[k]->Power[ 6] =  2;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  1;  rows[k]->Power[11] =  5;
    rows[k]->Power[12] =  3;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  2;  rows[k]->Power[15] =  3;
    rows[k]->Power[16] =  2;  rows[k]->Power[17] =  4;
    rows[k]->Power[18] =  3;  rows[k]->Power[19] =  3;
    rows[k]->Power[20] =  2;  rows[k]->Power[21] =  5;
    rows[k]->Power[22] =  3;  rows[k]->Power[23] =  2;
    rows[k]->Power[24] =  1;  rows[k]->Power[25] =  6;
    rows[k]->Power[26] =  3;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  5;  rows[k]->Power[29] =  6;
    rows[k]->Power[30] =  4;  rows[k]->Power[31] =  1;
    rows[k]->Power[32] =  4;  rows[k]->Power[33] =  2;
    rows[k]->Power[34] =  4;  rows[k]->Power[35] =  6;
    rows[k]->Power[36] =  4;  rows[k]->Power[37] =  3;
    rows[k]->Power[38] =  5;  rows[k]->Power[39] =  3;
    rows[k]->Power[40] =  5;  rows[k]->Power[41] =  5;
    rows[k]->Power[42] =  6;  rows[k]->Power[43] =  5;
    rows[k]->DMean =   0.5915;
    rows[k]->Coefficients[ 0]    =         0.12142;  rows[k]->Coefficients[ 1]    =           0.518;
    rows[k]->Coefficients[ 2]    =        -0.69565;  rows[k]->Coefficients[ 3]    =        0.035468;
    rows[k]->Coefficients[ 4]    =         0.24901;  rows[k]->Coefficients[ 5]    =         0.32729;
    rows[k]->Coefficients[ 6]    =       -0.082096;  rows[k]->Coefficients[ 7]    =          0.2748;
    rows[k]->Coefficients[ 8]    =        -0.15529;  rows[k]->Coefficients[ 9]    =        0.053036;
    rows[k]->Coefficients[10]    =        -0.18635;  rows[k]->Coefficients[11]    =        -0.21226;
    rows[k]->Coefficients[12]    =       -0.095869;  rows[k]->Coefficients[13]    =         0.46573;
    rows[k]->Coefficients[14]    =        -0.26165;  rows[k]->Coefficients[15]    =        0.039502;
    rows[k]->Coefficients[16]    =       -0.057019;  rows[k]->Coefficients[17]    =        0.080737;
    rows[k]->Coefficients[18]    =        -0.10641;  rows[k]->Coefficients[19]    =        0.072822;
    rows[k]->Coefficients[20]    =       -0.057196;  rows[k]->Coefficients[21]    =        0.072271;
    rows[k]->CoefficientsRMS[ 0] =      2.7331e-05;  rows[k]->CoefficientsRMS[ 1] =      6.8688e-05;
    rows[k]->CoefficientsRMS[ 2] =      0.00017815;  rows[k]->CoefficientsRMS[ 3] =      6.4463e-05;
    rows[k]->CoefficientsRMS[ 4] =      0.00017854;  rows[k]->CoefficientsRMS[ 5] =      0.00019005;
    rows[k]->CoefficientsRMS[ 6] =      0.00010505;  rows[k]->CoefficientsRMS[ 7] =      0.00035043;
    rows[k]->CoefficientsRMS[ 8] =      0.00030078;  rows[k]->CoefficientsRMS[ 9] =      0.00040523;
    rows[k]->CoefficientsRMS[10] =       0.0002739;  rows[k]->CoefficientsRMS[11] =      0.00023002;
    rows[k]->CoefficientsRMS[12] =       9.581e-05;  rows[k]->CoefficientsRMS[13] =      0.00037056;
    rows[k]->CoefficientsRMS[14] =      0.00044255;  rows[k]->CoefficientsRMS[15] =      9.5917e-05;
    rows[k]->CoefficientsRMS[16] =      0.00019139;  rows[k]->CoefficientsRMS[17] =      0.00038451;
    rows[k]->CoefficientsRMS[18] =      0.00043657;  rows[k]->CoefficientsRMS[19] =      0.00037608;
    rows[k]->CoefficientsRMS[20] =      0.00048424;  rows[k]->CoefficientsRMS[21] =      0.00036469;
    k = 1;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 2;
    // sigma
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   21;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  2;  rows[k]->Power[ 3] =  1;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  2;
    rows[k]->Power[ 6] =  3;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  2;  rows[k]->Power[11] =  3;
    rows[k]->Power[12] =  4;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  1;  rows[k]->Power[15] =  3;
    rows[k]->Power[16] =  2;  rows[k]->Power[17] =  5;
    rows[k]->Power[18] =  5;  rows[k]->Power[19] =  2;
    rows[k]->Power[20] =  1;  rows[k]->Power[21] =  4;
    rows[k]->Power[22] =  1;  rows[k]->Power[23] =  5;
    rows[k]->Power[24] =  5;  rows[k]->Power[25] =  1;
    rows[k]->Power[26] =  3;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  5;  rows[k]->Power[29] =  3;
    rows[k]->Power[30] =  6;  rows[k]->Power[31] =  2;
    rows[k]->Power[32] =  3;  rows[k]->Power[33] =  5;
    rows[k]->Power[34] =  4;  rows[k]->Power[35] =  5;
    rows[k]->Power[36] =  5;  rows[k]->Power[37] =  4;
    rows[k]->Power[38] =  6;  rows[k]->Power[39] =  6;
    rows[k]->Power[40] =  2;  rows[k]->Power[41] =  4;
    rows[k]->DMean =   0.1415;
    rows[k]->Coefficients[ 0]    =       -0.038573;  rows[k]->Coefficients[ 1]    =        -0.15368;
    rows[k]->Coefficients[ 2]    =        0.078618;  rows[k]->Coefficients[ 3]    =        0.076764;
    rows[k]->Coefficients[ 4]    =       -0.021717;  rows[k]->Coefficients[ 5]    =         0.18198;
    rows[k]->Coefficients[ 6]    =       -0.080556;  rows[k]->Coefficients[ 7]    =       -0.041054;
    rows[k]->Coefficients[ 8]    =        -0.12834;  rows[k]->Coefficients[ 9]    =      -0.0067788;
    rows[k]->Coefficients[10]    =       -0.040617;  rows[k]->Coefficients[11]    =        0.015402;
    rows[k]->Coefficients[12]    =         0.07045;  rows[k]->Coefficients[13]    =        0.026555;
    rows[k]->Coefficients[14]    =       -0.074055;  rows[k]->Coefficients[15]    =       -0.034786;
    rows[k]->Coefficients[16]    =        0.041479;  rows[k]->Coefficients[17]    =        0.023544;
    rows[k]->Coefficients[18]    =        0.031308;  rows[k]->Coefficients[19]    =        0.017737;
    rows[k]->Coefficients[20]    =       -0.011397;
    rows[k]->CoefficientsRMS[ 0] =      1.9296e-05;  rows[k]->CoefficientsRMS[ 1] =      5.7026e-05;
    rows[k]->CoefficientsRMS[ 2] =      6.0565e-05;  rows[k]->CoefficientsRMS[ 3] =      7.3436e-05;
    rows[k]->CoefficientsRMS[ 4] =      0.00010982;  rows[k]->CoefficientsRMS[ 5] =      0.00020313;
    rows[k]->CoefficientsRMS[ 6] =      0.00011317;  rows[k]->CoefficientsRMS[ 7] =       0.0001075;
    rows[k]->CoefficientsRMS[ 8] =      0.00026135;  rows[k]->CoefficientsRMS[ 9] =      0.00019875;
    rows[k]->CoefficientsRMS[10] =      9.2522e-05;  rows[k]->CoefficientsRMS[11] =      0.00012114;
    rows[k]->CoefficientsRMS[12] =        0.000102;  rows[k]->CoefficientsRMS[13] =      0.00027102;
    rows[k]->CoefficientsRMS[14] =      0.00017262;  rows[k]->CoefficientsRMS[15] =      0.00015424;
    rows[k]->CoefficientsRMS[16] =      0.00021758;  rows[k]->CoefficientsRMS[17] =      0.00021629;
    rows[k]->CoefficientsRMS[18] =      0.00030315;  rows[k]->CoefficientsRMS[19] =      0.00015055;
    rows[k]->CoefficientsRMS[20] =       0.0001791;
    k = 2;
    rows[k] = new MDFCorrection;
    rows[k]->nrows = 3;
    rows[k]->idx = 3;
    // 1/a0
    rows[k]->PolyType =        0;
    rows[k]->NVariables =      2;
    rows[k]->NCoefficients =   21;
    rows[k]->XMin[ 0] =       3.25;  rows[k]->XMin[ 1] =     1.5272;
    rows[k]->XMax[ 0] =      10.35;  rows[k]->XMax[ 1] =     2.6258;
    rows[k]->Power[ 0] =  1;  rows[k]->Power[ 1] =  1;
    rows[k]->Power[ 2] =  1;  rows[k]->Power[ 3] =  2;
    rows[k]->Power[ 4] =  1;  rows[k]->Power[ 5] =  3;
    rows[k]->Power[ 6] =  3;  rows[k]->Power[ 7] =  1;
    rows[k]->Power[ 8] =  2;  rows[k]->Power[ 9] =  2;
    rows[k]->Power[10] =  2;  rows[k]->Power[11] =  3;
    rows[k]->Power[12] =  4;  rows[k]->Power[13] =  1;
    rows[k]->Power[14] =  3;  rows[k]->Power[15] =  2;
    rows[k]->Power[16] =  1;  rows[k]->Power[17] =  5;
    rows[k]->Power[18] =  2;  rows[k]->Power[19] =  4;
    rows[k]->Power[20] =  3;  rows[k]->Power[21] =  3;
    rows[k]->Power[22] =  4;  rows[k]->Power[23] =  6;
    rows[k]->Power[24] =  2;  rows[k]->Power[25] =  1;
    rows[k]->Power[26] =  1;  rows[k]->Power[27] =  4;
    rows[k]->Power[28] =  4;  rows[k]->Power[29] =  2;
    rows[k]->Power[30] =  4;  rows[k]->Power[31] =  3;
    rows[k]->Power[32] =  5;  rows[k]->Power[33] =  5;
    rows[k]->Power[34] =  3;  rows[k]->Power[35] =  4;
    rows[k]->Power[36] =  4;  rows[k]->Power[37] =  5;
    rows[k]->Power[38] =  5;  rows[k]->Power[39] =  1;
    rows[k]->Power[40] =  5;  rows[k]->Power[41] =  6;
    rows[k]->DMean =   0.6806;
    rows[k]->Coefficients[ 0]    =        -0.50957;  rows[k]->Coefficients[ 1]    =          1.3324;
    rows[k]->Coefficients[ 2]    =          2.2306;  rows[k]->Coefficients[ 3]    =        0.018721;
    rows[k]->Coefficients[ 4]    =         -1.9494;  rows[k]->Coefficients[ 5]    =          1.4649;
    rows[k]->Coefficients[ 6]    =         0.46458;  rows[k]->Coefficients[ 7]    =        -0.16929;
    rows[k]->Coefficients[ 8]    =         -1.2244;  rows[k]->Coefficients[ 9]    =          2.6239;
    rows[k]->Coefficients[10]    =         -2.0547;  rows[k]->Coefficients[11]    =         -1.8734;
    rows[k]->Coefficients[12]    =        -0.47307;  rows[k]->Coefficients[13]    =        -0.18053;
    rows[k]->Coefficients[14]    =         0.74014;  rows[k]->Coefficients[15]    =         -1.0493;
    rows[k]->Coefficients[16]    =           1.299;  rows[k]->Coefficients[17]    =        -0.61577;
    rows[k]->Coefficients[18]    =        -0.32173;  rows[k]->Coefficients[19]    =       -0.088406;
    rows[k]->Coefficients[20]    =         0.22538;
    rows[k]->CoefficientsRMS[ 0] =      9.0093e-05;  rows[k]->CoefficientsRMS[ 1] =      0.00054122;
    rows[k]->CoefficientsRMS[ 2] =      0.00098388;  rows[k]->CoefficientsRMS[ 3] =       0.0005543;
    rows[k]->CoefficientsRMS[ 4] =      0.00097002;  rows[k]->CoefficientsRMS[ 5] =       0.0018305;
    rows[k]->CoefficientsRMS[ 6] =      0.00052539;  rows[k]->CoefficientsRMS[ 7] =       0.0014684;
    rows[k]->CoefficientsRMS[ 8] =       0.0010563;  rows[k]->CoefficientsRMS[ 9] =       0.0019343;
    rows[k]->CoefficientsRMS[10] =        0.002814;  rows[k]->CoefficientsRMS[11] =       0.0030169;
    rows[k]->CoefficientsRMS[12] =      0.00026871;  rows[k]->CoefficientsRMS[13] =       0.0011874;
    rows[k]->CoefficientsRMS[14] =       0.0016735;  rows[k]->CoefficientsRMS[15] =       0.0029385;
    rows[k]->CoefficientsRMS[16] =       0.0035866;  rows[k]->CoefficientsRMS[17] =        0.003569;
    rows[k]->CoefficientsRMS[18] =       0.0033066;  rows[k]->CoefficientsRMS[19] =      0.00073237;
    rows[k]->CoefficientsRMS[20] =       0.0037895;
  }
  return rows[i];
}
//________________________________________________________________________________
Double_t mdfcn(Double_t *x, Double_t *p = 0) {
  Int_t k = p[0];
  Double_t xx[2] = {x[0], p[1]};
  return MDFcor(k)->MDFunc(xx);
}
//________________________________________________________________________________
TF1 *GetXFunction(Int_t k = 0, Double_t y = 1e5) {
  static TF1 *fFuncX = 0;
  static Int_t kSave = -1;
  if (kSave != k) {SafeDelete(fFuncX);}
  if (! fFuncX) {
    fFuncX = new TF1(Form("Xfunc%i",k), mdfcn, 2, 11, 1);
    fFuncX->SetParameter(0,k);
    fFuncX->SetParameter(1,TMath::Log(TMath::Log(y)));
  }
  return fFuncX;
}
//________________________________________________________________________________
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Fri Jun  5 09:33:18 2020 by ROOT version 5.34/39
// from TTree FitP/Fit results
// found on file: EI_9ADCU.root
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

   // List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
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

   FitP(TTree *tree=0);
   virtual ~FitP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop(Int_t k = 0);
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

FitP::FitP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("EI_9ADCU.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("EI_9ADCU.root");
      }
      f->GetObject("FitP",tree);

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
void FitP::Loop(Int_t k)
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
  Double_t xx[2] = {0};
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    if (x < 3.2) continue;
    //    if (x < 2.3) continue;
    if (dmu <= 0 || dmu > 0.02) continue;
    if (dsigma <= 0 || dsigma > 0.02 || da0 > 0.2) continue;
    if (y <= 1.0) continue;
    xx[0] = x;
    xx[1] = TMath::Log(TMath::Log(y));
    Double_t V = mu;
    Double_t dV2 = dmu*dmu;
    if (k == 1) {
      V = sigma;
      dV2 = dsigma*dsigma;
    } else if (k == 2) {
      V = 1./a0;
      Double_t dV = da0*V*V;
      if (dV < 1e-6) dV = 1e-3;
      dV2 = dV*dV;
    }
    Double_t W = MDFcor(k)->MDFunc(xx);
    cout << "k = " << k << " x/y " << xx[0] << " / " << xx[1] << " V = " << V << " W = " << W << " +/-" << TMath::Sqrt(dV2) << endl;
    fit->AddRow(xx, V, dV2);
  }
}
#endif // #ifdef FitP_cxx

//using namespace std;
//________________________________________________________________________________
Double_t funcMDF(Double_t *x, Double_t *p=0) {
  if (! fit ) return 0;
  return fit->Eval(x, p);
}
//________________________________________________________________________________
void mdf2NeNpTCut(TChain *tChain = 0, Int_t k = 0) {
  if (! tChain) return;
  // Global data parameters 
  Int_t nVars      =  2;
  Int_t maxTerm    = 50;
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
#ifdef __CINT__
  EMDFPolyType type = (EMDFPolyType) 0;
#else
  TMultiDimFit::EMDFPolyType type = (TMultiDimFit::EMDFPolyType) 0;
#endif
  fit = TMultiDimFit::Instance();
  if (! fit)  fit = new TMultiDimFit(nVars, type,"vk");
  Int_t max = 5;
  Int_t mPowers[]   = { max, max};
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
  FitP t(tChain);
  t.Loop(k);
  // Print out the statistics
  fit->Print("s");
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
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
//____________________________________________________________________________
