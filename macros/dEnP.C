/* Distribution of deposited energy eV versus no. of primary clustres
   root.exe dEnP.C+
*/
#include "TH1.h"
#include "TH2.h"
#include "TSystem.h"
#include "TFile.h"
#include "TRandom3.h"
#include "TError.h"
#include "TMath.h"
#include "TF1.h"
#include "TROOT.h"
TH1D *mdNdE = 0;
TH2F *znpL  = 0;
TH2F *znpLS = 0;
TFile *fOut = 0;
void dEnP() {
  new TRandom3;
  TF1::InitStandardFunctions();
  if (! mdNdE) {
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[1] = {"dNdE_Bichsel.root"};
    Int_t i = 0;
    Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
    if (! file) Fatal("dEnP","File %s has not been found in path %s",Files[i],path);
    else        Warning("dEnP","File %s has been found as %s",Files[i],file);
    TFile       *pFile = new TFile(file);
    mdNdE = (TH1D *)         pFile->Get("dNdE");     if (mdNdE)    mdNdE->SetDirectory(0);
    if (! mdNdE) return;
    delete pFile;
    delete [] file;
  }
  Int_t nx =   100;
  Double_t xl1 =  0.4;
  Double_t xl2 = 10.4;
  if (! znpL) {
    if (! fOut) fOut = new TFile("znpLS6,root","recreate");
    znpL = new TH2F("znpL","z = log(dE/nP) versus log(nP)",nx,xl1,xl2,14000,0.,7.);
    znpLS = new TH2F("znpLS","z = log(dE/nP) versus log(nP) shifted and scaled",nx,xl1,xl2,2000,-5.0,15.0);
  }
  /*
root.exe [30] znpL_1->Fit("pol2","er","",0.5,5.5)
 FCN=1619.97 FROM MINOS     STATUS=SUCCESSFUL     30 CALLS         152 TOTAL
                     EDM=1.85318e-11    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           2.93649e+00   3.69754e-04   3.64080e-09  -8.12008e-08
   2  p1           7.33224e-03   2.31856e-04  -9.65120e-10  -1.13890e-04
   3  p2           2.23307e-03   3.39739e-05   3.39739e-05  -1.61530e-04
root.exe [10] znpL_2->Fit("pol4","e")
 FCN=21198.4 FROM MINOS     STATUS=FAILURE       502 CALLS        2566 TOTAL
                     EDM=1.58937e-13    STRATEGY= 1      ERR MATRIX NOT POS-DEF
  EXT PARAMETER                APPROXIMATE        STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           2.97780e-01   5.06605e-05  -6.92102e-06  -9.18198e-04
   2  p1          -1.24762e-01   1.17656e-05   8.17163e-06  -6.35876e-04
   3  p2           2.54248e-02   1.52554e-06  -2.70691e-06   6.29532e-02
   4  p3          -2.51517e-03   1.77540e-07   3.42556e-07  -4.55810e-02
   5  p4           9.36223e-05   1.47646e-08   1.47646e-08   6.76410e-01
   */
  Double_t parsShift[10] = {    2.9183,  -0.087344,     0.1909,   0.021381,  -0.073944,  0.0014578,   0.013435, -0.0017357, -0.00080603, 0.00015234};
  Double_t parsSigma[10] = {   0.59488,   -0.13761,   -0.18675, -0.00066448,   0.051821, -0.0049375, -0.0074166,  0.0020477, 0.00037501, -0.00016473};
  TF1 *pol9 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("pol9");
  if (! pol9) return;
  Int_t nev = 10000000;
  for (Int_t ev = 1; ev <= nev; ev++) {
    Double_t xl = xl1 + (xl2 - xl1)*gRandom->Rndm();
    Double_t P = TMath::Exp(xl);
    Int_t nP = gRandom->Poisson(P);
    Double_t dE = 0;
    for (Int_t iP = 1; iP <= nP; iP++) {
      dE += mdNdE->GetRandom();
    }
    dE /= P;
    if (dE <= 0.0) continue;
    Double_t yL = TMath::Log(dE);
    znpL->Fill(xl, yL);
#if 0
    Double_t X = xl;
    if (X < 0.5) X = 0.5;
    if (X > 5.5) X = 5.5;
    Double_t shift = 2.93649e+00 + X*(7.33224e-03 + X*2.23307e-03);
    X = xl;
    Double_t scale =  2.97780e-01
      +      X *(    -1.24762e-01
      +      X *(     2.54248e-02
      +      X *(    -2.51517e-03
      +      X *      9.36223e-05)));
#else
    Double_t X = 0;
    if (xl > 0) X = TMath::Log(xl);
    Double_t shift = pol9->EvalPar(&X, parsShift);
    Double_t scale = pol9->EvalPar(&X, parsSigma);
#endif
    Double_t yS = (yL - shift)/scale;
    znpLS->Fill(xl, yS);
  }
  fOut->Write();
}

