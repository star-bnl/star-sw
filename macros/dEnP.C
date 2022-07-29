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
TH1D *mdNdEL10 = 0;
TH2F *znpL  = 0;
TH2F *znpLS = 0;
TH2F *zenpL  = 0;
TH2F *zenpLS = 0;
TH1F *dN = 0;
TH1F *ne  = 0;
TFile *fOut = 0;
//________________________________________________________________________________
Double_t Ec(Double_t *x, Double_t *p) {
  if (x[0] < p[0]/2 || x[0] > 3.064*p[0]) return 0;
  if (x[0] < p[0]) return 1;
  return TMath::Power(p[0]/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc(Double_t w) {
  TF1 *f = new TF1("HeedEc",Ec,0,3.064*w,1);
  f->SetParameter(0,w);
  return f;
}
//________________________________________________________________________________
void dEnP(Double_t tMax = 1e6, Int_t nev = 10e6) {
  new TRandom3;
  TF1::InitStandardFunctions();
  if (! mdNdEL10) {
    const Char_t *path  = ".:./StarDb/dEdxModel:$STAR/StarDb/dEdxModel";
    const Char_t *Files[1] = {"dNdE_Bichsel.root"};
    Int_t i = 0;
    Char_t *file = gSystem->Which(path,Files[i],kReadPermission);
    if (! file) Fatal("dEnP","File %s has not been found in path %s",Files[i],path);
    else        Warning("dEnP","File %s has been found as %s",Files[i],file);
    TFile       *pFile = new TFile(file);
    mdNdEL10 = (TH1D *)         pFile->Get("dNdEL10");     if (mdNdEL10)    mdNdEL10->SetDirectory(0);
    if (! mdNdEL10) return;
    delete pFile;
    delete [] file;
  }
  Int_t nx =   100;
  Double_t xl1 =  0.4;
  Double_t xl2 = 10.4;
  if (! znpL) {
    //    if (! fOut) fOut = new TFile(Form("znptMax_%5.0e_%5.0e.root", tMax, (Double_t ) nev),"recreate");
    if (! fOut) fOut = new TFile(Form("znptMax_%5.0e.root", tMax),"recreate");
    znpL = new TH2F("znpL","z = log(dE/nP) versus log(nP)",  nx,xl1,xl2,2000, 0.0,10.);
    zenpL = new TH2F("zenpL","z = log(ne/nP) versus log(nP)",nx,xl1,xl2,2000,-2.0, 8.);
    dN = new TH1F("dN","Nc/Np",100,0.0, 2.0);
    ne = new TH1F("ne","dNp/dNe",100,-0.5, 95.5);
  }
  Double_t W = 26.2;// eV 
  TF1 *mHeed = fEc(W);
  static Double_t cLog10 = TMath::Log(10.);
  for (Int_t ev = 1; ev <= nev; ev++) {
    Double_t xl = xl1 + (xl2 - xl1)*gRandom->Rndm();
    Double_t P = TMath::Exp(xl);
    Int_t nP = gRandom->Poisson(P);
    Double_t dE = 0;
    Double_t ddE = 0;
    Double_t dEr = 0;
    Double_t Nt = 0; // HeedCsize(dE, dEr,rs);
    Double_t nPc = 0;
    for (Int_t iP = 1; iP <= nP; iP++) {
      ddE = TMath::Exp(cLog10*mdNdEL10->GetRandom());
#ifdef __Cut3__
      if (ddE < W/2) continue;
      if (ddE > tMax) continue;
#else /* __Cut5__ Cut6*/
      while ((ddE < W/2 || ddE > tMax)) {
	ddE = TMath::Exp(cLog10*mdNdEL10->GetRandom());
      }
#endif
      Double_t dET = ddE + dEr;
      dEr = dET;
      Double_t EC;
      Double_t nec = 0;
      while ((EC = mHeed->GetRandom()) < dEr) { 
	dEr -= EC; 
	Nt++;
	nec++;
	dE += EC; 
      }
      if (nec) nPc++;
      ne->Fill(nec);
    }
    if (Nt <= 0.0) continue;
    dN->Fill(nPc/P);
    dE /= P;
    if (dE <= 0.0) continue;
    Double_t yL = TMath::Log(dE);
    znpL->Fill(xl, yL);
    Double_t nL = TMath::Log(Nt/P);
    zenpL->Fill(xl, nL);
    if (nev > 100 && (ev % (nev/100)) == 0) {cout << ev << endl;}
  }
  fOut->Write();
}

