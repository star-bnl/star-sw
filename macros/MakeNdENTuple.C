#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TNtuple.h"
#include "TRandom.h"
#include "TF1.h"
#include "TH1.h"
#include "TString.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TLegend.h"
#endif
struct BPoint_t {
  Float_t N0, N, dE, Nt, n;
};
BPoint_t BPoint;
//________________________________________________________________________________
Double_t Ec(Double_t *x, Double_t */* p */) {
  /* Double_t ws = 30; // eV 
     Double_t Fs = 0.174;
     Double_t F  = Fs; 
  */
  Double_t w  =  26.2; // eV
  if (x[0] < w/2 || x[0] > 3.064*w) return 0;
  if (x[0] < w) return 1;
  return TMath::Power(w/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc() {
  return new TF1("Ec",Ec,0,100,0);
}
//________________________________________________________________________________
Int_t HeedCsize(TH1D *mdNdE) {
  static TF1 *fec = 0;
  if (! fec) fec = fEc();
  Double_t dEr = 0;
  Double_t dE = mdNdE->GetRandom();
  dEr += dE;
  Int_t n0 = 1;
  Double_t Ec;
  while ((Ec = fec->GetRandom()) < dEr) { dEr -= Ec; n0++;}
  return n0;
}
//________________________________________________________________________________
void MakeNdENTuple(Int_t nev = 1e6) {
  static TH1D *mdNdE = 0;
  static Double_t I0 = 13.1;// eV, CH4 
  static Double_t FanoFactor = 0.3;
  static Double_t W          = 26.2;// eV
  static Double_t cap        = 0.7; // e capture factor  
  TDirectory *fsave = gDirectory;
  if (! mdNdE) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    mdNdE = (TH1D *)  _file0->Get("dNdE");
    gDirectory = fsave;
  }
  if (! gRandom) new TRandom();
  TString fName("NdENTuple2.root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("FitP","TpcGas","N0:N:dE:Nt:n");
  for (Int_t ev = 0; ev < nev; ev++) {
    BPoint.N0 = 1 + 1000*gRandom->Rndm();
    Int_t N = gRandom->Poisson(BPoint.N0);
    BPoint.N  = N;
    Double_t dE = 0;
    BPoint.dE = 0;
    BPoint.Nt = 0;
    for (Int_t i = 0; i < N; i++) {
#if 0
      Int_t n = 0;
      while (((dE = mdNdE->GetRandom()) < I0)) n++;
      Int_t n0 = TMath::Nint((dE - I0)/W/(1. - FanoFactor));
      Int_t Nt = gRandom->Binomial(n0, 1. - FanoFactor) + 1;
      n = 0;
      for (Int_t i = 0; i < Nt; i++) {if (gRandom->Rndm() < cap) n++;}
      if (n <= 0) continue;
      BPoint.Nt += Nt;
#else
     Int_t  n = HeedCsize(mdNdE);
#endif
      BPoint.dE += n*W;
      BPoint.n  += n;
    }
    FitP->Fill(&BPoint.N0);
  }
   f->Write();
}
//________________________________________________________________________________
void ClusterSize(Int_t nev = 1e6) {
  static TH1D *mdNdE = 0;
  static Double_t I0 = 13.1;// eV, CH4 
  //  static Double_t FanoFactor = 0.3;
  //  static Double_t FanoFactor = 0.0;
  static Double_t FanoFactor = 0.174;
  static Double_t W          = 26.2;// eV
  static Double_t cap        = 0.7; // e capture factor  
  static Double_t mCutEle    = 100e3; // 100 keV
  if (! mdNdE) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    mdNdE = (TH1D *)  _file0->Get("dNdEI");
  }
  if (! gRandom) new TRandom();
  TString fName("ClusterSize.root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TH1D *cPiuz = new TH1D("cPiuz","Piuz table for Ar", 14, 0.5, 14.5);
  cPiuz->SetLineColor(2);
  Double_t yPiuz[14] = { 80.2, 7.7, 2.0, 1.3, 0.8, 0.6, 0.5, 0.6, 0.8, 0.9, 0.7, 0.5, 0.4, 0.3};
  for (Int_t i = 1; i <=14; i++) cPiuz->SetBinContent(i, yPiuz[i-1]);
  TH1D *clusterH = new TH1D("clusterH","Simulated cluster size Heed algorithm",100,-0.5,99.5);
  TH1D *clusterO = new TH1D("clusterO","Simulated cluster size Old TpcRS",100,-0.5,99.5);
  clusterO->SetLineColor(3);
  TH1D *clusterE = new TH1D("clusterE","Simulated cluster size Old TpcRS + inefficiency",100,-0.5,99.5);
  clusterE->SetLineColor(6);
  TH1D *clusterP = new TH1D("clusterP","Simulated cluster size Puiz algorithm",100,-0.5,99.5);
  clusterP->SetLineColor(4);
  TF1 *fec = fEc();
  Double_t dEr = 0;
  for (Int_t ev = 0; ev < nev; ev++) {
    BPoint.dE = 0;
    BPoint.Nt = 0;
    Int_t n = 0;
    Double_t dE;
    //    while (((dE = mdNdE->GetRandom()) < I0) && dE > mCutEle) n++;
    dE = mdNdE->GetRandom();
    // Heed
    dEr += dE;
    Int_t n0 = 1;
    Double_t Ec;
    while ((Ec = fec->GetRandom()) < dEr) { dEr -= Ec; n0++;}
    clusterH->Fill(n0);
    // old TpcRS
    n0 = TMath::Nint((dE - I0)/W/(1. - FanoFactor));
    Int_t Nt = gRandom->Binomial(n0, 1. - FanoFactor) + 1;
    clusterO->Fill(Nt);
    // + inefficiency
    n = 1;
    for (Int_t i = 0; i < Nt-1; i++) {if (gRandom->Rndm() < cap) n++;}
    clusterE->Fill(n);
    // Puiz
    Double_t Kp = dE;
    n0 = 0;
    while (Kp > I0) {
      Double_t Rexc = 1; // sigma_exc/sigma_I Kp ~ 20-25 eV
      if (Kp < 50 ) Rexc = 0.4;
      if (gRandom->Rndm() < Rexc / (1 + Rexc)) {Kp -= 13; continue;}
      n0++;
      Double_t Kmax = (Kp - I0)/2;
      if (Kp <= 100) {Kp = Kmax*gRandom->Rndm();}
      else {
	Double_t rI = Kmax;///I0;
	Kp = Kmax/(rI + gRandom->Rndm()*(1 - rI));
      }
    }
    clusterP->Fill(n0);
  }
  f->Write();
  TH1D *hists[] = {cPiuz, clusterH, clusterO, clusterE, clusterP};
  Int_t nh = sizeof(hists)/sizeof(TH1D *);
  TString same;
  TLegend *l = new TLegend(0.5,0.6,0.8,0.9);
  for (Int_t i = 0; i < nh; i++) {
    hists[i]->SetNormFactor(100);
    hists[i]->SetStats(0);
    hists[i]->Draw(same); same = "same";
    l->AddEntry(hists[i],Form("%s <n> = %4f.2",hists[i]->GetTitle(),hists[i]->GetMean()));
  }
  l->Draw();
}
//________________________________________________________________________________
void Fischle() {
  /* H.Fischle, J.Heintze, B.Schmidt, "Experemental determination of ionization cluster size distributions in counting gases",
     Mucl. Instr. and Meth. A301 (1991) 202-214
  */
  static Double_t I0 = 13.1;// eV, CH4 
  static Double_t W          = 26.2;// eV
  static Double_t FanoFactor = 0.3;
  static TH1D *mdNdE = 0;
  if (! mdNdE) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    mdNdE = (TH1D *)  _file0->Get("dNdEI");
  }
  TString fName("Fischle.root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TH1D *cFishleCH4 = new TH1D("cFishleCH4","Fishle table for CH4", 15, 0.5, 15.5);
  cFishleCH4->SetMarkerColor(2);
  Double_t yFishleCH4[30] = {
    78.7, 1.20, 11.9, 0.35, 3.24, 0.16, 1.34, 0.09, 0.98, 0.09,
    0.55, 0.07, 0.57, 0.07, 0.27, 0.05, 0.29, 0.04, 0.20, 0.03,
    0.16, 0.03, 0.13, 0.03, 0.10, 0.02, 0.12, 0.02, 0.06, 0.02};
  for (Int_t i = 1; i <= 15; i++) {
    cFishleCH4->SetBinContent(i, yFishleCH4[2*(i-1)]);
    cFishleCH4->SetBinError(i, yFishleCH4[2*(i-1)+1]);
  }
  TH1D *cFishleAr = new TH1D("cFishleAr","Fishle table for Ar", 18, 0.5, 18.5);
  cFishleAr->SetMarkerColor(3);
  Double_t yFishleAr[36] = {
    65.6, 1.58, 14.8, 0.67, 6.49, 0.45, 3.37, 0.25, 2.44, 0.19,
    1.41, 0.14, 0.78, 0.10, 0.95, 0.11, 0.63, 0.09, 0.62, 0.10, 
    0.42, 0.08, 0.28, 0.06, 0.18, 0.05, 0.23, 0.07, 0.17, 0.05, 
    0.14, 0.05, 0.06, 0.03, 0.05, 0.02};
  for (Int_t i = 1; i <= 18; i++) {
    cFishleAr->SetBinContent(i, yFishleAr[2*(i-1)]);
    cFishleAr->SetBinError(i, yFishleAr[2*(i-1)+1]);
  }
  TH1D *cluster = new TH1D("cluster","no. electrons in cluster",50,-.5,49.5);
  Double_t p = I0/W;
  for (Int_t ev = 0; ev < 1e6; ev++) {
    Int_t n = 0;
    Double_t dE;
    //    while (((dE = mdNdE->GetRandom()) < I0) && dE > mCutEle) n++;
    dE = mdNdE->GetRandom();
    Int_t n0 = (dE - I0)/I0;
    if (n0 <= 0) continue;
    Int_t Nt = gRandom->Binomial(n0,p);
    cluster->Fill(Nt);
  }
  f->Write();
}
