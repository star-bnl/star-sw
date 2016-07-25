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
#include "TCanvas.h"
#endif
struct BPoint_t {
  Float_t N0, N, dE, Nt, n;
};
BPoint_t BPoint;
//________________________________________________________________________________
Double_t Ec(Double_t *x, Double_t *p) {
  /* Double_t ws = 30; // eV 
     Double_t Fs = 0.174;
     Double_t F  = Fs; 
  */
  if (x[0] < p[0]/2 || x[0] > 3.064*p[0]) return 0;
  if (x[0] < p[0]) return 1;
  return TMath::Power(p[0]/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc(Double_t w = 26.2) {; // eV
  TF1 *f = new TF1("Ec",Ec,0,3.064*w,1);
  f->SetParameter(0,w);
  return f;
  
}
//________________________________________________________________________________
Int_t Heed(Double_t dE, Double_t &dEr) {
  static TF1 *fec = 0;
  if (! fec) fec = fEc();
  dEr += dE;
  Int_t n0 = 0;
  Double_t Ec;
  while ((Ec = fec->GetRandom()) < dEr) { dEr -= Ec; n0++;}
  return n0;
}
//________________________________________________________________________________
void MakeNdENTuple(Int_t nev = 1e6) {
  static TH1D *mdNdE = 0;
  static Double_t W          = 26.2;// eV
  TDirectory *fsave = gDirectory;
  if (! mdNdE) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    mdNdE = (TH1D *)  _file0->Get("dNdE");
    gDirectory = fsave;
  }
  if (! gRandom) new TRandom();
  TString fName("NdENTuple.root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("FitP","TpcGas","N0:N:dE:Nt:n");
  for (Int_t ev = 0; ev < nev; ev++) {
    BPoint.N0 = 1 + 499*gRandom->Rndm();
    Int_t N = gRandom->Poisson(BPoint.N0);
    BPoint.N  = N;
    Double_t dE = 0;
    BPoint.dE = 0;
    BPoint.Nt = 0;
    Double_t dEr = 0;
    for (Int_t i = 0; i < N; i++) {
      do {
	dE = mdNdE->GetRandom();
      } while (dE < W/2);
      Int_t  n = Heed(dE, dEr);
      BPoint.dE += n*W;
      BPoint.n  += n;
    }
    FitP->Fill(&BPoint.N0);
  }
   f->Write();
}
//________________________________________________________________________________
Int_t Puiz(Double_t Kp) {
  /*  "Simulation Of The Measurement By Primary Cluster Counting Of The Energy Lost By A Rela Tivistic Ionizing	Particle In Argon",
      F. Lapique And F. Piuz Nuclear Instruments and Methods 175 (1980) 297-318 */
  static Double_t I0 = 13.1;// eV, CH4 
  Int_t n0 = 0;
  while (Kp > I0) {
    Double_t Rexc = 1; // sigma_exc/sigma_I Kp ~ 20-25 eV
    if (Kp < 50 ) Rexc = 0.4;
    if (gRandom->Rndm() < Rexc / (1 + Rexc)) {Kp -= I0; continue;}
    n0++;
    Double_t Kmax = (Kp - I0)/2;
    if (Kp <= 100) {Kp = Kmax*gRandom->Rndm();}
    else {
      Double_t rI = Kmax;///I0;
	Kp = Kmax/(rI + gRandom->Rndm()*(1 - rI));
    }
  }
  return n0;
}
//________________________________________________________________________________
Int_t Fischle(Double_t dE) {
  /* H.Fischle, J.Heintze, B.Schmidt, "Experemental determination of ionization cluster size distributions in counting gases",
     Mucl. Instr. and Meth. A301 (1991) 202-214
  */
  static Double_t I0 = 13.1;// eV, CH4 
  static Double_t W  = 26.2;// eV
  static Double_t p  = I0/W;
  Int_t n0 = (dE - I0)/I0;
  if (n0 <= 0) return 0;
  Int_t Nt = gRandom->Binomial(n0,p);
  return Nt;
}
//________________________________________________________________________________
Int_t TpcRSOld(Double_t dE) {
  static Double_t I0 = 13.1;// eV, CH4 
  static Double_t FanoFactor = 0.174;
  static Double_t W          = 26.2;// eV
  //  static Double_t cap        = 0.7; // e capture factor  
  //  static Double_t mCutEle    = 100e3; // 100 keV
  // old TpcRS
  Int_t n0 = TMath::Nint((dE - I0)/W/(1. - FanoFactor));
  Int_t Nt = gRandom->Binomial(n0, 1. - FanoFactor) + 1;
  return Nt;
}
//________________________________________________________________________________
void ClusterSize(Int_t nev = 1e6) {
  static Double_t I0 = 13.1;// eV, CH4 
  static TH1D *mdNdE = 0;
  if (! mdNdE) {
    TFile *_file0 = TFile::Open("$STAR/StarDb/dEdxModel/dNdE_Bichsel.root");
    mdNdE = (TH1D *)  _file0->Get("dNdEI");
  }
  TString fName("Cluster.root");
  TFile *f = new TFile(fName.Data(),"RECREATE");
  // Tables for fast electrons with beta = 0.8 - 0.97
  TH1D *cFischleCH4 = new TH1D("cFischleCH4","Fischle table for CH4", 15, 0.5, 15.5);
  cFischleCH4->SetMarkerColor(2);
  Double_t yFischleCH4[30] = {
    78.7, 1.20, 11.9, 0.35, 3.24, 0.16, 1.34, 0.09, 0.98, 0.09,
    0.55, 0.07, 0.57, 0.07, 0.27, 0.05, 0.29, 0.04, 0.20, 0.03,
    0.16, 0.03, 0.13, 0.03, 0.10, 0.02, 0.12, 0.02, 0.06, 0.02};
  for (Int_t i = 1; i <= 15; i++) {
    cFischleCH4->SetBinContent(i, yFischleCH4[2*(i-1)]);
    cFischleCH4->SetBinError(i, yFischleCH4[2*(i-1)+1]);
  }
  TH1D *cFischleAr = new TH1D("cFischleAr","Fischle table for Ar", 18, 0.5, 18.5);
  cFischleAr->SetMarkerColor(3);
  Double_t yFischleAr[36] = {
    65.6, 1.58, 14.8, 0.67, 6.49, 0.45, 3.37, 0.25, 2.44, 0.19,
    1.41, 0.14, 0.78, 0.10, 0.95, 0.11, 0.63, 0.09, 0.62, 0.10, 
    0.42, 0.08, 0.28, 0.06, 0.18, 0.05, 0.23, 0.07, 0.17, 0.05, 
    0.14, 0.05, 0.06, 0.03, 0.05, 0.02};
  for (Int_t i = 1; i <= 18; i++) {
    cFischleAr->SetBinContent(i, yFischleAr[2*(i-1)]);
    cFischleAr->SetBinError(i, yFischleAr[2*(i-1)+1]);
  }
  TH1D *cPiuz = new TH1D("cPiuz","Piuz table for Ar", 14, 0.5, 14.5);
  cPiuz->SetLineColor(1);
  Double_t yPiuz[14] = { 80.2, 7.7, 2.0, 1.3, 0.8, 0.6, 0.5, 0.6, 0.8, 0.9, 0.7, 0.5, 0.4, 0.3};
  for (Int_t i = 1; i <=14; i++) cPiuz->SetBinContent(i, yPiuz[i-1]);
  TH1D *clFischle = new TH1D("clFischle","no. electrons in cl Fischle",50,0.5,50.5); clFischle->SetLineColor(2);
  TH1D *clPuiz = new TH1D("clPuiz","no. electrons in cl Puiz",50,0.5,50.5);          clPuiz->SetLineColor(3);
  TH1D *clHeed = new TH1D("clHeed","no. electrons in cl Heed",50,0.5,50.5);          clHeed->SetLineColor(4);
  TH1D *clTpcRSOld = new TH1D("clTpcRSOld","no. electrons in cl TpcRSOld",50,0.5,50.5); clTpcRSOld->SetLineColor(6);
  for (Int_t ev = 0; ev < nev; ev++) {
    Double_t dEr = 0;
    Double_t dE  = mdNdE->GetRandom();
    if (dE < I0) continue;
    clFischle->Fill(1+Fischle(dE-I0));
    clPuiz->Fill(1+Puiz(dE-I0));
    clTpcRSOld->Fill(TpcRSOld(dE));
    clHeed->Fill(Heed(dE,dEr));
  }
  TH1D *hists[] = {cFischleAr,cFischleCH4,cPiuz,clFischle,clHeed,clTpcRSOld}; // clPuiz,
  Int_t NH = sizeof(hists)/sizeof(TH1D *);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->SetLogy(1);
  TH1F *frame = c1->DrawFrame(0.5,1e-1,25.5,1e2);
  frame->SetTitle("Cluster size");
  frame->SetXTitle("No. of electrons");
  frame->SetYTitle("Fraction (%)");
  TString same("same"); 
  TLegend *l = new TLegend(0.5,0.6,0.9,0.9);
  for (Int_t i = 0; i < NH; i++) {
    hists[i]->SetNormFactor(100);
    hists[i]->SetStats(0);
    hists[i]->SetLineWidth(4);
    hists[i]->Draw(same); same = "same";
    if (i >= 2) same = "samel";
    //    Double_t F = hists[i]->GetRMS()*hists[i]->GetRMS()/hists[i]->GetMean();
    //    l->AddEntry(hists[i],Form("%s #mu = %5.2f F = %4.2f",hists[i]->GetName(),hists[i]->GetMean(),F));
    l->AddEntry(hists[i],Form("%s #mu = %5.2f",hists[i]->GetName(),hists[i]->GetMean()));
  }
  l->Draw();
  f->Write();
}
