/*
  root.exe lBichsel.C bichselG10.C+
  bichselG10("N");  // dN/dx
  bichselG10("70"; // I70
  bichselG10("z"); // Ifit
  TH1D *pB70  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("70p1"))->GetHistogram();
  TH1D *piB70 = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("70#pi1"))->GetHistogram();
  TH1D *diffB70 = new TH1D(*pB70);
  diffB70->SetName("diffB70");
  diffB70->Add(pB70,piB70,1,-1);
  diffB70->SetLineColor(1);
  TH1D *pBz  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("zp1"))->GetHistogram();
  TH1D *piBz = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("z#pi1"))->GetHistogram();
  TH1D *diffBz = new TH1D(*pBz);
  diffBz->SetName("diffBz");
  diffBz->Add(pBz,piBz,1,-1);
  diffBz->SetLineColor(2);
  TH1D *pdNdx  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("dNdxp1"))->GetHistogram();
  TH1D *pidNdx = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("dNdx#pi1"))->GetHistogram();
  TH1D *diffdNdx = new TH1D(*pdNdx);
  diffdNdx->SetName("diffdNdx");
  diffdNdx->Add(pdNdx,pidNdx,1,-1);
  diffdNdx->SetLineColor(3);
  cppi = new TCanvas("cppi","cppi");
  diffB70->SetXTitle("log_{10}p")
  diffB70->SetTitle("z_{p} - z_{#pi}");
  diffB70->Draw("l");
  diffBz->Draw("samel");
  diffdNdx->Draw("samel");
  TLegend *l = new TLegend(0.6,0.6,0.8,0.8);
  l->AddEntry(diff70,"I70");
  l->AddEntry(diffBz,"Ifit");
  l->AddEntry(diffdNdx,"dNdx");

  TH1D *eB70  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("70e1"))->GetHistogram();
  TH1D *piB70 = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("70#pi1"))->GetHistogram();
  TH1D *diffB70 = new TH1D(*eB70);
  diffB70->SetName("diffB70");
  diffB70->Add(eB70,piB70,1,-1);
  diffB70->SetLineColor(1);
  TH1D *eBz  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("ze1"))->GetHistogram();
  TH1D *piBz = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("z#pi1"))->GetHistogram();
  TH1D *diffBz = new TH1D(*eBz);
  diffBz->SetName("diffBz");
  diffBz->Add(eBz,piBz,1,-1);
  diffBz->SetLineColor(2);
  TH1D *edNdx  = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("dNdxe1"))->GetHistogram();
  TH1D *pidNdx = (TH1D *) ((TF1 *) gROOT->GetListOfFunctions()->FindObject("dNdx#pi1"))->GetHistogram();
  TH1D *diffdNdx = new TH1D(*edNdx);
  diffdNdx->SetName("diffdNdx");
  diffdNdx->Add(edNdx,pidNdx,1,-1);
  diffdNdx->SetLineColor(3);
  cepi = new TCanvas("cepi","cepi");
  diffB70->SetXTitle("log_{10}p")
  diffB70->SetTitle("z_{e} - z_{#pi}");
  diffB70->Draw("l");
  diffBz->Draw("samel");
  diffdNdx->Draw("samel");
  TLegend *l = new TLegend(0.6,0.6,0.8,0.8);
  l->AddEntry(diff70,"I70");
  l->AddEntry(diffBz,"Ifit");
  l->AddEntry(diffdNdx,"dNdx");

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
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TF1.h"
#include "TMath.h"
#include "TSystem.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "TLegend.h"
#include "TROOT.h"
#else
class Bichsel;
#endif
Bichsel *m_Bichsel = 0;
const Int_t NMasses = 19;
const Double_t kAu2Gev=0.9314943228;
struct Part_t {
  const Char_t *Name;
  Int_t         PiD; 
  Int_t         Charge;
  Int_t         Index;
  Double_t      Mass;
};
Part_t Part[NMasses] = {
  //name,   PiD, Charge,  Index, Mass 
  {"p",       1,      1,      4, 0.93827231},                 // 0 p        
  {"K",	      2,      1,      3, 0.493677},          	      // 1 K   
  {"#pi",     3,      1,      2, 0.13956995},        	      // 2 pi  
  {"e",       0,      1,      0, 0.51099907e-3},     	      // 3 e   
  {"d",	      5,      1,      5, 1.87561339},        	      // 4 d   
  {"#mu",     4,      1,      1, 0.1056584},         	      // 5 mu  
  {"t",	      6,      1,      6, 2.80925},           	      // 6 t   
  {"He3",     7,      2,      7, 2.80923}, //GEANT3  	      // 7 He3 
  {"#alpha",  8,      2,      8, 3.727417}, //GEANT3 	      // 8 He4 
  {"He6",    -1,      2,     12, 3*kAu2Gev+14.931e-3},        // 9
  {"Li6",    10,      3,     10, 5.6031},            	      //10 Li6 
  {"Li7",     9,      3,      9, 6.5354},            	      //11 Li7 
  {"Be7",    -1,      4,      0, 7.016003437*kAu2Gev},        //12
  {"Be9",    -1,      4,      0, 9.01218307*kAu2Gev},        //13
  {"Be10",   -1,      4,      0, 10.01353470*kAu2Gev},        //14
  {"B10",    -1,      5,      0, 10.01353470*kAu2Gev},        //15
  {"B11",    -1,      5,      0, 11.009305167*kAu2Gev},       //16
  {"2#pi",    3,     -1,     -2, -0.13956995},       	      //17 2*pi
  {"2p",      1,     -1,      0, -0.93827231}        	      //18 2*p 
};
const Int_t NF = 10;  //          0,  1,     2,  3,   4,    5.  6,    7,       8,     9,
const Char_t *FNames[NF] = {"Girrf","Sirrf","z","70","60","70M","dNdx","zM","70Trs","zTrs"};
const Int_t Nlog2dx = 3;
const Double_t log2dx[Nlog2dx] = {0,1,2};
//________________________________________________________________________________
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) {
    charge = par[1];
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  return  TMath::Log10(scale*TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),dx2)));//TMath::Exp(7.81779499999999961e-01));
}
//________________________________________________________________________________
Double_t bichselZM(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) {
    charge = par[1];
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  return  TMath::Log10(scale*TMath::Exp(m_Bichsel->GetMostProbableZM(TMath::Log10(poverm),dx2)));//TMath::Exp(7.81779499999999961e-01));
  //return charge*charge*TMath::Log10(m_Bichsel->GetI70(TMath::Log10(poverm),1.));
}
//________________________________________________________________________________
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) {
    charge = par[1];
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  // return  TMath::Log10(scale*charge*charge*m_Bichsel->GetI70M(TMath::Log10(poverm),dx2));//TMath::Exp(7.81779499999999961e-01));
  return TMath::Log10(scale*m_Bichsel->GetI70(TMath::Log10(poverm),1.));
}
//________________________________________________________________________________
Double_t bichsel70M(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) {
    charge = par[1];
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  return  TMath::Log10(scale*m_Bichsel->GetI70M(TMath::Log10(poverm),dx2));//TMath::Exp(7.81779499999999961e-01));
}
#if 0
//________________________________________________________________________________
Double_t bichsel70Trs(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale  = 1;
  Double_t mass   = par[0];
  Int_t    charge = par[1];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t dx2 = 1;
  if (charge > 1) {
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  return TMath::Log10(scale*TMath::Exp(m_Bichsel->I70Trs(part,TMath::Log10(poverm))));
}
#endif
//________________________________________________________________________________
Double_t bichselZTrs(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  Int_t    part = par[2];
  if (part < 0 || part > 9) part = 0;
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) {
    charge = par[1];
    poverm *= charge;
    dx2 = TMath::Log2(5.);
  }
  scale *= charge*charge;
  return  TMath::Log10(scale*TMath::Exp(m_Bichsel->IfitTrs(part,TMath::Log10(poverm))));//TMath::Exp(7.81779499999999961e-01));
}
//________________________________________________________________________________
Double_t dNdx(Double_t *x,Double_t *par) {
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t scale = 1;
  Double_t mass = par[0];
  if (mass < 0) {mass = - mass; scale = 2;}
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  Double_t dx2 = 1;
  if (par[1] > 1.0) charge = par[1];
  poverm *= charge;
  //  scale *= charge*charge;
  return  TMath::Log10(scale*StdEdxModel::instance()->dNdx(poverm,charge));//TMath::Exp(7.81779499999999961e-01));
}
#if !defined(__CINT__) && !defined(__CLING__)
//________________________________________________________________________________
Double_t aleph70P(Double_t *x,Double_t *par) {
  /* 
     W.Blum, L. Rolandi "Particle Detection with Drift Chambers", page 246, eq. (9.5)
     F_g(v) = p[0]/beta**p[3]*(p[1] - beta**p[3] - log(p[2] + (beta*gamma)**-p[4]);
     F_g(v) = p[0]*(1/beta**p[3]*(p[1] - log(p[2] + 1/(beta*gamma)**p[4])) - 1) 
  */
  Double_t bg = x[0];
  Double_t b2inv = 1. + 1./(bg*bg);
  Double_t beta  = 1./TMath::Sqrt(b2inv);
  Double_t dEdx = par[0]*(-1 + TMath::Power(beta,-par[3])*(par[1] - TMath::Log(TMath::Max(1e-10,par[2] + TMath::Power(bg,-par[4])))));
  return dEdx;
};
//________________________________________________________________________________
Double_t aleph70(Double_t *x,Double_t *par) {
  static const Double_t dEdxMIP = 2.39761562607903311;
  static Double_t MIPBetaGamma = 4.;
#if 0
  struct Par_t {Int_t h, N; Double_t xmin, xmax, pars[10];};
  const Par_t Par[9] = {
    /* name          h n+1   xmin   xmax      pars[n+1] */
    /* electron */{  0,  4,   3.0,   6.0,{     0.14105,    -0.09078,     0.01901,    -0.00128,           0,           0,           0,           0,           0,           0}},
    /*     muon */{  1,  2,   0.0,   4.5,{    -0.00689,     0.00256,           0,           0,           0,           0,           0,           0,           0,           0}},
    /*     pion */{  2, -1,   0.0,   4.5,{     0.00000,           0,           0,           0,           0,           0,           0,           0,           0,           0}},
    /*     kaon */{  3,  9,  -0.1,   3.7,{     0.00869,     0.21918,    -0.88919,     1.30023,    -0.97075,     0.41298,    -0.10214,     0.01379,    -0.00079,           0}},
    /*   proton */{  4,  9,  -0.6,   3.3,{     0.03052,    -0.02423,    -0.05636,    -0.11585,     0.41292,    -0.38956,     0.16837,    -0.03494,     0.00283,           0}},
    /* deuteron */{  5,  8,  -1.0,   2.9,{     0.03523,    -0.10625,     0.04182,     0.07820,    -0.03816,    -0.02735,     0.01940,    -0.00304,           0,           0}},
    /*   triton */{  6, 10,  -1.0,   2.8,{     0.03092,    -0.07846,     0.01668,     0.00331,     0.12771,    -0.05480,    -0.13897,     0.13928,    -0.04715,     0.00555}},
    /*      He3 */{  7,  9,  -0.8,   2.9,{     0.09158,    -0.07816,     0.07039,     0.00578,    -0.16160,     0.26547,    -0.18728,     0.05988,    -0.00710,           0}},
    /*    alpha */{  8, 10,  -0.8,   2.9,{     0.09366,    -0.08276,     0.06191,     0.02631,    -0.17044,     0.30536,    -0.26867,     0.11847,    -0.02505,     0.00201}}
  };
  //  const Double_t ppar[7]     = { 0.0857988,   9.46138, 0.000206655,     2.12222,       0.974,    -1, 0.13957}; /* pion */
  static Double_t ppar[7]     = { 0.0857988,   9.46138, 0.000206655,     2.12222,       0.974,    -1, 0.13957}; /* pion */
#else
  //  static Double_t ppar[7]     = { 0.0762,  10.632, 0.134e-4, 1.863,  1.948,    -1, -1}; /* Aleph parameters from Alice TPC TDR */
  //  static Double_t ppar[7] = { 0.08942,     8.91971,     0.00024,     2.27383,     1.54174,    -1.00000, 0}; /* pion */
  static Double_t ppar[7]     = {0.12337,     6.61371,     0.00201,     2.27381,     1.54174,    -1.00000, 0 }; /* g All */
#endif
  static Double_t Norm = dEdxMIP/aleph70P(&MIPBetaGamma,ppar);
  Int_t hyp = (Int_t ) par[0];
  Int_t h = Part[hyp].Index;
  Double_t ScaleL10 = 0;
  if (h < 0) {
    h = -h;
    ScaleL10 = TMath::Log10(2.);
  }
  Double_t pove   = TMath::Power(10.,x[0]);
  Double_t mass = Part[hyp].Mass;
  Double_t poverm = pove/mass; 
  Double_t charge = 1.;
  if (h > 6 && h > 9) charge = 2;
  else if (h == 10 || h == 11)   charge = 3;
  poverm *= charge;
  Double_t bg = poverm;
  /* 
     W.Blum, L. Rolandi "Particle Detection with Drift Chambers", page 246, eq. (9.5)
     F_g(v) = p[0]/beta**p[3]*(p[1] - beta**p[3] - log(p[2] + (beta*gamma)**-p[4]));
     F_g(v) = p[0]*(1/beta**p[3]*(p[1] - log(p[2] + 1/(beta*gamma)**p[4])) - 1) 
  */
  Double_t dEdxL10 =  TMath::Log10(Norm*aleph70P(&bg,ppar));
#if 0
  if (Par[h].N > 0) {
    TString fName(Form("pol%i",Par[h].N-1));
    TF1 *f = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
    if (! f) {
      f = new TF1(fName,fName,0,1);
    }
    f->SetParameters(&Par[h].pars[0]);
    Double_t bgL10 = TMath::Log10(bg);
    dEdxL10 += f->Eval(bgL10);
  }
#endif
  return 2*TMath::Log10(charge) + dEdxL10 + ScaleL10;
}
#endif /* __CINT__ */
//________________________________________________________________________________
void bichselG10(const Char_t *type="z", Int_t Nhyps = 9) {
  if (gClassTable->GetID("StBichsel") < 0 || !m_Bichsel) {
    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
  TString Type(type);
  TLegend *leg = new TLegend(0.65,0.45,0.75,0.9,"");
  Double_t xmax = 4;
  Int_t f = 3;
  for (Int_t i = NF-1; i >=0; i--) {
    if (Type.Contains(FNames[i],TString::kIgnoreCase)) {
      f = i;
      break;
    }
  }
  if (Nhyps < 9) Nhyps = 9;
  if (Nhyps > NMasses)  Nhyps = NMasses;
  for (int h = 0; h < Nhyps; h++) { // Masses
  //  for (int h = 0; h < 7; h++) { // Masses
    Int_t dx = 1;
    Char_t *FunName = Form("%s%s%i",FNames[f],Part[h].Name,(int)log2dx[dx]);
    cout << "Make " << h << "\t" << FunName << endl;
    Double_t xmin = -1.5;
    //    if (h == 0 || h >= 5) xmin = -0.75;
    if (h == 4) xmin = -0.70;
    if (h == 6) xmin = -0.50;
    TF1 *func = 0;
    if      (f == 3) func = new TF1(FunName,bichsel70,xmin, xmax,3);
    else if (f == 2) func = new TF1(FunName,bichselZ ,xmin, xmax,3);
    else if (f == 5) func = new TF1(FunName,bichsel70M ,xmin, xmax,3);
    else if (f == 6) func = new TF1(FunName,dNdx ,xmin, xmax,3);
    else if (f == 7) func = new TF1(FunName,bichselZM,xmin, xmax,3);
#if 0
    else if (f == 8) func = new TF1(FunName,bichsel70Trs,xmin, xmax,3);
    else if (f == 9) func = new TF1(FunName,bichselZTrs,xmin, xmax,3);
#endif
    else {
      return;
    }
    func->SetParameter(0,Part[h].Mass);
    func->SetParameter(1,Part[h].Charge);
    Int_t color = h+1;
    if (color > 8) color -= 8;
    //    if (color > 7) color++;
#if 1
    func->SetLineColor(color);
    func->SetMarkerColor(color);
#endif
    func->Draw("same");
    leg->AddEntry(func,Part[h].Name);
#if !defined( __CINT__) && defined(__Aleph__)
    TF1 *fA = new TF1(Form("Aleph%s",Part[h].Name),aleph70,xmin,xmax, 1);
    fA->SetParameter(0,h);
    fA->SetLineColor(color);
    fA->SetMarkerColor(color);
    fA->SetLineStyle(2);
    fA->Draw("same");
    leg->AddEntry(fA,Part[h].Name);
#endif
  }
  leg->Draw();
}
