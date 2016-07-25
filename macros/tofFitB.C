#include "TMinuit.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMath.h"
#include "TLegend.h"
#include "Names.h"
TCanvas *c1 = 0;
class Bichsel;
Bichsel *m_Bichsel = 0;
struct dEdxPoint_t {
  Char_t  *name;
  Int_t    type;
  Double_t p, dP, a, da, mu, dmu, sigma, dsigma;
};
#include "Tof.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
TF1 *gFunc = 0, *bFunc = 0;
TGraphErrors* gGraph =0;
Double_t params[6] = {
  /*   1  Offset          */   1.72376e-01,
  /*   2  beta2Inv1       */  -4.30665e-01,
  /*   3  beta2Inv2       */   1.29081e-01,
  /*   4  log(beta*gamma)1*/  -9.94174e-02,
  /*   5  log(beta*gamma)2*/   1.70633e-02,
  /*   6  log(beta*gamma)3*/  -1.03019e-03,
};
//--------------------------------------------------------------------------------
Double_t bichsel70(Double_t *x, Double_t *par) {
  return m_Bichsel->GetI70(x[0],1.);
}
//______________________________________________________________________________
Double_t func(Double_t *x,Double_t *par)
{
  Double_t poverm = pow(10.,x[0]);
  Double_t beta2Inv  = 1. + 1./(poverm*poverm);
  Double_t value = par[0] + TMath::Log(beta2Inv)*(par[1]+par[2]*TMath::Log(beta2Inv)) + 
    TMath::Log(poverm)*(par[3] + TMath::Log(poverm)*(par[4] + TMath::Log(poverm)*par[5]));
  return TMath::Exp(value)*bichsel70(x,par);
}
//______________________________________________________________________________
void tofFitB()
{
#if 1
  if (!m_Bichsel || gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libTable");
    //    gSystem->Load("St_base");
    //    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    m_Bichsel = Bichsel::Instance();
  }
#endif
  if (!c1) c1 = new TCanvas();//,"gerrors2",2,10,700,500);
  c1->SetFillColor(42);
  c1->SetGrid();
  TGraphErrors *gr[10];
  //  TGraphAsymmErrors *gr[10];
  Double_t xmin = 99, xmax = -99, ymin = 99, ymax = -99;
  if (gGraph) delete gGraph;
  gGraph = new TGraphErrors();
  Int_t nkt = 0;
  gFunc = new TF1("gFunc",func,-1.,4.,6);
  gFunc->SetParameters(params);
  gFunc->SetParName(0,"Offset");
  gFunc->SetParName(1,"beta2Inv1");
  gFunc->SetParName(2,"beta2Inv2");
  gFunc->SetParName(3,"log(beta*gamma)1");
  gFunc->SetParName(4,"log(beta*gamma)2");
  gFunc->SetParName(5,"log(beta*gamma)3");
  gFunc->SetLineColor(2);
  bFunc = new TF1("bFunc",bichsel70,-1.,4.,1);
  bFunc->SetLineColor(1);
  for (int k = 0; k < 4; k++) {
    gr[k] = new TGraphErrors();
    //    gr[k] = new TGraphAsymmErrors();
    Int_t nk = 0;
    //    if (k == 2 || k == 6) continue;
    for (int i = 0; i < N; i++) {
      if (k < 8 && dEdxZ[i].type != k) continue;
      Double_t xx[3];
      Double_t poverm = dEdxZ[i].p/Masses[dEdxZ[i].type];
      xx[0] = TMath::Log10(poverm);
      Double_t xl    = TMath::Log10((dEdxZ[i].p-dEdxZ[i].dP)/Masses[dEdxZ[i].type]);
      Double_t dx    = xx[0] - xl;
      Double_t d, ff;
      d = TMath::Exp(dEdxZ[i].mu)*bichsel70(xx,0);
      if (xx[0] < xmin) xmin = xx[0] - dx;
      if (xx[0] > xmax) xmax = xx[0] + dx;
      if (d     < ymin) ymin = d;
      if (d     > ymax) ymax = d;
      gr[k]->SetPoint(nk,xx[0],d);
      gr[k]->SetPointError(nk,dx,dEdxZ[i].dmu);
      gGraph->SetPoint(nkt,xx[0],d);
      gGraph->SetPointError(nkt,dx,dEdxZ[i].dmu);
      nk++; nkt++;
    }
    //    printf("k: %i nk: %i\n",k,nk);
  }
  printf("x|y min|max %f %f %f %f\n",xmin,ymin,xmax,ymax);
  ymin = 0.01;
  //  TH1F *fr = c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  TH1F *fr = c1->DrawFrame(-1.,0., 4.,20.);
  fr->SetXTitle("log_{10} (#beta#gamma)                  ");
  fr->SetYTitle("dE/dx (keV/cm)");
  TLegend *leg = new TLegend(0.81,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  bFunc->Draw("same");  leg->AddEntry(bFunc,"Original","l");
  gFunc->Draw("same");  leg->AddEntry(gFunc,"Corrected by ToF","l");
  for (int k = 0; k < 4; k++) {
    if (gr[k]->GetN()>0) {
      Int_t c = k + 1;
      Int_t s = k + 20;
      gr[k]->SetMarkerColor(c);
      gr[k]->SetMarkerStyle(s);
      gr[k]->Draw("P");
      leg->AddEntry(gr[k],Names[k],"p");
    }
  }
  leg->Draw();
}
