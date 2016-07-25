#include "TMinuit.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMath.h"
#include "TLegend.h"
#include "Names.h"
TCanvas *c1 = 0;
struct dEdxPoint_t {
  Char_t  *name;
  Int_t    type;
  Double_t p, dP, a, da, mu, dmu, sigma, dsigma;
};
#include "Tof.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
TF1 *gFunc = 0;
TGraphErrors* gGraph =0;
Double_t params[6] = {
  /*   1  Offset          */   1.72376e-01,
  /*   2  beta2Inv1       */  -4.30665e-01,
  /*   3  beta2Inv2       */   1.29081e-01,
  /*   4  log(beta*gamma)1*/  -9.94174e-02,
  /*   5  log(beta*gamma)2*/   1.70633e-02,
  /*   6  log(beta*gamma)3*/  -1.03019e-03,
};
//______________________________________________________________________________
Double_t func(Double_t *x,Double_t *par)
{
  Double_t xx[2];
  //  xx[0] = pow(10.,par[3]+par[4]*x[0]);
  Double_t poverm = pow(10.,x[0]);
  Double_t beta2Inv  = 1. + 1./(poverm*poverm);
  Double_t value = par[0] + TMath::Log(beta2Inv)*(par[1]+par[2]*TMath::Log(beta2Inv)) + 
    TMath::Log(poverm)*(par[3] + TMath::Log(poverm)*(par[4] + TMath::Log(poverm)*par[5]));
  return value;
}
//______________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag){
  Int_t i;
  
  //calculate chisquare
  Double_t chisq = 0;
  Double_t delta;
  Int_t bad = 0; 
  Double_t worse = 0., badval = 0;
  for (i=0;i<N; i++) {
    Double_t xx[3];
    xx[0]= TMath::Log10(dEdxZ[i].p/Masses[dEdxZ[i].type]);
    Double_t val =  func(xx,par);   
    //    Double_t poverm = pow(10.,xx);
    //    Double_t val = par[0]+TMath::Log(1.e-3*Girrf(poverm,dEdxZ[i].type));
    if (dEdxZ[i].dmu <= 0) continue;
    delta  = (dEdxZ[i].mu-val)/dEdxZ[i].dmu;
    if (iflag == 3) {
      printf ("%i %s p = %f mu = %f +/- %f function  %f delta = %f\n",i,
	      dEdxZ[i].name,dEdxZ[i].p,dEdxZ[i].mu,dEdxZ[i].dmu,val,delta);
      if (dEdxZ[i].dmu > 0.03 || dEdxZ[i].dmu < 0.0005) 
	printf("=======================================================\n");
      if (TMath::Abs(worse) < TMath::Abs(delta)) {
	worse = delta; bad = i; badval = val;
      }
    }
    chisq += delta*delta;
  }
  if (iflag == 3) {
    printf("The worst bin = %i\n",bad);
    i = bad; delta = worse; val = badval;
    printf ("%i %s p = %f mu = %f +/- %f function  %f delta = %f\n",i,
	    dEdxZ[i].name,dEdxZ[i].p,dEdxZ[i].mu,dEdxZ[i].dmu,val,delta);
  }
  f = chisq;
}
//______________________________________________________________________________
void tofFit()
{
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
      d = dEdxZ[i].mu;
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
  c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  TLegend *leg = new TLegend(0.91,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
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
