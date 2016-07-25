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
  Int_t    bin;
  Int_t    NPoint;
  Double_t x;
  Double_t p;
  Double_t z; 
  Double_t y;
  Double_t dy;
  Double_t chisq;
};
Double_t ParsFit[6] = {0., 1., 0., 0., 0., 0.};
#if 0
  /*   1  Offset          */   1.72376e-01,
  /*   2  beta2Inv1       */  -4.30665e-01,
  /*   3  beta2Inv2       */   1.29081e-01,
  /*   4  log(beta*gamma)1*/  -9.94174e-02,
  /*   5  log(beta*gamma)2*/   1.70633e-02,
  /*   6  log(beta*gamma)3*/  -1.03019e-03,
#endif
    //#include "FitParszDD.h"
    //#include "FitPars70DD.h"
    //#include "FitParsF70M.h"
#include "FitPars70B541.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
const Double_t XM = 2.0;
//______________________________________________________________________________
Double_t funA(Double_t *x,Double_t *par)
{
  return par[0]*(1. - TMath::Exp(-par[1]*x[0]));
}
//______________________________________________________________________________
Double_t func(Double_t *x,Double_t *par)
{
#if 0
  Double_t xx[4];
  //  xx[0] = pow(10.,par[3]+par[4]*x[0]);
  Double_t poverm = pow(10.,x[0]);
  Double_t beta2Inv  = 1. + 1./(poverm*poverm);
  Double_t value = par[0] + TMath::Log(beta2Inv)*(par[1]+par[2]*TMath::Log(beta2Inv)) + 
    TMath::Log(poverm)*(par[3] + TMath::Log(poverm)*(par[4] + TMath::Log(poverm)*par[5]));
#endif
  Double_t value = par[0];
  if (x[3] > XM) {
    Double_t dX = x[3] - XM;
    value += dX*(par[1] + dX*(par[2]+dX*par[3]));
  }
  //  Double_t value = par[0]*(1. - TMath
  return value;
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag){
  Int_t i;
  
  //calculate chisquare
  Double_t chisq = 0;
  Double_t delta;
  Int_t bad = 0; 
  Double_t worse = 0., badval = 0;
  Double_t xx[4];
  for (i=0;i<N; i++) {
    if (dEdxZ[i].dy <= 0) continue;
    xx[0] = dEdxZ[i].x;
    xx[1] = dEdxZ[i].type;
    xx[2] = dEdxZ[i].bin;
    xx[3] = TMath::Log(dEdxZ[i].y);
    //    if (dEdxZ[i].dy > 0.02) continue;
    //    if (dEdxZ[i].type == 2 || dEdxZ[i].type == 6) continue;
    //    Double_t val =  func(xx,par);   
    //    Double_t poverm = pow(10.,xx);
    Double_t val = func(xx,par);
    delta  = (dEdxZ[i].z-val)/dEdxZ[i].dy;
    if (iflag == 3) {
      printf ("%i %s bin %i point %i x = %f y = %f +/- %f function  %f delta = %f\n",i,
	      dEdxZ[i].name,dEdxZ[i].bin, dEdxZ[i].NPoint, dEdxZ[i].x,dEdxZ[i].z,dEdxZ[i].dy,val,delta);
      if (dEdxZ[i].dy > 0.03 || dEdxZ[i].dy < 0.0005) 
	printf("=======================================================\n");
      if (TMath::Abs(worse) < TMath::Abs(delta)) {
	worse = delta; bad = i; badval = val;
      }
    }
    chisq += delta*delta;
  }
  if (iflag == 3) {
    printf("The worst bin = %i ==> %i\n",bad,  dEdxZ[bad].bin);
    i = bad; delta = worse;
    printf ("%i %s bin %i point %i x = %f y = %f +/- %f function  %f delta = %f\n",i,
	    dEdxZ[i].name, dEdxZ[i].bin, dEdxZ[i].NPoint, dEdxZ[i].x,dEdxZ[i].z,dEdxZ[i].dy,badval,delta);
  }
  f = chisq;
}
//______________________________________________________________________________
void IFit()
{
  Double_t Z[20],dZ[20];
  for (int i = 0; i < N; i++) {
    if (dEdxZ[i].dy > 0.03 || dEdxZ[i].dy < 0.0005) dEdxZ[i].dy = 0;
    if (dEdxZ[i].type > NHYPS) dEdxZ[i].dy = 0;
    if (dEdxZ[i].x < -99.0) dEdxZ[i].dy = 0;
    if (dEdxZ[i].chisq > 1.e4) dEdxZ[i].dy = 0;
    //       if (TMath::Abs(dEdxZ[i].z) > 0.09) dEdxZ[i].dy = 0;
    if (dEdxZ[i].dy > 0.025 || dEdxZ[i].dy < 0.0001) dEdxZ[i].dy = 0;
#if 0
    if (dEdxZ[i].type%6 != 1 && dEdxZ[i].type%6 != 5) dEdxZ[i].dy = 0;
    if (TMath::Log(dEdxZ[i].y) < XM) dEdxZ[i].dy = 0;
#endif
  }
#if 1
  TMinuit *gMinuit = new TMinuit(5);  //initialize TMinuit with a maximum of 5 params
  gMinuit->SetFCN(fcn);
  
  Double_t arglist[10];
  Int_t ierflg = 0;
  //   f1->SetParNames("constant","coefficient");
  arglist[0] = 1;
  gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
  //  gMinuit->DefineParameter(0,"A0", 2.73117e-02, 0.01, 0.0, 0.0);// gMinuit->FixParameter(0);
  gMinuit->DefineParameter(0,"A0", 0.00000e+00, 0.01, 0.0, 0.0); gMinuit->FixParameter(0);
  gMinuit->DefineParameter(1,"A1", 0.00000e+00, 0.01, 0.0, 0.0); gMinuit->FixParameter(1);
  gMinuit->DefineParameter(2,"A2", 0.00000e+00, 0.01, 0.0, 0.0); gMinuit->FixParameter(2);
  gMinuit->DefineParameter(3,"A3", 0.00000e+00, 0.01, 0.0, 0.0); gMinuit->FixParameter(3);
//   gMinuit->DefineParameter(0,"A0", 0.00000e+00, 0.01, 0.0, 0.0); gMinuit->FixParameter(0);
//   gMinuit->DefineParameter(1,"A1",-4.48058e-02, 0.01, 0.0, 0.0); gMinuit->FixParameter(1);
//   gMinuit->DefineParameter(2,"A2", 5.19474e-02, 0.01, 0.0, 0.0); gMinuit->FixParameter(2);
//   gMinuit->DefineParameter(3,"A3",-2.74181e-02, 0.01, 0.0, 0.0); gMinuit->FixParameter(3);
//   gMinuit->DefineParameter(3,"log(beta*gamma)1", ParsFit[3], 0.01, 0.0, 0.0); gMinuit->FixParameter(3);
//   gMinuit->DefineParameter(4,"log(beta*gamma)2", ParsFit[4], 0.01, 0.0, 0.0); gMinuit->FixParameter(4);
//   gMinuit->DefineParameter(5,"log(beta*gamma)3", ParsFit[5], 0.01, 0.0, 0.0); gMinuit->FixParameter(5);
  gMinuit->Migrad();
  gMinuit->mnexcm("IMPROVE", arglist ,0,ierflg);
  gMinuit->mnexcm("HESSE", arglist ,0,ierflg);
  gMinuit->mnexcm("end", arglist ,0,ierflg);
  // Print results
  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat;
  gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  gMinuit->mnprin(3,amin);
  for (int k = 0; k < 20; k++)  gMinuit->GetParameter(k, Z[k], dZ[k]);
#endif
  if (!c1) c1 = new TCanvas();//,"gerrors2",2,10,700,500);
  c1->SetFillColor(42);
  c1->SetGrid();
  TGraphErrors *gr[13];
  Double_t xmin = 99, xmax = -99, ymin = 99, ymax = -99;
  const Int_t m = 0;
  for (int k = 0; k < NHYPS; k++) {
    gr[k] = new TGraphErrors();
    Int_t nk = 0;
    for (int i = 0; i < N; i++) {
      if (dEdxZ[i].dy  <= 0.0) continue;
      if (k < NHYPS && dEdxZ[i].type != k) continue;
#if 0
     printf ("%s :hyp = %i bin=%i, Point=%i, x=%f, p=%f, Delta_I=%f, I=%f, Sigma_I=%f,\n"
	     "chisq=%f\n",
	     dEdxZ[i].name,dEdxZ[i].type,dEdxZ[i].bin,dEdxZ[i].NPoint,dEdxZ[i].x,dEdxZ[i].p,
	     dEdxZ[i].z,dEdxZ[i].y,dEdxZ[i].dy,dEdxZ[i].chisq);
#endif
      Double_t xx[4];
      xx[0] = dEdxZ[i].x;
      xx[1] = dEdxZ[i].type;
      xx[2] = dEdxZ[i].bin;
      xx[3] = TMath::Log(dEdxZ[i].y);
      
      Double_t d, ff;
#if 1
      ff  = func(xx,Z);
      d   = dEdxZ[i].z - ff;
      //      xx[0] = ff;
#else
      d = dEdxZ[i].z;
#endif
      if (xx[m] < xmin) xmin = xx[m];
      if (xx[m] > xmax) xmax = xx[m];
      if (d     < ymin) ymin = d;
      if (d     > ymax) ymax = d;
      gr[k]->SetPoint(nk,xx[m],d);
      gr[k]->SetPointError(nk,0.,dEdxZ[i].dy);
      nk++;
    }
    //    printf("k: %i nk: %i\n",k,nk);
  }
  printf("x|y min|max %f %f %f %f\n",xmin,ymin,xmax,ymax);
  TH1F *frame = c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  frame->SetYTitle("Z");
  frame->SetXTitle("log10(#beta#gamma)                  ");
  TLegend *leg = new TLegend(0.91,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  for (int k = 0; k < NHYPS; k++) {
    if (gr[k]->GetN()>0) {
      Int_t c = k/(NHYPS/2) + 1;
      Int_t s = k%(NHYPS/2) + 20;
      gr[k]->SetMarkerColor(c);
      gr[k]->SetMarkerStyle(s);
      gr[k]->Draw("P");
      leg->AddEntry(gr[k],Names[k],"p");
    }
  }
  leg->Draw();
}
