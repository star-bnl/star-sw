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
  Double_t z; // correction wrt Manuel
  Double_t y;
  Double_t dy;
  Double_t chisq;
};
//#define NOELECTRONS
//#include "fitPars0.h"
//#include "fitPars2.h"
//#include "FitPars3.h"
//#include "FitParsG.h"
//#include "FitParsGG.h"
//#include "FitParsGGG.h"
//#include "FitParsG5.h"
#include "FitPars122.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
//________________________________________________________________________________
Double_t Cirrf(Double_t poverm, Int_t k, Double_t *par) {
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t gamma  = TMath::Sqrt(poverm*poverm + 1);
  Double_t Lbeta2inv = TMath::Log(beta2inv);
  //  Double_t K      = 0.307075e+3;// keV/g cm^2 
  Double_t K      = 153.537;// keV/g cm^2 
  Double_t A      = 38.691;
  Double_t Z      = 17.436;
  Double_t rho    = 1.5607e-03;//  0.9*0.00166+0.1*0.000667
  Double_t I      = 16e-6*pow(Z,0.9); //par[1]; //15.8e-6; //15.8; // MeV for Ar, 13.1e-6 MeV for CH4
  Double_t m      =   0.510998902;// MeV electron Mass
  Double_t pim    = 139.570180;// MeV pion Mass
  Double_t Delta  = 0;
  Double_t M = pim;
  if (k) {M = m;   Delta = par[6];}
  else   {M = pim; Delta = par[2];}
  Double_t r = m/M;
  Double_t Tmax = 2*m*poverm*poverm/(1. + r*(2*gamma + r));
  Double_t Tupper = Tmax;
  Double_t si =  K*Z/A*rho*beta2inv*(TMath::Log(2*m*poverm*poverm*Tupper/(I*I)) 
			  - (1 + Tupper/Tmax)/beta2inv - Delta);
  if (si <= 0) si = 1.e-12;
  Double_t xx    = TMath::Log(poverm);
  Double_t value = TMath::Log(si) + par[7]*Lbeta2inv + par[0] + xx*(par[3] + xx*(par[4] + xx*par[5]));
  return TMath::Exp(value);
}
//________________________________________________________________________________
Double_t Firrf(Double_t Poverm, Int_t k, Double_t *par) {
  Double_t poverm = Poverm;
  Double_t Beta2inv = 1. + 1./(Poverm*Poverm);
  if (poverm <   0.15) poverm =   0.15;
  Double_t pSatur = par[7];
  //  if (k)   pSatur = par[8];
  if (poverm > pSatur) poverm = pSatur;
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t gamma  = TMath::Sqrt(poverm*poverm + 1);
  Double_t Lpoverm = TMath::Log(poverm);
  Double_t K      = 0.307075e+3;// keV/g cm^2 
  Double_t A      = 38.691;
  Double_t Z      = 17.436;
  Double_t rho    = 1.5607e-03;//  0.9*0.00166+0.1*0.000667
  Double_t I      = par[1]; //15.8e-6; //15.8; // MeV for Ar, 13.1e-6 MeV for CH4
  Double_t m      =   0.510998902;// MeV electron Mass
  Double_t pim    = 139.570180;// MeV pion Mass
  Double_t Delta;
  Double_t M = pim;
  if (k) {M = m;   Delta = par[6];}
  else   {M = pim; Delta = par[2];}
  Double_t r = m/M;
  Double_t Tmax = 2*m*poverm*poverm/(1. + r*(2*gamma + r));
  Double_t Tupper = Tmax;
  Double_t si = K*Z/A*rho/2*Beta2inv*
    (TMath::Log(2*m*poverm*poverm*Tupper/(I*I)) 
     - (1 + Tupper/Tmax)/beta2inv - Delta);
  if (si <= 0) si = 1.e-12;
  Double_t value = par[0] + TMath::Log(si) + 
    Lpoverm*(par[3] + Lpoverm*(par[4] + Lpoverm*par[5]));
  //  value += par[0];// +  par[7]*Lbeta2inv + xx*(par[3] + xx*(par[4] + xx*par[5]));
  return TMath::Exp(value);
}
//______________________________________________________________________________
Double_t func(Double_t *x,Double_t *par)
{
  Double_t poverm = pow(10.,x[0]);
  Double_t p = poverm;
  Int_t k = (int) x[1];
  //  if (poverm < 1.0) p = 1.;
  //  Double_t C  = 1.e-6*Sirrf(p,k);
  //  Double_t C  = 1.e-6*Birrf(p,k);
  //  Double_t C  = BetheBlochFunction(x);
  //  Double_t value= TMath::Log(C);// + Lpoverm*par[1];
  //  Double_t value = TMath::Log(C) + par[0] + par[1]*Lbeta2inv + x[0]*(par[2] + x[0]*par[3]);
  Int_t l = 0;
  if (k == 2 || k == 6) l = 1;
  //  Double_t C = 1.e-6*Dirrf(p,l,par);
  //  Double_t C = 1.e-6*Cirrf(p,l,par);
  Double_t C = 1.e-6*Firrf(p,l,par);
  Double_t value = TMath::Log(C);
  return value;
}
//______________________________________________________________________________
Double_t funcG(Double_t *x,Double_t *par) {
  Double_t xx[2];
  xx[0] = TMath::Log10(x[0]);
  xx[1] = par[1];
  Double_t pars[7] = {
    7.18738629245735616e-01,//2.48735e-01,
    1.58587e-05,
    1.15874e+01,
   -3.30560e-01,
    8.62137e-02,
   -7.21336e-03,
    1.06436e+01};
  return 1.e6*TMath::Exp(func(xx,pars));
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag){
  Int_t i;
  
  //calculate chisquare
  Double_t chisq = 0;
  Double_t delta;
  Int_t bad = 0; 
  Double_t worse = 0., badval = 0;
  for (i=0;i<N; i++) {
    Double_t xx[3];
    xx[0]= dEdxZ[i].x;
    //    if (xx[0] < 0) continue;
    //    Double_t val =  func(&xx,par);   
    xx[1] = dEdxZ[i].type;
    xx[2] = dEdxZ[i].bin;
    //    if (dEdxZ[i].dy > 0.02) continue;
    //    if (dEdxZ[i].type == 2 || dEdxZ[i].type == 6) continue;
    Double_t val =  func(xx,par);   
    //    Double_t poverm = pow(10.,xx);
    if (dEdxZ[i].dy <= 0) continue;
    delta  = (dEdxZ[i].y-val)/dEdxZ[i].dy;
    if (iflag == 3) {
      printf ("%i %s bin %i point %i x = %f y = %f +/- %f function  %f delta = %f\n",i,
	      dEdxZ[i].name,dEdxZ[i].bin, dEdxZ[i].NPoint, dEdxZ[i].x,dEdxZ[i].y,dEdxZ[i].dy,val,delta);
      if (dEdxZ[i].dy > 0.03 || dEdxZ[i].dy < 0.0005) 
	printf("=======================================================\n");
      if (TMath::Abs(worse) < TMath::Abs(delta)) {
	worse = delta; bad = i; badval = val;
      }
    }
    chisq += delta*delta;
  }
  if (iflag == 3) {
    printf("The worst bin = %i\n",bad);
    i = bad; delta = worse;
    printf ("%i %s bin %i point %i x = %f y = %f +/- %f function  %f delta = %f\n",i,
	    dEdxZ[i].name, dEdxZ[i].bin, dEdxZ[i].NPoint, dEdxZ[i].x,dEdxZ[i].y,dEdxZ[i].dy,badval,delta);
  }
  f = chisq;
}
//______________________________________________________________________________
void bFit()
{
  TMinuit *gMinuit = new TMinuit(5);  //initialize TMinuit with a maximum of 5 params
  gMinuit->SetFCN(fcn);
  
  Double_t arglist[10];
  Int_t ierflg = 0;
  //   f1->SetParNames("constant","coefficient");
  arglist[0] = 1;

  gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
#if 0
  gMinuit->DefineParameter(0, "A0"   , 3.10332e-04,  0.01, 0, 0);
  gMinuit->DefineParameter(1, "max"  , 4.81033e+02,   0.1,   400, 10000);  //gMinuit->FixParameter(1);
#endif
      gMinuit->DefineParameter(0, "Scale  ", 7.53599e-01,  0.01, 0, 0);
      gMinuit->DefineParameter(1, "I      ", 1.28683e-05,  0.01, 0, 0);
      gMinuit->DefineParameter(2, "Delta  ", 1.24876e+01,  0.01, 0, 0);
      gMinuit->DefineParameter(3, "a0     ",-3.43540e-01,  0.01, 0, 0);
      gMinuit->DefineParameter(4, "a1     ", 9.86380e-02,  0.01, 0, 0);
      gMinuit->DefineParameter(5, "a2     ",-1.14049e-02,  0.01, 0, 0);
      gMinuit->DefineParameter(6, "Delta_e", 2.94151e+00,  0.01, 0, 0);
      gMinuit->DefineParameter(7, "Satur  ", 1.70952e+02,  0.01, 1.e1, 1.e4);
#if 1
  gMinuit->Migrad();
  gMinuit->mnexcm("IMPROVE", arglist ,0,ierflg);
  gMinuit->mnexcm("HESSE", arglist ,0,ierflg);
  gMinuit->mnexcm("end", arglist ,0,ierflg);
  // Print results
  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat;
  gMinuit->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  gMinuit->mnprin(3,amin);
#endif
  Double_t Z[10],dZ[10];
  if (!c1) c1 = new TCanvas();//,"gerrors2",2,10,700,500);
  c1->SetFillColor(42);
  c1->SetGrid();
  for (int k = 0; k < 10; k++)  gMinuit->GetParameter(k, Z[k], dZ[k]);
  TGraphErrors *gr[10];
  Double_t xmin = 99, xmax = -99, ymin = 99, ymax = -99;
  for (int k = 0; k < 8; k++) {
    gr[k] = new TGraphErrors();
    Int_t nk = 0;
#if 0
    if (k == 2 || k == 6) continue;
#endif
    for (int i = 0; i < N; i++) {
      if (k < 8 && dEdxZ[i].type != k) continue;
      Double_t xx[3];
      xx[0] = dEdxZ[i].x;
      xx[1] = dEdxZ[i].type;
      xx[2] = dEdxZ[i].bin;
      Double_t d = dEdxZ[i].y - func(xx,Z);
      //      Double_t d = dEdxZ[i].y;
      if (k == 9) d = dEdxZ[i].y;
      if (xx[0] < xmin) xmin = xx[0];
      if (xx[0] > xmax) xmax = xx[0];
      if (d     < ymin) ymin = d;
      if (d     > ymax) ymax = d;
      gr[k]->SetPoint(nk,dEdxZ[i].x,d);
      gr[k]->SetPointError(nk,0.,dEdxZ[i].dy);
      nk++;
    }
    //    printf("k: %i nk: %i\n",k,nk);
  }
  printf("x|y min|max %f %f %f %f\n",xmin,ymin,xmax,ymax);
  c1->DrawFrame(xmin-0.01,ymin-0.01,xmax+0.01,ymax+0.01);
  TLegend *leg = new TLegend(0.91,0.11,1.00,0.89,"");//TLegend(0.79,0.91,0.89,0.89,"");
  for (int k = 0; k < 8; k++) {
    if (gr[k]->GetN()>0) {
      Int_t c = k/4 + 1;
      Int_t s = k%4 + 20;
      gr[k]->SetMarkerColor(c);
      gr[k]->SetMarkerStyle(s);
      gr[k]->Draw("P");
      leg->AddEntry(gr[k],Names[k],"p");
    }
  }
  leg->Draw();
}
