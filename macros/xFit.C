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
Double_t ParsFit[8] = {
   7.25410e-01, //     1  Scale       
   1.56806e-06, //     2  Tmin        
   4.61027e-07, //     3  TminE       
  -2.78686e-07, //     4  A0          
   5.04261e-02, //     5  B1          
  -2.43081e+00, //     6  D0          
  -9.12143e+00, //     7  D1          
  -1.14640e+00  //     8  D2          
};
//#define  NOELECTRONS
//#include "fitPars0.h"
//#include "fitPars2.h"
//#include "FitPars3.h"
//#include "FitParsG.h"
//#include "FitParsGGG.h"
//#include "FitParsG5.h"
//#include "FitPars122.h"
#include "FitParsP01hi.h"
const Int_t N = sizeof(dEdxZ)/sizeof(dEdxPoint_t);
//________________________________________________________________________________
Double_t Birrf(Double_t poverm) {
  //        returns value of relative ionisation: Hans Bichsel calculations
//The average value of z = log(dE/dx) versus log10(beta*gamma) at dx = 2 cm
  const Int_t nBinx = 101;
#if 0
//The most probable value of log(dE/dx) versus log10(beta*gamma) at dx = 2 cm
//In range of log10(beta*gamma) = [-1,4]
Double_t si[101] = {
        4.53952,        4.36209,        4.18111,        3.99294,        3.79164,
        3.57338,        3.36683,        3.15596,        2.93695,        2.71249,
        2.50330,        2.29898,        2.09695,        1.91529,        1.71602,
        1.52743,        1.38583,        1.23329,        1.08886,        0.961815,
        0.843879,       0.742792,       0.654583,       0.581276,       0.51954,
        0.475454,       0.436706,       0.408739,       0.39054,        0.37649,
        0.370481,       0.366125,       0.367475,       0.372359,       0.379226,
        0.387315,       0.397873,       0.408977,       0.421418,       0.432486,
        0.447259,       0.451908,       0.473695,       0.486612,       0.501272,
        0.513068,       0.527721,       0.540714,       0.553262,       0.566609,
        0.579301,       0.590719,       0.604378,       0.614556,       0.624666,
        0.635655,       0.647084,       0.655394,       0.664834,       0.673681,
        0.681946,       0.691743,       0.698873,       0.706288,       0.714941,
        0.720239,       0.721973,       0.735738,       0.741889,       0.740983,
        0.753035,       0.75872,        0.764949,       0.770584,       0.773772,
        0.777204,       0.780829,       0.784371,       0.786573,       0.789431,
        0.792118,       0.793406,       0.79488,        0.796393,       0.790962,
        0.798942,       0.799345,       0.800826,       0.800826,       0.800982,
        0.800982,       0.804149,       0.804149,       0.804149,       0.804149,
        0.804149,       0.801674,       0.801674,       0.801674,       0.801674,
        0.801674
};
#else
//The average value of z = log(dE/dx) versus log10(beta*gamma) at dx = 2 cm
//In range of log10(beta*gamma) = [-1,4]
Double_t si[nBinx] = {
        4.54252,        4.36748,        4.19103,        4.01074,        3.82794,
        3.64267,        3.45571,        3.26570,        3.07361,        2.87984,
        2.68587,        2.49227,        2.29768,        2.10321,        1.91531,
        1.73586,        1.56616,        1.40787,        1.26102,        1.12796,
        1.00848,        0.903208,       0.812495,       0.73573,        0.671757,
        0.620363,       0.579561,       0.549155,       0.526437,       0.511341,
        0.501788,       0.497332,       0.497112,       0.499846,       0.505342,
        0.512879,       0.522002,       0.532542,       0.54378,        0.55567,
        0.568259,       0.581033,       0.593998,       0.607315,       0.620386,
        0.633697,       0.646537,       0.659649,       0.672083,       0.684759,
        0.69691,        0.709057,       0.720425,       0.731635,       0.742106,
        0.752268,       0.76188,        0.771054,       0.780194,       0.788644,
        0.797061,       0.804904,       0.812778,       0.820195,       0.827449,
        0.834594,       0.841339,       0.847812,       0.854269,       0.86029,
        0.865989,       0.871727,       0.876819,       0.881675,       0.886284,
        0.890448,       0.894312,       0.897913,       0.900784,       0.903875,
        0.906241,       0.908895,       0.910329,       0.912917,       0.9141,
        0.916187,       0.917466,       0.918929,       0.918929,       0.920702,
        0.920702,       0.923222,       0.923222,       0.923222,       0.923222,
        0.923222,       0.932872,       0.932872,       0.932872,       0.932872,
        0.932872
};
#endif
 Double_t X =  20*(TMath::Log10(poverm) + 1.);
 Int_t iX = X;
 if (iX < 0) iX = 0;
 if (iX > nBinx - 2) iX = nBinx - 2;
 Double_t dX = X - iX;
 Double_t siD = si[iX] + dX*(si[iX+1] - si[iX]);
 Double_t sirrf = TMath::Exp(siD);
 return sirrf;
}
//______________________________________________________________________________
Double_t funG(Double_t *x,Double_t *par)
{
  Double_t poverm = x[0];
  Int_t k = (int) x[1];
  Int_t l = 0;
  if (k == 2 || k == 6) l = 1;
  Double_t Tmin  = par[l+1];
  Double_t dEL   = TMath::Log(1.e-6*Birrf(poverm));
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t bL    = TMath::Log(beta2inv);
  dEL           += bL*par[4] + par[5]*TMath::Exp(par[6]*(TMath::Log10(poverm)-par[7]));
  Double_t gL    = TMath::Log10(poverm);
  dEL           += gL*(par[8] + gL*(par[9] + gL*par[10]));
  Double_t dE    = TMath::Exp(dEL);
  Double_t Loss  = par[3];
  Double_t value = par[0] + TMath::Log(Loss+dE);
#if 0
  printf("poverm: %f l:%i Tmin:%f dEL: %f value: %f\n",poverm,l,Tmin,dEL,value);
#endif
  return value;
}
//______________________________________________________________________________
Double_t funGH(Double_t *xx,Double_t *par)
{
  Double_t x[2];
  x[0] = xx[0];
  x[1] = 0;
  return 1.e6*TMath::Exp(funG(x,ParsFit));
}
//______________________________________________________________________________
Double_t funGE(Double_t *xx,Double_t *par)
{
  Double_t x[2];
  x[0] = xx[0];
  x[1] = 2;
  return 1.e6*TMath::Exp(funG(x,ParsFit));
}
//______________________________________________________________________________
Double_t func(Double_t *x,Double_t *par)
{
  Double_t xx[2];
  //  xx[0] = pow(10.,par[3]+par[4]*x[0]);
  Double_t poverm = pow(10.,x[0]);
  xx[0] = poverm;
  xx[1] = x[1];
  Double_t value = funG(xx,par);
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
    xx[0]= dEdxZ[i].x;
    //    if (xx[0] < 0) continue;
    //    Double_t val =  func(&xx,par);   
    xx[1] = dEdxZ[i].type;
    xx[2] = dEdxZ[i].bin;
    //    if (dEdxZ[i].dy > 0.02) continue;
    //    if (dEdxZ[i].type == 2 || dEdxZ[i].type == 6) continue;
    Double_t val =  func(xx,par);   
    //    Double_t poverm = pow(10.,xx);
    //    Double_t val = par[0]+TMath::Log(1.e-3*Girrf(poverm,dEdxZ[i].type));
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
void xFit()
{
  TMinuit *gMinuit = new TMinuit(5);  //initialize TMinuit with a maximum of 5 params
  gMinuit->SetFCN(fcn);
  
  Double_t arglist[10];
  Int_t ierflg = 0;
  //   f1->SetParNames("constant","coefficient");
  arglist[0] = 1;
  gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
  gMinuit->DefineParameter( 0, "Scale"  , 7.25410e-01,  0.01,     0, 0.0);        
  gMinuit->DefineParameter( 1, "Tmin "  , 1.56806e-06,  0.00, 1.e-8, 1.e-3); 
  gMinuit->DefineParameter( 2, "TminE"  , 4.61027e-07,  0.00, 1.e-8, 1.e-2); 
  gMinuit->DefineParameter( 3, "A0   "  ,-2.78686e-07,  0.01,     0, 0.0);  
  gMinuit->DefineParameter( 4, "B1   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 5, "D0   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 6, "D1   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 7, "D2   "  , 0.00000e+00,  0.00,     0, 0.0);  
  gMinuit->DefineParameter( 8, "g1   "  , 0.00000e+00,  0.01,     0, 0.0);  
  gMinuit->DefineParameter( 9, "g2   "  , 0.00000e+00,  0.01,     0, 0.0);  
  gMinuit->DefineParameter(10, "g3   "  , 0.00000e+00,  0.00,     0, 0.0);  
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
  Double_t Z[20],dZ[20];
  if (!c1) c1 = new TCanvas();//,"gerrors2",2,10,700,500);
  c1->SetFillColor(42);
  c1->SetGrid();
  for (int k = 0; k < 20; k++)  gMinuit->GetParameter(k, Z[k], dZ[k]);
  TGraphErrors *gr[10];
  Double_t xmin = 99, xmax = -99, ymin = 99, ymax = -99;
  for (int k = 0; k < 8; k++) {
    gr[k] = new TGraphErrors();
    Int_t nk = 0;
    //    if (k == 2 || k == 6) continue;
    for (int i = 0; i < N; i++) {
      if (k < 8 && dEdxZ[i].type != k) continue;
      Double_t xx[3];
      xx[0] = dEdxZ[i].x;
      xx[1] = dEdxZ[i].type;
      xx[2] = dEdxZ[i].bin;
      Double_t d, ff;
#if 1
      ff  = func(xx,Z);
      d = dEdxZ[i].y - ff;
      //      xx[0] = ff;
#else
      d = dEdxZ[i].y;
#endif
      if (xx[0] < xmin) xmin = xx[0];
      if (xx[0] > xmax) xmax = xx[0];
      if (d     < ymin) ymin = d;
      if (d     > ymax) ymax = d;
      gr[k]->SetPoint(nk,xx[0],d);
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
