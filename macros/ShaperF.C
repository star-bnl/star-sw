#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TString.h"
#include "TDirectory.h"
#include "TAxis.h"
#include "Riostream.h"
#include "StarRoot/TTreeIter.h"
#include "StarRoot/TDirIter.h"
//#define __SLOPE401__
//________________________________________________________________________________
Double_t ShaperF(Double_t *x, Double_t *par=0 ) {  
  /*
    .L ShaperF.C+
    TF1 *t = new TF1("t",ShaperF,-10,50,6);
    t->SetParameters(1.14045e+00, 4.06636e+00,-1.58630e+00, 4.89662e+01,1,10e-6);
    t->FixParameter(3,55);
    t->FixParameter(1,3);
    t->SetParName(0,"Yeald");
    t->SetParName(1,"Pole");
    t->SetParName(2,"Shift");
    t->SetParName(3,"tau");
    t->SetParName(4,"A_{undershoot}");
    t->SetParName(5,"T_{undershoot}");
   




    1  p0           1.14045e+00   2.41325e-04  -2.41598e-04   2.41600e-04
    2  p1           4.06636e+00   9.23017e-03  -9.42199e-03   9.46044e-03
    3  p2          -1.58630e+00   1.89253e-03  -1.93739e-03   1.93182e-03
    4  p3           4.89662e+01   6.87019e-02  -7.01949e-02   7.02014e-02

 1.14045e+00, 4.06636e+00,-1.58630e+00, 4.89662e+01,0.05,10
TF1 *t = new TF1("t",ShaperF,-10,50,6)
t->SetParameters(1.14045e+00, 4.06636e+00,-1.58630e+00, 4.89662e+01,0.05,01)
root.exe [34] profI->Fit(t,"er","",-10,50)
 FCN=717205 FROM MINOS     STATUS=SUCCESSFUL    118 CALLS         208 TOTAL
                     EDM=3.03598e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                  PARABOLIC         MINOS ERRORS        
  NO.   NAME      VALUE            ERROR      NEGATIVE      POSITIVE   
   1  Yeald        1.12255e+00   1.54740e-04  -1.54734e-04   1.54748e-04
   2  Pole         3.00000e+00     fixed    
   3  Shift       -1.32288e+00   2.19568e-04  -2.19562e-04   2.19576e-04
   4  tau          5.50000e+01     fixed    
   5  undershoot   1.27231e-02   2.32913e-05  -2.32706e-05   2.32976e-05
   6  decay time   1.36405e-02   1.39511e-04  -1.39435e-04   1.39500e-04
root.exe [37] profO->Fit(t,"er","",-10,50)
 FCN=5.44013e+06 FROM MINOS     STATUS=SUCCESSFUL    122 CALLS         222 TOTAL
                     EDM=2.72985e-08    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                  PARABOLIC         MINOS ERRORS        
  NO.   NAME      VALUE            ERROR      NEGATIVE      POSITIVE   
   1  Yeald        1.11449e+00   7.82399e-05  -7.82504e-05   7.82558e-05
   2  Pole         3.00000e+00     fixed    
   3  Shift       -1.26887e+00   9.81152e-05  -9.81159e-05   9.81142e-05
   4  tau          5.50000e+01     fixed    
   5  undershoot   2.14018e-02   1.00070e-05  -1.00115e-05   1.00173e-05
   6  decay time   4.72636e-02   4.15881e-05  -4.16132e-05   4.16281e-05
root.exe [40] profA->Fit(t,"er","",-10,50)
 FCN=1.40491e+06 FROM MINOS     STATUS=SUCCESSFUL    121 CALLS         215 TOTAL
                     EDM=3.12392e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                  PARABOLIC         MINOS ERRORS        
  NO.   NAME      VALUE            ERROR      NEGATIVE      POSITIVE   
   1  Yeald        1.11832e+00   1.89316e-04  -1.89362e-04   1.89376e-04
   2  Pole         3.00000e+00     fixed    
   3  Shift       -1.46866e+00   2.73463e-04  -2.73466e-04   2.73489e-04
   4  tau          5.50000e+01     fixed    
   5  undershoot   2.17823e-02   3.06951e-05  -3.07119e-05   3.07494e-05
   6  decay time   4.65246e-02   1.17312e-04  -1.17419e-04   1.17484e-04

    
   */
  static Double_t Frequency = 9215890.;//  run5341024
  //  static Double_t width     = 1./Frequency;
  Double_t tau       = par[3]*1e-9; // 55e-9;
  Double_t widthOvertau = 1./(Frequency*tau);
  Double_t p     = par[1];
  Double_t t     = (x[0]-par[2])*widthOvertau;
  Double_t Delta = widthOvertau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = par[0]*(TMath::Gamma(p,t2) - TMath::Gamma(p,t1));
  if (t > 0 && par[5]) val = par[0]*par[4]*par[5]/tau*TMath::Gamma(p,t)*(TMath::Exp(-t1*tau/par[5])-TMath::Exp(-t2*tau/par[5]));
  //  cout << "Called" << endl;
  return val;
  //  return TMath::Power(tau,-p+1)*val/3.;
}
//________________________________________________________________________________
Double_t ShaperT(Double_t *x, Double_t *par=0 ) {  
  //  t->SetParameters(1,3,-1)
  Double_t tau   = 1.;
  Double_t width = 1.;
  Double_t p     = par[1];
  Double_t t0    = par[2];
  Double_t t     = (x[0]-t0)*width/tau;
  Double_t Delta = width/tau;
  Double_t t1 = t - Delta/2.;
  Double_t t2 = t1 + Delta;
  Double_t val = par[0]*((TMath::Gamma(p,t2) - TMath::Gamma(p,t1)) + par[3]*TMath::Exp(-par[4]*t));
  //  cout << "Called" << endl;
  return val;
  //  return TMath::Power(tau,-p+1)*val/3.;
}
//________________________________________________________________________________
Double_t GammaD(Double_t *x, Double_t *par=0 ) {  
  Double_t val = 0;
  Double_t t0 = par[1];
  if (x[0] <= t0) return val;
  Double_t alpha = par[2];
  Double_t beta  = par[3];
  if (alpha <= 0 || beta <= 0) return val;
  Double_t t = beta*(x[0]- t0);
  val = par[0]*beta*TMath::Power(t,alpha-1)*TMath::Exp(-t)/TMath::Gamma(alpha);
  return val;
}
//________________________________________________________________________________
Double_t TPX_Pulser(Double_t *x, Double_t *par=0 ) {  
  Double_t Value = 0;
  Double_t t = x[0] - par[0];
  if (t <= 0) return Value;
#if 0
  if (par[2] <= 0) return Value;
  Double_t t1 = t - 0.5;
  if (t1 < 0) t1 = 0.;
  Double_t t2 = t + 0.5;
  if (t2 < 0) t2 = 0.;
  if (t2 <= 0) return Value;
  Value = par[1]*(1 - par[2]/(t2-t1)*(TMath::Exp(-t1/par[2]) - TMath::Exp(-t2/par[2])));
  if (par[3] > 0 && par[4] > 0) Value += par[3]*par[4]*(TMath::Exp(-t1/par[4]) - TMath::Exp(-t2/par[4]))/(t2-t1);
  if (par[5] > 0 && par[6] > 0) Value += par[5]*par[6]*(TMath::Exp(-t1/par[6]) - TMath::Exp(-t2/par[6]))/(t2-t1);
#else
  // 1/(s-a)/(s-b);
  Double_t ABF = TMath::Exp(par[1]);
  Double_t a = -1./par[2];
  Double_t b = -1./par[3];
  Double_t AB = 1./(a-b);
  if (TMath::Abs(AB) > 1e-5) {
    Value = ABF*AB*(TMath::Exp(a*t) - TMath::Exp(b*t));
  } else {
    Value = ABF*t*TMath::Exp(a*t);
  }
  if (par[4] > -90) {
    Double_t ACF = ABF*TMath::Exp(par[4]);
    Double_t c = -1./par[5];
    Double_t AC = 1./(a-c);
    Value += ACF*AC*(TMath::Exp(a*t) - TMath::Exp(c*t));
  }
  if (par[6] > -90) {
    Double_t ADF = ABF*TMath::Exp(par[6]);
    Double_t d = -1./par[7];
    Double_t AD = 1./(a-d);
    Value += ADF*AD*(TMath::Exp(a*t) - TMath::Exp(d*t));
  }
#endif
  
  return Value;
}
//________________________________________________________________________________
TF1* TPX_Function() {
  Int_t n = 8;
  TF1 *f = new TF1("shape",TPX_Pulser,-5,20,n);
  f->SetParName(0,"t_0");
  f->SetParName(1,"LA");
  f->SetParName(2,"#tau_I");
  f->SetParName(3,"#tau_a");
  f->SetParName(4,"LB");
  f->SetParName(5,"#tau_b");
  f->SetParName(6,"LC");
  f->SetParName(7,"#tau_c");
  f->SetParameters(-2.70185e+00, 5.61749e+00, 1.3, 13., -4.45761e+00, 10., -3.68807e+00, 20);
  return f;
}
//________________________________________________________________________________ 
Double_t ex2(Double_t *x, Double_t *p=0 ) {  
  Double_t Value = p[8]+x[0]*p[9];
  Double_t t = x[0] - p[0];
  Double_t t1 = t - 0.5;
  Double_t t2 = t1 + 1;
  if (t1 < 0) t1 = 0;
  if (t2 <= t1) return Value;
  Double_t tauI = 0;
  if (p[1] > 0) tauI = p[1];
  Double_t AL   = p[2];
  if (p[3] <= 0) return Value;
  Double_t tauA = p[3];
  Double_t BL = p[4];
  Double_t tauB = 0;
  if (p[5] > 0) tauB = p[5];
  Double_t CL = p[6];
  Double_t tauC = 0;
  if (p[7] > 0) tauC = p[7];
  if (tauA > 0) {
    Value += tauA *(TMath::Exp(AL-t1/tauA ) - TMath::Exp(AL-t2/tauA ));
    if (tauI > 0) {
      Double_t tauAp = 1./(1./tauI + 1./tauA);
      Value -= tauAp*(TMath::Exp(AL-t1/tauAp) - TMath::Exp(AL-t2/tauAp));
    }
  }
  if (tauB > 0) {
    Value += tauB *(TMath::Exp(AL+BL-t1/tauB ) - TMath::Exp(AL+BL-t2/tauB ));
    if (tauI > 0) {
      Double_t tauBp = 1./(1./tauI + 1./tauB);
      Value -= tauBp*(TMath::Exp(AL+BL-t1/tauBp) - TMath::Exp(AL+BL-t2/tauBp));
    }
  }
  if (tauC > 0) {
    Value += tauC *(TMath::Exp(AL+CL-t1/tauC ) - TMath::Exp(AL+CL-t2/tauC ));
    if (tauI > 0) {
      Double_t tauCp = 1./(1./tauI + 1./tauC);
      Value -= tauCp*(TMath::Exp(AL+CL-t1/tauCp) - TMath::Exp(AL+CL-t2/tauCp));
    }
  }
  return Value;
}
//________________________________________________________________________________
TF1 *Ex2(Int_t k = 0) {// fit of pulser (TPX without zero supression and shaper)
  // tauAp = 1./(1./tauI + 1./tauA)
  // tauBp = 1./(1./tauI + 1./tauB)
  // t = x - t_0; 
  // shape = tauA*Exp(LA - t/tauA)  - tauP*Exp(LA-t/tauP) + tauB*Exp(AL+BL-t/tauB) - Exp(AL+BL-t/tauBp) =>
  //         Exp(LA)*(Exp(-t/tauA) - tauP*Exp(-t/tauP) + Exp(BL)*(Exp(-t/tauB) - Exp(-t/tauBp)));
  struct FitPar_t {//    0    1      2   3      4   5      6   7      8    9
    Int_t row; Double_t  t_0, tau_I, LA, tau_A, LB, tau_B, LC, tau_C, ped, pedSlope;
  };
  FitPar_t fitPars[45] = {
    //           t_0,       tau_I,          LA,       tau_A,          LB,       tau_B,          LC,       tau_C,         ped,   pedSlope;
    { 1,       183.6,          20,      8.8485,      1.1685,     -3.8405,        4.62,        -100,        -100,     0.74841,  -0.0065484},
    { 2,      183.61,          20,      8.8585,       1.164,     -3.8695,      4.6612,        -100,        -100,     0.80784,    -0.00712},
    { 3,      183.59,          20,      8.8245,      1.1815,     -3.8586,      4.7043,        -100,        -100,     0.79164,  -0.0070336},
    { 4,      183.59,          20,       8.832,      1.1762,     -3.8668,      4.7052,        -100,        -100,     0.86976,  -0.0074414},
    { 5,       183.6,          20,      8.8487,      1.1643,     -3.8509,      4.6303,        -100,        -100,     0.79208,  -0.0068672},
    { 6,      183.61,          20,      8.8493,      1.1607,     -3.8585,      4.6334,        -100,        -100,      0.6929,  -0.0061577},
    { 7,      183.61,          20,      8.8372,      1.1689,     -3.8562,      4.6567,        -100,        -100,     0.66706,  -0.0059754},
    { 8,      183.62,          20,      8.8433,       1.162,     -3.8514,      4.6221,        -100,        -100,     0.71431,  -0.0061743},
    { 9,      183.62,          20,      8.8301,      1.1691,     -3.8456,      4.6384,        -100,        -100,     0.75925,  -0.0064975},
    {10,      183.63,          20,      8.8193,      1.1775,     -3.8731,      4.7166,        -100,        -100,     0.72249,  -0.0061527},
    {11,      183.61,          20,      8.8442,      1.1609,     -3.8539,      4.6339,        -100,        -100,     0.72197,  -0.0063569},
    {12,      183.62,          20,      8.8298,      1.1647,      -3.861,      4.6587,        -100,        -100,     0.78137,   -0.006579},
    {13,      183.62,          20,      8.7921,       1.189,     -3.8435,      4.6731,        -100,        -100,     0.72722,  -0.0060866},
    {14,       183.6,          20,      9.1367,      1.1548,     -3.7455,      4.4805,        -100,        -100,      0.6379,  -0.0057348},
    {15,       183.6,          20,      9.1411,      1.1551,     -3.7495,      4.4931,        -100,        -100,     0.56085,  -0.0052822},
    {16,       183.6,          20,      9.1445,      1.1593,     -3.8194,      4.6006,        -100,        -100,     0.56525,  -0.0051849},
    {17,       183.6,          20,      9.1454,      1.1589,     -3.8223,      4.5946,        -100,        -100,     0.56114,  -0.0052125},
    {18,      183.59,          20,      9.1055,      1.1781,     -3.7022,      4.4726,        -100,        -100,     0.68122,  -0.0061023},
    {19,      183.58,          20,      9.1062,      1.1771,     -3.6967,      4.4697,        -100,        -100,      0.6307,  -0.0058138},
    {20,      183.59,          20,      9.1171,       1.174,     -3.7249,      4.4938,        -100,        -100,     0.64662,  -0.0057488},
    {21,      183.59,          20,      9.1188,      1.1723,      -3.722,      4.4836,        -100,        -100,     0.63283,  -0.0057055},
    {22,       183.6,          20,      9.1469,      1.1553,     -3.7522,      4.5028,        -100,        -100,      0.6113,  -0.0055988},
    {23,       183.6,          20,      9.1544,      1.1506,      -3.748,      4.4819,        -100,        -100,     0.62177,    -0.00569},
    {24,      183.61,          20,      9.1349,       1.164,     -3.7367,       4.499,        -100,        -100,     0.59266,  -0.0053944},
    {25,      183.61,          20,      9.1377,      1.1601,     -3.7281,      4.4715,        -100,        -100,     0.60267,  -0.0054668},
    {26,      183.59,          20,      9.1197,        1.17,      -3.723,      4.4752,        -100,        -100,     0.71497,  -0.0062707},
    {27,      183.59,          20,      9.1251,       1.166,     -3.7185,      4.4694,        -100,        -100,     0.68137,  -0.0060712},
    {28,      183.61,          20,      9.1462,        1.16,     -3.7907,      4.5578,        -100,        -100,      0.7266,   -0.006217},
    {29,      183.61,          20,      9.1474,      1.1584,      -3.781,      4.5375,        -100,        -100,     0.67925,  -0.0059789},
    {30,       183.6,          20,      9.1199,       1.172,     -3.7024,      4.4486,        -100,        -100,     0.66171,  -0.0058857},
    {31,      183.61,          20,      9.1249,      1.1674,     -3.6956,      4.4345,        -100,        -100,     0.62531,  -0.0056984},
    {32,      183.61,          20,      9.1486,      1.1546,      -3.769,      4.5203,        -100,        -100,      0.6141,  -0.0054891},
    {33,      183.61,          20,      9.1492,      1.1529,      -3.758,      4.4907,        -100,        -100,     0.61927,  -0.0055831},
    {34,      183.61,          20,      9.1451,      1.1569,      -3.741,       4.477,        -100,        -100,     0.68961,  -0.0061126},
    {35,      183.61,          20,      9.1444,      1.1558,     -3.7344,      4.4674,        -100,        -100,     0.68115,  -0.0060753},
    {36,      183.61,          20,      9.1259,      1.1704,     -3.7784,      4.5624,        -100,        -100,     0.65061,  -0.0056986},
    {37,       183.6,          20,      9.1289,      1.1678,     -3.7722,      4.5557,        -100,        -100,      0.6096,  -0.0054849},
    {38,      183.62,          20,      9.1454,      1.1584,      -3.769,      4.5148,        -100,        -100,     0.64392,  -0.0057734},
    {39,      183.62,          20,      9.1492,       1.156,     -3.7693,      4.5139,        -100,        -100,     0.61136,  -0.0056016},
    {40,      183.62,          20,      9.1327,      1.1666,     -3.7923,      4.5732,        -100,        -100,     0.66113,  -0.0057468},
    {41,      183.62,          20,      9.1319,      1.1669,     -3.7966,      4.5682,        -100,        -100,     0.65668,  -0.0057616},
    {42,      183.61,          20,      9.1358,      1.1621,     -3.7433,      4.4963,        -100,        -100,      0.6488,  -0.0059047},
    {43,      183.61,          20,      9.1408,      1.1579,     -3.7362,      4.4682,        -100,        -100,     0.65939,  -0.0060017},
    {44,      183.63,          20,      9.1449,       1.156,     -3.7584,      4.4902,        -100,        -100,      0.7222,  -0.0062295},
    {45,      183.62,          20,       9.145,      1.1547,      -3.762,       4.507,        -100,        -100,     0.65803,  -0.0058456}
  };
  //  TF1 *f = new TF1("ex2F",ex2,180,250,8);
  TF1 *f = new TF1("ex2F",ex2,-5,20,10);
  if (k <= 0 || k > 45) 
    //    f->SetParameters(-2.01135, 874.861, 20.0966, 0.00161097, -10.89, 3.44995, -7.33127, 0.966391);
    f->SetParameters(183.6,-1,5.30371,1.54786,-1.65642,4.91194,-10,100,40,0);
  else 
    f->SetParameters(&fitPars[k-1].t_0);
  //  f->SetParName(0,"t_0");    f->SetParLimits(0,-5,5);
  f->SetParName(0,"t_0");    f->SetParLimits(0,183.5,183.7);
  f->SetParName(1,"#tau_I"); f->SetParLimits(1,0.5,20); // f->FixParameter(1,-1); //
  f->SetParName(2,"LA");
  f->SetParName(3,"#tau_A"); f->SetParLimits(3,0,1.5);
  f->SetParName(4,"LB");     f->SetParLimits(4,-100,0);
  f->SetParName(5,"#tau_B"); f->SetParLimits(5,1.5,1e3);
  f->SetParName(6,"LC");     f->FixParameter(6,-100); //f->SetParLimits(6,-100,0); //
  f->SetParName(7,"#tau_C"); f->FixParameter(7,-100); //f->SetParLimits(7,0,1e3);	//
  f->SetParName(8,"ped");    f->SetParameter(8,40);
  f->SetParName(9,"pedSlope");    f->SetParameter(9,0);
  return f;
}
//________________________________________________________________________________
void FitEx2(Int_t ii = 0) {
  TF1 *ex2 = Ex2();
  Float_t  buff[200];
  Int_t np = ex2->GetNpar();
  TDirectory *dir = gDirectory;
  TFile *f = new TFile("FitEx2.root","RECREATE");
  TString name("row:NDF:chisq");
  for (Int_t p = 0; p < np; p++) {
    name += ":"; name += ex2->GetParName(p); name += ":d"; name +=  ex2->GetParName(p);
  }
  name.ReplaceAll("#","");
  TNtuple *FitP = new TNtuple("FitP","Pulser Fit",name);
  Int_t i1 = 1;
  Int_t i2 = 45;
  if (ii > 0 && ii <= 45) {i1 = i2 = ii;}
  for (Int_t i = i1; i <= i2; i++) {
    //    TH1 *h = (TH1 *) dir->Get(Form("row%02i",i));
    TH1 *h = (TH1 *) dir->Get(Form("r%02i",i));
    if (! h) continue;
    cout << "Fit row " << i << endl;
    ex2->SetParameters(  183.6,  6.09523e-01,  6.07842e+00,  1, -2.18905e+00,  5.31, -1.00000e+02, -1.00000e+02,  4.00000e+01,  0.00000e+00);
    ex2->FixParameter(6,-1.00000e+02);
    ex2->FixParameter(7,-1.00000e+02);
    /*
      ex2->FixParameter(0,1.83619e+02);
      ex2->FixParameter(4,0);
      ex2->FixParameter(5,0.);
      ex2->FixParameter(6,0.);
      ex2->FixParameter(7,0.);
      h->GetXaxis()->SetRange(180,250);
    */
    ex2->FixParameter(6,-100);
    ex2->FixParameter(7,-100);
    h->Fit(ex2,"e");
//     ex2->ReleaseParameter(6); ex2->SetParameter(6,-10);
//     ex2->ReleaseParameter(7); ex2->SetParameter(7,5*ex2->GetParameter(5)); ex2->SetParLimits(7,ex2->GetParameter(5),1e5);
//     h->Fit(ex2,"em");
    /*
      ex2->ReleaseParameter(4);
      if (i <= 13) ex2->SetParameter(4,-3.67263e+00);
      else         ex2->SetParameter(4,-3.59633e+00);
      ex2->ReleaseParameter(5);
      ex2->SetParameter(5,2.67150e-01);
      h->Fit(ex2,"er","",180,250);
      ex2->ReleaseParameter(6);
      ex2->SetParameter(6,-1.);
      ex2->ReleaseParameter(7);
      ex2->SetParameter(7,0.01);
      h->Fit(ex2,"er","",180,250);
    */
    buff[0] = i;
    buff[1] = ex2->GetNDF();
    buff[2] = ex2->GetChisquare();
    Int_t j = 3;
    for (Int_t p = 0; p < np; p++) {
      buff[j  ] = ex2->GetParameter(p);
      buff[j+1] = ex2->GetParError(p);
      j += 2;
    }
    FitP->Fill(buff);
    h->Write();
  }
  f->Write();
}
//_______ banch of inv. Laplca transoformations
struct MapL_t {
  Int_t key;
  Char_t *Name;
};
static MapL_t MapL[] = {
  {  1, "1/s"},
  {  2, "1/s**2"},
  {  3, "1/s**n"},
  {  4, "1/sqrt(s)"},
  {  5, "s**(-3/2)"},
  {  6, "s**(-(n+1)/2"},
  {  8, "1/(s-a)"},
  {  9, "1/(s-a)**2"},
  { 10, "1/(s-a)**n"},
  { 12, "1/(s-a)/(s-b)"},
  { 13, "s/(s-a)/(s-b)"},
  { 14, "1/(s-a)/(s-b)/(s-c)"},
  { 15, "1/(s**2 + a**2)"},
  { 16, "s/(s**2 + a**2)"},
  { 17, "1/(s**2 - a**2)"},
  { 18, "s/(s**2 - a**2)"},
  { 19, "1/s/(s**2 + a**2)"},
  { 20, "1/s**2/(s**2 - a**2)"},
  { 21, "1/(s**2 + a**2)**2"},
  { 22, "s/(s**2 + a**2)**2"},
  { 23, "s**2/(s**2 + a**2)**2"},
  { 24, "(s**2 - a**2)/(s**2 + a**2)**2"},
  { 25, "s/(s**2 + a**2)/(s**2 + b**2)"},
  { 26, "1/((s-a)**2 + b**2)"},
  { 27, "(s-a)/((s-a)**2 - b**2)"},
  {101, "1/s"},
  {102, "1/(s-a)"},
  {103, "1/s/(s-a)"},
  {104, "(s + b)/s/(s - a)"},
  {106, "(s+c)/(s-a)/(s-b)"},
  {301, "1/s**2"},
  {302, "1/(s-a)**2"},
  {307, "1/s/(s-a)**2"},
  {310, "1/(s-a)**2/(s-b)"},
  {401, "1/(s-a)/(s-b)/(s-c)/s-d)"}
};
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Int_t n=0) {
  Double_t val = 0;
  switch (key) {
  case 1:// key is formula no. in Table 8.4-1 of Korn
  case 101:
    // 1/s
    val = 1;
    break;
  case 2:
  case 301: //             3.1 in Table 8.4-2
    // 1/s**2
    val = t;
    break;
  case 3:
    //  case 7: // n != float
    // 1/s**n 
    assert(n);
    if (n == 0) val = 1;
    else        val = TMath::Power(t,n-1)/TMath::Factorial(n-1);
    break;
  case 4:
    // 1/sqrt(s)
    val = 1./TMath::Sqrt(TMath::Pi()*t);
    break;
  case 5:
    // s**(-3/2)
    val = 2.*TMath::Sqrt(t/TMath::Pi());
    break;
  case 6:
    // s**-(n+1/2)
    val = TMath::Power(2.,n)*TMath::Power(t,n-0.5)/TMath::Sqrt(TMath::Pi());
    for (int i = 1; i <= n; i++) val /= 2*n - 1;
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key," << n << ")" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Int_t n = 0) {
  Double_t val = 0;
  switch (key) {
  case 8:
  case 102:
    // 1/(s - a)
    if (a == 0.) val = InvLaplace(t,1);
    else         val = TMath::Exp(a*t);
    break;
  case 9:
  case 302:
    // 1/(s-a)**2
    if (a == 0.) val = InvLaplace(t,2);
    else         val = t*TMath::Exp(a*t);
    break;
  case 10:
    //  case 11:
    // 1/(s - a)**n
    if (a == 0) val = InvLaplace(t,3,n);
    else        val = TMath::Power(t,n-1)*TMath::Exp(a*t)/TMath::Factorial(n-1);
    break;
  case 103:
    // 1/s/(s-a)
    if (a != 0) val = (TMath::Exp(a*t) - 1)/a;
    else        val = InvLaplace(t,2);
    break;
  case 307:
    // 1/s/(s-a)**2
    if (a != 0) val = 1./(a*a)*(1 + (a*t - 1)*TMath::Exp(a*t));
    else        val = InvLaplace(t,3,3);
    break;
  case 15:
    // 1/(s**2 + a**2)
    if (a != 0) val = TMath::Sin(a*t)/a;
    else        val = InvLaplace(t,2);
    break;
  case 16:
    // s/(s**2 + a**2)
    if (a != 0) val = TMath::Cos(a*t);
    else        val = InvLaplace(t,1);
    break;
  case 17:
    // 1/(s**2 - a**2)
    if (a != 0) val = TMath::SinH(a*t)/a;
    else        val = InvLaplace(t,2);
    break;
  case 18:
    // s/(s**2 - a**2)
    if (a != 0) val = TMath::CosH(a*t);
    else        val = InvLaplace(t,1);
    break;
  case 19:
    // 1/s/(s**2 + a**2)
    if (a != 0) val = (1. - TMath::Cos(a*t))/(a*a);
    else        val = InvLaplace(t,3,3);
    break;
  case 20:
    // 1/s**2/(s**2 - a**2)
    if (a != 0) val = (a*t - TMath::Sin(a*t))/(a*a*a);
    else        val = InvLaplace(t,3,4);
    break;
  case 21:
    // 1/(s**2 + a**2)**2
    if (a != 0) val = (TMath::Sin(a*t) - a*t*TMath::Cos(a*t))/(2*a*a*a);
    else        val = InvLaplace(t,3,4);
    break;
  case 22:
    // s/(s**2 + a**2)**2
    if (a != 0) val = t*TMath::Sin(a*t)/(2*a);
    else        val = InvLaplace(t,3,3);
    break;
  case 23:
    // s**2/(s**2 + a**2)**2
    if (a != 0) val = (TMath::Sin(a*t) + a*t*TMath::Cos(a*t))/(2*a);
    else        val = InvLaplace(t,2);
    break;
  case 24:
    // (s**2 - a**2)/(s**2 + a**2)**2
    if (a != 0) val = t*TMath::Cos(a*t);
    else        val = InvLaplace(t,2);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a )" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Double_t b) {
  Double_t val = 0;
  switch (key) {
  case 25:
    // s/(s**2 + a**2)/(s**2 + b**2)
    if (a*a != b*b) val = (TMath::Cos(a*t) - TMath::Cos(b*t))/(b*b - a*a);
    else            val = InvLaplace(t,22,a,b);
    break;
  case 26:
    // 1/((s-a)**2 + b**2)
    if (b != 0) val = TMath::Exp(a*t)*TMath::Sin(b*t)/b;
    else        val = InvLaplace(t,9,a);
    break;
  case 27:
    // (s - a)/((s-a)**2 - b**2)
    if (b != 0) val = TMath::Exp(a*t)*TMath::Cos(b*t);
    else        val = InvLaplace(t,8,a);
    break;
  case 104:
    // (s + b)/s/(s - a)
    if (a != 0)  val  = (1 + b/a)*TMath::Exp(a*t) - b/a;
    else         val  = InvLaplace(t,1) + b*InvLaplace(t,2);
    break;
  case 12:
    // 1/(s-a)/(s-b)
    if (a != b) val = 1./(a-b)*(TMath::Exp(a*t)-TMath::Exp(b*t));
    else        val = InvLaplace(t,302,a,2);
    break;
  case 13:
    // s/(s-a)/(s-b)
    if (a == 0)   val = InvLaplace(t,8,b);
    else {
      if (b == 0) val = InvLaplace(t,8,a);
      else {
	if (a != b)  val = (a*TMath::Exp(a*t) - b*TMath::Exp(b*t))/(a-b);
	else         val = InvLaplace(t,8,a) + a*InvLaplace(t,9,a);
      }
    }
    break;
  case 310:
    // 1/(s-a)**2/(s-b) => (A + A_1*t)*exp(a*t) + B*exp(b*t); A = -1/(a-b)**2; A1 = 1/(a-b); B = -A;
    //                     (t/(a-b) - 1/(a-b)**2)*exp(a*t) + 1/(a-b)**2*exp(b*t)
    //                     (((a-b)*t - 1)*exp(a*t) + exp(b*t))/(a-b)**2
    //                   

    if (a != b) val = TMath::Power(a-b,-2)*(((a-b)*t - 1)*TMath::Exp(a*t) + TMath::Exp(b*t)); 
    else        val = InvLaplace(t,10,a,3);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a,b)" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Double_t b, Double_t c) {
  Double_t val = 0;
  if (t <= 0.0) return val;
  switch (key) {
  case 14:
    // 1/(s-a)/(s-b)/(s-c)
    if (a != b && a != c && a != b) 
      val = - ((b-c)*TMath::Exp(a*t) + (c-a)*TMath::Exp(b*t) + (a-b)*TMath::Exp(c*t))/
	((a-b)*(b-c)*(c-a));
    else {
      if ( a == b && a == c) 
	val = InvLaplace(t,10, a, 3);
      else {
	if (a == c) val = InvLaplace(t,310, a, b);
	if (a == b) val = InvLaplace(t,310, a, c);
	if (b == c) val = InvLaplace(t,310, b, a);
      }
    }
    break;
  case 106:
    // (s+c)/(s-a)/(s-b) = s/(s-a)/(s-b) + c/(s-a)/(s-b)
    val = InvLaplace(t,13,a,b) + c*InvLaplace(t,12,a,b);
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a,b,c)" << endl;
  }
  return val;
}
//________________________________________________________________________________
Double_t InvLaplace(Double_t t, Int_t key, Double_t a, Double_t b, Double_t c, Double_t d) {
  Double_t val = 0;
  Double_t sk[4] = {a,b,c,d};
  Int_t i, j;
  Double_t P;
  switch (key) {
  case 401:
    // 1/(s-a)/(s-b)/(s-c)/(s-d)
    for (i = 0; i <4 ; i++) {
      P = 1;
      for (j = 0; j < 4; j++) {
	if (i == j) continue;
	P *= (sk[i]-sk[j]);
      }
      if (P != 0) val += P*TMath::Exp(sk[i]*t);
    }
    break;
  default:
    cout << "Invalid key =" << key << " in InvLaplace(t,key,a,b,c,d)" << endl;
  }
  return val;
}
#if 0
//________________________________________________________________________________ 
Double_t InvLaplace(Double_t t, Int_t np = 0, Double_t *par = 0) {
  /* F(s) = 1/D(s)
     D(s)  = Prod_i=0_i<np  (s - par[i])**m_k;
  */
  Double_t V = 0;
  TArrayI mk(np);
  for (Int_t i = 0; i < np; i++) {
    mk[i] = 1;
    for (Int_t j = 0; j < i; j++) {
      if (! mk[j]) continue;
      if (TMath::Abs(par[i]-par[j])<1e-7) {
	mk[j] += mk[i];
	mk[i] = 0;
      }
    }
  }
  for (Int_t i = 0; i < np; i++) {
    if (mk[i] != 1) continue;
    Double_t P = 1;
    for (Int_t j = 0; j < np; j++) {
      if (i == j) continue;
      P *= (par[i]-par[j]);
    }
    V += 1./P*TMath::Exp(par[i]*t);
  }
  return V;
}
#endif
//________________________________________________________________________________  
Double_t ex3(Double_t *x, Double_t *p=0 ) {  
  static const Char_t *names[8] = {"#tau_{1}","A_{1}",
				   "#tau_{2}","A_{2}",
				   "#tau_{3}","A_{3}",
				   "t_{0}","t_{shift}"};
  static Double_t pulserBlair[8] = {1.03557e+02, -5.02589e+00,
				    4.02561e+02, -7.13579e+00,
				    6.78522e+02, -9.90000e+01,
				    1.00000e+00,  1.69713e+04};
  static const Double_t Frequency = 9215890.;// local clock  run5341024 , 8302002
  static const Double_t width     = 1.e9/Frequency; // => ns
  Double_t Value = p[4]+x[0]*p[5]; //pedestal
  Double_t Norm  = p[0];
  Double_t t0 = p[1];              //p[1] offset in tb
  Double_t t  = width*(x[0] - t0);
  if (t < 0) return Value;
  Double_t T1 = width*p[2];
  Double_t T2 = width*p[3];
  Double_t b = -1./T1;
  Double_t c = -1./T2;
#ifndef __SLOPE401__
  pulserBlair[4] = p[7];
  pulserBlair[5] = p[6];
  for (Int_t i = 0; i<3; i++) {
    Double_t tau = pulserBlair[2*i];
    Double_t a = -1./tau;
    Value += Norm*TMath::Exp(pulserBlair[2*i+1])*InvLaplace(t,14,a,b,c);
  }
#else
  Double_t T3 = width*p[6];
  Double_t d = -1./T3;
  for (Int_t i = 0; i < 2; i++) {
    Double_t tau = pulserBlair[2*i];
    Double_t a = -1./tau;
    Value += Norm*TMath::Exp(pulserBlair[2*i+1])*InvLaplace(t,401,a,b,c,d);
  }
  
#endif
  return Value;
}
//________________________________________________________________________________
TF1 *Ex3(Int_t k = 0) {// fit of pulser (TPX without zero supression and shaper) with Blair's pulser shape as input
#ifndef __SLOPE401__
  const Int_t Np = 8;
  static const Char_t *parNames[8] = {"Norm",  "t0",    "T1",    "T2", "ped", "pedSlope", "A_{3}", "#tau_{3}"};
  static const Double_t parV[8]    = //{     1, 183.6, 1.54786, 4.91194,   40.,         0.,    -10,    1e3};
    {9.75098e+00,  1.83311e+02,  6.88519e-01,  6.88865e-01,  8.82829e-01, -7.34415e-03, -8.80651e+00,  9.78161e+02};
#else
  const Int_t Np = 7;
  static const Char_t *parNames[7] = {"Norm",              "t0",         "T1",         "T2",        "ped", "pedSlope",   "T3"};
  static const Double_t parV[7]   =                        
    {9.75098e+00,  1.83311e+02,  6.88519e-01,  6.88865e-01,  8.82829e-01, -7.34415e-03, 10.};
#endif
  TF1 *f = new TF1("ex3F",ex3,180,250,Np);
  for (Int_t i = 0; i < Np; i++) {
    f->SetParName(i,parNames[i]);
    f->SetParameter(i,parV[i]);
#ifdef __SLOPE401__
    if (i == 2 || i == 3 || i == 6) f->SetParLimits(i,0,100);
#endif
  }
  return f;
}
//________________________________________________________________________________
void FitEx3(Int_t ii = 0) {
  TF1 *ex3 = Ex3();
  Float_t  buff[200];
  Int_t np = ex3->GetNpar();
  TDirectory *dir = gDirectory;
  TFile *f = new TFile("FitEx3P.root","RECREATE");
  TString name("row:NDF:chisq");
  for (Int_t p = 0; p < np; p++) {
    name += ":"; name += ex3->GetParName(p); name += ":d"; name +=  ex3->GetParName(p);
  }
  name.ReplaceAll("#","");
  TNtuple *FitP = new TNtuple("FitP","Pulser Fit",name);
  Int_t i1 = 1;
  Int_t i2 = 45;
  if (ii > 0 && ii <= 45) {i1 = i2 = ii;}
  for (Int_t i = i1; i <= i2; i++) {
    //    TH1 *h = (TH1 *) dir->Get(Form("row%02i",i));
    TH1 *h = (TH1 *) dir->Get(Form("r%02i",i));
    if (! h) continue;
    cout << "Fit row " << i << endl;
    ex3->SetParameters(9.75098e+00,  1.83311e+02,  6.88519e-01,  6.88865e-01,  8.82829e-01, -7.34415e-03, -8.80651e+00,  9.78161e+02);
    h->Fit(ex3,"i");
    buff[0] = i;
    buff[1] = ex3->GetNDF();
    buff[2] = ex3->GetChisquare();
    Int_t j = 3;
    for (Int_t p = 0; p < np; p++) {
      buff[j  ] = ex3->GetParameter(p);
      buff[j+1] = ex3->GetParError(p);
      j += 2;
    }
    FitP->Fill(buff);
    h->Write();
  }
  f->Write();
}
//________________________________________________________________________________ 
Double_t ex4(Double_t *x, Double_t *p=0 ) {  
  static const Char_t *names[8] = {"#tau_{1}","A_{1}",
				   "#tau_{2}","A_{2}",
				   "#tau_{3}","A_{3}",
				   "t_{0}","t_{shift}"};
  static Double_t pulserBlair[8] = {1.03557e+02, -5.02589e+00,
				    4.02561e+02, -7.13579e+00,
				    6.78522e+02, -9.90000e+01,
				    1.00000e+00,  1.69713e+04};
  static const Double_t Frequency = 9215890.;// local clock  run5341024 , 8302002
  static const Double_t width     = 1.e9/Frequency; // => ns
  Double_t Value = p[3]+x[0]*p[4]; //pedestal
  Double_t Norm  = p[0];
  Double_t t0 = p[1];              //p[1] offset in tb
  Double_t t  = width*(x[0] - t0);
  if (t < 0) return Value;
  Double_t T1 = width*p[2];
  Double_t a = -1./T1;
  pulserBlair[4] = p[6];
  pulserBlair[5] = p[5];
  for (Int_t i = 0; i<3; i++) {
    Double_t tau = pulserBlair[2*i];
    Double_t b = -1./tau;
    Value += Norm*TMath::Exp(pulserBlair[2*i+1])*InvLaplace(t,310,a,b);
  }
  return Value;
}
//________________________________________________________________________________
TF1 *Ex4(Int_t k = 0) {// fit of pulser (TPX without zero supression and shaper) with Blair's pulser shape as input
  const Int_t Np = 7;
  static const Char_t *parNames[7] = {"Norm",  "t0",    "T1",   "ped", "pedSlope", "A_{3}", "#tau_{3}"};
  static const Double_t parV[7]    = 
    {9.75098e+00,  1.83311e+02,  6.88519e-01,  8.82829e-01, -7.34415e-03, -8.80651e+00,  9.78161e+02};
  TF1 *f = new TF1("ex4F",ex4,180,250,Np);
  for (Int_t i = 0; i < Np; i++) {
    f->SetParName(i,parNames[i]);
    f->SetParameter(i,parV[i]);
  }
  return f;
}
//________________________________________________________________________________
void FitEx4(Int_t ii = 0) {
  TF1 *ex4 = Ex4();
  Float_t  buff[200];
  Int_t np = ex4->GetNpar();
  TDirectory *dir = gDirectory;
  TFile *f = new TFile("FitEx4P.root","RECREATE");
  TString name("row:NDF:chisq");
  for (Int_t p = 0; p < np; p++) {
    name += ":"; name += ex4->GetParName(p); name += ":d"; name +=  ex4->GetParName(p);
  }
  name.ReplaceAll("#","");
  TNtuple *FitP = new TNtuple("FitP","Pulser Fit",name);
  Int_t i1 = 1;
  Int_t i2 = 45;
  if (ii > 0 && ii <= 45) {i1 = i2 = ii;}
  for (Int_t i = i1; i <= i2; i++) {
    //    TH1 *h = (TH1 *) dir->Get(Form("row%02i",i));
    TH1 *h = (TH1 *) dir->Get(Form("r%02i",i));
    if (! h) continue;
    cout << "Fit row " << i << endl;
    ex4->SetParameters(8.39805e+00,  1.83171e+02,  7.54907e-01,  9.10414e-01, -7.53423e-03, -9.04079e+00,  1.08556e+03);
    h->Fit(ex4,"i");
    buff[0] = i;
    buff[1] = ex4->GetNDF();
    buff[2] = ex4->GetChisquare();
    Int_t j = 3;
    for (Int_t p = 0; p < np; p++) {
      buff[j  ] = ex4->GetParameter(p);
      buff[j+1] = ex4->GetParError(p);
      j += 2;
    }
    FitP->Fill(buff);
    h->Write();
  }
  f->Write();
}
//--------------------------------------------------------------------------------
void PrintFitP(const Char_t *files="FitEx2Pedestal.root") {
  TDirIter Dir(files);
  TTreeIter iter("FitP");
  Char_t *file = 0, *file1 = 0;
  Int_t NFiles = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << "file1 " << file1 << endl;
  const Float_t&     row                                      = iter("row");
  //  const Float_t&     NDF                                      = iter("NDF");
  //  const Float_t&     chisq                                    = iter("chisq");
  const Float_t&     t_0                                      = iter("t_0");
  //  const Float_t&     dt_0                                     = iter("dt_0");
  const Float_t&     tau_I                                    = iter("tau_I");
  //  const Float_t&     dtau_I                                   = iter("dtau_I");
  const Float_t&     LA                                       = iter("LA");
  //  const Float_t&     dLA                                      = iter("dLA");
  const Float_t&     tau_A                                    = iter("tau_A");
  //  const Float_t&     dtau_A                                   = iter("dtau_A");
  const Float_t&     LB                                       = iter("LB");
  //  const Float_t&     dLB                                      = iter("dLB");
  const Float_t&     tau_B                                    = iter("tau_B");
  //  const Float_t&     dtau_B                                   = iter("dtau_B");
  const Float_t&     LC                                       = iter("LC");
  //  const Float_t&     dLC                                      = iter("dLC");
  const Float_t&     tau_C                                    = iter("tau_C");
  //  const Float_t&     dtau_C                                   = iter("dtau_C");
  const Float_t&     ped                                      = iter("ped");
  //  const Float_t&     dped                                     = iter("dped");
  const Float_t&     pedSlope                                 = iter("pedSlope");
  //  const Float_t&     dpedSlope                                = iter("dpedSlope");
  cout << "// row, t_0, tau_I, LA, tau_A, LB, tau_B, LC, tau_C" << endl;
  while (iter.Next()) {
    cout << Form("{%2.0f,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g,%12.5g},",row,t_0,tau_I,LA,tau_A,LB,tau_B,LC,tau_C,ped,pedSlope) << endl;
  }
}
//________________________________________________________________________________
Double_t shaperP(Double_t *x, Double_t *par=0 ) {  
  static TF1 *ex2 = 0;
  static Int_t kOld = 0;
  Double_t Value = 0;
  Int_t k = (Int_t) par[0];
  // H(s) = (1 + s*tau_P)/(1 + s*tau_F) => (s + d)/(s - b);
  if (par[1] <= 0) return Value;
  Double_t tau_F = par[1];
  if (par[2] <= 0) return Value;
  Double_t tau_P = par[2];
  Double_t  t = x[0] - par[3];
  Double_t  t1 = t - 0.5;
  if (t1 < 0) t1 = 0;
  Double_t t2 = t + 0.5;
  Double_t  Norm = par[4];
  if (t <= 0) return Value;
  if (k != kOld || ! ex2) {
    ex2 = Ex2(k);
    kOld = k;
  }
  Double_t LA    = ex2->GetParameter(1);
  Double_t tau_I = ex2->GetParameter(2);
  Int_t NL = 2;
  if (tau_I <= 0) NL = 1;
  Double_t b = -1./tau_F;
  Double_t d =  1./tau_P;
  for (Int_t k = 0; k < 3; k++) {
    Double_t tau_A = ex2->GetParameter(3 + 2*k);
    if (tau_A <= 0) continue;
    Double_t A = LA;
    if (k) {
      A += ex2->GetParameter(2 + 2*k);
    }
    for (Int_t l = 0; l < NL; l++) {
      Double_t tau = tau_A;
      Double_t si = 1;
      if (l) {
	tau = 1./(1./tau + 1./tau_I);
	si = -1;
      }
      Double_t C = si*TMath::Exp(A);
      Double_t a = -1./tau;
      if (TMath::Abs(a-b) < 1e-7) {
	/* (s+d)/(s-a)**2 */
	//	C *= (1 + (a+d)*t)*TMath::Exp(a*t);
	C *= -((TMath::Exp(a*t1) - TMath::Exp(a*t2)) + (a+d)*(TMath::Gamma(2,a*t2) - TMath::Gamma(2,a*t1)))/a;
      } else {
	if (TMath::Abs(d+a) < 1e-7) {
	  /* 1/(s-b) */
	  //	  C *= TMath::Exp(b*t);
	  C *= -(TMath::Exp(b*t1) - TMath::Exp(b*t2))/b;
	} else {
	  if (TMath::Abs(d+b) < 1e-7) {
	    /* 1/(s-a) */
	    //	    C *= TMath::Exp(a*t);
	    C *= -(TMath::Exp(a*t1) - TMath::Exp(a*t2))/a;
	  } else {
	    //	    C *= (a+d)/(a-b)*TMath::Exp(a*t) + (b+d)/(b-a)*TMath::Exp(b*t);
	    C *= -(a+d)/(a-b)*(TMath::Exp(a*t1) - TMath::Exp(a*t2))/a + -(b+d)/(b-a)*(TMath::Exp(b*t1) - TMath::Exp(b*t2))/b;
	  }
	}
      }
      Value += C;
    }
  }
  return Norm*Value;
}
//________________________________________________________________________________
TF1 *ShaperP() {
  TF1 *f = new TF1("ShapeP",shaperP,-10,50,5);
  f->SetParameters(1,  3.23004e+00,  6.44089e+00, -1.35447e+00,  2.36870e-08);
  f->SetParName(0,"row");
  f->SetParName(1,"#tau_F");
  f->SetParName(2,"#tau_P");
  f->SetParName(3,"t_0");
  f->SetParName(4,"Norm");
  return f;
}
//________________________________________________________________________________
void FitShaperP(Int_t ii = 0) {
  TF1 *ex2 = ShaperP();
  Float_t  buff[200];
  Int_t np = ex2->GetNpar();
  TDirectory *dir = gDirectory;
  TFile *f = new TFile("FitShaperP.root","RECREATE");
  TString name("row:NDF:chisq");
  for (Int_t p = 0; p < np; p++) {
    name += ":"; name += ex2->GetParName(p); name += ":d"; name +=  ex2->GetParName(p);
  }
  name.ReplaceAll("#","");
  TNtuple *FitP = new TNtuple("FitP","Shaper Fit",name);
  Int_t i1 = 1;
  Int_t i2 = 45;
  if (ii > 0 && ii <= 45) {i1 = i2 = ii;}
  for (Int_t i = i1; i <= i2; i++) {
    TH1 *h = (TH1 *) dir->Get(Form("s%02i",i));
    if (! h) continue;
    h->GetXaxis()->SetRange(20,160);
    cout << "Fit row " << i << endl;
    ex2->FixParameter(0,i);
    h->Fit(ex2,"er","",-10,60);
    buff[0] = i;
    buff[1] = ex2->GetNDF();
    buff[2] = ex2->GetChisquare();
    Int_t j = 3;
    for (Int_t p = 0; p < np; p++) {
      buff[j  ] = ex2->GetParameter(p);
      buff[j+1] = ex2->GetParError(p);
      j += 2;
    }
    FitP->Fill(buff);
    h->Write();
  }
  f->Write();
}
//________________________________________________________________________________
Double_t convI(Double_t *x, Double_t *p = 0) {
  Double_t t0 = p[0];
  Double_t tau_F = p[1];
  Double_t t = x[0];
  return TMath::Exp(t/tau_F)/(1 + t/t0);
}
//________________________________________________________________________________
Double_t ConvI(Double_t *x, Double_t *p) {
  static TF1 *convIFI = 0;
  static TF1 *convIFO = 0;
  static Double_t OnlFreq = 9.21588898; // MHz
  static Double_t tau_P = 7.205/(1.e-3*OnlFreq); // ns
  static Double_t tau_F = 3.68 /(1.e-3*OnlFreq); // ns
  static Double_t t0I   = 1.4362;// ns Outer
  static Double_t t0O   = 1.2087;// ns Inner
  Double_t  t0 = t0I;
  TF1 *convIF = convIFI;
  Int_t k = (Int_t) p[0];
  if (k == 2) {
    t0 = t0O;
    convIF = convIFO;
  }
  Double_t t     = x[0];
  if (! convIF) {
    if (k != 2) {
      convIFI = new TF1("convIFI",convI,0,1e5,2);
      convIFI->SetParameters(t0I,tau_F);
      convIF = convIFI;
    } else {
      convIFO = new TF1("convIFO",convI,0,1e5,2);
      convIFO->SetParameters(t0I,tau_F);
      convIF = convIFO;
    }
  }
  Double_t Value = 1./(1. + t/t0) - (1./tau_F - 1./tau_P)*TMath::Exp(-t/tau_F)*convIF->Integral(0,t);
  return Value;
}
//________________________________________________________________________________
TF1 *Conv(Int_t k = 1) {
  /* tau_F = (3.73 +/- 0.53) / 9.21588898e6;
     tau_P =  7.29 +/- 1.01
     tau_F =-0.433 + 0.569*tau_P; 
  */
 TF1 *f = new TF1(Form("Conv%i",k),ConvI,0,1e4,1);
  f->SetParameter(0,k);
  return f;
}
