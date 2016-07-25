#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TH1.h"
#include "TF1.h"
#include "TGraph.h"
#include "TNtuple.h"
#include "TProfile2D.h"
#include "TDirectory.h"
#include "TFile.h"
#endif
//________________________________________________________________________________
Double_t PLap(Double_t *x, Double_t *p) {
  /* (s+a)/(s*(s-b)*(s-c)) =>
     a/(b*c) + (-c*(a+b)exp(b*t) - b(a+c)*exp(ct))/(b*c*(-b-c)) */
  Double_t t = x[0];
  Double_t a = -1./p[0];
  Double_t b = -1./p[1];
  Double_t c = -1./p[2];
  return a/(b*c) + (-c*(a+b)*TMath::Exp(b*t) - b*(a+c)*TMath::Exp(c*t))/(b*c*(-b+c));
}
//________________________________________________________________________________
Double_t fT0(Double_t *x, Double_t *p) {
  Double_t val = 0;
  Double_t t = x[0] - p[1];
  Double_t t0 = p[0];
  Double_t tmax = p[2];
  if (t > 0 && t0 > 0) {
    if (t < tmax)  val = TMath::Log(1.+t/p[0])/TMath::Log(1.+tmax/p[0]);
    else           val = 1;
  }
  return val;
}
//________________________________________________________________________________
Double_t fExpT0(Double_t *x, Double_t *p) {
  static Double_t As[3] = {0.79, 0.185, 0.024};
  static Double_t taus[3] = {1.6, 13.5, 113};
  Double_t t0 = p[0];
  Double_t tmax = p[2];
  Double_t t  = x[0]-p[1];;
  Double_t value = 0;
  Double_t V = 0;
  if (t > 0 && t0 > 0) {
    if (t > tmax) value = 1;
    else {
      for (Int_t i = 0; i < 3; i++) {
	value += As[i]*(t0*taus[i])*(1. - TMath::Exp(-t/(t0*taus[i])));
	V     += As[i]*(t0*taus[i])*(1. - TMath::Exp(-tmax/(t0*taus[i])));
      }
      value /= V;
    }
  }
  return value;
}
//________________________________________________________________________________
Double_t fExpInt(Double_t *x, Double_t *p) {
  Double_t As[3] = {p[1],p[3],p[5]};
  Double_t taus[3] = {p[0],p[2],p[4]};
  Double_t t0 = p[6];
  Double_t value = 0;
  if (x[0] > p[7] && t0 > 0) {
    Double_t t =  (x[0]-p[7])/t0;
    for (Int_t i = 0; i < 3; i++) {
      value += TMath::Exp(As[i])*(t0*taus[i])*(1. - TMath::Exp(-t/(t0*taus[i])));
    }
  }
  return value;
}
//________________________________________________________________________________
Double_t fExp(Double_t *x, Double_t *p) {
  Double_t As[3] = {p[1],p[3],p[5]};
  Double_t taus[3] = {p[0],p[2],p[4]};
  Double_t t0 = p[6];
  Double_t value = 0;
  if (x[0] > p[7] && t0 > 0) {
    Double_t t =  (x[0]-p[7])/t0;
    for (Int_t i = 0; i < 3; i++) {
      if (As[i] > -90) 
	value += TMath::Exp(As[i] - t/(t0*taus[i]));
    }
  }
  return value;
}
//________________________________________________________________________________
//TGraph *PulserShape() {
TH1D *PulserS() {
  /*
    Hi Yuri - I have read out the stored waveform that we use for
    pulser runs. It is stored as 380 points, with each point = 100 nanosec.
    The amplitude out is relative. For a standard pulser run we use
    maximum output = 200 mV. Here is the data as stored:
    
    1-169 0
    170 417
    171 1151
    172 1501
    173 1686
    174 1775
    175 1818
    176 1855
    177 1887
    178 1914
    179 1936
    180 1955
    181 1969
    182 1981
    183 1991
    184 1997
    185 2001
    186 2002
    187-269 2002
    270-380 0
    

    Period = 38 microsec
    380 points => 100 nsec per point
    Sample Freq = 10.0 MHz
    Wave Form Freq = 26.32 kHz
    Amplitude 200 mVp
    Arbitrary Waveform "Laser"
    
    Trigger In: External, Positive, Level = 1.0 V
    
    
    This should yield a pulse that is 10 microsec wide with a
    slightly rounded leading edge and a delta function falling edge.
    
    Let me know if you have questions.
    
    Blair
    
    TF1 *pulser = new TF1("pulser","[0]*(1 - exp(-[1]*(x-[2])))")
    pulser->SetParameters(2000,0.5,170)
    gr->Fit("pulser")
    
    
  */
  Double_t x;
  Double_t y;
  //  TGraph* Pulser = new TGraph(100);
  TH1D *Pulser = new TH1D("Pulser","Pulser",100,1.645e4,2.655e4);
  Pulser->SetMarkerStyle(20);
  Int_t p = 0;
  for (int i = 161; i < 270; i++, p++) {
    x = 100*(i+0.5);
    y = 0;
    if (i == 170) y = 417;
    if (i == 171) y = 1151;
    if (i == 172) y = 1501;
    if (i == 173) y = 1686;
    if (i == 174) y = 1775;
    if (i == 175) y = 1818;
    if (i == 176) y = 1855;
    if (i == 177) y = 1887;
    if (i == 178) y = 1914;
    if (i == 179) y = 1936;
    if (i == 180) y = 1955;
    if (i == 181) y = 1969;
    if (i == 182) y = 1981;
    if (i == 183) y = 1991;
    if (i == 184) y = 1997;
    if (i == 185) y = 2001;
    if (i >= 186 && i <= 269) y = 2002;
    //    Pulser->SetPoint(p,x,y/2002.);
    Int_t bin = Pulser->FindBin(x);
    Pulser->SetBinContent(bin,y/2002.);
    Pulser->SetBinError(bin,1e-3);
  }
  //  return gr = new TGraph(20,&x[169],&y[169]);
  TF1 *ExpInt = new TF1("ExpInt",fExpInt,1.6,3.0,8);
  const Char_t *names[8] = {"#tau_{1}","A_{1}","#tau_{2}","A_{2}","#tau_{3}","A_{3}","t_{0}","t_{shift}"};
  const Double_t par[8] = {1.03557e+02, -5.02589e+00,
			   4.02561e+02, -7.13579e+00,
			   6.78522e+02, -9.90000e+01,
			   1.00000e+00,  1.69713e+04};
  for (Int_t i = 0; i < 8; i++) {
    ExpInt->SetParName(i,names[i]);
    if (i > 3 && i < 7) ExpInt->FixParameter(i,par[i]);
    else                ExpInt->SetParameter(i,par[i]);
  }
#if 0
  double params[10];
  TF1 *pT0 =  new TF1("pT0",fT0,1.7,2.6,3);
  pT0->SetParName(0,"t0");
  pT0->SetParName(1,"shift");
  pT0->SetParName(2,"tmax");
  pT0->SetParameters(4.02632e-04,1.69918e+00,8.82986e-02);                            

  pT0->SetLineColor(1);
  cout << "pT0" << endl;
  //  Pulser->Fit(pT0,"er","",1.7,2.6);
  TF1 *pExp = new TF1("pExp",fExpT0,1.7,2.6,3);
  pExp->SetParName(0,"t0");
  pExp->SetParName(1,"shift");
  pExp->SetParName(2,"tmax");
  pExp->SetLineColor(3);
  pExp->SetParameters(0.0742672,1.71318,10.8876);
  cout << "Exp from NIM" << endl;
  Pulser->Fit(pExp,"er+","",1.7,2.6);
  TF1 *pulser1 = new TF1("pulser1","(1 - exp(-[0]*(x-[1])))");
  pulser1->SetParName(0,"#tau");
  pulser1->SetParName(1,"shift");
  pulser1->SetParameters(5.36069e+01,1.7);
  cout << "Single exponent" << endl;
  pulser1->SetLineColor(2);
  //  Pulser->Fit("pulser1","i+");
  TF1 *pulser2 = new TF1("pulser2","[2]*(1 - exp(-[0]*(x-[1])))+(1-[2])*(1 - exp(-[3]*(x-[1])))");
  pulser1->GetParameters(params);
  pulser2->SetParameters(params);
  pulser2->SetParLimits(2,0,1);
  pulser2->SetParameter(2,10);
  pulser2->SetParameter(3,0.9);
  pulser2->SetParName(0,"tau1");
  pulser2->SetParName(1,"Shift");
  pulser2->SetParName(2,"fract");
  pulser2->SetParName(3,"tau2");
  pulser2->SetParameters(2.48771e+01,  1.70133e+02,  3.11708e-01,  9.33658e+01);
  pulser2->SetLineColor(3);
  cout << "Two exponents" << endl;
  //  Pulser->Fit("pulser2","i+");
  TF1 *pulser3 = new TF1("pulser3","[4]*[2]*(1 - exp(-[0]*(x-[1])))+(1-[2])*(1 - exp(-[3]*(x-[1])))+(1-[4])*(1 - exp(-[5]*(x-[1])))");
  pulser2->GetParameters(params);
  pulser3->SetParameters(params);
  pulser3->SetParLimits(2,0,1);
  pulser3->SetParLimits(4,0,1);
  pulser3->SetParameter(5,2.45968e-01);
  pulser3->SetParName(0,"tau1");
  pulser3->SetParName(1,"Shift");
  pulser3->SetParName(2,"fract2");
  pulser3->SetParName(3,"tau2");
  pulser3->SetParName(4,"fract3");
  pulser3->SetParName(5,"tau3");
  pulser3->SetLineColor(4);
  cout << "Three exponents" << endl;
  //  Pulser->Fit("pulser3","i+");
#endif
  return Pulser;
};
//________________________________________________________________________________
TGraph *Signal() {
  Int_t N = 10000;
  TGraph *gr = new TGraph(N);
  for (Int_t i = 0; i < N; i++) {
    Double_t x = 0.1*i;
    Double_t y = 1./(1. + x);
    gr->SetPoint(i,x,y);
  }
  // Outer Sector t0 = 1.4362 ns
  // Inner Sector t0 = 1.2087 ns
  Double_t s = 1.;
  Double_t params[24] = {0.597872,0.844480,0.318033, 4.88884,0.0611673,47.2965, 1.,1.,  // fit
                         0.79    ,1.6     ,0.185   ,13.5    ,0.024    ,113    , 1.,8.63624e-011, // NIM 192 (1982) 365
			 26.8*s  ,20.9    ,1.*s    ,287.    ,0.1*s    ,2280.  , 1.,8.73357e-03};// STAR node
  for (Int_t k = 0; k < 3; k++) {
    TF1 *f = new TF1(Form("f%i",k),"[7]*([0]*TMath::Exp(-x/[6]/[1])+[2]*TMath::Exp(-x/[6]/[3])+[4]*TMath::Exp(-x/[6]/[5]))",0,1000);
    f->SetParName(0,"A");
    f->SetParName(1,"#alpha");
    f->SetParName(2,"B");
    f->SetParName(3,"#beta");
    f->SetParName(4,"C");
    f->SetParName(5,"#gamma");
    f->SetParName(6,"t_{0}");
    f->SetParName(7,"Norm");
    //    f->SetParameters(&params[8*k]);
    for (Int_t i = 0; i < 8; i++) f->FixParameter(i,params[8*k+i]);
    f->SetLineColor(k+1);
  }
  return gr;
}
//________________________________________________________________________________
Double_t ShaperF(Double_t *x, Double_t *par=0 ) {  
  /*
    .L ShaperF.C+
    TF1 *t = new TF1("t",ShaperF,-10,50,7);
    t->SetParameters(1.08577e+00,  3, -1.32874e+00,  55, 0., ,1000, 83.);
    t->SetParName(0,"Norm");
    t->SetParName(1,"Pole");
    t->FixParameter(1,3);
    t->SetParName(2,"Shift");
    t->SetParName(3,"tau");
    t->FixParameter(3,55);
    t->SetParName(4,"A_{undershoot}");
    t->FixParameter(4,0);
    t->SetParName(5,"T_{undershoot}");
    t->FixParameter(5,1e3);
    t->SetParName(6,"Shift2");
    t->FixParameter(6,83);
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
  Double_t val = 0;
  if (t2 > 0) {
    val = par[0]*(TMath::Gamma(p,t2) - TMath::Gamma(p,t1));
    if (t > 0 && par[4]) val -= par[0]*TMath::Gamma(p,t)*par[4]*par[5]/tau*(TMath::Exp(-t1*tau/par[5])-TMath::Exp(-t2*tau/par[5]));
  }
  t     = (x[0]-par[6])*widthOvertau;
  t1 = t - Delta/2.;
  t2 = t1 + Delta;
  if (t2 > 0) {
    val -= par[0]*(TMath::Gamma(p,t2) - TMath::Gamma(p,t1));
    if (t > 0 && par[4]) val += par[0]*TMath::Gamma(p,t)*par[4]*par[5]/tau*(TMath::Exp(-t1*tau/par[5])-TMath::Exp(-t2*tau/par[5]));
  }
  return val;
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
void PulserShape() {
}
//________________________________________________________________________________
TF1 *PShape() {
  static TF1 *f = 0;
  if (! f) {
    //    f = new TF1("PShape",fExpInt,0,100,8);
    f = new TF1("PShape",fExp,0,1e4,8);
    const Double_t par[8] = {1.06481e+02, 6.44103e-03, 4.05462e+02, 7.75824e-04, 6.78522e+02, 0.00000e+00, 1.00000e+00, 0};
    f->SetParameters(par);
  }
  return f;
}
//________________________________________________________________________________
Double_t pol2I(Double_t *x, Double_t *par) {
  Double_t a = 1./par[0];
  Double_t b = 1./par[1];
  Double_t c = 1./par[2];
  Double_t t = x[0];
  Double_t val = 0;
  if (t >= 0) {
    //    val = (a + (c*(a-b)*TMath::Exp(-b*t) + b*(a-c)*TMath::Exp(-c*t))/(b-c))/(b*c);
    val = ((c*(a-b)*TMath::Exp(-b*t) + b*(a-c)*TMath::Exp(-c*t))/(b-c))/(b*c) - 
    ((c*(a-b) + b*(a-c))/(b-c))/(b*c);
  }
  return val;
}
//________________________________________________________________________________
TF1 *Filter() {
  static TF1 *f = 0;
  // Filter (s + a)/(s + b)  => dirac(t) + (a-b)*exp(-b*t)
  //  if (! f) f = new TF1("Filter","([0]-[1])*TMath::Exp(-[1]*x)",0,1e4);
  // Filter (s+a)/(s*(s+b)*(s+c) => 1./(b*c)(a + 1./(b-c)*(c*(a-b)*exp(-b*t) + b*(a-c)*exp(-c*t))); 
  if (! f) {
    f = new TF1("filter",pol2I,0,1e4,3);
    f->SetParameters(200,50,260);
  }
  return f;
}
//________________________________________________________________________________
Double_t Product(Double_t *x, Double_t *par) {
  // f1(x)*f2(x - p)
  TF1 *p = PShape();
  TF1 *f = Filter();
  Double_t dif = par[0]-x[0];
  return (p->Eval(x[0]))*(f->EvalPar(&dif,&par[1]));
}
//________________________________________________________________________________
TF1 *ProductF() {
  static TF1 *prod = 0;
  if (! prod) prod = new TF1("ProductF",Product,0,1e4,3);
  return prod;
}
//--------------------------------------------------------------------------------
Double_t Conv(Double_t *x, Double_t *par) {
  TF1 *prod = ProductF();
  Double_t parn[3] = {x[0], par[0], par[1]};
  TF1 *p    = PShape();
  return p->Eval(x[0]) + prod->Integral(0,x[0],parn);
}
//___________________________[_____________________________________________________
TF1 *ConvF() {
  TF1 *f = new TF1("ConvF",Conv,0,1e4,2);
  f->SetParameters(1./55.,1./800.);
  return f;
}
//________________________________________________________________________________
void PulserShapeFit() {
  TProfile2D   *prof = (TProfile2D   *) gDirectory->Get("prof");
  struct Fit_t {
    Float_t PadRow,Chisq,Ndf,Norm,Pole,Shift,tau,A_u,T_u,Shift2,dNorm,dPole,dShift,dtau,dA_u,dT_u,dShift2;};
  if (! prof) return;
  TFile *f = new TFile("PulserShapeFit.root","recreate");
  TNtuple *FitP = new TNtuple("FitP","Fit results",
			      "PadRow:Chisq:Ndf:Norm:Pole:Shift:tau:A_u:T_u:Shift2:dNorm:dPole:dShift:dtau:dA_u:dT_u:dShift2");
  
  FitP->SetMarkerStyle(20);
  FitP->SetLineWidth(2);
  TF1 *t = new TF1("t",ShaperF,-10,50,7);
  t->SetParameters(1.256, 2.76, -1.375,  72, 0.02, 1.6e-6, 83.45);
  t->SetParName(0,"Norm");
  t->SetParName(1,"Pole");
  t->SetParName(2,"Shift");
  t->SetParName(3,"tau");
  t->SetParName(4,"A_{undershoot}");
  t->SetParName(5,"T_{undershoot}");
  t->SetParName(6,"Shift2");
  Fit_t Fit;
  for (Int_t i = 1; i <= 45; i++) {
    t->SetParameters(1.256, 2.76, -1.375,  72, 0.02, 1.6e-6, 83.45);
    prof->ProjectionY(Form("s%02i",i),i,i)->Fit(t,"m");
    Fit.PadRow = i;
    Fit.Chisq  = t->GetChisquare();
    Fit.Ndf    = t->GetNDF();
    Float_t *par = &Fit.Norm;
    Float_t *dpar = &Fit.dNorm;
    for (Int_t j = 0; j < 7; j++) {
      par[j] = t->GetParameter(j);
      dpar[j] = t->GetParError(j);
    }
    FitP->Fill(&Fit.PadRow);
  }
}
