// Extracted from StTpcRSMaker Shaper for TPX  (Tonko's request)
//                                    Inner        Outer
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TLegend.h"
#include "Riostream.h"
#include "TSystem.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TDirectory.h"
#include "TROOT.h"
#include "TFile.h"
#include "TVector3.h"
#include "TRMatrix.h"
#endif
Double_t t0IO[2]          = {1.20868e-9, 1.43615e-9}; // recalculated in InducedCharge
Double_t mtauIntegrationX = 74.6e-9; // secs
Double_t mTimeBinWidth    = 1.06574e-07; // = 1.e-6/gStTpcDb->Electronics()->samplingFrequency();
Char_t *Names[2]          = {"I","O"};
Double_t timeBinMin = -0.5;
Double_t timeBinMax = 24.5;
Int_t    sector = 16;
TF1*     fgTimeShape0[2]    = {0, 0};
//______________ EXPONENTIAL INTEGRALS En __________________________________________________________________
// define: E_n(x) = \int_1^infty{exp(-xt)/t^n}dt, x>0, n=0,1,...
Double_t expint(Int_t n, Double_t x) {
  // based on Numerical Recipes in C
  const Double_t euler = 0.57721566; // Euler's constant, gamma
  const Int_t    maxit = 100;        // max. no. of iterations allowed
  const Double_t fpmin = 1.0e-30;    // close to smallest floating-point   number
  const Double_t eps = 6.0e-8;       // relative error, or absolute error near
  // the zero of Ei at x=0.3725
  
  Int_t i, ii, nm1;
  Double_t a,b,c,d,del,fact,h,psi,ans;
  
  nm1=n-1;
  if(n<0 || x<0 || (x==0 && (n==0 || n==1))) {
    cout << "Bad argument for expint(n,x)" << endl; return -1;
  }
  else {
    if(n==0) ans=exp(-x)/x;
    else {
      if(x==0) ans=1.0/nm1;
      else {
	if(x>1) {
	  b=x+n;
	  c=1.0/fpmin;
	  d=1.0/b;
	  h=d;
	  for(i=1; i<maxit; i++) {
	    a = -i*(nm1+i);
	    b += 2.0;
	    d=1.0/(a*d+b);
	    c=b+a/c;
	    del=c*d;
	    h *= del;
	    if(fabs(del-1.0)<eps) {
	      ans=h*exp(-x);
	      return ans;
	    }
	  }
	  cout << "***continued fraction failed in expint(n,x)!!!" << endl;
	  return -1;
	} else {
	  ans = (nm1!=0 ? 1.0/nm1 : -log(x)-euler);
	  fact=1;
	  for(i=1; i<=maxit; i++) {
	    fact *= -x/i;
	    if(i!=nm1) del = -fact/(i-nm1);
	    else {
	      psi = -euler;
	      for(ii=1; ii<=nm1; ii++) psi += 1.0/ii;
	      del = fact*(-log(x)+psi);
	    }
	    ans += del;
	    if(fabs(del)<fabs(ans)*eps) return ans;
	  }
	  cout << "***series failed in expint!!!" << endl;
	  return -1;
	}
      }
    }
  }
  
  return ans;
}
//______________ EXPONENTIAL INTEGRAL Ei __________________________________________________________________
// define: ei(x) = -\int_{-x}^{\infty}{exp(-t)/t}dt,  for x>0
// power series: ei(x) = eulerconst + ln(x) + x/(1*1!) + x^2/(2*2!) + ...
Double_t ei(Double_t x)
{ // taken from Numerical Recipes in C
  const Double_t euler = 0.57721566; // Euler's constant, gamma
  const Int_t maxit = 100;           // max. no. of iterations allowed
  const Double_t fpmin = 1.e-7; //1.0e-40;    // close to smallest floating-point number
  const Double_t eps = 1.e-7; //1.0e-30;       // relative error, or absolute error  near
                                    // the zero of Ei at x=0.3725
  //  I actually changed fpmin and eps into smaller values than in NR
  
  Int_t k;
  Double_t fact, prev, sum, term;
  
  // special case
  if(x < 0) return -expint(1,-x);
  
  if(x == 0.0) { cout << "Bad argument for ei(x)" << endl; return -1; }
  if(x < fpmin) return log(x)+euler;
  if(x <= -log(eps)) {
    sum = 0;
    fact = 1;
    for(k=1; k<=maxit; k++) {
      fact *= x/k;
      term = fact/k;
      sum += term;
      if(term < eps*sum) break;
    }
    if(k>maxit) { cout << "Series failed in ei(x)" << endl; return -1; }
    return sum+log(x)+euler;
  } else {
    sum = 0;
    term = 1;
    for(k=1; k<=maxit; k++) {
      prev = term;
      term *= k/x;
      if(term<eps) break;
      if(term<prev) sum+=term;
      else {
	sum -= prev;
	break;
      }
    }
    return exp(x)*(1.0+sum)/x;
  }
}
//________________________________________________________________________________
Double_t shapeEI(Double_t *x, Double_t *par) {// does not work. It is needed to 1/s
  Double_t t  = x[0];
  Double_t value = 0;
  if (t <= 0) return value;
  Double_t t0    = par[0];
  Double_t tau_I = par[1];
  Double_t tau_C = par[3];
  Double_t a[2] = {- 1./tau_I, 0};
  Double_t A[2] = {  1., 0.};
  Int_t N = 1;
  if (tau_C > 0) {
    N = 2;
    a[1] = - 1./tau_C;
    A[0] = 1./(a[0]-a[1]);
    A[1] = -A[0];
  }
  for (Int_t i = 0; i < N; i++) {
    value += A[i]*TMath::Exp(a[i]*(t+t0))*(ei(-a[i]*(t+t0))-ei(-a[i]*t0));
  }
  return value;
}
//________________________________________________________________________________
Double_t shapeEI_I(Double_t *x, Double_t *par) { //Integral of shape over time bin
  static Double_t sqrt2 = TMath::Sqrt(2.);
  Double_t TimeBinWidth = par[2];
  Double_t norm = par[3];
  Double_t t1 = TimeBinWidth*(x[0] - 0.5);
  Double_t t2 = t1 + TimeBinWidth;
  Int_t io = (Int_t) par[4];
  assert(io >= 0 && io <= 1);
  return sqrt2*fgTimeShape0[io]->Integral(t1,t2)/norm;
}
//________________________________________________________________________________
TF1 *ShaperT(Int_t io = 0) { // io = 0 -> Inner, io = 1 -> Outer
  Double_t params0[5] = {t0IO[io],             mtauIntegrationX, mTimeBinWidth, 0, io};
  fgTimeShape0[io] = new TF1(Form("TimeShape%s;time[s];signal",Names[io]),
			     shapeEI,timeBinMin*mTimeBinWidth,timeBinMax*mTimeBinWidth,5);
  fgTimeShape0[io]->SetParNames("t0","tauI","width","norm","io");
  fgTimeShape0[io]->SetParameters(params0);
  params0[3] = fgTimeShape0[io]->Integral(0,timeBinMax*mTimeBinWidth);
  
  TF1 *mShaperResponses = new TF1(Form("ShaperFunc_%s_S%02i;time[buckets];signal",Names[io],sector),
				  shapeEI_I,timeBinMin,timeBinMax,5);  
  mShaperResponses->SetParameters(params0);
  mShaperResponses->SetParNames("t0","tauI", "width","norm","io");
  mShaperResponses->SetNpx((Int_t) (10*(timeBinMax-timeBinMin)));
  mShaperResponses->Save(timeBinMin,timeBinMax,0,0,0,0);
  return mShaperResponses;
}
