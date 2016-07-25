#include "TMath.h"
#include "Math/SpecFuncMathMore.h"
Double_t fei(Double_t t, Double_t t0, Double_t T) {
  static const Double_t xmaxt = 708.39641853226408;
  static const Double_t xmax  = xmaxt - TMath::Log(xmaxt);
  Double_t t01 = xmax, t11 = xmax;
  if (T > 0) {t11 = (t+t0)/T;}
  if (t11 > xmax) t11 = xmax;
  if (T > 0) {t01 = t0/T;}
  if (t01 > xmax) t01  = xmax;
  return TMath::Exp(-t11)*(ROOT::Math::expint(t11) - ROOT::Math::expint(t01));
}
//________________________________________________________________________________
Double_t fei(Double_t *x, Double_t *p) {
  Double_t t  = x[0];
  if (t <= 0) return 0;
  Double_t t0 = p[0];
  Double_t T1 = p[1];
  Double_t T2 = p[2];
  /*
   i(t) = 1/(t + t0); 
   1. T2 = 0 => h(t) = 1 - exp(-t/T1); 
   */
  if (TMath::Abs((T1-T2)/(T1+T2)) < 1e-7) {
    return TMath::Max(0.,(t + t0)/T1*fei(t,t0,T1) + TMath::Exp(-t/T1) - 1);
  } 
  if (T2 <= 0) return fei(t,t0,T1);
  if (T1 <= 0) return 0;
  return T1/(T1 - T2)*(fei(t,t0,T1) - fei(t,t0,T2));
}
