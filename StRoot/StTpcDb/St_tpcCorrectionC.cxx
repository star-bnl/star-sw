#include "St_tpcCorrectionC.h"
#include "TMath.h"
ClassImp(St_tpcCorrectionC);
//________________________________________________________________________________
Double_t St_tpcCorrectionC::CalcCorrection(Int_t i, Double_t x) {
  tpcCorrection_st *cor =  ((St_tpcCorrection *) Table())->GetTable() + i;
  return SumSeries(cor, x);
}
//________________________________________________________________________________
Double_t St_tpcCorrectionC::SumSeries(tpcCorrection_st *cor,  Double_t x) {
  Double_t Sum = 0;
  if (! cor) return Sum;
  Int_t N = TMath::Abs(cor->npar)%100;
  if (N == 0) return Sum;
  static Double_t T0, T1, T2;
  // parameterization variable
  Double_t X;
  if (cor->npar  < 0) X = TMath::Exp(x);
  else {
    switch  (cor->type) {
    case 10:// ADC correction offset + poly for ADC
      X = TMath::Log(x);      break;
    case 1: // Tchebyshev [-1,1] 
      if (cor->min < cor->max)   X = -1 + 2*TMath::Max(0.,TMath::Min(1.,(X - cor->min)/( cor->max - cor->min)));
      break;
    case 2: // Shifted TChebyshev [0,1]
      if (cor->min < cor->max)   X = TMath::Max(0.,TMath::Min(1.,(X - cor->min)/( cor->max - cor->min)));
      break;
    case 3: 
      if (TMath::Abs(TMath::Abs(x) - 1) < 1.e-7) X = 0;
      else                                       X = TMath::Log(1. - TMath::Abs(x));
      break;
    case 4:
      if (TMath::Abs(TMath::Abs(x) - 1) < 1.e-7) X = 0;
      else                                       X = TMath::Sign(TMath::Log(1. - TMath::Abs(x)),x);
      break;
    default:      X = x;    break;
    }
  }
  if (cor->type != 1 && cor->type != 2 &&
      cor->min < cor->max) {
    if (X < cor->min) X = cor->min;
    if (X > cor->max) X = cor->max;
  }
  switch (cor->type) {
  case 1: // Tchebyshev [-1,1] 
    T0 = 1;
    Sum = cor->a[0]*T0;
    if (N == 1) break;
    T1 = X;
    Sum += cor->a[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*X*T1 - T0;
      Sum += cor->a[n]*T2;
      T0 = T1;
      T1 = T2;
    }
    break;
  case 2: // Shifted TChebyshev [0,1]
    T0 = 1;
    Sum = cor->a[0]*T0;
    if (N == 1) break;
    T1 = 2*X - 1;
    Sum += cor->a[1]*T1;
    for (int n = 2; n <= N; n++) {
      T2 = 2*(2*X - 1)*T1 - T0;
      Sum += cor->a[n]*T2;
      T0 = T1;
      T1 = T2;
    }
    break;
  case 10: // ADC correction offset + poly for ADC
    Sum = cor->a[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
    Sum += TMath::Log(1. + cor->OffSet/x);
    Sum  = TMath::Exp(Sum);
    Sum *= x;
    break;
  default: // polynomials
    Sum = cor->a[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
    break;
  }
  return Sum;
}
