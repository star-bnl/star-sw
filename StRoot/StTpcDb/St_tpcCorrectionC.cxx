#include "St_tpcCorrectionC.h"
ClassImp(St_tpcCorrectionC);
//________________________________________________________________________________
Double_t St_tpcCorrectionC::CalcCorrection(Int_t i,Double_t x) {
  Double_t Sum = 0;
  tpcCorrection_st *cor =  ((St_tpcCorrection *) Table())->GetTable() + i;
  if (cor) {
    Int_t N = TMath::Abs(cor->npar);
    Double_t X = x;
    if (cor->npar  < 0) X = TMath::Exp(x);
    if (N > 0) {
      if (cor->min < cor->max) {
	if (X < cor->min) X = cor->min;
	if (X > cor->max) X = cor->max;
      }
      if (N > 0) {
	Sum = cor->a[N-1];
	for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
      }
    }
  }
  return Sum;
}
//________________________________________________________________________________
Double_t St_tpcCorrectionC::SumSeries(Int_t i,Double_t x) {
  Double_t Sum = 0;
  tpcCorrection_st *cor =  ((St_tpcCorrection *) Table())->GetTable() + i;
  if (cor) {
    Int_t N = TMath::Abs(cor->npar);
    Double_t X = x - cor->min; 
    if (X > 0) {
      if (N > 0) {
	Sum = cor->a[N-1];
	for (int n = N-2; n>=0; n--) Sum = X*Sum + cor->a[n];
      }
    }
  }
  return Sum;
}
