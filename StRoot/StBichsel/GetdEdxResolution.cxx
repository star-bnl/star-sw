#include "Bichsel.h"
#include "TMath.h"
#include "tpcCorrection.h"
Double_t Bichsel::GetdEdxResolution(Int_t k, Double_t TrackLengthInTPC) {
  static tpcCorrection_st params[2] = {
    //type,nrow,index, N OffSet min   max  a[10]
    {0,1,2,4, 0, 2.3, 4.79, {1.82545e+00,-1.13077e+00, 2.49432e-01,-1.89606e-02,           0,           0,0,0,0,0}}, //run II
    {0,2,2,6, 0, 2.5, 5.00, {4.57033e+00,-5.57244e+00, 2.77197e+00,-6.78880e-01, 8.11349e-02,-3.77213e-03,0,0,0,0}}  //run III
  };
  if (TrackLengthInTPC <= 0.0 || k < 0 || k > 1) return -999;
  Double_t X = TMath::Log(TrackLengthInTPC);
  return CalcCorrection(&params[k], X);
}//________________________________________________________________________________
Double_t Bichsel::CalcCorrection(const tpcCorrection_st *cor,const Double_t x) {
  Int_t N = TMath::Abs(cor->npar);
  Double_t X = x;
  if (cor->npar  < 0) X = TMath::Exp(x);
  if (N > 0) {
    if (cor->min < cor->max) {
      if (X < cor->min) X = cor->min;
      if (X > cor->max) X = cor->max;
    }
    return SumSeries(X,N,&cor->a[0]);
  }
  else return 0;
}
//________________________________________________________________________________
Double_t Bichsel::SumSeries(const Double_t &X,const Int_t &N,const Double_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}
