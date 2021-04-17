#include "TStyle.h"
/*
     mode = ksiourmen  (default = 000001111)
      k = 1;  kurtosis printed
      k = 2;  kurtosis and kurtosis error printed
      s = 1;  skewness printed
      s = 2;  skewness and skewness error printed
      i = 1;  integral of bins printed
      o = 1;  number of overflows printed
      u = 1;  number of underflows printed
      r = 1;  rms printed
      r = 2;  rms and rms error printed
      m = 1;  mean value printed
      m = 2;  mean and mean error values printed
      e = 1;  number of entries printed
      n = 1;  name of histogram is printed
*/
//                       ourmen
void SetStats(Int_t mode=111111) {
  //void SetStats(Int_t mode=112211) {
  gStyle->SetOptStat(mode);
}
