#include "TF2.h"
Double_t dead(Double_t *x, Double_t *p = 0) {
  Double_t f_m = x[0];
  Double_t f_d = x[1];
  if (f_d <= f_m) return -1;
  return f_m/(1 - f_m/f_d);
}
TF2 *deadF() {
  /* <t_m> = <t> + tau;  tau is dead time, T is measurement time
     f_m = T/<t_m>; f = T/<t>; f_d T/tau;
     1/f_m = 1/f + 1/f_d;
     1/f = 1/f_m - 1/f_d;
     f = 1/(1/f_m - 1/f_d) = f_m*f_d/(f_d - f_m);
     f = f_m/(1 - f_d*f_m); f_m - measured frequency, 
  */
  return new TF2("deadF",dead,1,1e4,1,1e4);
}
