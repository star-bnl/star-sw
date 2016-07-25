#include "TF1.h"
TF1 *f1 = 0;
Double_t Pol3(Double_t x) {
  Double_t params[4] = {
    2.09447e-02,
    9.40477e-02,
    -1.04169e-01,
    2.87285e-02};
  if (! f1) {
    f1 = new TF1("f1","pol3");
    f1->SetParameters(params);
  }
  return f1->Eval(x);
}
