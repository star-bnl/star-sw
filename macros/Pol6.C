#include "Rtypes.h"
#include "TF1.h"
Double_t Pol6(Double_t x) {
  static TF1 *f = 0;
  if (! f) {
    f = new TF1("p6","pol6(0)",-1.5,1.5);
    Double_t pars[7] = {6.28286e-01, -5.95004e-02, -1.54229e-01, -9.16618e-02,  5.87187e-01,  3.66580e-02, -1.45838e-01};
    f->SetParameters(pars);
  }
  return f->Eval(x);
}
