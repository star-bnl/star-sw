#include "TMath.h"
#include "StBichsel/Bichsel.h"
static Bichsel *fBichsel = 0;
double dEdx_formula(double x,double massin){
  if (fBichsel==0) fBichsel = Bichsel::Instance();
  double real_momentum = x;
  double mass = massin;
  double bg2 = real_momentum*real_momentum/(mass*mass);
  double bg = ::sqrt(bg2);
  double result = 1.e-6*fBichsel->GetI70(TMath::Log10(bg),1.0);
  return result;
}
