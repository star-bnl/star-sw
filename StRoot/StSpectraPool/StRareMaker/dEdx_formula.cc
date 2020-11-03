#include "TMath.h"
#if 0
#include "StarClassLibrary/BetheBloch.h"
static BetheBloch* bb = 0;
#else
#include "StBichsel/Bichsel.h"
#include "TMath.h"
#endif
double dEdx_formula(double x,double massin){
#if 0
  if (bb==0) bb = new BetheBloch();
#endif
  double real_momentum = x;
  double mass = massin;
  double bg2 = real_momentum*real_momentum/(mass*mass);
  double bg = ::sqrt(bg2);
#if 0
  //double result = bb->operator()(bg);
  double result = bb->Sirrf(bg);
  return result;
#else
  return 1e-6*Bichsel::Instance()->GetI70M(TMath::Log10(bg)); 
#endif
}

