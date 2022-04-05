#include "TMath.h"
#include "StarClassLibrary/BetheBloch.h"

static BetheBloch* bb = 0;
double dEdx_formula(double x,double massin){
  if (bb==0) bb = new BetheBloch();
  double real_momentum = x;
  double mass = massin;
  double bg2 = real_momentum*real_momentum/(mass*mass);
  double bg = ::sqrt(bg2);
  //double result = bb->operator()(bg);
  double result = bb->Sirrf(bg);
  return result;
}

