#include <string.h>
#include <math.h>
#define __NO_TGEO__
#include "StarVMC/Rotations/Rotations.cxx"
static int n = 5;
extern "C" int itorotm_(float r[6], char s[5], int nc) {
  int nrotm = fgRotations.size();
  if (! nrotm) {
    rotm_t::Rotations();
    nrotm = fgRotations.size(); //std::cout << "Total no. of rotaion matrices  = " << nrotm << std::endl;
  }
  static const char name[5] = "rotm";
  nc = n;
  for (int i = 0; i < nrotm; i++) {
    rotm_t &rotm =  fgRotations[i];
    float *rset = &rotm.Thet1;
    for (int j = 0; j < 6; j++) {
      if (fabs(r[j] - rset[j])>1.e-7) goto NEXT;
    }
    memcpy(s,rotm.name,n); return i+1;
  NEXT:
    continue;
  }
  memcpy(s,name,n);
  return 0;
}
