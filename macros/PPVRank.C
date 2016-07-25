#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Rtypes.h"
#endif
//________________________________________________________________________________
Float_t PPVRank(Float_t Rank) {
  if (Rank > 0) Rank += - 1e6 + 2200;
  else          Rank +=   1e6 - 2200;
  Rank   =  Rank/30000.;
  return Rank;
}
