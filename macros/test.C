#include "Riostream.h"
#include "Rtypes.h"
#include "TMath.h"
#include "IOSectorPar.h"
void test() {
  const Int_t NP = sizeof(Passes)/sizeof(SurveyPass_t);
  for (Int_t k = 0; k < NP; k++) {
    Passes[k].Print();
  }
}
