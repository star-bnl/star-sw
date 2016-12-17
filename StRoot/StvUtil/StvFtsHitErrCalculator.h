#ifndef __StvFtsHitErrCalculatorulator_h_
#define __StvFtsHitErrCalculatorulator_h_
#include "StEvent/StEnumerations.h"
#include "StvUtil/StvHitErrCalculator.h"

class StvHit;

class StvFtsHitErrCalculator : public StvHitRrCalculator {

public:	
  StvFtsHitErrCalculator(const char *name="FtsHitErrs"):StvHitRrCalculator(name){};

protected:
ClassDef(StvFtsHitErrCalculator,0)
};

#endif
