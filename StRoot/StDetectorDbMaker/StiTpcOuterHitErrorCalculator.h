#ifndef StiTpcOuterHitErrorCalculator_h
#define StiTpcOuterHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiTpcOuterHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiTpcOuterHitErrorCalculator* 	instance();
 protected:
  StiTpcOuterHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiTpcOuterHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiTpcOuterHitErrorCalculator* fgInstance;
  ClassDef(StiTpcOuterHitErrorCalculator,1) //C++ TChair for tpcOuterHitError table class
};
#endif
