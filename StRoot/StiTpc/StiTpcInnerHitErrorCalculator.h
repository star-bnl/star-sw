#ifndef StiTpcInnerHitErrorCalculator_h
#define StiTpcInnerHitErrorCalculator_h
#include "Sti/StiHitErrorCalculator.h"
class StiTpcInnerHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiTpcInnerHitErrorCalculator* 	instance();
 protected:
  StiTpcInnerHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiTpcInnerHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiTpcInnerHitErrorCalculator* fgInstance;
  ClassDef(StiTpcInnerHitErrorCalculator,1) //C++ TChair for tpcInnerHitError table class
};
#endif
