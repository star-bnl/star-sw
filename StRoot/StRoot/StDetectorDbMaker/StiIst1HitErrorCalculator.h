#ifndef StiIst1HitErrorCalculator_h
#define StiIst1HitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiIst1HitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiIst1HitErrorCalculator* 	instance();
 protected:
  StiIst1HitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiIst1HitErrorCalculator() {fgInstance = 0;}
 private:
  static StiIst1HitErrorCalculator* fgInstance;
  ClassDef(StiIst1HitErrorCalculator,1) //C++ TChair for Ist1HitError table class
};
#endif
