#ifndef StiIst2HitErrorCalculator_h
#define StiIst2HitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiIst2HitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiIst2HitErrorCalculator* 	instance();
 protected:
  StiIst2HitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiIst2HitErrorCalculator() {fgInstance = 0;}
 private:
  static StiIst2HitErrorCalculator* fgInstance;
  ClassDef(StiIst2HitErrorCalculator,1) //C++ TChair for Ist2HitError table class
};
#endif
