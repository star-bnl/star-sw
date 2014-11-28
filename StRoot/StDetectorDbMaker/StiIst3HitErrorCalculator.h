#ifndef StiIst3HitErrorCalculator_h
#define StiIst3HitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiIst3HitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiIst3HitErrorCalculator* 	instance();
 protected:
  StiIst3HitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiIst3HitErrorCalculator() {fgInstance = 0;}
 private:
  static StiIst3HitErrorCalculator* fgInstance;
  ClassDef(StiIst3HitErrorCalculator,1) //C++ TChair for Ist3HitError table class
};
#endif
