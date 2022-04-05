#ifndef StiTPCHitErrorCalculator_h
#define StiTPCHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiTPCHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiTPCHitErrorCalculator* 	instance();
 protected:
  StiTPCHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiTPCHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiTPCHitErrorCalculator* fgInstance;
  ClassDef(StiTPCHitErrorCalculator,1) //C++ TChair for iTPC HitError table class
};
#endif
