#ifndef StiSvtHitErrorCalculator_h
#define StiSvtHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiSvtHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiSvtHitErrorCalculator* 	instance();
 protected:
  StiSvtHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiSvtHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiSvtHitErrorCalculator* fgInstance;
  ClassDef(StiSvtHitErrorCalculator,1) //C++ TChair for SvtHitError table class
};
#endif
