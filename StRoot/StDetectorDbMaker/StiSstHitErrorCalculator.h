#ifndef StiSstHitErrorCalculator_h
#define StiSstHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiSstHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiSstHitErrorCalculator* 	instance();
 protected:
  StiSstHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiSstHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiSstHitErrorCalculator* fgInstance;
  ClassDef(StiSstHitErrorCalculator,1) //C++ TChair for SstHitError table class
};
#endif
