#ifndef StiSsdHitErrorCalculator_h
#define StiSsdHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiSsdHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiSsdHitErrorCalculator* 	instance();
 protected:
  StiSsdHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiSsdHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiSsdHitErrorCalculator* fgInstance;
  ClassDef(StiSsdHitErrorCalculator,1) //C++ TChair for SsdHitError table class
};
#endif
