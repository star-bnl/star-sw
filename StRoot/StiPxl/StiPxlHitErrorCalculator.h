#ifndef StiPxlHitErrorCalculator_h
#define StiPxlHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiPxlHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiPxlHitErrorCalculator* 	instance();
 protected:
  StiPxlHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiPxlHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiPxlHitErrorCalculator* fgInstance;
  ClassDef(StiPxlHitErrorCalculator,1) //C++ TChair for PixelHitError table class
};
#endif
