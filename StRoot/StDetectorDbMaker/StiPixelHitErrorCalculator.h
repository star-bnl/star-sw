#ifndef StiPixelHitErrorCalculator_h
#define StiPixelHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiPixelHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiPixelHitErrorCalculator* 	instance();
 protected:
  StiPixelHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiPixelHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiPixelHitErrorCalculator* fgInstance;
  ClassDef(StiPixelHitErrorCalculator,1) //C++ TChair for PixelHitError table class
};
#endif
