#ifndef StiBTofHitErrorCalculator_h
#define StiBTofHitErrorCalculator_h
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
class StiBTofHitErrorCalculator : public StiHitErrorCalculator {
 public:
  static StiBTofHitErrorCalculator* 	instance();
 protected:
  StiBTofHitErrorCalculator(St_HitError *table=0) : StiHitErrorCalculator(table) {}
  virtual ~StiBTofHitErrorCalculator() {fgInstance = 0;}
 private:
  static StiBTofHitErrorCalculator* fgInstance;
  ClassDef(StiBTofHitErrorCalculator,1) //C++ TChair for BTofHitError table class
};
#endif
