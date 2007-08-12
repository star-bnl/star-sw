//
//  StBemcTriggerDbThresholds.h,v 0.01
//
#ifndef STAR_StBemcTriggerDbThresholds
#define STAR_StBemcTriggerDbThresholds

#include "StMessMgr.h"

class StBemcTriggerDbThresholds {
 private:



 public:

  StBemcTriggerDbThresholds();
  virtual     ~StBemcTriggerDbThresholds();

  Int_t GetHtFEEbitOffset(int);

  ClassDef(StBemcTriggerDbThresholds, 1)
 };


#endif
