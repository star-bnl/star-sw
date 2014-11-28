#ifndef St_tpcTimeBucketCorC_h
#define St_tpcTimeBucketCorC_h
#include "St_tpcCorrectionC.h"
class St_tpcTimeBucketCorC : public St_tpcCorrectionC {
 public:
  static St_tpcTimeBucketCorC* 	instance();
 protected:
  St_tpcTimeBucketCorC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcTimeBucketCorC() {fgInstance = 0;}
 private:
  static St_tpcTimeBucketCorC* fgInstance;
  ClassDef(St_tpcTimeBucketCorC,1)
};
#endif
