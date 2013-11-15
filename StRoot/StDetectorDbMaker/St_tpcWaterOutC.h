#ifndef St_tpcWaterOutC_h
#define St_tpcWaterOutC_h
#include "St_tpcCorrectionC.h"
class St_tpcWaterOutC : public St_tpcCorrectionC {
 public:
  static St_tpcWaterOutC* 	instance();
 protected:
  St_tpcWaterOutC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcWaterOutC() {fgInstance = 0;}
 private:
  static St_tpcWaterOutC* fgInstance;
  ClassDef(St_tpcWaterOutC,1)
};
#endif
