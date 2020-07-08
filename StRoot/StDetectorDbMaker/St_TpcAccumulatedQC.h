#ifndef St_TpcAccumulatedQC_h
#define St_TpcAccumulatedQC_h
#include "St_tpcCorrectionC.h"
class St_TpcAccumulatedQC : public St_tpcCorrectionC {
 public:
  static St_TpcAccumulatedQC* 	instance();
 protected:
  St_TpcAccumulatedQC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcAccumulatedQC() {fgInstance = 0;}
 private:
  static St_TpcAccumulatedQC* fgInstance;
  ClassDef(St_TpcAccumulatedQC,1)
};
#endif
