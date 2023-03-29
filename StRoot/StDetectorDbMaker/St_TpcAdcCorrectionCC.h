#ifndef St_TpcAdcCorrectionCC_h
#define St_TpcAdcCorrectionCC_h
#include "St_tpcCorrectionC.h"
class St_TpcAdcCorrectionCC : public St_tpcCorrectionC {
 public:
  static St_TpcAdcCorrectionCC* 	instance();
 protected:
  St_TpcAdcCorrectionCC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcAdcCorrectionCC() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrectionCC* fgInstance;
  ClassDef(St_TpcAdcCorrectionCC,1)
};
#endif
