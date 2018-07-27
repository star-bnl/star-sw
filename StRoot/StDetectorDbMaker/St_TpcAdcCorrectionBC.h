#ifndef St_TpcAdcCorrectionBC_h
#define St_TpcAdcCorrectionBC_h
#include "St_tpcCorrectionC.h"
class St_TpcAdcCorrectionBC : public St_tpcCorrectionC {
 public:
  static St_TpcAdcCorrectionBC* 	instance();
 protected:
  St_TpcAdcCorrectionBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcAdcCorrectionBC() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrectionBC* fgInstance;
  ClassDef(St_TpcAdcCorrectionBC,1)
};
#endif
