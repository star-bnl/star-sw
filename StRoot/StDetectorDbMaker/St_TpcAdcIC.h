#ifndef St_TpcAdcIC_h
#define St_TpcAdcIC_h
#include "St_tpcCorrectionC.h"
class St_TpcAdcIC : public St_tpcCorrectionC {
 public:
  static St_TpcAdcIC* 	instance();
 protected:
  St_TpcAdcIC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcAdcIC() {fgInstance = 0;}
 private:
  static St_TpcAdcIC* fgInstance;
  ClassDef(St_TpcAdcIC,1)
};
#endif
