#ifndef St_TpcAdcCorrectionMDF_h
#define St_TpcAdcCorrectionMDF_h
#include "St_MDFCorrectionC.h"
class St_TpcAdcCorrectionMDF : public St_MDFCorrectionC {
 public:
  static St_TpcAdcCorrectionMDF* 	instance();
 protected:
  St_TpcAdcCorrectionMDF(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcAdcCorrectionMDF() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrectionMDF* fgInstance;
  ClassDef(St_TpcAdcCorrectionMDF,1)
};
#endif
