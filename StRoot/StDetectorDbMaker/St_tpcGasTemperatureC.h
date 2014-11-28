#ifndef St_tpcGasTemperatureC_h
#define St_tpcGasTemperatureC_h
#include "St_tpcCorrectionC.h"
class St_tpcGasTemperatureC : public St_tpcCorrectionC {
 public:
  static St_tpcGasTemperatureC* 	instance();
 protected:
  St_tpcGasTemperatureC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcGasTemperatureC() {fgInstance = 0;}
 private:
  static St_tpcGasTemperatureC* fgInstance;
  ClassDef(St_tpcGasTemperatureC,1)
};
#endif
