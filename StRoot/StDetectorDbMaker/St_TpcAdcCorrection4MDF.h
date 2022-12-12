#ifndef St_TpcAdcCorrection4MDF_h
#define St_TpcAdcCorrection4MDF_h
#include "St_MDFCorrection4C.h"
class St_TpcAdcCorrection4MDF : public St_MDFCorrection4C {
 public:
  static St_TpcAdcCorrection4MDF* 	instance();
 protected:
  St_TpcAdcCorrection4MDF(St_MDFCorrection4 *table=0) : St_MDFCorrection4C(table) {}
  virtual ~St_TpcAdcCorrection4MDF() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrection4MDF* fgInstance;
  ClassDef(St_TpcAdcCorrection4MDF,1)
};
#endif
