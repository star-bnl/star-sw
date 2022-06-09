#ifndef St_TpcAdcCorrection6MDF_h
#define St_TpcAdcCorrection6MDF_h
#include "St_MDFCorrection4C.h"
class St_TpcAdcCorrection6MDF : public St_MDFCorrection4C {
 public:
  static St_TpcAdcCorrection6MDF* 	instance();
 protected:
  St_TpcAdcCorrection6MDF(St_MDFCorrection4 *table=0) : St_MDFCorrection4C(table) {}
  virtual ~St_TpcAdcCorrection6MDF() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrection6MDF* fgInstance;
  ClassDef(St_TpcAdcCorrection6MDF,1)
};
#endif
