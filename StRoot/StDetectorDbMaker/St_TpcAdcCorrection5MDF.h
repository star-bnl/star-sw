#ifndef St_TpcAdcCorrection5MDF_h
#define St_TpcAdcCorrection5MDF_h
#include "St_MDFCorrection4C.h"
class St_TpcAdcCorrection5MDF : public St_MDFCorrection4C {
 public:
  static St_TpcAdcCorrection5MDF* 	instance();
 protected:
  St_TpcAdcCorrection5MDF(St_MDFCorrection4 *table=0) : St_MDFCorrection4C(table) {}
  virtual ~St_TpcAdcCorrection5MDF() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrection5MDF* fgInstance;
  ClassDef(St_TpcAdcCorrection5MDF,1)
};
#endif
