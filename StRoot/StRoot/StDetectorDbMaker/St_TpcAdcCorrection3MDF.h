#ifndef St_TpcAdcCorrection3MDF_h
#define St_TpcAdcCorrection3MDF_h
#include "St_MDFCorrection3C.h"
class St_TpcAdcCorrection3MDF : public St_MDFCorrection3C {
 public:
  static St_TpcAdcCorrection3MDF* 	instance();
 protected:
  St_TpcAdcCorrection3MDF(St_MDFCorrection3 *table=0) : St_MDFCorrection3C(table) {}
  virtual ~St_TpcAdcCorrection3MDF() {fgInstance = 0;}
 private:
  static St_TpcAdcCorrection3MDF* fgInstance;
  ClassDef(St_TpcAdcCorrection3MDF,1)
};
#endif
