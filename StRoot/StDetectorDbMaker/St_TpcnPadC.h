#ifndef St_TpcnPadC_h
#define St_TpcnPadC_h
#include "St_tpcCorrectionC.h"
class St_TpcnPadC : public St_tpcCorrectionC {
 public:
  static St_TpcnPadC* 	instance();
 protected:
  St_TpcnPadC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcnPadC() {fgInstance = 0;}
 private:
  static St_TpcnPadC* fgInstance;
  ClassDef(St_TpcnPadC,1)
};
#endif
