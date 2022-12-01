#ifndef St_TpcEtaCorrectionC_h
#define St_TpcEtaCorrectionC_h
#include "St_tpcCorrectionC.h"
class St_TpcEtaCorrectionC : public St_tpcCorrectionC {
 public:
  static St_TpcEtaCorrectionC* 	instance();
 protected:
  St_TpcEtaCorrectionC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcEtaCorrectionC() {fgInstance = 0;}
 private:
  static St_TpcEtaCorrectionC* fgInstance;
  ClassDef(St_TpcEtaCorrectionC,1)
};
#endif
