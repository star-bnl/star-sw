#ifndef St_TpcSpaceChargeC_h
#define St_TpcSpaceChargeC_h
#include "St_tpcCorrectionC.h"
class St_TpcSpaceChargeC : public St_tpcCorrectionC {
 public:
  static St_TpcSpaceChargeC* 	instance();
 protected:
  St_TpcSpaceChargeC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcSpaceChargeC() {fgInstance = 0;}
 private:
  static St_TpcSpaceChargeC* fgInstance;
  ClassDef(St_TpcSpaceChargeC,1)
};
#endif
