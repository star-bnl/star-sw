#ifndef St_TpcrChargeC_h
#define St_TpcrChargeC_h
#include "St_tpcCorrectionC.h"
class St_TpcrChargeC : public St_tpcCorrectionC {
 public:
  static St_TpcrChargeC* 	instance();
 protected:
  St_TpcrChargeC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcrChargeC() {fgInstance = 0;}
 private:
  static St_TpcrChargeC* fgInstance;
  ClassDef(St_TpcrChargeC,1)
};
#endif
