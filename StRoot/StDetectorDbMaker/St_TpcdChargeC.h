#ifndef St_TpcdChargeC_h
#define St_TpcdChargeC_h
#include "St_tpcCorrectionC.h"
class St_TpcdChargeC : public St_tpcCorrectionC {
 public:
  static St_TpcdChargeC* 	instance();
 protected:
  St_TpcdChargeC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdChargeC() {fgInstance = 0;}
 private:
  static St_TpcdChargeC* fgInstance;
  ClassDef(St_TpcdChargeC,1)
};
#endif
