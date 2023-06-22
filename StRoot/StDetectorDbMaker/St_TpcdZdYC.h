#ifndef St_TpcdZdYC_h
#define St_TpcdZdYC_h
#include "St_tpcCorrectionC.h"
class St_TpcdZdYC : public St_tpcCorrectionC {
 public:
  static St_TpcdZdYC* 	instance();
 protected:
  St_TpcdZdYC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdZdYC() {fgInstance = 0;}
 private:
  static St_TpcdZdYC* fgInstance;
  ClassDef(St_TpcdZdYC,1)
};
#endif
