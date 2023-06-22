#ifndef St_TpcdXdYC_h
#define St_TpcdXdYC_h
#include "St_tpcCorrectionC.h"
class St_TpcdXdYC : public St_tpcCorrectionC {
 public:
  static St_TpcdXdYC* 	instance();
 protected:
  St_TpcdXdYC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdXdYC() {fgInstance = 0;}
 private:
  static St_TpcdXdYC* fgInstance;
  ClassDef(St_TpcdXdYC,1)
};
#endif
