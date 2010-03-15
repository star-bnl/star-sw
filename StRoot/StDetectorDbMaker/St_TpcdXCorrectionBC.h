#ifndef St_TpcdXCorrectionBC_h
#define St_TpcdXCorrectionBC_h
#include "St_tpcCorrectionC.h"
class St_TpcdXCorrectionBC : public St_tpcCorrectionC {
 public:
  static St_TpcdXCorrectionBC* 	instance();
 protected:
  St_TpcdXCorrectionBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdXCorrectionBC() {fgInstance = 0;}
 private:
  static St_TpcdXCorrectionBC* fgInstance;
  ClassDef(St_TpcdXCorrectionBC,1)
};
#endif
