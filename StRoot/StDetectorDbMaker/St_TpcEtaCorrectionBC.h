#ifndef St_TpcEtaCorrectionBC_h
#define St_TpcEtaCorrectionBC_h
#include "St_tpcCorrectionC.h"
class St_TpcEtaCorrectionBC : public St_tpcCorrectionC {
 public:
  static St_TpcEtaCorrectionBC* 	instance();
 protected:
  St_TpcEtaCorrectionBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcEtaCorrectionBC() {fgInstance = 0;}
 private:
  static St_TpcEtaCorrectionBC* fgInstance;
  ClassDef(St_TpcEtaCorrectionBC,1)
};
#endif
