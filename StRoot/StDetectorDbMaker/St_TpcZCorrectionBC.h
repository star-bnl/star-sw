#ifndef St_TpcZCorrectionBC_h
#define St_TpcZCorrectionBC_h
#include "St_tpcCorrectionC.h"
class St_TpcZCorrectionBC : public St_tpcCorrectionC {
 public:
  static St_TpcZCorrectionBC* 	instance();
 protected:
  St_TpcZCorrectionBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcZCorrectionBC() {fgInstance = 0;}
 private:
  static St_TpcZCorrectionBC* fgInstance;
  ClassDef(St_TpcZCorrectionBC,1)
};
#endif
