#ifndef St_TpcLengthCorrectionBC_h
#define St_TpcLengthCorrectionBC_h
#include "St_tpcCorrectionC.h"
class St_TpcLengthCorrectionBC : public St_tpcCorrectionC {
 public:
  static St_TpcLengthCorrectionBC* 	instance();
 protected:
  St_TpcLengthCorrectionBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcLengthCorrectionBC() {fgInstance = 0;}
 private:
  static St_TpcLengthCorrectionBC* fgInstance;
  ClassDef(St_TpcLengthCorrectionBC,1)
};
#endif
