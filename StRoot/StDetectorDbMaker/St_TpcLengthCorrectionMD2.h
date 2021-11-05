#ifndef St_TpcLengthCorrectionMD2_h
#define St_TpcLengthCorrectionMD2_h
#include "St_MDFCorrectionC.h"
class St_TpcLengthCorrectionMD2 : public St_MDFCorrectionC {
 public:
  static St_TpcLengthCorrectionMD2* 	instance();
 protected:
  St_TpcLengthCorrectionMD2(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcLengthCorrectionMD2() {fgInstance = 0;}
 private:
  static St_TpcLengthCorrectionMD2* fgInstance;
  ClassDef(St_TpcLengthCorrectionMD2,1)
};
#endif
