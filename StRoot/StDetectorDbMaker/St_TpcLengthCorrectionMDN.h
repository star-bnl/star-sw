#ifndef St_TpcLengthCorrectionMDN_h
#define St_TpcLengthCorrectionMDN_h
#include "St_MDFCorrectionC.h"
class St_TpcLengthCorrectionMDN : public St_MDFCorrectionC {
 public:
  static St_TpcLengthCorrectionMDN* 	instance();
 protected:
  St_TpcLengthCorrectionMDN(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcLengthCorrectionMDN() {fgInstance = 0;}
 private:
  static St_TpcLengthCorrectionMDN* fgInstance;
  ClassDef(St_TpcLengthCorrectionMDN,1)
};
#endif
