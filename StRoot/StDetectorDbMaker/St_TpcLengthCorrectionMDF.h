#ifndef St_TpcLengthCorrectionMDF_h
#define St_TpcLengthCorrectionMDF_h
#include "St_MDFCorrectionC.h"
class St_TpcLengthCorrectionMDF : public St_MDFCorrectionC {
 public:
  static St_TpcLengthCorrectionMDF* 	instance();
 protected:
  St_TpcLengthCorrectionMDF(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcLengthCorrectionMDF() {fgInstance = 0;}
 private:
  static St_TpcLengthCorrectionMDF* fgInstance;
  ClassDef(St_TpcLengthCorrectionMDF,1)
};
#endif
