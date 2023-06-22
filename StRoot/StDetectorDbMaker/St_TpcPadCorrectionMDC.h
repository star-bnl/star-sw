#ifndef St_TpcPadCorrectionMDC_h
#define St_TpcPadCorrectionMDC_h
#include "St_MDFCorrectionC.h"
class St_TpcPadCorrectionMDC : public St_MDFCorrectionC {
 public:
  static St_TpcPadCorrectionMDC* 	instance();
 protected:
  St_TpcPadCorrectionMDC(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcPadCorrectionMDC() {fgInstance = 0;}
 private:
  static St_TpcPadCorrectionMDC* fgInstance;
  ClassDef(St_TpcPadCorrectionMDC,1)
};
#endif
