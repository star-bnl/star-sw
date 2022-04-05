#ifndef St_TpcPadCorrectionMDF_h
#define St_TpcPadCorrectionMDF_h
#include "St_MDFCorrectionC.h"
class St_TpcPadCorrectionMDF : public St_MDFCorrectionC {
 public:
  static St_TpcPadCorrectionMDF* 	instance();
 protected:
  St_TpcPadCorrectionMDF(St_MDFCorrection *table=0) : St_MDFCorrectionC(table) {}
  virtual ~St_TpcPadCorrectionMDF() {fgInstance = 0;}
 private:
  static St_TpcPadCorrectionMDF* fgInstance;
  ClassDef(St_TpcPadCorrectionMDF,1)
};
#endif
