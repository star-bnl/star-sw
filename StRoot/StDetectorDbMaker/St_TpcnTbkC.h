#ifndef St_TpcnTbkC_h
#define St_TpcnTbkC_h
#include "St_tpcCorrectionC.h"
class St_TpcnTbkC : public St_tpcCorrectionC {
 public:
  static St_TpcnTbkC* 	instance();
 protected:
  St_TpcnTbkC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcnTbkC() {fgInstance = 0;}
 private:
  static St_TpcnTbkC* fgInstance;
  ClassDef(St_TpcnTbkC,1)
};
#endif
