#ifndef St_TpcCurrentCorrectionC_h
#define St_TpcCurrentCorrectionC_h
#include "St_tpcCorrectionC.h"
class St_TpcCurrentCorrectionC : public St_tpcCorrectionC {
 public:
  static St_TpcCurrentCorrectionC* 	instance();
 protected:
  St_TpcCurrentCorrectionC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcCurrentCorrectionC() {fgInstance = 0;}
 private:
  static St_TpcCurrentCorrectionC* fgInstance;
  ClassDef(St_TpcCurrentCorrectionC,1)
};
#endif
