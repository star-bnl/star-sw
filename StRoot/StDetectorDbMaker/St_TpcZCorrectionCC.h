#ifndef St_TpcZCorrectionCC_h
#define St_TpcZCorrectionCC_h
#include "St_tpcCorrectionC.h"
class St_TpcZCorrectionCC : public St_tpcCorrectionC {
 public:
  static St_TpcZCorrectionCC* 	instance();
 protected:
  St_TpcZCorrectionCC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcZCorrectionCC() {fgInstance = 0;}
 private:
  static St_TpcZCorrectionCC* fgInstance;
  ClassDef(St_TpcZCorrectionCC,1)
};
#endif
