#ifndef St_TpcRowQC_h
#define St_TpcRowQC_h
#include "St_tpcCorrectionC.h"
class St_TpcRowQC : public St_tpcCorrectionC {
 public:
  static St_TpcRowQC* 	instance();
 protected:
  St_TpcRowQC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcRowQC() {fgInstance = 0;}
 private:
  static St_TpcRowQC* fgInstance;
  ClassDef(St_TpcRowQC,1)
};
#endif
