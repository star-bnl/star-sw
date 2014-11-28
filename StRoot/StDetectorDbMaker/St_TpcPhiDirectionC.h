#ifndef St_TpcPhiDirectionC_h
#define St_TpcPhiDirectionC_h
#include "St_tpcCorrectionC.h"
class St_TpcPhiDirectionC : public St_tpcCorrectionC {
 public:
  static St_TpcPhiDirectionC* 	instance();
 protected:
  St_TpcPhiDirectionC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcPhiDirectionC() {fgInstance = 0;}
 private:
  static St_TpcPhiDirectionC* fgInstance;
  ClassDef(St_TpcPhiDirectionC,1)
};
#endif
