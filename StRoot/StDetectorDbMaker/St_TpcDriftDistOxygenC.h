#ifndef St_TpcDriftDistOxygenC_h
#define St_TpcDriftDistOxygenC_h
#include "St_tpcCorrectionC.h"
class St_TpcDriftDistOxygenC : public St_tpcCorrectionC {
 public:
  static St_TpcDriftDistOxygenC* 	instance();
 protected:
  St_TpcDriftDistOxygenC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcDriftDistOxygenC() {fgInstance = 0;}
 private:
  static St_TpcDriftDistOxygenC* fgInstance;
  ClassDef(St_TpcDriftDistOxygenC,1)
};
#endif
