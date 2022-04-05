#ifndef St_TpcDriftVelRowCorC_h
#define St_TpcDriftVelRowCorC_h
#include "St_tpcCorrectionC.h"
class St_TpcDriftVelRowCorC : public St_tpcCorrectionC {
 public:
  static St_TpcDriftVelRowCorC* 	instance();
 protected:
  St_TpcDriftVelRowCorC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcDriftVelRowCorC() {fgInstance = 0;}
 private:
  static St_TpcDriftVelRowCorC* fgInstance;
  ClassDef(St_TpcDriftVelRowCorC,1)
};
#endif
