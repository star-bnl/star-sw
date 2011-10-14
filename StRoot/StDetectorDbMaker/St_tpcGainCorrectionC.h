#ifndef St_tpcGainCorrectionC_h
#define St_tpcGainCorrectionC_h
#include "St_tpcCorrectionC.h"
class St_tpcGainCorrectionC : public St_tpcCorrectionC {
 public:
  static St_tpcGainCorrectionC* 	instance();
 protected:
  St_tpcGainCorrectionC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcGainCorrectionC() {fgInstance = 0;}
 private:
  static St_tpcGainCorrectionC* fgInstance;
  ClassDef(St_tpcGainCorrectionC,1)
};
#endif
