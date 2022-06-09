#ifndef St_TpcPadPedRMSC_h
#define St_TpcPadPedRMSC_h
#include "St_tpcCorrectionC.h"
class St_TpcPadPedRMSC : public St_tpcCorrectionC {
 public:
  static St_TpcPadPedRMSC* 	instance();
 protected:
  St_TpcPadPedRMSC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcPadPedRMSC() {fgInstance = 0;}
 private:
  static St_TpcPadPedRMSC* fgInstance;
  ClassDef(St_TpcPadPedRMSC,1)
};
#endif
