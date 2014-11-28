#ifndef St_TpcZDCC_h
#define St_TpcZDCC_h
#include "St_tpcCorrectionC.h"
class St_TpcZDCC : public St_tpcCorrectionC {
 public:
  static St_TpcZDCC* 	instance();
 protected:
  St_TpcZDCC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcZDCC() {fgInstance = 0;}
 private:
  static St_TpcZDCC* fgInstance;
  ClassDef(St_TpcZDCC,1)
};
#endif
