#ifndef St_TpcdEdxCorC_h
#define St_TpcdEdxCorC_h
#include "St_tpcCorrectionC.h"
class St_TpcdEdxCorC : public St_tpcCorrectionC {
 public:
  static St_TpcdEdxCorC* 	instance();
 protected:
  St_TpcdEdxCorC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdEdxCorC() {fgInstance = 0;}
 private:
  static St_TpcdEdxCorC* fgInstance;
  ClassDef(St_TpcdEdxCorC,1)
};
#endif
