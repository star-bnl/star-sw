#ifndef St_tpcPressureBC_h
#define St_tpcPressureBC_h
#include "St_tpcCorrectionC.h"
class St_tpcPressureBC : public St_tpcCorrectionC {
 public:
  static St_tpcPressureBC* 	instance();
 protected:
  St_tpcPressureBC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcPressureBC() {fgInstance = 0;}
 private:
  static St_tpcPressureBC* fgInstance;
  ClassDef(St_tpcPressureBC,1)
};
#endif
