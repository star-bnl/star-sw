#ifndef St_tpcMethaneInC_h
#define St_tpcMethaneInC_h
#include "St_tpcCorrectionC.h"
class St_tpcMethaneInC : public St_tpcCorrectionC {
 public:
  static St_tpcMethaneInC* 	instance();
 protected:
  St_tpcMethaneInC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcMethaneInC() {fgInstance = 0;}
 private:
  static St_tpcMethaneInC* fgInstance;
  ClassDef(St_tpcMethaneInC,1)
};
#endif
