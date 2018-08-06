#ifndef St_tpcTimeDependenceC_h
#define St_tpcTimeDependenceC_h
#include "St_tpcCorrectionC.h"
class St_tpcTimeDependenceC : public St_tpcCorrectionC {
 public:
  static St_tpcTimeDependenceC* 	instance();
 protected:
  St_tpcTimeDependenceC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_tpcTimeDependenceC() {fgInstance = 0;}
 private:
  static St_tpcTimeDependenceC* fgInstance;
  ClassDef(St_tpcTimeDependenceC,1)
};
#endif
