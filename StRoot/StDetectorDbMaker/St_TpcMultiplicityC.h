#ifndef St_TpcMultiplicityC_h
#define St_TpcMultiplicityC_h
#include "St_tpcCorrectionC.h"
class St_TpcMultiplicityC : public St_tpcCorrectionC {
 public:
  static St_TpcMultiplicityC* 	instance();
 protected:
  St_TpcMultiplicityC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcMultiplicityC() {fgInstance = 0;}
 private:
  static St_TpcMultiplicityC* fgInstance;
  ClassDef(St_TpcMultiplicityC,1)
};
#endif
