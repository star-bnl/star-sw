#ifndef St_TpcTanLC_h
#define St_TpcTanLC_h
#include "St_tpcCorrectionC.h"
class St_TpcTanLC : public St_tpcCorrectionC {
 public:
  static St_TpcTanLC* 	instance();
 protected:
  St_TpcTanLC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcTanLC() {fgInstance = 0;}
 private:
  static St_TpcTanLC* fgInstance;
  ClassDef(St_TpcTanLC,1)
};
#endif
