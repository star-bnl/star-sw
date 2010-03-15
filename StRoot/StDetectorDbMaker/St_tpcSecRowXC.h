#ifndef St_TpcSecRowXC_h
#define St_TpcSecRowXC_h
#include "St_TpcSecRowCorC.h"
class St_TpcSecRowXC : public St_TpcSecRowCorC {
 public:
  static St_TpcSecRowXC* 	instance();
 protected:
  St_TpcSecRowXC(St_TpcSecRowCor *table=0) : St_TpcSecRowCorC(table) {}
  virtual ~St_TpcSecRowXC() {fgInstance = 0;}
 private:
  static St_TpcSecRowXC* fgInstance;
  ClassDef(St_TpcSecRowXC,1)
};
#endif
