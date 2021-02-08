#ifndef St_TpcSecRowBC_h
#define St_TpcSecRowBC_h
#include "St_TpcSecRowCorC.h"
class St_TpcSecRowBC : public St_TpcSecRowCorC {
 public:
  static St_TpcSecRowBC* 	instance();
 protected:
  St_TpcSecRowBC(St_TpcSecRowCor *table=0) : St_TpcSecRowCorC(table) {}
  virtual ~St_TpcSecRowBC() {fgInstance = 0;}
 private:
  static St_TpcSecRowBC* fgInstance;
  ClassDef(St_TpcSecRowBC,1)
};
#endif
