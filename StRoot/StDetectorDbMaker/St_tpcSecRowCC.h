#ifndef St_TpcSecRowCC_h
#define St_TpcSecRowCC_h
#include "St_TpcSecRowCorC.h"
class St_TpcSecRowCC : public St_TpcSecRowCorC {
 public:
  static St_TpcSecRowCC* 	instance();
 protected:
  St_TpcSecRowCC(St_TpcSecRowCor *table=0) : St_TpcSecRowCorC(table) {}
  virtual ~St_TpcSecRowCC() {fgInstance = 0;}
 private:
  static St_TpcSecRowCC* fgInstance;
  ClassDef(St_TpcSecRowCC,1)
};
#endif
