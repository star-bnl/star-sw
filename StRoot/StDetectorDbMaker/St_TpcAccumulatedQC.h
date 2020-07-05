#ifndef St_TpcAccumulatedQC_h
#define St_TpcAccumulatedQC_h
#include "St_tpcCorrect192C.h"
class St_TpcAccumulatedQC : public St_tpcCorrect192C {
 public:
  static St_TpcAccumulatedQC* 	instance();
 protected:
  St_TpcAccumulatedQC(St_tpcCorrect192 *table=0) : St_tpcCorrect192C(table) {}
  virtual ~St_TpcAccumulatedQC() {fgInstance = 0;}
 private:
  static St_TpcAccumulatedQC* fgInstance;
  ClassDef(St_TpcAccumulatedQC,1)
};
#endif
