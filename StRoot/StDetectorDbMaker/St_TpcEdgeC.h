#ifndef St_TpcEdgeC_h
#define St_TpcEdgeC_h
#include "St_tpcCorrectionC.h"
class St_TpcEdgeC : public St_tpcCorrectionC {
 public:
  static St_TpcEdgeC* 	instance();
 protected:
  St_TpcEdgeC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcEdgeC() {fgInstance = 0;}
 private:
  static St_TpcEdgeC* fgInstance;
  ClassDef(St_TpcEdgeC,1)
};
#endif
