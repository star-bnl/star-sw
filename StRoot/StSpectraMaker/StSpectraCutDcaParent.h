#ifndef StSpectraCutDcaParent_hh
#define StSpectraCutDcaParent_hh
#include "StEventTypes.h"
#include "StSpectraCut.h"

class StSpectraCutDcaParent : public StSpectraCut {

 private:

 protected:

 public:

  StSpectraCutDcaParent(double lowLimit, double highLimit);

  ~StSpectraCutDcaParent();
 
  bool satisfiesCut(StV0Vertex* v0, StEvent* event);

};

#endif
