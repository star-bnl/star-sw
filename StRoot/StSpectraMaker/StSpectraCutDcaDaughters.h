#ifndef StSpectraCutDcaDaughters_hh
#define StSpectraCutDcaDaughters_hh
#include "StEventTypes.h"
#include "StSpectraCut.h"

class StSpectraCutDcaDaughters : public StSpectraCut {

 private:

 protected:

 public:

  StSpectraCutDcaDaughters(double lowLimit, double highLimit);

  ~StSpectraCutDcaDaughters();
 
  bool satisfiesCut(StV0Vertex* v0, StEvent* event);

};

#endif
