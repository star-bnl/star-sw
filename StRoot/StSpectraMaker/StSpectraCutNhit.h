#ifndef StSpectraCutNhit_hh
#define StSpectraCutNhit_hh
#include "StEventTypes.h"
#include "StSpectraCut.h"

class StSpectraCutNhit : public StSpectraCut {

 private:

 protected:

 public:

  StSpectraCutNhit(double lowLimit, double highLimit);

  ~StSpectraCutNhit();
 
  bool satisfiesCut(StTrack* track, StEvent* event);

};

#endif
