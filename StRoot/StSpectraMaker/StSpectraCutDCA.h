#ifndef StSpectraCutDCA_hh
#define StSpectraCutDCA_hh
#include "StEventTypes.h"
#include "StSpectraCut.h"

class StSpectraCutDCA : public StSpectraCut {

 private:
 double mLowLimit;
 double mHighLimit;

 protected:

 public:
  StSpectraCutDCA();
  StSpectraCutDCA(double lowLimit, double highLimit);

  ~StSpectraCutDCA();
 
  bool satisfiesCut(StTrack* track,StEvent* event);

};

#endif
