#ifndef StSpectraCutDCA_hh
#define StSpectraCutDCA_hh
#include "StEvent.h"
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
 
  bool satisfiesCut(const StGlobalTrack* track, const StEvent* event);

};

#endif
