#ifndef StSpectraCutNhit_hh
#define StSpectraCutNhit_hh
#include "StEvent.h"
#include "StSpectraCut.h"

class StSpectraCutNhit : public StSpectraCut {

 private:
 int mLowLimit;
 int mHighLimit;

 protected:

 public:
  StSpectraCutNhit();
  StSpectraCutNhit(int lowLimit, int highLimit);

  ~StSpectraCutNhit();
 
  bool satisfiesCut(const StGlobalTrack* track, const StEvent* event);

};

#endif
