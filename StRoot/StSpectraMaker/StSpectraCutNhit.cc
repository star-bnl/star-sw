#include "StSpectraCutNhit.h"

StSpectraCutNhit::StSpectraCutNhit(double lowLimit, double highLimit) 
   : StSpectraCut(lowLimit, highLimit) {
}

StSpectraCutNhit::~StSpectraCutNhit() {
}

bool StSpectraCutNhit::satisfiesCut(StTrack* track, StEvent* event){
  int nhit = track->fitTraits().numberOfFitPoints();
  if (nhit > mLowLimit && nhit < mHighLimit) { 
    return true;
  } else {
    return false;
  }
}

