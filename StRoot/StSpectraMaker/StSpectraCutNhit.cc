#include "StSpectraCutNhit.h"

StSpectraCutNhit::StSpectraCutNhit(){

}
StSpectraCutNhit::StSpectraCutNhit(int lowLimit, int highLimit){
 mLowLimit = lowLimit;
 mHighLimit = highLimit;
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

