#include "StSpectraCutDCA.h"

StSpectraCutDCA::StSpectraCutDCA(){

}
StSpectraCutDCA::StSpectraCutDCA(double lowLimit, double highLimit){
 mLowLimit = lowLimit;
 mHighLimit = highLimit;
}

StSpectraCutDCA::~StSpectraCutDCA() {
}

bool StSpectraCutDCA::satisfiesCut(const StGlobalTrack* track, const StEvent* event){
  StVertex* primvtx = event->primaryVertex();

  float dca = track->helix().distance(primvtx->position());
  if (dca > mLowLimit && dca < mHighLimit) { 
    return true;
  } else {
    return false;
  }
}

