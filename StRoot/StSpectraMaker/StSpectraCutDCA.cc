#include "StSpectraCutDCA.h"

StSpectraCutDCA::StSpectraCutDCA(double lowLimit, double highLimit)
    : StSpectraCut(lowLimit, highLimit) {
}

StSpectraCutDCA::~StSpectraCutDCA() {
}

bool StSpectraCutDCA::satisfiesCut(StTrack* track, StEvent* event){
  StVertex* primvtx = event->primaryVertex();
  float dca = track->geometry()->helix().distance(primvtx->position());
  if (dca > mLowLimit && dca < mHighLimit) { 
    return true;
  } else {
    return false;
  }
}

