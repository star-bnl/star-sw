#include "StSpectraCutDcaDaughters.h"

StSpectraCutDcaDaughters::StSpectraCutDcaDaughters(double lowLimit, double highLimit)
    : StSpectraCut(lowLimit, highLimit) {
}

StSpectraCutDcaDaughters::~StSpectraCutDcaDaughters() {
}

bool StSpectraCutDcaDaughters::satisfiesCut(StV0Vertex* v0, StEvent* event){
  float dca = v0->dcaDaughters();
  if (dca > mLowLimit && dca < mHighLimit) { 
    return true;
  } else {
    return false;
  }
}

