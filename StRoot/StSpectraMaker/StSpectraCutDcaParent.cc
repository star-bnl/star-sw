#include "StSpectraCutDcaParent.h"

StSpectraCutDcaParent::StSpectraCutDcaParent(double lowLimit, double highLimit)
    : StSpectraCut(lowLimit, highLimit) {
}

StSpectraCutDcaParent::~StSpectraCutDcaParent() {
}

bool StSpectraCutDcaParent::satisfiesCut(StV0Vertex* v0, StEvent* event){
  float dca = v0->dcaParentToPrimaryVertex();
  if (dca > mLowLimit && dca < mHighLimit) { 
    return true;
  } else {
    return false;
  }
}

