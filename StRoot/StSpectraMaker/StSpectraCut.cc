#include "StSpectraCut.h"

StSpectraCut::StSpectraCut(double lowLimit, double highLimit){

  mLowLimit = lowLimit;
  mHighLimit = highLimit;

  // place to construct diagnostics
}

bool StSpectraCut::satisfiesCut(StTrack* track, StEvent* event)
{
  return 0;
}

bool StSpectraCut::satisfiesCut(StV0Vertex* v0, StEvent* event)
{
  return 0;
}

