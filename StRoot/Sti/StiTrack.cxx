#include <iostream.h>
#include <stdlib.h>

//StEvent
#include "StEventTypes.h"
#include "StiTrack.h"
#include "StiConstants.h"

StiTrack::StiTrack()
{
  reset();
}
 
StiTrack::~StiTrack()
{
}

void StiTrack::reset()
{
  q = 0;
  nPts = 0;
  nFitPts = 0;
  vertex = 0;
  status = StiConstants::Ok;
  m      = 0.139;
  chi2   = 0.;
}
