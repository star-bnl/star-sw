#include <iostream.h>
#include <stdlib.h>

//StEvent
#include "StEventTypes.h"

//Sti
#include "StiHit.h"
#include "StiConstants.h"
#include "StiTrack.h"

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

ostream& operator<<(ostream& os, const StiTrack& track)
{
    return os <<"q: "<<track.getCharge()
	      <<" pt: "<<track.getPt()
	      <<" eta: "<<track.getPseudoRapidity()
	      <<" tanLambda: "<<track.getTanL()
	      <<" Chi2: "<<track.getChi2()
	      <<" points: "<<track.getPointCount()
	      <<" fitPoints: "<<track.getFitPointCount();
}
