#include <iostream.h>
#include <stdlib.h>

//StEvent
#include "StEventTypes.h"

//Sti
#include "StiHit.h"
#include "StiConstants.h"
#include "StiTrack.h"
#include "StiTrackFitter.h"
StiTrackFitter * StiTrack::trackFitter = 0;

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
  mSeedHitCount = 0;
  vertex = 0;
  m      = -1.;
  chi2   = 0.;
}

void StiTrack::fit() //throw (Exception)
{
  trackFitter->fit(this);
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

void StiTrack::setTrackFitter(StiTrackFitter * fitter)
{
	trackFitter = fitter;
}

StiTrackFitter * StiTrack::getTrackFitter()
{
	return trackFitter;
}
