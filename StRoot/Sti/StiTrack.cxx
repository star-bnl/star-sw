#include <iostream.h>
#include <stdexcept>
#include <stdlib.h>

//StEvent
#include "StEventTypes.h"

//Sti
#include "StiHit.h"
#include "StiConstants.h"
#include "StiTrack.h"
#include "StiTrackFitter.h"

StiTrackFinder * StiTrack::trackFinder = 0;
StiTrackFitter * StiTrack::trackFitter = 0;

ostream& operator<<(ostream& os, const StiTrack& track)
{
  try 
    {
      os <<" Chi2: "<<track.getChi2()
	 <<" q: "<<track.getCharge()
	 <<" pt: "<<track.getPt()
	 <<" eta: "<<track.getPseudoRapidity()
	 <<" tanLambda: "<<track.getTanL()
	 <<" points/fit/max: "<<track.getPointCount()
	 <<"/"<<track.getFitPointCount()
	 <<"/"<<track.getMaxPointCount()<<endl;
    }
  catch (runtime_error & rte)
    {
      os << " Run-time Error while accessing track parameters: " << rte.what() << endl;
    }
  catch (logic_error & le)
    {
      os << " Logic Error while accessing track parameters: " << le.what() << endl;
    }
  return os;
}

void StiTrack::setTrackFinder(StiTrackFinder * finder)
{
  trackFinder = finder;
}

void StiTrack::setTrackFitter(StiTrackFitter * fitter)
{
  trackFitter = fitter;
}

StiTrackFinder * StiTrack::getTrackFinder()
{
  return trackFinder;
}

StiTrackFitter * StiTrack::getTrackFitter()
{
  return trackFitter;
}


void StiTrack::fit(int direction)
{
  trackFitter->fit(this,direction);
}

void StiTrack::find(int direction)
{
  trackFinder->find(this,direction);
}
