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

bool StiTrack::find(int direction)
{
  return trackFinder->find(this,direction);
}


double StiTrack::getValue(int key) const
{
  double value;
  switch (key)
    {
    case kCharge: value = getCharge(); break;
    case kMass:   value = getMass(); break;
    case kChi2: value = getChi2(); break;
    case kDca2: value = 0.;break;// getDca2(); break;
    case kDca3: value = 0.;break;// getDca3(); break;
    case kFlag: value = getFlag(); break;
    case kPointCount: value = getPointCount(); break;
    case kFitPointCount: value = getFitPointCount(); break;
    case kGapCount: value = getGapCount(); break;
    case kTrackLength: value = getTrackLength(); break;
    case kMaxPointCount: value = getMaxPointCount(); break;
    case kTpcDedx: value = 0; break;
    case kSvtDedx: value = 0; break;
    case kCurvature: value = getCurvature(); break;
    case kP: value = getP(); break;
    case kPt: value = getPt(); break;
    case kRapidity: value = getRapidity(); break;
    case kPseudoRapidity: value = getPseudoRapidity(); break;
    case kPhi: value = getPhi(); break;
    case kTanL: value = getTanL(); break;
    default: value = -999999.; break;
    }
  return value;  
}
