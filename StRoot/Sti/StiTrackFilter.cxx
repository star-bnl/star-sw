
#include "StiTrack.h"
#include "StiTrackFilter.h"

ClassImp(StiTrackFilter)

StiTrackFilter::StiTrackFilter()
{
  //----------------------------------------------------------
  //setDefaults(); this would do nothing in the base class
  //----------------------------------------------------------
  reset();
}
  
void StiTrackFilter::setDefaults()
{
  //----------------------------------------------------------
  // Whatever must be done to initialize the filter, must be done
  // in the derived class which implements this function.
  //----------------------------------------------------------
}

void StiTrackFilter::reset()
{
  //----------------------------------------------------------
  // Reset counters to zero
  //----------------------------------------------------------
  analyzedTrackCount = 0;
  acceptedTrackCount = 0;
}

bool StiTrackFilter::accept(StiTrack * event)
{
  //----------------------------------------------------------
  // Determine whether the given track is acceptable given 
  // the constraints encapsulated by this filter.
  // Instances of this base class will accept all tracks
  // incondionally. This method must be overiden in derived 
  // to perform actual track filtering... 
  //----------------------------------------------------------
  analyzedTrackCount++;
  acceptedTrackCount++;
  return true;
}

int StiTrackFilter::getAnalyzedTrackCount()
{
  //----------------------------------------------------------
  // Returns the number of tracks analyzed by this filter
  // since last reset.
  //----------------------------------------------------------
  return analyzedTrackCount;
}

int StiTrackFilter::getAcceptedTrackCount()
{
  //----------------------------------------------------------
  // Returns the number of tracks accpeted by this filter
  // since last reset.
  //----------------------------------------------------------
  return acceptedTrackCount;
}

