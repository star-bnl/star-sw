#include "StiTrack.h"
#include "StiTrackFilter.h"

StiTrackFilter::StiTrackFilter()
{
  //----------------------------------------------------------
  //setDefaults(); this would do nothing in the base class
  //----------------------------------------------------------
  reset();
}
  
void StiTrackFilter::reset()
{
  //----------------------------------------------------------
  // Reset counters to zero
  //----------------------------------------------------------
  analyzedTrackCount = 0;
  acceptedTrackCount = 0;
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

