//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include "Stiostream.h"
#include <algorithm>
using namespace std;

#include "StiTrack.h"
#include "StiTrackContainer.h"
#include "Sti/Base/Filter.h"

StiTrackContainer::StiTrackContainer(const string & name, const string & description)
  : Named(name),
    Described(description)
{
  cout <<"StiTrackContainer::StiTrackContainer() -I- Started with name:"<< name <<endl;
}

StiTrackContainer::~StiTrackContainer()
{
    cout <<"StiTrackContainer::~StiTrackContainer()"<<endl;
}



bool StiTrackLessThan::operator()(const StiTrack* lhs, const StiTrack* rhs) const
{
  double lhsCurv = lhs->getCurvature();
  double rhsCurv = rhs->getCurvature();
  if (lhsCurv==0. || rhsCurv==0.) 
    return false;
  return (1./lhsCurv < 1./rhsCurv);
}

/*! Get the number of tracks held by this container that satisfies the given track filter.
  <p>
  This convenience method returns the number of tracks in this container that satisfy the
  given filter. If the filter is null pointer, return the size of this container.
*/
int StiTrackContainer::getTrackCount(Filter<StiTrack> * filter) const
{
  if (filter)
    {
      // reset filter counter to zero.
      filter->reset();
      // loop over all tracks and filter
      TrackToTrackMap::const_iterator it;
      for (it=begin(); it!=end(); ++it) 
	filter->filter((*it).second);
      return filter->getAcceptedCount();
    }
  else
    return size();
}
