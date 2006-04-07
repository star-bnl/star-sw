//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include "Stiostream.h"
#include <algorithm>
using namespace std;

#include "StiTrack.h"
#include "StiKalmanTrack.h"
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
  cout <<"StiTrackContainer::~StiTrackContainer() -I- Done"<<endl;
}



bool StiTrackLessThan::operator()(const StiTrack* lhs, const StiTrack* rhs) const
{
#if 0
  double lhsCurv = lhs->getCurvature();
  double rhsCurv = rhs->getCurvature();
  return fabs(lhsCurv) < fabs(rhsCurv);
#endif //0
  int lN = ((StiKalmanTrack*)lhs)->getNNodes(3);
  int rN = ((StiKalmanTrack*)rhs)->getNNodes(3);
  return lN >= rN;


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
      vector<StiTrack*>::const_iterator it;
      for (it=begin(); it!=end(); ++it) 
	filter->filter(*it);
      return filter->getAcceptedCount();
    }
  else
    return size();
}
void StiTrackContainer::sort() 
{
  std::sort(begin(),end(),StiTrackLessThan());
}
