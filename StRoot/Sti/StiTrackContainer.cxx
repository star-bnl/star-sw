//StiTrackContainer.cxx
//M.L. Miller (Yale Software)
//05/05

//std
#include <iostream>
#include <algorithm>
using namespace std;

#include "StiTrack.h"
#include "StiTrackContainer.h"

StiTrackContainer::StiTrackContainer()
{
    cout <<"StiTrackContainer::StiTrackContainer()"<<endl;
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
