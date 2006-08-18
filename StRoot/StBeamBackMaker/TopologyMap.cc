//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// November 3, 2005
//

#include "Track.hh"
#include "TopologyMap.hh"

TopologyMap::TopologyMap(Track* track) : mFarEast(0), mNearEast(0), mNearWest(0), mFarWest(0)
{
  for (Track::iterator i = track->begin(); i != track->end(); ++i) {
    StHit* hit = *i;
    float z = hit->position().z();
    if      (-200 <= z && z < -150) ++mFarEast;
    else if ( -50 <= z && z <    0) ++mNearEast;
    else if (   0 <= z && z <   50) ++mNearWest;
    else if ( 150 <= z && z <= 200) ++mFarWest;
  }
}

ostream& operator<<(ostream& os, const TopologyMap& topoMap)
{
  return os << topoMap. farEast() << '\t'
	    << topoMap.nearEast() << '\t'
	    << topoMap.nearWest() << '\t'
	    << topoMap. farWest();
}
