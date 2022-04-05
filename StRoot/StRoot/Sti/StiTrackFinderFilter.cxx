#include "StiTrackFinderFilter.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiKTNIterator.h"

StiTrackFinderFilter::StiTrackFinderFilter()
	: _minPtsCount(4),
		_minFitPtsCount(4),
		_minFitToPtsRatio(0.5)
{}
 
StiTrackFinderFilter::StiTrackFinderFilter(const string & name, const string & description)	
	: _minPtsCount(4),
		_minFitPtsCount(4),
		_minFitToPtsRatio(0.3)
{}

StiTrackFinderFilter::~StiTrackFinderFilter()
{}

bool StiTrackFinderFilter::accept(const StiTrack *track) const
{
  int npts = track->getPointCount();
  int fitNpts = track->getFitPointCount();

  if (npts > _minPtsCount && fitNpts > _minFitPtsCount) {
#ifdef NODE_CHECK
      //cout << " $";
      const StiKalmanTrack * kTrack = static_cast<const StiKalmanTrack*>(track);
      if (kTrack==0) return false;
      StiKalmanTrackNode* leaf = kTrack->getLastNode();
      if (!leaf) return false;
      //cout << " = ";
      StiKTNForwardIterator it(leaf);
      StiKTNForwardIterator end = it.end();
      //cout << " #";
      bool weird = false;
      int bad = 0;
      while (it!=end) 
	{
	  const StiKalmanTrackNode& node = *it;
	  double x_g = node.x_g();
	  double y_g = node.y_g();
	  double z_g = node.z_g();
	  double rt_g = sqrt(x_g*x_g+y_g*y_g);
	  if (rt_g>210. || fabs(z_g)>250. )
	    bad++;
	  if (bad>2) weird = true;
	  ++it;
	}
      
      if (!weird)
	return kNoErrors;
      else
	return kWeird;
#else
      return kNoErrors;
#endif /* NODE_CHECK */
    }
  else{
    if (npts > _minPtsCount)
      return kNoEnoughValidHits;
    else
      return kNoEnoughFittedValidHits;    
  }
    return false;
}

void StiTrackFinderFilter::initialize()
{
}
 
void StiTrackFinderFilter::setDefaults()
{
}
