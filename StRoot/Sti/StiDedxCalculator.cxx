#include <math.h>
#include <float.h>
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiShape.h"
#include "StDetectorId.h"
#include "StMeasuredPoint.h"

#include <algorithm>
using std::transform;
#include <numeric>
using std::accumulate;
#include <functional>
using std::less;
#include "StDetectorId.h"
#include "StiDedxCalculator.h"

StiDedxCalculator::StiDedxCalculator()
  : mFraction(0.) //Default to zero so that user must set
{}
  
/*! Dedx represents the mean dedx of those hits that are specified by the
  truncation fraction (see setFractionUsed(double)).  Dedx has units of
  KeV/cm.
 */
void StiDedxCalculator::getDedx(const StiKalmanTrack* track,
				double &dEdx, double &dEdxE,
				double &nPointsUsed)
{
  mVector.clear();
  vector<StiKalmanTrackNode*> nodes = track->getNodes(mDetector); 
  if(nodes.size()==0)
    {
      nPointsUsed=0.;
      dEdx=DBL_MAX;
      dEdxE=DBL_MAX;
      return;
    }
  
  //Transform each node to a double=dedx of it's hit
  //I did it in one line because it should be a little faster than
  //a loop, since the call to NodeDedxCalculator() should be
  //a guarunteed inline, and we don't have to call once nodes.end()
  // (it constructs an iterator each time)
  //But, if we gotta break it into a loop,well, no big deal
  mVector.reserve(nodes.size());
  transform(nodes.begin(), nodes.end(), back_inserter(mVector),
	    NodeDedxCalculator());
  
  //sort in ascending order
  sort(mVector.begin(), mVector.end(), less<double>() );
  // nPointsUsed needs to be an int, this needs to be fixed
  nPointsUsed = mFraction*static_cast<double>( mVector.size() ); 
  double sum = accumulate(mVector.begin(),
			  mVector.begin()+static_cast<int>(nPointsUsed), 0.);
  if(nPointsUsed>0.)
    {
      dEdx=sum/nPointsUsed;
      dEdxE=9999;
    }
  else
    {
      dEdx=9999;
      dEdxE=9999;
    }
}

///The dE of the hit is provided as a hit parameter, but the dx is
///a characteristic of the track. There are two ways to calculate the
///dx; first, a straight line approximation can be used, and second,
///the helix can be assumed to be a circle intersecting the pad, and 
///the pathlength can be calculated explicitly.
///The first method involves a straightforward calculation of the 
///length of the    
double NodeDedxCalculator::operator()(const StiKalmanTrackNode *mNode)
{
  double dedx = mNode->getDedx();
  if(false && dedx<0.)
    {
      cout <<"Eloss: " << mNode->getHit()->getEloss()<<endl;
      cout <<"sinCrossAngle: "<<mNode->sinCrossAngle()<<endl;
      cout <<"Dedx: "<<dedx<<endl;
      const StiDetector* det = mNode->getHit()->detector();
      cout<<"Detector: " <<det->getName()<<endl
	  <<endl;
      cout <<"Detector is active:"
	   <<mNode->getHit()->detector()->isActive()
	   <<endl;
    }
  return dedx;
}
