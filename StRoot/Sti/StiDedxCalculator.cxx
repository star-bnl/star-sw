#include <math.h>
#include <float.h>
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrack.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "StiShape.h"
#include "StDetectorId.h"

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
{
}
  
/*! Dedx represents the mean dedx of those hits that are specified by the
  truncation fraction (see setFractionUsed(double)).  Dedx has units of
  KeV/cm.
 */
void StiDedxCalculator::getDedx(const StiKalmanTrack* track,
 			         double &dEdx, double &dEdxE,
			         double &nPointsUsed)
{
  mVector.clear();
    //hopefully this can be a reference to a vec
    //We assume that the vector contains only nodes with hits!!!!!
    vector<StiKalmanTrackNode*> nodes = track->getNodes(mDetector); //Claude t.b.d.

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
    nPointsUsed = mFraction*static_cast<double>( mVector.size() );
    double sum = accumulate(mVector.begin(),
			    mVector.begin()+static_cast<int>(nPointsUsed), 0.);
    if(nPointsUsed>=0.)
      {
        dEdx=sum/nPointsUsed;
        dEdxE=DBL_MAX;
      }
    else
      {
        dEdx=DBL_MAX;
        dEdxE=DBL_MAX;
      }
    
}

double NodeDedxCalculator::operator()(const StiKalmanTrackNode *mNode)
{
  //The dE of the hit is provided as a hit parameter, but the dx is
  //a characteristic of the track. There are two ways to calculate the
  //dx; first, a straight line approximation can be used, and second,
  //the helix can be assumed to be a circle intersecting the pad, and 
  //the pathlength can be calculated explicitly.
  //The first method involves a straightforward calculation of the 
  //length of the  

 
  //line aproximation
    double dedx= (mNode->getHit()->getEloss())/
                  mNode->getDetector()->getShape()->getThickness()*
                  sqrt((1.+1./(mNode->fP4)*(mNode->fP4)));


  //curve calculation
//     dedx=(mNode->getHit()->getEloss())/
//       (2.*(mNode->fP3)*
//        (asin(.5*(mNode->getDetector()->getShape()->getThickness())*
// 	    sqrt(1.+(mNode->fP4)*(mNode->fP4))/(mNode->fP3))
// 	+M_PI/2.));

	    


    return dedx;
}
