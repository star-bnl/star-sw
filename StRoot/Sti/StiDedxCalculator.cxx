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
    double dedx=0.;
    double dx, ds;
    //double dx;
    //double radius, dr;                //radius and thickness of the detector
    //StThreeVectorD dPlane, dNormal;

    //The real dx for each hit is calculated through the helix 
    //extrapolation between top and bottom of pad. A fair 
    //approximation is to use the TanL to get the pathlength.

    //To use the helix extrapolation, first we need the coordinates 
    //of the top and bottom mid point of the pad in question. The 
    //detector coordinates are specified as z,r,phi of the center of
    //the detector, and dx,dy,dz the detector spans.
    //For the helix extraplolation, we need the global coordinates of a 
    //point on the top and bottom of the detector, and the normal.
    //radius = mNode->getDetector()->getPlacement()->getCenterRadius();
    //dr     = mNode->getDetector()->getShape()->getThickness() / 2.;

    //dNormal.setMagnitude(mNode->getDetector()->getPlacement());
    //dsBot = mNode->getDetecotr()->getPlacement();
    //dx = abs( dsTop - dsBot );
    ds   = mNode->getDetector()->getShape()->getThickness();
    ds   = ds * ds;
    dx   = sqrt(ds + ds / (mNode->getTanL() * mNode->getTanL()));
 

    //dx is checked here for pathologic values; if the pathlength is
    //greater than the length of the pad, then there is significant
    //curvature in the pad. If the pathlength is greater than
    //length+2*width, then the track must oscillate in the
    //pad itself.
    
    dedx = (mNode->getHit()->getEloss())/dx;
    return dedx;
}
