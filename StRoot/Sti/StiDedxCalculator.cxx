#include <math.h>
#include <float.h>

#include <algorithm>
using std::transform;

#include <numeric>
using std::accumulate;

#include <functional>
using std::less;

#include "StiDedxCalculator.h"


StiDedxCalculator::StiDedxCalculator()
    : mFraction(0.) //Default to zero so that user must set
{
}
  
/*! Dedx represents the mean dedx of those hits that are specified by the
  truncation fraction (see setFractionUsed(double)).  Dedx has units of
  KeV/cm.
 */
float StiDedxCalculator::getDedx(const StiTrack* track)
{
    mVector.clear();
    //hopefully this can be a reference to a vec
    //We assume that the vector contains only nodes with hits!!!!!
    const StiTrackNodeVec nodes; 
    //nodes = track->getNodes(); //Claude t.b.d.

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
    double nPoints = mFraction*static_cast<double>( mVector.size() );
    double sum = accumulate(mVector.begin(),
			    mVector.begin()+static_cast<int>(nPoints), 0.);
    return (nPoints>=0.) ? (sum/nPoints) : DBL_MAX;
}

double NodeDedxCalculator::operator()(const StiTrackNode*)
{
    double dedx=0.;
    // ... add code to calculate the dedx of each track node w/ a hit ...
    return dedx;
}
