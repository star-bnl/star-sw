#include "StChargedPionTrack.h"

#include "TClass.h"

ClassImp(StChargedPionTrack)

double StChargedPionTrack::length() const 
{ 
	return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) ); 
}

double StChargedPionTrack::lengthMeasured() const 
{ 
	return fabs( helix().pathLength(StThreeVectorD(mLastPoint)) - helix().pathLength(StThreeVectorD(mFirstPoint)) ); 
}