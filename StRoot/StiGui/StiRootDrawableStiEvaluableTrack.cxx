//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiMapUtilities.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiRootDrawableStiEvaluableTrack.h"

using std::sort;

StiRootDrawableStiEvaluableTrack::StiRootDrawableStiEvaluableTrack()
{
    mremoved_each_event=true;
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    mline->SetPolyLine(0);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);

    if (!msttrack) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error! msttrack==0"<<endl;
	return;
    }
    
   StPtrVecHit hits = msttrack->detectorInfo()->hits();
   sort( hits.begin(), hits.end(), StHitRadiusLessThan() );
   
    for (vector<StHit*>::iterator it=hits.begin(); it!=hits.end(); ++it) {
	const StThreeVectorD& pos = (*it)->position();
	mline->SetNextPoint( pos.x(), pos.y(), pos.z() );
    }
    return;
}
