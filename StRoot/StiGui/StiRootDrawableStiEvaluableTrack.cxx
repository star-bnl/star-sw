//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>

//StEvent
#include "StEventTypes.h"

//Association
#include "StAssociationMaker/StTrackPairInfo.hh"

//Sti
#include "Sti/StiMapUtilities.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiDisplayManager.h"
#include "StiRootDrawableStiEvaluableTrack.h"

using std::sort;

StiRootDrawableStiEvaluableTrack::StiRootDrawableStiEvaluableTrack()
{
    mremoved_each_event=true;
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{
}

void StiRootDrawableStiEvaluableTrack::reset()
{
    StiEvaluableTrack::reset();
    const_hit_vector::clear();
}

void StiRootDrawableStiEvaluableTrack::update()
{
    cout <<"void StiRootDrawableStiEvaluableTrack::update()"<<endl;
    fillHitsForDrawing();
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    if (!mPair) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error! mPair==0"<<endl;
	return;
    }
    
    mline->SetPolyLine(0);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);
    
    for (double xLocal=0.; xLocal<200.; xLocal+=1.) {
	StThreeVector<double> pos = getGlobalPointNear(xLocal);
	//cout <<"Adding Position:\t"<<pos<<endl;
	mline->SetNextPoint( pos.x(), pos.y(), pos.z() );
    }
    StiDisplayManager::instance()->addDrawable(this);
    
    return;
}
