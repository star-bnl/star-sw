//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//Association
#include "StAssociationMaker/StTrackPairInfo.hh"

//SCL
#include "StParticleTypes.hh"

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
    //cout <<"void StiRootDrawableStiEvaluableTrack::update()"<<endl;
    fillHitsForDrawing();
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    if (!mPair) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error:";
	cout <<"mPair==0"<<endl;
	return;
    }

    //Set color and line type
    setLineInfo();
    
    mline->ResetBit(kCanDelete);

    for (double xLocal=0.; xLocal<200.; xLocal+=1.) {
	StThreeVector<double> pos = getGlobalPointNear(xLocal);
	//cout <<"Adding Position:\t"<<pos<<endl;
	mline->SetNextPoint( pos.x(), pos.y(), pos.z() );
    }
    StiDisplayManager::instance()->addDrawable(this);
    
    return;
}

void StiRootDrawableStiEvaluableTrack::setLineInfo()
{
    //Choose the right color, base on pid of m.c.
    StParticleDefinition* particle =
	mPair->partnerMcTrack()->particleDefinition();

    unsigned int lineStyle=1;
    
    if (particle == StPionPlus::instance()) {
	mcolor=1;
	//lineStyle=1;
    }
    else if (particle == StPionMinus::instance()) {
	mcolor=1;
	//lineStyle=2;
    }
    else if (particle == StKaonPlus::instance()) {
	mcolor=2;
	//lineStyle=1;
    }
    else if (particle == StKaonMinus::instance()) {
	mcolor=2;
	//lineStyle=2;
    }
    else if (particle == StProton::instance()) {
	mcolor=4;
	//lineStyle=1;
    }
    else if (particle == StAntiProton::instance()) {
	mcolor=4;
	//lineStyle=2;
    }
    else if (particle == StMuonPlus::instance()) {
	mcolor=6;
	//lineStyle=1;
    }
    else if (particle == StMuonMinus::instance()) {
	mcolor=6;
	//lineStyle=1;
    }
    else if (particle == StElectron::instance()) {
	mcolor=3;
	//lineStyle=1;
    }
    else if (particle == StPositron::instance()) {
	mcolor=3;
	//lineStyle=2;
    }
    else { //Unkown
	mcolor=5;
	//lineStyle=1;
    }
    mline->SetPolyLine(0);
    mline->SetLineColor(mcolor);
    mline->SetLineStyle(lineStyle);
    mline->SetLineWidth(1.5);

}
