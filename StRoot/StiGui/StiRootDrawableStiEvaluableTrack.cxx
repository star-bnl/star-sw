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
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

//Sti
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiMapUtilities.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiDisplayManager.h"
#include "StiRootDrawableLine.h"
#include "StiRootDrawableHits.h"
#include "StiRootDrawableStiEvaluableTrack.h"
#include "StiGuiIOBroker.h"

using std::sort;

StiRootDrawableStiEvaluableTrack::StiRootDrawableStiEvaluableTrack()
{
    StiGuiIOBroker* broker = StiGuiIOBroker::instance();
    
    mLine = new StiRootDrawableLine();
    
    mHits = new StiRootDrawableHits();
    //these are temp, need to be changed
    mHits->setColor( broker->markedHitColor() );
    mHits->setMarkerSize( broker->markedHitSize() );
    mHits->setMarkerStyle( broker->markedHitStyle() );
    
    mLine->setRemoved(true);
    mHits->setRemoved(true);
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{
    // cout <<"StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()"<<endl;
    delete mLine;
    mLine=0;
    delete mHits;
    mHits=0;
    // cout <<"\tdone"<<endl;
}

void StiRootDrawableStiEvaluableTrack::reset()
{
    StiEvaluableTrack::reset();
    mLine->clear();
    mHits->clear();
    mLine->setIsAdded(false);
    mHits->setIsAdded(false);
}

void StiRootDrawableStiEvaluableTrack::update()
{
    //cout <<"void StiRootDrawableStiEvaluableTrack::update()"<<endl;
    fillHitsForDrawing();
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    //be sure to reset internal state
    mLine->clear();
    mHits->clear();

    if (!mPair) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error:";
	cout <<"mPair==0"<<endl;
	return;
    }

    //Set color and line type
    mLine->clearLine();
    setLineInfo();
    
    //Let's try to find out where the first node is:
    StiKalmanTrackNode* inner = getNodeNear(0.);
    double xStart = inner->fX;
    
    for (double xLocal=xStart; xLocal<200.; xLocal+=1.) {
	StThreeVector<double> pos = getGlobalPointNear(xLocal);
	//cout <<"Adding Position:\t"<<pos<<endl;
	//mLine->push_back( pos );
	mLine->push_back( pos.x() );
	mLine->push_back( pos.y() );
	mLine->push_back( pos.z() );
	
    }

    //now fill hits for real:
    //remember, we *ARE* an StiKalmanTrack (public inheritance)
    StiKalmanTrackNode* node = lastNode; //start at innermost
    int hits=0;
    bool go=true;
    while (go) {
	if (node->getHit()) {
	    //Add this point to the drawable hits
	    const StThreeVectorF& pos = node->getHit()->globalPosition();
	    mHits->push_back( pos.x() );
	    mHits->push_back( pos.y() );
	    mHits->push_back( pos.z() );
	    ++hits;
	}
	//now check for parent:
	if (node->isRoot()) {
	    go=false;
	}
	else {
	    node = dynamic_cast<StiKalmanTrackNode*>(node->getParent());
	    if (!node) {
		cout <<"StiRootDrawableStiEvaluableTrac;::fillHitsForDrawing. ERROR:\t"
		     <<"Cast to StiKalmanTrackNodeFailed.  Abort"<<endl;
		return;
	    }
	}
    }

    //cout <<"Hits on track:\t"<<hits<<endl;
    mLine->fillHitsForDrawing();
    mHits->fillHitsForDrawing();

    //These get automatically removed from display each event
    //The display dynamically shrinks temp objects each event (tracks, hits, etc)
    if (!mLine->isAdded()) {
	StiDisplayManager::instance()->addDrawable( mLine );
    }
    if (!mHits->isAdded()) {
	StiDisplayManager::instance()->addDrawable( mHits );
    }
    
    return;
}

void StiRootDrawableStiEvaluableTrack::setLineInfo()
{
    //Choose the right color, base on pid of m.c.
    StParticleDefinition* particle =
	mPair->partnerMcTrack()->particleDefinition();

    unsigned int lineStyle=1;

    int color;
    
    if (particle == StPionPlus::instance()) {
	color=2;
	//lineStyle=1;
    }
    else if (particle == StPionMinus::instance()) {
	color=2;
	//lineStyle=2;
    }
    else if (particle == StKaonPlus::instance()) {
	color=3;
	//lineStyle=1;
    }
    else if (particle == StKaonMinus::instance()) {
	color=3;
	//lineStyle=2;
    }
    else if (particle == StProton::instance()) {
	color=4;
	//lineStyle=1;
    }
    else if (particle == StAntiProton::instance()) {
	color=4;
	//lineStyle=2;
    }
    else if (particle == StMuonPlus::instance()) {
	color=6;
	//lineStyle=1;
    }
    else if (particle == StMuonMinus::instance()) {
	color=6;
	//lineStyle=1;
    }
    else if (particle == StElectron::instance()) {
	color=1;
	//lineStyle=1;
    }
    else if (particle == StPositron::instance()) {
	color=1;
	//lineStyle=2;
    }
    else { //Unkown
	color=5;
	//lineStyle=1;
    }
    mLine->clearLine();
    mLine->setColor(color);
    mLine->setLineStyle(lineStyle);
    mLine->setLineWidth(1.5);

}
