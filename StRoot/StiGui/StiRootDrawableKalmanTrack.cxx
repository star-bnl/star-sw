//StiRootDrawableKalmanTrack.cxx
//M.L. Miller (Yale Software)
//11/01

//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>

//StEvent
#include "StEventTypes.h"

//SCL
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
#include "StiRootDrawableKalmanTrack.h"
#include "StiGuiIOBroker.h"

using std::sort;

StiRootDrawableKalmanTrack::StiRootDrawableKalmanTrack()
    : mBroker(StiGuiIOBroker::instance()), mSubject(StiGuiIOBroker::instance())
{
    mLineHitPair.first = new StiRootDrawableLine();
    mLineHitPair.second = new StiRootDrawableHits();
    
    mLineHitPair.first->setRemoved(true);
    mLineHitPair.second->setRemoved(true);

    mSubject->attach(this);
    getNewValues();
}

StiRootDrawableKalmanTrack::~StiRootDrawableKalmanTrack()
{
    // cout <<"StiRootDrawableKalmanTrack::~StiRootDrawableKalmanTrack()"<<endl;
    delete mLineHitPair.first;
    mLineHitPair.first=0;
    delete mLineHitPair.second;
    mLineHitPair.second=0;

    if (mSubject) {
	mSubject->detach(this);
    }
    // cout <<"\tdone"<<endl;
}

void StiRootDrawableKalmanTrack::getNewValues()
{
    // cout <<"StiRootDrawableKalmanTrack::getNewValues()"<<endl;
    mLineHitPair.second->setColor( mBroker->markedHitColor() );
    mLineHitPair.second->setMarkerSize( mBroker->markedHitSize() );
    mLineHitPair.second->setMarkerStyle( mBroker->markedHitStyle() );
    // cout <<"\tdone"<<endl;
    
}

void StiRootDrawableKalmanTrack::reset()
{
    StiKalmanTrack::reset();
    mLineHitPair.first->clear();
    mLineHitPair.second->clear();
    mLineHitPair.first->setIsAdded(false);
    mLineHitPair.second->setIsAdded(false);
}

void StiRootDrawableKalmanTrack::update()
{
    // cout <<"StiRootDrawableKalmanTrack::update()"<<endl;
    //getNewValues();
    fillHitsForDrawing();
    // cout <<"\t done"<<endl;
}

void StiRootDrawableKalmanTrack::fillHitsForDrawing()
{
    //be sure to reset internal state
    mLineHitPair.first->clear();
    mLineHitPair.second->clear();

    //Set color and line type
    mLineHitPair.first->clearLine();
    
    //Let's try to find out where the first node is:
    StiKalmanTrackNode* inner = getNodeNear(0.);
    double xStart = inner->fX;
    
    for (double xLocal=xStart; xLocal<200.; xLocal+=1.) {
	StThreeVector<double> pos = getGlobalPointNear(xLocal);
	//cout <<"Adding Position:\t"<<pos<<endl;
	//mLineHitPair.first->push_back( pos );
	mLineHitPair.first->push_back( pos.x() );
	mLineHitPair.first->push_back( pos.y() );
	mLineHitPair.first->push_back( pos.z() );
	
    }

    //now fill hits for real:
    //remember, we *ARE* an StiKalmanTrack (public inheritance)
    StiKalmanTrackNode* node = lastNode; //start at innermost
    int hits=0;
    bool go=true;
    while (go) {
	if (node->getHit()) {

	    //Warning:! This is only temporary!  Should be done in tracker
	    node->getHit()->setUsed(true);
	    
	    //Add this point to the drawable hits
	    const StThreeVectorF& pos = node->getHit()->globalPosition();
	    mLineHitPair.second->push_back( pos.x() );
	    mLineHitPair.second->push_back( pos.y() );
	    mLineHitPair.second->push_back( pos.z() );
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
    mLineHitPair.first->fillHitsForDrawing();
    mLineHitPair.second->fillHitsForDrawing();

    //These get automatically removed from display each event
    //The display dynamically shrinks temp objects each event (tracks, hits, etc)
    if (!mLineHitPair.first->isAdded()) {
	StiDisplayManager::instance()->addDrawable( mLineHitPair.first );
    }
    if (!mLineHitPair.second->isAdded()) {
	StiDisplayManager::instance()->addDrawable( mLineHitPair.second );
    }
    
    return;
}

