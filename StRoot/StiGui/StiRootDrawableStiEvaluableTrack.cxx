//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <algorithm>
#include <math.h>
using namespace std;

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
#include "Sti/StiKTNIterator.h"
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
    : mBroker(StiGuiIOBroker::instance()), mSubject(StiGuiIOBroker::instance())
{
    mLineHitPair.first = new StiRootDrawableLine();
    mLineHitPair.second = new StiRootDrawableHits();
    
    mLineHitPair.first->setRemoved(true);
    mLineHitPair.second->setRemoved(true);

    mSubject->attach(this);
    getNewValues();
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{
    // cout <<"StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()"<<endl;
    delete mLineHitPair.first;
    mLineHitPair.first=0;
    delete mLineHitPair.second;
    mLineHitPair.second=0;

    if (mSubject) {
	mSubject->detach(this);
    }
    // cout <<"\tdone"<<endl;
}

void StiRootDrawableStiEvaluableTrack::getNewValues()
{
    // cout <<"StiRootDrawableStiEvaluableTrack::getNewValues()"<<endl;
    mLineHitPair.second->setColor( mBroker->markedHitColor() );
    mLineHitPair.second->setMarkerSize( mBroker->markedHitSize() );
    mLineHitPair.second->setMarkerStyle( mBroker->markedHitStyle() );
    // cout <<"\tdone"<<endl;
    
}

void StiRootDrawableStiEvaluableTrack::reset()
{
    StiEvaluableTrack::reset();
    mLineHitPair.first->clear();
    mLineHitPair.second->clear();
    mLineHitPair.first->setIsAdded(false);
    mLineHitPair.second->setIsAdded(false);
}

void StiRootDrawableStiEvaluableTrack::update()
{
    // cout <<"StiRootDrawableStiEvaluableTrack::update()"<<endl;
    //getNewValues();
    fillHitsForDrawing();
    // cout <<"\t done"<<endl;
}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    //be sure to reset internal state
    mLineHitPair.first->clear();
    mLineHitPair.second->clear();

    if (!mPair) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error:";
	cout <<"mPair==0"<<endl;
	return;
    }

    //Set color and line type
    mLineHitPair.first->clearLine();
    setLineInfo();
    
    //Loop over nodes by hand (faster than using StiKalmanTrack interface)
    //This is essentailly the guts of an interpolation routine that should become
    // a class at some point.
    double step = 1.; //cm
    
    bool go = true;
    double xLocal = lastNode->fX;
    StiKTNForwardIterator it(lastNode);
    StiKTNForwardIterator end = it.end();
    
    while( go && it!=end ) {
	//cout <<"Entered loop";
	StiKalmanTrackNode& node = *it;
	//cout <<"\tDereferenced it";
	
	++it;
	if (it==end) { //we're done
	    //cout <<"We're finished, go on"<<endl;
	    go=false;
	}
	else {
	    StiKalmanTrackNode& next = *it;
	    while (xLocal<next.fX) {
		double xx = node.fX;
		double yy = node.fP0;
		double zz = node.fP1;
		double alpha = node.fAlpha;
		double ca = cos(alpha);
		double sa = sin(alpha);
		double gx = ca*xx-sa*yy;
		double gy = sa*xx+ca*yy;
		mLineHitPair.first->push_back(gx);
		mLineHitPair.first->push_back(gy);
		mLineHitPair.first->push_back(zz);
		
		xLocal+=step;
	    }
	    //cout <<"Done stepping"<<endl;
	    //} //temporary patch
	}
    }
    
    //now fill hits for real:
    //remember, we *ARE* an StiKalmanTrack (public inheritance)
    StiKalmanTrackNode* node = lastNode; //start at innermost
    int hits=0;
    go=true;
    while (go) {
	if (node->getHit()) {
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
    mLineHitPair.first->clearLine();
    mLineHitPair.first->setColor(color);
    mLineHitPair.first->setLineStyle(lineStyle);
    mLineHitPair.first->setLineWidth(1.5);

}
