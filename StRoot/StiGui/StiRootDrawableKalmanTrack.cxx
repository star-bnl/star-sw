//StiRootDrawableKalmanTrack.cxx
//M.L. Miller (Yale Software)
//11/01

//StiRootDrawableStiEvaluableTrack.cxx
//M.L. Miller (Yale Software)
//07/01

//STD
#include <stdexcept>
#include <iostream.h>
#include <algorithm>
using namespace std;

//StEvent
#include "StEventTypes.h"

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

//Sti
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/StiKTNIterator.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiRootDisplayManager.h"
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
    
    //Loop over nodes by hand (faster than using StiKalmanTrack interface)
    //This is essentailly the guts of an interpolation routine that should become
    // a class at some point.
    double step = 1.; //cm
    
    bool go = true;
    StiKalmanTrackNode * lastNode = getLastNode();
    double xLocal = lastNode->fX;
    StiKTNForwardIterator it(lastNode);
    StiKTNForwardIterator end = it.end();
    while( go && it!=end ) {
			StiKalmanTrackNode& node = *it;
	
	++it;
	if (it==end) { //we're done
	    go=false;
	}
	else {
	    StiKalmanTrackNode& next = *it;
	    while (xLocal<next.fX) {
		bool threw = false;
		//Try the new method:
		StThreeVector<double> pos;
		try {
		    pos = node.getPointAt(xLocal);
		}
		catch (runtime_error & rte)	{
		    threw=true;
		    //cout << "RunTime Error Exception: " << rte.what()<<endl;
		}
		catch (exception & e) {
		    threw=true;
		    //cout << "Exception: " << e.what()<<endl;
		}

		if (threw==false) {
		    mLineHitPair.first->push_back(pos.x());
		    mLineHitPair.first->push_back(pos.y());
		    mLineHitPair.first->push_back(pos.z());
		}
		else {
		    mLineHitPair.first->setColor( 3 );
		    mLineHitPair.second->setColor( 3 );
		}
		
		xLocal+=step;
	    }
	    //cout <<"Done stepping"<<endl;
	    //} //temporary patch
	}
    }
		//cout <<"now fill hits for real"<<endl;
    //now fill hits for real:
    //remember, we *ARE* an StiKalmanTrack (public inheritance)
    StiKalmanTrackNode* node = getLastNode(); //start at innermost
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

    cout <<"Hits on track:\t"<<hits<<endl;
    mLineHitPair.first->fillHitsForDrawing();
    mLineHitPair.second->fillHitsForDrawing();

    //These get automatically removed from display each event
    //The display dynamically shrinks temp objects each event (tracks, hits, etc)
    if (!mLineHitPair.first->isAdded()) {
	StiRootDisplayManager::instance()->addDrawable( mLineHitPair.first );
    }
    if (!mLineHitPair.second->isAdded()) {
	StiRootDisplayManager::instance()->addDrawable( mLineHitPair.second );
    }
    
    return;
}

