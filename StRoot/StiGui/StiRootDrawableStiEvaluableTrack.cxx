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
#include "StiRootDisplayManager.h"
#include "StiRootDrawableLine.h"
#include "StiRootDrawableHits.h"
#include "StiRootDrawableStiEvaluableTrack.h"
using std::sort;

StiRootDrawableStiEvaluableTrack::StiRootDrawableStiEvaluableTrack()
    : StiRootDrawableTrack()
{    
	_hits->setColor( 2 );
	_hits->setMarkerSize( 0.3 );
	_hits->setMarkerStyle( 3 );
}

StiRootDrawableStiEvaluableTrack::~StiRootDrawableStiEvaluableTrack()
{}

void StiRootDrawableStiEvaluableTrack::fillHitsForDrawing()
{
    _line->clear();
    _hits->clear();
    if (!mPair) {
	cout <<"StiRootDrawableStiEvaluableTrack::fillHitsForDrawing() Error:";
	cout <<"mPair==0"<<endl;
	return;
    }

    //Set color and line type
    _line->clearLine();
    setLineInfo();
    
    //Loop over nodes by hand (faster than using StiKalmanTrack interface)
    //This is essentailly the guts of an interpolation routine that should become
    // a class at some point.
    double step = 1.; //cm
    
    bool go = true;
    StiKalmanTrackNode * lastNode = getLastNode();
    double xLocal = lastNode->getRefPosition();
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
	    while (xLocal<next.getRefPosition() ) {
		double xx = node.getRefPosition();
		double yy = node.getY();
		double zz = node.getZ();
		double alpha = node.getRefAngle();
		double ca = cos(alpha);
		double sa = sin(alpha);
		double gx = ca*xx-sa*yy;
		double gy = sa*xx+ca*yy;
		_line->push_back(gx);
		_line->push_back(gy);
		_line->push_back(zz);
		
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
	    _hits->push_back( pos.x() );
	    _hits->push_back( pos.y() );
	    _hits->push_back( pos.z() );
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
    _line->fillHitsForDrawing();
    _hits->fillHitsForDrawing();

    //These get automatically removed from display each event
    //The display dynamically shrinks temp objects each event (tracks, hits, etc)
    if (!_line->isAdded()) {
	StiRootDisplayManager::instance()->addDrawable( _line );
    }
    if (!_hits->isAdded()) {
	StiRootDisplayManager::instance()->addDrawable( _hits );
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
    _line->clearLine();
    _line->setColor(color);
    _line->setLineStyle(lineStyle);
    _line->setLineWidth(1.5);

}

void StiRootDrawableStiEvaluableTrack::reset()
{
  this->StiEvaluableTrack::reset();
  this->StiRootDrawableTrack::reset();
}
