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
    : StiRootDrawableTrack()
{
  getNewState();
}

StiRootDrawableKalmanTrack::~StiRootDrawableKalmanTrack()
{ }

void StiRootDrawableKalmanTrack::getNewState()
{
    _hits->setColor( mBroker->markedHitColor() );
    _hits->setMarkerSize( mBroker->markedHitSize() );
    _hits->setMarkerStyle( mBroker->markedHitStyle() );
}

void StiRootDrawableKalmanTrack::fillHitsForDrawing()
{
  cout<<"StiRootDrawableKalmanTrack::fillHitsForDrawing()"<<endl;
    //be sure to reset internal state
    _line->clear();
    _hits->clear();

    //Set color and line type
    _line->clearLine();
    
    //Loop over nodes by hand (faster than using StiKalmanTrack interface)
    //This is essentailly the guts of an interpolation routine that should become
    // a class at some point.
    double step = 1.; //cm
    
    StiKalmanTrackNode * lastNode = getInnerMostNode();
    double xLocal = lastNode->fX;
    StiKTNForwardIterator it(lastNode);
    StiKTNForwardIterator end = it.end();
    while(it!=end ) 
      {
	StiKalmanTrackNode& node = *it;
	++it;
	if (it==end)   
	  break;
	StiKalmanTrackNode& next = *it;

	// Fill node position itself.
	StiHit * hit = node.getHit();
	if (hit)
	  {
	    const StThreeVectorF& pos = hit->globalPosition();
	    _hits->push_back( pos.x() );
	    _hits->push_back( pos.y() );
	    _hits->push_back( pos.z() );
	  }
	// Fill interpolation to muck up a continuous track
	StThreeVector<double> pos;
	while (xLocal<next.fX) 
	  {
	    try 
	      {
		pos = node.getPointAt(xLocal);
		_line->push_back(pos.x());
		_line->push_back(pos.y());
		_line->push_back(pos.z());
	      }
	    catch (runtime_error & rte)	
	      {
		_line->setColor( 1 );
	      }
	    catch (exception & e) 
	      {
		_line->setColor( 1 );
	      }
	    xLocal+=step;
	  }
      }
	
    _line->fillHitsForDrawing();
    _hits->fillHitsForDrawing();

    //These get automatically removed from display each event
    //The display dynamically shrinks temp objects each event (tracks, hits, etc)
    if (!_line->isAdded())
	StiRootDisplayManager::instance()->addDrawable( _line );
    if (!_hits->isAdded()) 
	StiRootDisplayManager::instance()->addDrawable( _hits );
    return;
}

void StiRootDrawableKalmanTrack::reset()
{
  this->StiKalmanTrack::reset();
  this->StiRootDrawableTrack::reset();
}
