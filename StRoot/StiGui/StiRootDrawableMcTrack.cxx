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

//StiGui
#include "StiGui/StiTPolyLine3D.h"
#include "StiGui/StiRootDisplayManager.h"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StMcTrack.hh"
#include "StMcContainers.hh"
#include "StMcTpcHit.hh"

StiRootDrawableMcTrack::StiRootDrawableMcTrack()
  : StiRootDrawableTrack()
{
  getNewState();
}

StiRootDrawableMcTrack::~StiRootDrawableMcTrack()
{}

void StiRootDrawableMcTrack::getNewState()
{
  _hits->setColor( mBroker->markedHitColor() );
  _hits->setMarkerSize( mBroker->markedHitSize() );
  _hits->setMarkerStyle( mBroker->markedHitStyle() );
}

void StiRootDrawableMcTrack::fillHitsForDrawing()
{
  cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - start " << endl;
  _line->clear();
  _hits->clear();
  _line->clearLine();
    
  //Loop over nodes by hand (faster than using StiMcTrack interface)
  //This is essentailly the guts of an interpolation routine that should become
  // a class at some point.
  //double step = 1.; //cm

  cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - cleared " << endl;
  
  const StPtrVecMcTpcHit & tpcHits = mcTrack->tpcHits();
  StMcTpcHitConstIterator hitIter;
  cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - have tpcHits " << endl;

  for (hitIter= tpcHits.begin();hitIter!=tpcHits.end();hitIter++)
    {
      StThreeVectorF position = (*hitIter)->position();
      _hits->push_back( position.x() );
      _hits->push_back( position.y() );
      _hits->push_back( position.z() );
    }
  cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - tpcHits filled into _hits" << endl;
  
  ///_line->fillHitsForDrawing();
  _hits->fillHitsForDrawing();
  
  //These get automatically removed from display each event
  //The display dynamically shrinks temp objects each event (tracks, hits, etc)
  //if (!_line->isAdded())
  //  StiRootDisplayManager::instance()->addDrawable( _line );
  if (!_hits->isAdded()) 
    StiRootDisplayManager::instance()->addDrawable( _hits );
  cout << "StiRootDrawableMcTrack::fillHitsForDrawing() - Done"<<endl;
  return;
}

void StiRootDrawableMcTrack::reset()
{
  this->StiMcTrack::reset();
  this->StiRootDrawableTrack::reset();
}

StiRootDrawableMcTrackFactory::StiRootDrawableMcTrackFactory(const string& newName,
							     int original,
							     int incremental, 
							     int maxInc)
  : StiMcTrackFactory(newName, 
		      original, 
		      incremental, 
		      maxInc)
{
  initialize();
}

StiRootDrawableMcTrackFactory::~StiRootDrawableMcTrackFactory()
{
  // cout <<"StiRootDrawableMcTrackFactory::~StiRootDrawableMcTrackFactory()"<<endl;
}
