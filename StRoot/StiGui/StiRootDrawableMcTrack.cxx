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
  _line->clear();
  _hits->clear();
  _line->clearLine();
    
  //Loop over nodes by hand (faster than using StiMcTrack interface)
  //This is essentailly the guts of an interpolation routine that should become
  // a class at some point.
  double step = 1.; //cm
  
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
