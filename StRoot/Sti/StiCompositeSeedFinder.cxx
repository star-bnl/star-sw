///\File StiCompositeSeedFinder.cxx
///\author M.L. Miller (Yale Software) 03/01
///\author C. Pruneau (Wayne) Jan 03
#include <stdexcept>
#include <stdio.h>
#include <Stiostream.h>
#include <algorithm>
using std::copy;
#include "StMeasuredPoint.h"
#include "Sti/Base/MessageType.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "StiToolkit.h"
#include "StiHitContainer.h"
#include "StiDetectorContainer.h"
#include "StiTrackSeedFinder.h"
#include "StiLocalTrackSeedFinder.h"
#include "StiDetector.h"
#include "StiDetectorFinder.h"
#include "StiCompositeSeedFinder.h"
#include "StiMasterDetectorBuilder.h"

StiCompositeSeedFinder::StiCompositeSeedFinder(const string&             name,
					       Factory<StiKalmanTrack> * trackFactory,
					       StiHitContainer         * hitContainer,
					       StiDetectorContainer    * detectorContainer)
  : StiTrackSeedFinder(name,trackFactory,hitContainer,detectorContainer)
{}

///Destructor 
///Nothing to do because the base class takes care of deleting 
///the vector.
StiCompositeSeedFinder::~StiCompositeSeedFinder()
{
  cout <<"StiCompositeSeedFinder::~StiCompositeSeedFinder() -I- Started/Done"<<endl;
}

bool StiCompositeSeedFinder::hasMore()
{
  _messenger <<"StiCompositeSeedFinder::hasMore() -I- Started"<<endl;
  bool has_more;
  if (_currentTrackSeedFinder != this->vector<StiTrackSeedFinder*>::end())
    has_more = (*_currentTrackSeedFinder)->hasMore();
  else
    has_more = false;
  return has_more;
}

StiKalmanTrack* StiCompositeSeedFinder::next()
{
  _messenger <<"StiCompositeSeedFinder::next() -I- Starting"<<endl;
  
  StiKalmanTrack* track=0;
  track = (*_currentTrackSeedFinder)->next();
  //Check to see if we ran out
  if ( (*_currentTrackSeedFinder)->hasMore()==false ) ++_currentTrackSeedFinder;
  _messenger <<"StiCompositeSeedFinder::next() -I- Done"<<endl;
  return track;
}

void StiCompositeSeedFinder::reset()
{
  _messenger <<"StiCompositeSeedFinder::reset() -I- Started"<<endl;
  //reset all!
  for (vector<StiTrackSeedFinder*>::iterator it=this->vector<StiTrackSeedFinder*>::begin(); 
       it!=this->vector<StiTrackSeedFinder*>::end(); ++it)
    (*it)->reset();
  _currentTrackSeedFinder = this->vector<StiTrackSeedFinder*>::begin();
  _messenger <<"StiCompositeSeedFinder::reset() -I- Done"<<endl;
}

void StiCompositeSeedFinder::initialize()
{
	// no ops because the Composite finder does not have parameters
	// other than the actual finder themselves.
}
