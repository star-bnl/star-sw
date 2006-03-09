///\File StiCompositeFinder.cxx
///\author M.L. Miller (Yale Software) 03/01
///\author C. Pruneau (Wayne) Jan 03
#include <stdexcept>
#include <stdio.h>
#include <Stiostream.h>
#include <algorithm>
using std::copy;
#include "StiCompositeFinder.h"

StiCompositeFinder::StiCompositeFinder(const string& name,
				       const string& description)
  : StiTrackFinder()
{}

///Destructor 
StiCompositeFinder::~StiCompositeFinder()
{
  cout <<"StiCompositeFinder::~StiCompositeFinder() -I- Started/Done"<<endl;
}

StiTrack * StiCompositeFinder::findTrack()
{
  StiTrack* track=0;
  while (track==0 && _currentFinder!=end() )
    {
      track = (*_currentFinder)->findTrack();
      if (track) 
	break;
      else
	++_currentFinder;
    }
  return track;
}

void StiCompositeFinder::reset()
{
  cout <<"StiCompositeFinder::reset() -I- Started"<<endl;
  for (vector<StiTrackFinder*>::iterator it=this->vector<StiTrackFinder*>::begin(); 
       it!=this->vector<StiTrackFinder*>::end(); ++it)
    (*it)->reset();
  _currentFinder = this->vector<StiTrackFinder*>::begin();
  cout <<"StiCompositeFinder::reset() -I- Done"<<endl;
}

