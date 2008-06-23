#include "StiSortedHitIterator.h"

/// Default constructor used to create an iterator that points to no hit (e.g. to be returned by end())
StiSortedHitIterator::StiSortedHitIterator()
  :  _currentHit(0),
     _hitContainer(0),
     _currentDet(),
     _firstDet(),
     _lastDet(),
     _currentDetHit(),
     _lastDetHit()
{ }


/// Constructor using a StiHitContainer
StiSortedHitIterator::StiSortedHitIterator(StiHitContainer * hitContainer,
					   vector<StiDetector*>::iterator firstDet,
					   vector<StiDetector*>::iterator lastDet): _currentHit(0)
{
  _hitContainer = hitContainer;
  _currentDet = firstDet;
  _lastDet    = lastDet;
 if (_currentDet<_lastDet && _hitContainer->hasDetector(*_currentDet) )
    {
      _currentDetHit =  _hitContainer->hitsBegin(*_currentDet);
      _lastDetHit    =  _hitContainer->hitsEnd(*_currentDet);
      if (_currentDetHit<_lastDetHit)
	{
	  _currentHit = *_currentDetHit;
	}
      else
	{
	  // Current detector has no hit, traverse detectors to  look for the next detector with hits.
	  bool go = true;
	  while (_currentDet<_lastDet && go )
	    {
	      ++_currentDet;
	      if (_currentDet<_lastDet)
		{  
         if ( _hitContainer->hasDetector(*_currentDet) ) {
            // valid detector
		  _currentDetHit =  _hitContainer->hitsBegin(*_currentDet);
		  _lastDetHit    =  _hitContainer->hitsEnd(*_currentDet);
		  if (_currentDetHit < _lastDetHit)
		    {
		      // current detector has hits done for now.
		      _currentHit = *_currentDetHit;
		      go = false;
		    }
		} }
	      else
		{
		  // reached past the last detector
		  // no more hits to serve
		  _currentHit = 0;
		  _currentDet = _lastDet;
		  _currentDetHit = _lastDetHit;
		}
	    }
	}
    }
  else
    {
      _currentHit = 0;
      _currentDet = _lastDet;
    }
}   

/// Copy Constructor
StiSortedHitIterator::StiSortedHitIterator(const StiSortedHitIterator&iter)
  :  _currentHit(iter._currentHit),
     _hitContainer(iter._hitContainer),
     _currentDet(iter._currentDet),
     _firstDet(iter._firstDet),
     _lastDet(iter._lastDet),
     _currentDetHit(iter._currentDetHit),
     _lastDetHit(iter._lastDetHit)
{}
