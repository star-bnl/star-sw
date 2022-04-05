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
  _lastDet    = lastDet;
  for ( _currentDet = firstDet; _currentDet<_lastDet; ++_currentDet) {
    if (_hitContainer->hasDetector(*_currentDet) ) {
      _currentDetHit =  _hitContainer->hitsBegin(*_currentDet);
      _lastDetHit    =  _hitContainer->hitsEnd(*_currentDet);
      if (_currentDetHit<_lastDetHit) {
	_currentHit = *_currentDetHit;
	break;
      }
    }
    _currentHit = 0;
    _currentDetHit = _lastDetHit;
  }
  if (! _currentHit) _currentDet = _lastDet;
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
