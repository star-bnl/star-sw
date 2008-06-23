#ifndef StiSortedHitIterator_HH
#define StiSortedHitIterator_HH
#ifdef GNU_GCC
  #if __GNUC__<3
    #define HACK_forward_iterator
  #endif
#endif
#include <vector>
#include <iterator>
using namespace std;
#include "StiHit.h"
typedef StiHit Hit_t;
#include "StiDetector.h"
#include "StiHitContainer.h"

/// \class StiSortedHitIterator
/// A STL compliant forward iterator that traverse all the hit held by 
/// the hit container.  
///
/// \author Claude A Pruneau (Wayne State University)
class StiSortedHitIterator
#ifndef HACK_forward_iterator
: public iterator<forward_iterator_tag, Hit_t, ptrdiff_t, Hit_t*, Hit_t&>
#else
    : public forward_iterator<Hit_t, int>
#endif
{
public:
  /// Default constructor used to create an iterator that points to no hit (e.g. to be returned by end())
  StiSortedHitIterator();
  /// Constructor using a StiHitContainer
  StiSortedHitIterator(StiHitContainer * hitContainer,
		       vector<StiDetector*>::iterator firstDet,
		       vector<StiDetector*>::iterator lastDet);
  /// Copy Constructor
  StiSortedHitIterator(const StiSortedHitIterator&iter);
  /// Asignment operator
  const StiSortedHitIterator& operator=(const StiSortedHitIterator&iter);
  /// Destructor
  ~StiSortedHitIterator() {};
  
  ///equality:
  bool operator==(const StiSortedHitIterator& rhs);
  ///inequlity
  bool operator!=(const StiSortedHitIterator& rhs);
  ////Dereference
  StiHit& operator*();
  ///prefix
  StiSortedHitIterator& operator++();
  ///postfix
  StiSortedHitIterator operator++(int);
  ///We demarcate the end of the traversal via  a singular iterator
  //StiSortedHitIterator end();
  
private:
    /// Pointer to hit
    StiHit * _currentHit;
    /// Pointer to hit container this iterator is meant to traverse
    StiHitContainer * _hitContainer;
    /// Iterator pointing to current detector
    vector<StiDetector*>::iterator _currentDet;
    /// Iterator pointing to the first detector to traverse
    vector<StiDetector*>::iterator _firstDet;
    /// Iterator pointing to the last detector to traverse
    vector<StiDetector*>::iterator _lastDet;
    /// Iterator pointig to current hit on current detector
    vector<StiHit*>::iterator _currentDetHit;
    /// Iterator pointing to last hit on current detector
    vector<StiHit*>::iterator _lastDetHit;
};



/// Asignment operator
inline const StiSortedHitIterator& StiSortedHitIterator::operator=(const StiSortedHitIterator&iter)
{
  _currentHit = iter._currentHit; 
  _hitContainer = iter._hitContainer;
  _currentDet = iter._currentDet;
  _firstDet = iter._firstDet;
  _lastDet  = iter._lastDet;
  _currentDetHit = iter._currentDetHit;
  _lastDetHit = iter._lastDetHit;
  return *this;
}


/// Equality operator
/// Two iterators are equal (return true) if they point to the same hit.
inline bool StiSortedHitIterator::operator==(const StiSortedHitIterator& rhs)
{
  return _currentHit==rhs._currentHit;
}

/// Inequality
/// Two iterators are not equal unless they point to the same hit.
inline bool StiSortedHitIterator::operator!=(const StiSortedHitIterator& rhs)
{
  return _currentHit!=rhs._currentHit;
}

/// Dereferencing operator
/// Returns a reference to the hit pointed at by this iterator
inline StiHit& StiSortedHitIterator::operator*()
{
  assert(_currentHit);
  return *_currentHit;
}

/// Prefix Increment Operator
/// The iterator is incremented to point to the next hit in the sorted container.
/// In the case where the prefix operator increments beyond the end of the container,
/// the pointer to _currentHit is set to 0.   This demarcates the end of the traversal.
inline StiSortedHitIterator& StiSortedHitIterator::operator++()
{
  // if this iterator points to a null hit, return because there is nothing to do.
  // This guarantees the end() iterator will behave properly.
  if (_currentHit==0)
    return *this;

  ++_currentDetHit;
  if (_currentDetHit<_lastDetHit)
    {
      _currentHit = *_currentDetHit;
    }
  else
    {
      // Reached end of current detector hits
      // Must go to next detector with hits.
      if (_currentDet<_lastDet)
	{
	  // Still have detectors to traverse, find next one which
	  // has hits.
	  bool go = true;
	  while (_currentDet<_lastDet && go )
	    {
	      ++_currentDet;
	      if (_currentDet<_lastDet )
		{
		  // valid detector
        if (_hitContainer->hasDetector(*_currentDet)) {

		  _currentDetHit =  _hitContainer->hitsBegin(*_currentDet);
		  _lastDetHit    =  _hitContainer->hitsEnd(*_currentDet);
		  if (_currentDetHit < _lastDetHit)
		    {
		      // current detector has hits
		      // done for now.
		      _currentHit = *_currentDetHit;
		      go = false;
		    }
       }
		}
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
      else  // !(_currentDet<_lastDet)
	{
	  _currentHit = 0;
	  _currentDet = _lastDet;
	  _currentDetHit = _lastDetHit;
	}
    }
  return *this;
}
    

/// Postfix Increment Operator
/// The iterator is increment to point at the next hit in the sorted container.
/// In the case where the prefix operator increments beyond the end of the container,
/// the pointer to _currentHit is set to 0.   This demarcates the end of the traversal.
inline StiSortedHitIterator StiSortedHitIterator::operator++(int)
{
    StiSortedHitIterator temp = *this;
    ++(*this);
    return temp;
}

#endif
