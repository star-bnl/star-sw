#include <iostream>
#include "StiTpcIsActiveFunctor.h"

StiTpcIsActiveFunctor::StiTpcIsActiveFunctor(bool active, bool west, bool east)
  : StiIsActiveFunctor(active,true),
    _eastActive(east),
    _westActive(west)
{} 

StiTpcIsActiveFunctor::~StiTpcIsActiveFunctor()
{} 

///Determines and returns whether the object is active at the given 
///coordinates. Note that the variable "_active" controls the overall
///state of the object whereas _eastActive and _westActive control whether
///the east  and west parts of the TPC pad  row are activated.
bool StiTpcIsActiveFunctor::operator()(double y, double z) const
{
  if (_active)
    {
      if (z<0.)
	return _eastActive && z>=-200.0;
      else
	return _westActive && z<= 200.0;
    }
  return false;
} // operator()

