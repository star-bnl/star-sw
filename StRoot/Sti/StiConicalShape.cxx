#include <math.h>
#include "StiConicalShape.h"

StiConicalShape::StiConicalShape(const string &name,
				 float halfDepth,
                                 float thickness, 
				 float outerRadiusEast,
                                 float outerRadiusWest, 
				 float openingAngle)
  : StiShape(name,halfDepth, thickness),
    _outerRadiusEast(outerRadiusEast),
    _outerRadiusWest(outerRadiusWest),
    _openingAngle(nice(openingAngle))
{}// StiConicalShape()

void StiConicalShape::setOuterRadiusEast(float val){
  if (val >= 0.){ _outerRadiusEast = val; }
}// setOuterRadiusEast()

void StiConicalShape::setOuterRadiusWest(float val){
  if (val >= 0.){ _outerRadiusWest = val; }
}// setOuterRadiusWest()

void StiConicalShape::setOpeningAngle(float val)
{
  _openingAngle = nice(val);
}// setOpeningAngle()
