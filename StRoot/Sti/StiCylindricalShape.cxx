#include "StiCylindricalShape.h"

StiCylindricalShape::StiCylindricalShape(const string &name,
					 float halfDepth,
                                         float thickness,
					 float outerRadius,
                                         float openingAngle)
  : StiShape(name,halfDepth, thickness),
    _outerRadius(outerRadius),
    _openingAngle(openingAngle)
{}// StiCylindricalShape()

void StiCylindricalShape::setOuterRadius(float val)
{
  if (val >= 0.){ _outerRadius = val; }
}// setOuterRadius()

void StiCylindricalShape::setOpeningAngle(float val){
  _openingAngle = nice(val);
}// setOpeningAngle()

