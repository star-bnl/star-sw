#include "StiPlanarShape.h"

StiPlanarShape::StiPlanarShape(const string &name,
			       float halfDepth,
                               float thickness, float halfWidth)
  : StiShape(name,halfDepth, thickness),
    _halfWidth(halfWidth)
{}// StiPlanarShape()

void StiPlanarShape::setHalfWidth(float val){
  if(val >= 0.){ _halfWidth = val; }
}// setHalfWidth()


double  StiPlanarShape::getVolume() const
{ return getHalfWidth()*getHalfDepth()*getThickness()*4;}

