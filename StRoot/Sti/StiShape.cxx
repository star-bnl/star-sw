#include "StiShape.h"

StiShape::StiShape(const string &name, float halfDepth, float thickness)
  : Named(name),
    _halfDepth(halfDepth),
    _thickness(thickness),
    _edgeWidth(0)
{}// StiShape()

void StiShape::setHalfDepth(float val){
  if(val >= 0.){ _halfDepth = val; }
}// setHalfDepth()

void StiShape::setThickness(float val){
  if(val >= 0.){ _thickness = val; }
}// setThickness()

