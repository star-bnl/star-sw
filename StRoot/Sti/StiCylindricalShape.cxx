#include <math.h>

#include "StiCylindricalShape.h"

StiCylindricalShape::StiCylindricalShape(float halfDepth_,
                                         float thickness_, float outerRadius_,
                                         float openingAngle_):
        StiShape(halfDepth_, thickness_){
  
  setOuterRadius(outerRadius_);
  setOpeningAngle(openingAngle_);
 
}// StiCylindricalShape()

void StiCylindricalShape::setOuterRadius(float val){
  if (val >= 0.){ outerRadius = val; }
}// setOuterRadius()

void StiCylindricalShape::setOpeningAngle(float val){
  while (val < 0.){ val += 2*M_PI; }
  while (val >= 2*M_PI){ val -= 2*M_PI; }
  openingAngle = val;
}// setOpeningAngle()
