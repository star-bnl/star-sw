#include <math.h>

#include "StiConicalShape.h"

StiConicalShape::StiConicalShape(float halfDepth_,
                                 float thickness_, float outerRadiusEast_,
                                 float outerRadiusWest_, float openingAngle_):
        StiShape(halfDepth_, thickness_){
  
  setOuterRadiusEast(outerRadiusEast_);
  setOuterRadiusWest(outerRadiusWest_);
  setOpeningAngle(openingAngle_);
 
}// StiConicalShape()

void StiConicalShape::setOuterRadiusEast(float val){
  if (val >= 0.){ outerRadiusEast = val; }
}// setOuterRadiusEast()

void StiConicalShape::setOuterRadiusWest(float val){
  if (val >= 0.){ outerRadiusWest = val; }
}// setOuterRadiusWest()

void StiConicalShape::setOpeningAngle(float val){
  while (val <      0.){ val += 2*M_PI; }
  while (val > 2.*M_PI){ val -= 2*M_PI; }
  openingAngle = val;
}// setOpeningAngle()
