#include "StiPlanarShape.h"

StiPlanarShape::StiPlanarShape(float halfDepth_,
                               float thickness_, float halfWidth_):
        StiShape(halfDepth_, thickness_){
  
  setHalfWidth(halfWidth_);

}// StiPlanarShape()

void StiPlanarShape::setHalfWidth(float val){
  if(val >= 0.){ halfWidth = val; }
}// setHalfWidth()
