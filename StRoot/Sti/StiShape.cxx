#include "StiShape.h"

StiShape::StiShape(float halfDepth_, float thickness_){

  setHalfDepth(halfDepth_);
  setThickness(thickness_);

}// StiShape()

void StiShape::setHalfDepth(float val){
  if(val >= 0.){ halfDepth = val; }
}// setHalfDepth()

void StiShape::setThickness(float val){
  if(val >= 0.){ thickness = val; }
}// setThickness()

