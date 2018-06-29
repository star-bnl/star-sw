// Class to represent a shape within the STAR geometry
// Ben Norman, Kent State
// 25 July 01

#ifndef STI_PLANAR_SHAPE_H
#define STI_PLANAR_SHAPE_H

#include "StiShape.h"

class StiPlanarShape: public StiShape
{
 public:
  
  // constructor
  StiPlanarShape(): StiShape(), _halfWidth(0.){}
  StiPlanarShape(const string &name, float halfDepth, float thickness, float halfWidth);
  
  // accessors
virtual  float getHalfWidth() 	const { return _halfWidth;}	//deltaY
virtual  float getOpeningAngle()const { return 999       ;} 
  StiShapeCode getShapeCode() 	const { return kPlanar   ;}
double getVolume() 	const;


  // mutators
  void setHalfWidth(float val);
  
 protected:
  /// Half extent in local y, always >= 0
  float _halfWidth;  
};

#endif
