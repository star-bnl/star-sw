#ifndef STI_CYLINDRICAL_SHAPE_H
#define STI_CYLINDRICAL_SHAPE_H

#include "StiShape.h"

/*!
  Class to represent a shape within the STAR geometry
  \author Ben Norman, Kent State, 25 July 01
*/
class StiCylindricalShape: public StiShape{
public:

  // constructor
    StiCylindricalShape(): StiShape(), _outerRadius(0.), _openingAngle(0.){}
    StiCylindricalShape(const string &name,
			float halfDepth_, 
			float thickness_,
                        float outerRadius_, 
			float openingAngle_)
  : StiShape(name,halfDepth_, thickness_),
    _outerRadius(outerRadius_),
    _openingAngle(openingAngle_) {}// StiCylindricalShape()
    // accessors
    float getOuterRadius() const { return _outerRadius; }
    float getOpeningAngle() const { return _openingAngle; } 
    float getHalfWidth() const;
    StiShapeCode getShapeCode() const { return kCylindrical; };
  virtual double getVolume() const;

    // mutators
    void setOuterRadius(float val) {if (val >= 0.) _outerRadius = val; }
    void setOpeningAngle(float val){_openingAngle = val;}

protected:
    
    float _outerRadius;  // >= 0
    float _openingAngle; // azimuthal extent of cylinder in radians in [0, 2pi]
    
};

inline float StiCylindricalShape::getHalfWidth() const
{
  return _outerRadius*sin(_openingAngle/2.);
}
inline double StiCylindricalShape::getVolume() const
{
  double rmax = _outerRadius,rmin=rmax-getThickness();
  return _openingAngle*(rmax-rmin)*(rmax+rmin)*getHalfDepth();
}


#endif
