// Class to represent a shape within the STAR geometry
// Ben Norman, Kent State
// 25 July 01

#ifndef STI_CYLINDRICAL_SHAPE_H
#define STI_CYLINDRICAL_SHAPE_H

#include "StiShape.h"

class StiCylindricalShape: public StiShape{
public:

  // constructor
    StiCylindricalShape(): StiShape(), outerRadius(0.), openingAngle(0.){}
    StiCylindricalShape(float halfDepth_, float thickness_,
                        float outerRadius_, float openingAngle_);

    // accessors
    float getOuterRadius() const { return outerRadius; }
    float getOpeningAngle() const { return openingAngle; }
    StiShapeCode getShapeCode() const { return kCylindrical; };

    // mutators
    void setOuterRadius(float val);
    void setOpeningAngle(float val);

protected:
    
    float outerRadius;  // >= 0
    float openingAngle; // azimuthal extent of cylinder in radians in [0, 2pi]
    
};

#endif
