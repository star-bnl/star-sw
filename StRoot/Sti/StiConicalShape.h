// Class to represent a shape within the STAR geometry
// Ben Norman, Kent State
// 25 July 01

#ifndef STI_CONICAL_SHAPE_H
#define STI_CONICAL_SHAPE_H

#include "StiShape.h"

class StiConicalShape: public StiShape
{
public:
    
    // constructor
    StiConicalShape(): StiShape(), outerRadiusEast(0.), outerRadiusWest(0.),
                       openingAngle(0.){}
    StiConicalShape(float halfDepth_, float thickness_,
                    float outerRadiusEast_, float outerRadiusWest_,
                    float openingAngle_);
    
    // accessors
    float getOuterRadiusEast() const { return outerRadiusEast; }
    float getOuterRadiusWest() const { return outerRadiusWest; }
    float getOpeningAngle() const { return openingAngle; }
    StiShapeCode getShapeCode() const { return kConical; };

    // mutators
    void setOuterRadiusEast(float val);
    void setOuterRadiusWest(float val);
    void setOpeningAngle(float val);

protected:
    
    float outerRadiusEast;  // >= 0
    float outerRadiusWest;  // >= 0
    float openingAngle; // azimuthal extent of cylinder in [0, 2pi]
    
};

#endif
