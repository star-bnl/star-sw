// Class to represent a shape within the STAR geometry
// Ben Norman, Kent State
// 25 July 01

#ifndef STI_PLANAR_SHAPE_H
#define STI_PLANAR_SHAPE_H

#include "StiShape.h"

class StiPlanarShape: public StiShape{
public:

    // constructor
    StiPlanarShape(): StiShape(), halfWidth(0.){}
    StiPlanarShape(float halfDepth_, float thickness_, float halfWidth_);

    // accessors
    float getHalfWidth() const { return halfWidth; };
    StiShapeCode getShapeCode() const { return kPlanar; };

    // mutators
    void setHalfWidth(float val);

protected:
    
    float halfWidth;  // 1/2 extent in local x, always >= 0
    
};

#endif
