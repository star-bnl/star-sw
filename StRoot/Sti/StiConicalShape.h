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
    StiConicalShape(): StiShape(), _outerRadiusEast(0.), _outerRadiusWest(0.),
                       _openingAngle(0.){}
    StiConicalShape(const string &name,
		    float halfDepth, 
		    float thickness,
                    float outerRadiusEast, 
		    float outerRadiusWest,
                    float openingAngle);
    
    // accessors
    float getOuterRadiusEast() const { return _outerRadiusEast; }
    float getOuterRadiusWest() const { return _outerRadiusWest; }
    float getOpeningAngle() const { return _openingAngle; }
    StiShapeCode getShapeCode() const { return kConical; };

    // mutators
    void setOuterRadiusEast(float val);
    void setOuterRadiusWest(float val);
    void setOpeningAngle(float val);

protected:
    
    float _outerRadiusEast;  // >= 0
    float _outerRadiusWest;  // >= 0
    float _openingAngle; // azimuthal extent of cylinder in [0, 2pi]
    
};

#endif
