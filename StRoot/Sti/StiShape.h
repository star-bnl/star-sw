// Class to represent a shape within the STAR geometry
// Ben Norman, Kent State
// 25 July 01

#ifndef STI_SHAPE_H
#define STI_SHAPE_H

// allowed values for shapeCode
enum StiShapeCode {kPlanar = 1, kCylindrical, kConical}; 

class StiShape{
public:

    // constructor (for subclass use)
    StiShape(): halfDepth(0.), thickness(0.){}
    StiShape(float halfDepth_, float thickness_);

    // accessors
    float getHalfDepth() const { return halfDepth; }
    float getThickness() const { return thickness; }
    virtual StiShapeCode getShapeCode() const = 0;
    float getEdgeWidth() const { return edgeWidth; }

    // mutators
    void setHalfDepth(float val);
    void setThickness(float val);

protected:
    
    float halfDepth;  // 1/2 extent in z, always >= 0
    float thickness;  // radial thickness, always >= 0
    float edgeWidth;  // size of the edge using in tracking
};

#endif
