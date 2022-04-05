#ifndef STI_SHAPE_H
#define STI_SHAPE_H
#include <math.h>
#include "Sti/Base/Named.h"
#include "Stiostream.h"
using namespace std;

// allowed values for shapeCode
enum StiShapeCode {kPlanar = 1, kCylindrical, kSector, kConical, kDisk}; 

/*! Class encapsulating the notion of detector/volume shape.
\author Ben Norman, Kent State, 25 July 01
\author Claude Pruneau, Wayne State, Oct 2002
*/
class StiShape : public Named
{
public:

  // constructor (for subclass use)
  StiShape(const string &name="undefined",float halfDepth=0, float thickness=0, float edge=0) :
  Named(name),_halfDepth(halfDepth),_thickness(thickness),_edgeWidth(edge) {}
  
    // accessors
    float getHalfDepth() const { return _halfDepth; } //Z direction
virtual float getHalfWidth() const=0;
        float getThickness() const { return _thickness; }
virtual StiShapeCode getShapeCode() const = 0;
        float getEdgeWidth() const { return _edgeWidth; }
virtual float getOpeningAngle() const =0; 
virtual float getOuterRadius() const {return -999;}

    virtual double getVolume() const = 0;

    // mutators
    void setHalfDepth(float val) {if(val >= 0.) _halfDepth = val; }
    void setThickness(float val) {if(val >= 0.) _thickness = val; }

 protected:
    
    double nice(double val);

    /// half extent along z, always >= 0
    float _halfDepth;  
    /// "thickness", always >= 0
    float _thickness;
    /// size of the edge used in tracking, deltaX local
    float _edgeWidth;  
};
//Non-members--------------------------

ostream& operator<<(ostream& os, const StiShape& m);


inline double StiShape::nice(double val)
{
  while (val < 0.){ val += 2*M_PI; }
  while (val >= 2*M_PI){ val -= 2*M_PI; }
  return val;
}

#endif
