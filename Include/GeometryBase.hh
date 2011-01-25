// Abstract base class for geometry classes

#ifndef G_GEOMETRY_BASE_H
#define G_GEOMETRY_BASE_H

#include <string>

#include "Medium.hh"

namespace Garfield {

class GeometryBase {

  public:
    // Constructor
    GeometryBase() : className("GeometryBase") {}
    virtual ~GeometryBase() {}

    virtual bool 
    GetMedium(const double x, const double y, const double z, Medium*& m) = 0;

    // Check if a point is inside the geometry
    virtual bool
    IsInside(const double x, const double y, const double z) = 0;

    // Bounding box (envelope of geometry)
    virtual bool 
    GetBoundingBox(double& xmin, double& ymin, double& zmin,
                   double& xmax, double& ymax, double& zmax) = 0;

    protected:

      std::string className;

};
  
}

#endif
