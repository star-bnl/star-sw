// Abstract base class for geometry classes

#ifndef G_GEOMETRY_BASE_H
#define G_GEOMETRY_BASE_H

#include <string>

#include "Medium.hh"

namespace Garfield {

class GeometryBase {

 public:
  // Constructor
  GeometryBase() : m_className("GeometryBase") {}
  // Destructor
  virtual ~GeometryBase() {}

  virtual Medium* GetMedium(const double x, const double y, 
                            const double z) const = 0;

  // Check if a point is inside the geometry
  virtual bool IsInside(const double x, const double y, 
                        const double z) const = 0;

  // Bounding box (envelope of geometry)
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax) = 0;

 protected:
  std::string m_className;
};
}

#endif
