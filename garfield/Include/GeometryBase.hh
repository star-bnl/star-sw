#ifndef G_GEOMETRY_BASE_H
#define G_GEOMETRY_BASE_H

#include <string>

#include "Medium.hh"
#include "Solid.hh"

namespace Garfield {

/// Abstract base class for geometry classes.

class GeometryBase {

 public:
  /// Constructor
  GeometryBase() = default;
  /// Destructor
  virtual ~GeometryBase() {}

  /// Retrieve the medium at a given point.
  virtual Medium* GetMedium(const double x, const double y, 
                            const double z) const = 0;
 
  /// Return the number of solids in the geometry.
  virtual unsigned int GetNumberOfSolids() const { return 0; }
  /// Get a solid from the list.
  virtual Solid* GetSolid(const unsigned int /*i*/) const { return nullptr; }

  /// Check if a point is inside the geometry.
  virtual bool IsInside(const double x, const double y, 
                        const double z) const = 0;

  /// Get the bounding box (envelope of the geometry).
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax) = 0;

 protected:
  std::string m_className = "GeometryBase";
};
}

#endif
