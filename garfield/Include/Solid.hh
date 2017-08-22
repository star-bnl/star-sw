#ifndef G_SOLID_H
#define G_SOLID_H

namespace Garfield {

/// Abstract base class for solids.

class Solid {

 public:
  /// Constructor
  Solid() : m_debug(false) {}
  /// Destructor
  virtual ~Solid() {}

  /// Check whether a given point is inside the solid.
  virtual bool IsInside(const double x, const double y, 
                        const double z) const = 0;
  /// Return the bounding box of the solid.
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax) const = 0;
  /// Return true if the solid is a box.
  virtual bool IsBox() const { return false; }
  /// Return true if the solid is a tube.
  virtual bool IsTube() const { return false; }
  /// Return true if the solid is a sphere.
  virtual bool IsSphere() const { return false; }

  virtual bool GetCenter(double& x, double& y, double& z) const = 0;
  virtual bool GetDimensions(double& l1, double& l2, double& l3) const = 0;
  virtual bool GetOrientation(double& ctheta, double& stheta, double& cphi,
                              double& shpi) const = 0;

  /// Switch on debugging messages.
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  bool m_debug;
};
}

#endif
