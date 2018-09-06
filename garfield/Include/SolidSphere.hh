#ifndef G_SOLID_SPHERE_H
#define G_SOLID_SPHERE_H

#include "Solid.hh"

namespace Garfield {

/// Sphere.

class SolidSphere : public Solid {

 public:
  /// Constructor
  SolidSphere(const double cx, const double cy, const double cz,
              const double rmin, const double rmax);
  /// Destructor
  ~SolidSphere() {}

  virtual bool IsInside(const double x, const double y, 
                        const double z) const override;
  virtual bool GetBoundingBox(double& xmin, double& ymin, 
                              double& zmin, 
                              double& xmax, double& ymax, 
                              double& zmax) const override;
  virtual bool IsSphere() const override { return true; }

  virtual bool GetCenter(double& x, double& y, double& z) const override;
  virtual bool GetDimensions(double& l1, double& l2, double& l3) const override;
  virtual bool GetOrientation(double& ctheta, double& stheta, double& cphi,
                              double& sphi) const override;

  void SetInnerRadius(const double rmin);
  void SetOuterRadius(const double rmax);

 private:
  // Center of the sphere 
  double m_cX, m_cY, m_cZ;
  // Inner and outer radius
  double m_rMin, m_rMax;
};
}

#endif
