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

  bool IsInside(const double x, const double y, const double z) const override;
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, 
                      double& xmax, double& ymax, double& zmax) const override;
  bool IsSphere() const override { return true; }

  bool GetDimensions(double& l1, double& l2, double& l3) const override;

  void SetInnerRadius(const double rmin);
  void SetOuterRadius(const double rmax);

  /// When calculating surface panels, the sphere is approxiamted by a set of 
  /// parallelograms, much the same way maps are drawn. N specifies the number 
  /// of meridians and also the number of parallels.
  void SetMeridians(const unsigned int n);
 
  bool SolidPanels(std::vector<Panel>& panels) override;

 private:
  // Inner and outer radius
  double m_rMin = 0., m_rMax;

  // Number of meridians.
  unsigned int m_n = 10;
};
}

#endif
