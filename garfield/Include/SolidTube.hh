#ifndef G_SOLID_TUBE_H
#define G_SOLID_TUBE_H

#include "Solid.hh"

namespace Garfield {

/// Cylindrical tube.

class SolidTube : public Solid {

 public:
  /// Constructor from centre, inner/outer radii, and half-length.
  SolidTube(const double cx, const double cy, const double cz,
            const double rmin, const double rmax, const double lz);
  /// Constructor from centre, outer radius, and half-length.
  SolidTube(const double cx, const double cy, const double cz,
            const double r, const double lz);
  /// Constructor from centre, inner/outer radii, half-length and orientation.
  SolidTube(const double cx, const double cy, const double cz,
            const double rmin, const double rmax, const double lz,
            const double dx, const double dy, const double dz);
  /// Constructor from centre, outer radius, half-length and orientation.
  SolidTube(const double cx, const double cy, const double cz,
            const double r, const double lz,
            const double dx, const double dy, const double dz);
  /// Destructor
  ~SolidTube() {}

  bool IsInside(const double x, const double y, const double z) const override;
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, 
                      double& xmax, double& ymax, double& zmax) const override;
  bool IsTube() const override { return true; }

  bool GetDimensions(double& l1, double& l2, double& l3) const override;

  void SetHalfLength(const double lz);
  void SetInnerRadius(const double rmin);
  void SetOuterRadius(const double rmax);
  void SetRadius(const double r);

  double GetHalfLength() const override { return m_lZ; }
  double GetInnerRadius() const override { return m_rMin; }
  double GetOuterRadius() const override { return m_rMax; }
  double GetRadius() const override { return m_r; }
 
  /// When calculating the surface panels, the cylinder is 
  /// approximated as a polygon with a finite number of panels. 
  /// The number of corners of the polygon equals \f$4(n - 1)\f$. 
  /// Thus, \f$n = 2\f$ will produce a square, \f$n = 3\f$ an octagon etc.
  void SetSectors(const unsigned int n);
  /// Specify a rotation angle (radian) of the cylinder.
  /// Such a rotation is meaningful only if the number of sectors 
  /// (when approximating the circle with a polygon) has been chosen small.
  void SetRotation(const double angle) { m_rot = angle; }
  /// By default, the polygon used for approximating the cylinder when 
  /// calculating surface panels is inscribed in a circle 
  /// of the specified radius. If the "average-radius" flag is activated, 
  /// then the radius will be interpreted as the mean radius of the polygon 
  /// that approximates the cylinder.
  void SetAverageRadius(const bool average) { m_average = average; }
  /// Request the cylinder to be closed with a (polygonal) lid at +z.
  void SetToplid(const bool toplid) { m_toplid = toplid; }
  /// Request the cylinder to be closed with a (polygonal) lid at -z.
  void SetBottomlid(const bool bottomlid) { m_bottomlid = bottomlid; }

  unsigned int GetSectors() const { return m_n; }
  double GetRotation() const { return m_rot; }
  double GetAverage() const { return m_average; }

  bool SolidPanels(std::vector<Panel>& panels) override;

 private:
  // Inner and outer radius
  double m_rMin, m_rMax;
  double m_r;
  /// Half-length
  double m_lZ;

  /// Rotation angle
  double m_rot = 0.;
  /// Number of sectors
  unsigned int m_n = 2;
  /// Average chord over the sectors.
  bool m_average = false;
  /// Have a top lid?
  bool m_toplid = true;
  /// Have a bottom lid?
  bool m_bottomlid = true;
};
}

#endif
