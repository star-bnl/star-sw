#ifndef G_SOLID_BOX_H
#define G_SOLID_BOX_H

#include "Solid.hh"

namespace Garfield {

/// Box.

class SolidBox : public Solid {

 public:
  /// Constructor from centre and half-widths.
  SolidBox(const double cx, const double cy, const double cz, 
           const double lx, const double ly, const double lz);
  /// Constructor from centre, half-widths, and orientation.
  SolidBox(const double cx, const double cy, const double cz, 
           const double lx, const double ly, const double lz, 
           const double dx, const double dy, const double dz);
  /// Destructor
  ~SolidBox() {}

  bool IsInside(const double x, const double y, const double z) const override;
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, 
                      double& xmax, double& ymax, double& zmax) const override;
  bool IsBox() const override { return true; }

  bool GetDimensions(double& l1, double& l2, double& l3) const override;

  double GetHalfLengthX() const override { return m_lX; }
  double GetHalfLengthY() const override { return m_lY; }
  double GetHalfLengthZ() const override { return m_lZ; }

  void SetHalfLengthX(const double lx);
  void SetHalfLengthY(const double ly);
  void SetHalfLengthZ(const double lz);

  bool SolidPanels(std::vector<Panel>& panels) override;

 private:
  /// Half lengths.
  double m_lX = 0., m_lY = 0., m_lZ = 0.;

};
}

#endif
