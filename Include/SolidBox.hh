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

  virtual bool IsInside(const double x, const double y, 
                        const double z) const override;
  virtual bool GetBoundingBox(double& xmin, double& ymin, 
                              double& zmin, 
                              double& xmax, double& ymax, 
                              double& zmax) const override;
  virtual bool IsBox() const override { return true; }

  virtual bool GetCenter(double& x, double& y, double& z) const override;
  virtual bool GetDimensions(double& l1, double& l2, double& l3) const override;
  virtual bool GetOrientation(double& ctheta, double& stheta, double& cphi,
                              double& sphi) const override;

  void SetHalfLengthX(const double lx);
  void SetHalfLengthY(const double ly);
  void SetHalfLengthZ(const double lz);

 private:
  // Center of the box
  double m_cX, m_cY, m_cZ;
  // Half lengths
  double m_lX, m_lY, m_lZ;
  // Direction
  double m_dX, m_dY, m_dZ;
  double m_cPhi, m_sPhi;
  double m_cTheta, m_sTheta;
};
}

#endif
