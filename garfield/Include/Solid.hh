#ifndef G_SOLID_H
#define G_SOLID_H

#include <vector>

namespace Garfield {

/// Surface panel.

struct Panel {
  /// Perpendicular vector
  double a, b, c;
  /// X-coordinates of vertices
  std::vector<double> xv;
  /// Y-coordinates of vertices
  std::vector<double> yv;
  /// Z-coordinates of vertices
  std::vector<double> zv;
  /// Colour index
  double colour;
  /// Reference to solid to which the panel belongs
  int volume;
};

/// Abstract base class for solids.

class Solid {

 public:
  /// Default constructor.
  Solid() = default;
  /// Constructor.
  Solid(const double cx, const double cy, const double cz,
        const std::string& name)
    : m_cX(cx), m_cY(cy), m_cZ(cz), m_className(name) {}
 
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

  /// Retrieve the centre point of the solid.
  bool GetCentre(double& x, double& y, double& z) const {
    x = m_cX;
    y = m_cY;
    z = m_cZ;
    return true;
  }
  /// Retrieve the orientation (azimuthal and polar angles) of the solid.
  bool GetOrientation(double& ctheta, double& stheta, double& cphi,
                      double& sphi) const {
    ctheta = m_cTheta;
    stheta = m_sTheta;
    cphi = m_cPhi;
    sphi = m_sPhi;
    return true;
  }

  /// Return the characteristic dimensions of the solid.
  virtual bool GetDimensions(double& l1, double& l2, double& l3) const = 0;

  virtual double GetHalfLengthX() const {
    return NotImplemented("GetHalfLengthX");
  }
  virtual double GetHalfLengthY() const {
    return NotImplemented("GetHalfLengthY");
  }
  virtual double GetHalfLengthZ() const {
    return NotImplemented("GetHalfLengthZ");
  }
  virtual double GetHalfLength() const {
    return NotImplemented("GetHalfLength");
  }
  virtual double GetInnerRadius() const {
    return NotImplemented("GetInnerRadius");
  }
  virtual double GetOuterRadius() const {
    return NotImplemented("GetOuterRadius");
  }
  virtual double GetRadius() const {
    return NotImplemented("GetRadius");
  }
  virtual double GetRidgeOffset() const {
    return NotImplemented("GetRidgeOffset");
  }
  virtual double GetRidgeHeight() const {
    return NotImplemented("GetRidgeHeight");
  }

  /// Retrieve the surface panels of the solid.
  virtual bool SolidPanels(std::vector<Panel>& panels) = 0;

  /// Apply Dirichlet boundary conditions (fixed voltage).
  void SetBoundaryPotential(const double v) {
    m_volt = v;
    m_bctype = Voltage;
  }
  /// Apply fixed-charge boundary conditions.
  void SetBoundaryCharge(const double q) {
    m_charge = q; 
    m_bctype = Charge;
  }
  /// Make the potential at the surface of the solid floating.
  void SetBoundaryFloat() { m_bctype = Float; }
  /// Set the dielectric constant.
  void SetBoundaryDielectric(const double eps) {
    m_eps = eps;  
    m_bctype = Dielectric;
  }
  void SetBoundaryParallelField() { m_bctype = ParallelField; }
  void SetBoundaryPerpendicularField() { m_bctype = PerpendicularField; }

  /// Switch debugging messages on/off.
  void EnableDebugging(const bool on = true) { m_debug = on; }

 protected:
  /// Centre of the solid.
  double m_cX = 0., m_cY = 0., m_cZ = 0.;

  /// Direction vector.
  double m_dX = 0., m_dY = 0., m_dZ = 1.;
  /// Azimuthal angle.
  double m_cPhi = 1., m_sPhi = 0.;
  /// Polar angle.
  double m_cTheta = 1., m_sTheta = 0.;

  /// Class name.
  std::string m_className = "Solid";

  /// Debug flag.
  bool m_debug = false;

  enum BoundaryCondition {
    Voltage = 1,
    Charge,
    Float,
    Dielectric,
    ParallelField = 6,
    PerpendicularField
  };
  /// Type of boundary condition.
  BoundaryCondition m_bctype = Voltage;
  /// Potential at the surface.
  double m_volt = 0.;
  /// Surface charge.
  double m_charge = 0.;
  /// Dielectric constant.
  double m_eps = 0.;

  /// Transform global coordinates (x, y, z) to local coordinates (u, v, w).
  void ToLocal(const double x, const double y, const double z,
               double& u, double& v, double& w) const {
    const double dx = x - m_cX;
    const double dy = y - m_cY;
    const double dz = z - m_cZ;

    u =  m_cPhi * m_cTheta * dx + m_sPhi * m_cTheta * dy - m_sTheta * dz;
    v = -m_sPhi * dx + m_cPhi * dy;
    w =  m_cPhi * m_sTheta * dx + m_sPhi * m_sTheta * dy + m_cTheta * dz;
  }
  /// Transform local coordinates (u, v, w) to global coordinates (x, y, z).
  void ToGlobal(const double u, const double v, const double w,
                double& x, double& y, double& z) const {
    x = m_cX + m_cPhi * m_cTheta * u - m_sPhi * v + m_cPhi * m_sTheta * w;
    y = m_cY + m_sPhi * m_cTheta * u + m_cPhi * v + m_sPhi * m_sTheta * w;
    z = m_cZ - m_sTheta * u + m_cTheta * w;
  } 

  void SetDirection(const double dx, const double dy, const double dz);

 private:
  double NotImplemented(const std::string& fcn) const;
};
}

#endif
