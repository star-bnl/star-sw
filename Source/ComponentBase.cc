#include <iostream>
#include "ComponentBase.hh"

namespace Garfield {

ComponentBase::ComponentBase()
    : m_className("ComponentBase"),
      m_geometry(NULL),
      m_ready(false),
      m_xPeriodic(false),
      m_yPeriodic(false),
      m_zPeriodic(false),
      m_xMirrorPeriodic(false),
      m_yMirrorPeriodic(false),
      m_zMirrorPeriodic(false),
      m_xAxiallyPeriodic(false),
      m_yAxiallyPeriodic(false),
      m_zAxiallyPeriodic(false),
      m_xRotationSymmetry(false),
      m_yRotationSymmetry(false),
      m_zRotationSymmetry(false),
      m_bx0(0.),
      m_by0(0.),
      m_bz0(0.),
      m_debug(false) {}

void ComponentBase::SetGeometry(GeometryBase* geo) {

  // Make sure the geometry is defined
  if (!geo) {
    std::cerr << "ComponentBase::SetGeometry:\n";
    std::cerr << "    Geometry pointer is null.\n";
    return;
  }

  m_geometry = geo;
}

Medium* ComponentBase::GetMedium(const double x, const double y, 
                                 const double z) {

  if (!m_geometry) return NULL;
  return m_geometry->GetMedium(x, y, z);
}

void ComponentBase::Clear() {

  m_geometry = NULL;
  Reset();
}

void ComponentBase::WeightingField(const double x, const double y,
                                   const double z, double& wx, double& wy,
                                   double& wz, const std::string& label) {
  if (m_debug) {
    std::cerr << m_className << "::WeightingField:\n";
    std::cerr << "    This function is not implemented.\n";
    std::cerr << "    Weighting field at (" << x << ", " << y << ", " << z
              << ") for electrode " << label << " cannot be calculated.\n";
  }
  wx = wy = wz = 0.;
}

double ComponentBase::WeightingPotential(const double x, const double y,
                                         const double z,
                                         const std::string& label) {

  if (m_debug) {
    std::cerr << m_className << "::WeightingPotential:\n";
    std::cerr << "    This function is not implemented.\n";
    std::cerr << "    Weighting potential at (" << x << ", " << y << ", " << z
              << ") for electrode " << label << " cannot be calculated.\n";
  }
  return 0.;
}

void ComponentBase::MagneticField(const double x, const double y,
                                  const double z, double& bx, double& by,
                                  double& bz, int& status) {
  bx = m_bx0;
  by = m_by0;
  bz = m_bz0;
  if (m_debug) {
    std::cout << m_className << "::MagneticField:\n";
    std::cout << "    Magnetic field at (" << x << ", " << y << ", " << z
              << ") is (" << bx << ", " << by << ", " << bz << ")\n";
  }
  status = 0;
}

void ComponentBase::SetMagneticField(const double bx, const double by,
                                     const double bz) {

  m_bx0 = bx;
  m_by0 = by;
  m_bz0 = bz;
}

bool ComponentBase::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                   double& xmax, double& ymax, double& zmax) {

  if (!m_geometry) return false;
  return (m_geometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax));
}

bool ComponentBase::IsWireCrossed(const double x0, const double y0,
                                  const double z0, 
                                  const double /*x1*/, const double /*y1*/, 
                                  const double /*z1*/, double& xc,
                                  double& yc, double& zc) {

  xc = x0;
  yc = y0;
  zc = z0;
  return false;

}

bool ComponentBase::IsInTrapRadius(const double /*q0*/, const double x0, 
                                   const double y0, const double /*z0*/, 
                                   double& xw, double& yw, double& rw) {

  xw = x0;
  yw = y0;
  rw = 0.;
  return false;

}
}
