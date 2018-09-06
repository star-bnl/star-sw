#include <iostream>
#include "ComponentBase.hh"

namespace Garfield {

ComponentBase::ComponentBase() {}

void ComponentBase::SetGeometry(GeometryBase* geo) {

  // Make sure the geometry is defined
  if (!geo) {
    std::cerr << m_className << "::SetGeometry: Null pointer.\n";
    return;
  }

  m_geometry = geo;
}

Medium* ComponentBase::GetMedium(const double x, const double y, 
                                 const double z) {

  if (!m_geometry) return nullptr;
  return m_geometry->GetMedium(x, y, z);
}

void ComponentBase::Clear() {

  m_geometry = nullptr;
  Reset();
}

void ComponentBase::WeightingField(const double /*x*/, const double /*y*/,
                                   const double /*z*/, double& wx, double& wy,
                                   double& wz, const std::string& /*label*/) {
  if (m_debug) {
    std::cerr << m_className << "::WeightingField: Function not implemented.\n";
  }
  wx = wy = wz = 0.;
}

double ComponentBase::WeightingPotential(const double /*x*/, const double /*y*/,
                                         const double /*z*/,
                                         const std::string& /*label*/) {

  if (m_debug) {
    std::cerr << m_className << "::WeightingPotential: "
              << "Function not implemented.\n";
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
    std::cout << m_className << "::MagneticField: Field at ("
              << x << ", " << y << ", " << z << ") is (" 
              << bx << ", " << by << ", " << bz << ")\n";
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
  return m_geometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax);
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
