#include <iostream>
#include <cmath>

#include "SolidSphere.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidSphere::SolidSphere(const double& cx, const double& cy, const double& cz,
                         const double& rmin, const double& rmax)
    : Solid(),
      m_cX(cx), m_cY(cy), m_cZ(cz),
      m_rMin(rmin), m_rMax(rmax) {}

bool SolidSphere::IsInside(const double& x, const double& y, const double& z) const {

  // Transform the point to local coordinates
  const double dx = x - m_cX;
  const double dy = y - m_cY;
  const double dz = z - m_cZ;

  if (fabs(dx) > m_rMax || fabs(dy) > m_rMax || fabs(dz) > m_rMax) {
    if (m_debug) {
      std::cout << "SolidSphere::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is outside.\n";
    }
    return false;
  }

  const double r = sqrt(dx * dx + dy * dy + dz * dz);
  if (r >= m_rMin && r <= m_rMax) {
    if (m_debug) {
      std::cout << "SolidSphere::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is inside.\n";
    }
    return true;
  }

  if (m_debug) {
    std::cout << "SolidSphere::IsInside:\n";
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is outside.\n";
  }
  return false;
}

bool SolidSphere::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                 double& xmax, double& ymax, double& zmax) const {

  xmin = m_cX - m_rMax;
  xmax = m_cX + m_rMax;
  ymin = m_cY - m_rMax;
  ymax = m_cY + m_rMax;
  zmin = m_cZ - m_rMax;
  zmax = m_cZ + m_rMax;
  return true;
}

bool SolidSphere::GetCenter(double& x, double& y, double& z) const {

  x = m_cX;
  y = m_cY;
  z = m_cZ;
  return true;
}

bool SolidSphere::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_rMin;
  l2 = m_rMax;
  l3 = 0.;
  return true;
}

bool SolidSphere::GetOrientation(double& ctheta, double& stheta, double& cphi,
                                 double& sphi) const {

  ctheta = 1.;
  stheta = 0.;
  cphi = 1.;
  sphi = 0.;
  return true;
}

void SolidSphere::SetInnerRadius(const double& rmin) {

  if (rmin <= 0.) {
    std::cerr << "SolidSphere::SetInnerRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmin >= m_rMax) {
    std::cerr << "SolidSphere::SetInnerRadius:\n";
    std::cerr << "    Inner radius must be smaller than outer radius.\n";
    return;
  }
  m_rMin = rmin;
}

void SolidSphere::SetOuterRadius(const double& rmax) {

  if (rmax <= 0.) {
    std::cerr << "SolidSphere::SetOuterRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmax <= m_rMin) {
    std::cerr << "SolidSphere::SetOuterRadius:\n";
    std::cerr << "    Outer radius must be greater than inner radius.\n";
    return;
  }
  m_rMax = rmax;
}

}
