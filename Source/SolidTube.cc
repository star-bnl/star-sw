#include <iostream>
#include <cmath>

#include "SolidTube.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidTube::SolidTube(const double& cx, const double& cy, const double& cz,
                     const double& rmin, const double& rmax, const double& lz)
    : Solid(),
      m_cX(cx), m_cY(cy), m_cZ(cz),
      m_rMin(rmin), m_rMax(rmax),
      m_lZ(lz),
      m_dX(0.), m_dY(0.), m_dZ(1.),
      m_cPhi(1.), m_sPhi(0.),
      m_cTheta(1.), m_sTheta(0.) {}

SolidTube::SolidTube(const double& cx, const double& cy, const double& cz,
                     const double& rmin, const double& rmax, const double& lz,
                     const double& dx, const double& dy, const double& dz)
    : Solid(),
      m_cX(cx), m_cY(cy), m_cZ(cz),
      m_rMin(rmin), m_rMax(rmax),
      m_lZ(lz),
      m_dX(0.), m_dY(0.), m_dZ(1.),
      m_cPhi(1.), m_sPhi(0.),
      m_cTheta(1.), m_sTheta(0.) {

  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d < Small) {
    std::cerr << "SolidTube: Direction vector has zero norm.\n";
  } else {
    m_dX = dx / d;
    m_dY = dy / d;
    m_dZ = dz / d;
    double phi, theta;
    const double dt = sqrt(m_dX * m_dX + m_dY * m_dY);
    if (dt < Small) {
      phi = 0.;
      if (m_dZ > 0.) {
        theta = 0.;
      } else {
        theta = Pi;
      }
    } else {
      phi = atan2(m_dY, m_dX);
      theta = atan2(dt, m_dZ);
    }
    m_cTheta = cos(theta);
    m_sTheta = sin(theta);
    m_cPhi = cos(phi);
    m_sPhi = sin(phi);
  }
}

bool SolidTube::IsInside(const double& x, const double& y, const double& z) const {

  // Transform the point to local coordinates
  const double dx = x - m_cX;
  const double dy = y - m_cY;
  const double dz = z - m_cZ;
  const double u = m_cPhi * m_cTheta * dx + m_sPhi * m_cTheta * dy - m_sTheta * dz;
  const double v = -m_sPhi * dx + m_cPhi * dy;
  const double w = m_cPhi * m_sTheta * dx + m_sPhi * m_sTheta * dy + m_cTheta * dz;

  if (fabs(w) > m_lZ) {
    if (m_debug) {
      std::cout << "SolidTube::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is outside.\n";
    }
    return false;
  }

  const double r = sqrt(u * u + v * v);
  if (r >= m_rMin && r <= m_rMax) {
    if (m_debug) {
      std::cout << "SolidTube::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is inside.\n";
    }
    return true;
  }

  if (m_debug) {
    std::cout << "SolidTube::IsInside:\n";
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is outside.\n";
  }
  return false;
}

bool SolidTube::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                               double& xmax, double& ymax, double& zmax) const {

  if (m_cTheta == 1. && m_cPhi == 1.) {
    xmin = m_cX - m_rMax;
    xmax = m_cX + m_rMax;
    ymin = m_cY - m_rMax;
    ymax = m_cY + m_rMax;
    zmin = m_cZ - m_lZ;
    zmax = m_cZ + m_lZ;
    return true;
  }

  const double dd = sqrt(m_rMax * m_rMax + m_lZ * m_lZ);
  xmin = m_cX - dd;
  xmax = m_cX + dd;
  ymin = m_cY - dd;
  ymax = m_cY + dd;
  zmin = m_cZ - dd;
  zmax = m_cZ + dd;
  return true;
}

bool SolidTube::GetCenter(double& x, double& y, double& z) const {

  x = m_cX;
  y = m_cY;
  z = m_cZ;
  return true;
}

bool SolidTube::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_rMin;
  l2 = m_rMax;
  l3 = m_lZ;
  return true;
}

bool SolidTube::GetOrientation(double& ctheta, double& stheta, double& cphi,
                               double& sphi) const {

  ctheta = m_cTheta;
  stheta = m_sTheta;
  cphi = m_cPhi;
  sphi = m_sPhi;
  return true;
}

void SolidTube::SetInnerRadius(const double& rmin) {

  if (rmin <= 0.) {
    std::cerr << "SolidTube::SetInnerRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmin >= m_rMax) {
    std::cerr << "SolidTube::SetInnerRadius:\n";
    std::cerr << "    Inner radius must be smaller than outer radius.\n";
    return;
  }
  m_rMin = rmin;
}

void SolidTube::SetOuterRadius(const double& rmax) {

  if (rmax <= 0.) {
    std::cerr << "SolidTube::SetOuterRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmax <= m_rMin) {
    std::cerr << "SolidTube::SetOuterRadius:\n";
    std::cerr << "    Outer radius must be greater than inner radius.\n";
    return;
  }
  m_rMax = rmax;
}

void SolidTube::SetHalfLengthZ(const double& lz) {

  if (lz <= 0.) {
    std::cerr << "SolidTube::SetHalfLengthZ:\n";
    std::cerr << "    Half-length must be > 0.\n";
    return;
  }
  m_lZ = lz;
}
}
