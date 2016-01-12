#include <iostream>
#include <cmath>

#include "SolidBox.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidBox::SolidBox(const double& cx, const double& cy, const double& cz,
                   const double& lx, const double& ly, const double& lz)
    : Solid(),
      m_cX(cx), m_cY(cy), m_cZ(cz),
      m_lX(lx), m_lY(ly), m_lZ(lz),
      m_dX(0.), m_dY(0.), m_dZ(1.),
      m_cPhi(1.), m_sPhi(0.),
      m_cTheta(1.), m_sTheta(0.) {

  std::cout << "SolidBox:\n";
  std::cout << "    " << cx - lx << " < x [cm] < " << cx + lx << "\n";
  std::cout << "    " << cy - ly << " < y [cm] < " << cy + ly << "\n";
  std::cout << "    " << cz - lz << " < z [cm] < " << cz + lz << "\n";
}

SolidBox::SolidBox(const double& cx, const double& cy, const double& cz,
                   const double& lx, const double& ly, const double& lz,
                   const double& dx, const double& dy, const double& dz)
    : Solid(),
      m_cX(cx), m_cY(cy), m_cZ(cz),
      m_lX(lx), m_lY(ly), m_lZ(lz),
      m_dX(0.), m_dY(0.), m_dZ(1.),
      m_cPhi(1.), m_sPhi(0.),
      m_cTheta(1.), m_sTheta(0.) {

  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d < Small) {
    std::cerr << "SolidBox: Direction vector has zero norm.\n";
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

bool SolidBox::IsInside(const double& x, const double& y, 
                        const double& z) const {

  // Transform the point to local coordinates
  const double dx = x - m_cX;
  const double dy = y - m_cY;
  const double dz = z - m_cZ;
  const double u =  m_cPhi * m_cTheta * dx + m_sPhi * m_cTheta * dy - m_sTheta * dz;
  const double v = -m_sPhi * dx + m_cPhi * dy;
  const double w =  m_cPhi * m_sTheta * dx + m_sPhi * m_sTheta * dy + m_cTheta * dz;

  // See whether the point is inside
  if (fabs(u) > m_lX || fabs(v) > m_lY || fabs(w) > m_lZ) {
    if (m_debug) {
      std::cout << "SolidBox::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ") "
                << " is outside.\n";
    }
    return false;
  }

  if (m_debug) {
    std::cout << "SolidBox::IsInside:\n";
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is inside.\n";
  }

  return true;
}

bool SolidBox::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax) const {

  if (m_cTheta == 1. && m_cPhi == 1.) {
    xmin = m_cX - m_lX;
    xmax = m_cX + m_lX;
    ymin = m_cY - m_lY;
    ymax = m_cY + m_lY;
    zmin = m_cZ - m_lZ;
    zmax = m_cZ + m_lZ;
    return true;
  }

  const double dd = sqrt(m_lX * m_lX + m_lY * m_lY + m_lZ * m_lZ);
  xmin = m_cX - dd;
  xmax = m_cX + dd;
  ymin = m_cY - dd;
  ymax = m_cY + dd;
  zmin = m_cZ - dd;
  zmax = m_cZ + dd;
  return true;
}

bool SolidBox::GetCenter(double& x, double& y, double& z) const {

  x = m_cX;
  y = m_cY;
  z = m_cZ;
  return true;
}

bool SolidBox::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_lX;
  l2 = m_lY;
  l3 = m_lZ;
  return true;
}

bool SolidBox::GetOrientation(double& ctheta, double& stheta, double& cphi,
                              double& sphi) const {

  ctheta = m_cTheta;
  stheta = m_sTheta;
  cphi = m_cPhi;
  sphi = m_sPhi;
  return true;
}

void SolidBox::SetHalfLengthX(const double& lx) {

  if (lx > 0.) {
    m_lX = lx;
  } else {
    std::cerr << "SolidBox::SetHalfLengthX:\n";
    std::cerr << "    Half-length must be > 0.\n";
  }
}

void SolidBox::SetHalfLengthY(const double& ly) {

  if (ly > 0.) {
    m_lY = ly;
  } else {
    std::cerr << "SolidBox::SetHalfLengthY:\n";
    std::cerr << "    Half-length must be > 0.\n";
  }
}

void SolidBox::SetHalfLengthZ(const double& lz) {

  if (lz > 0.) {
    m_lZ = lz;
  } else {
    std::cerr << "SolidBox::SetHalfLengthZ:\n";
    std::cerr << "    Half-length must be > 0.\n";
  }
}
}
