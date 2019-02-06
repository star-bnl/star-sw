#include <iostream>
#include <cmath>

#include "SolidTube.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

SolidTube::SolidTube(const double cx, const double cy, const double cz,
                     const double rmin, const double rmax, const double lz)
    : Solid(cx, cy, cz, "SolidTube"),
      m_rMin(rmin), m_rMax(rmax), m_r(rmax),
      m_lZ(lz) {}

SolidTube::SolidTube(const double cx, const double cy, const double cz,
                     const double rmin, const double rmax, const double lz,
                     const double dx, const double dy, const double dz)
    : SolidTube(cx, cy, cz, rmin, rmax, lz) {

  SetDirection(dx, dy, dz);
}

SolidTube::SolidTube(const double cx, const double cy, const double cz,
                     const double r, const double lz)
    : SolidTube(cx, cy, cz, 0., r, lz) {}

SolidTube::SolidTube(const double cx, const double cy, const double cz,
                     const double r, const double lz,
                     const double dx, const double dy, const double dz)
    : SolidTube(cx, cy, cz, 0., r, lz, dx, dy, dz) {}

bool SolidTube::IsInside(const double x, const double y, const double z) const {

  // Transform the point to local coordinates.
  double u = x, v = y, w = z;
  ToLocal(x, y, z, u, v, w);

  if (fabs(w) > m_lZ) {
    if (m_debug) {
      std::cout << "SolidTube::IsInside: (" << x << ", " << y << ", " << z 
                << ") is outside.\n";
    }
    return false;
  }

  const double r = sqrt(u * u + v * v);
  if (r >= m_rMin && r <= m_rMax) {
    if (m_debug) {
      std::cout << "SolidTube::IsInside: (" << x << ", " << y << ", " << z 
                << ") is inside.\n";
    }
    return true;
  }

  if (m_debug) {
    std::cout << "SolidTube::IsInside: (" << x << ", " << y << ", " << z 
              << ") is outside.\n";
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

bool SolidTube::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_rMin;
  l2 = m_rMax;
  l3 = m_lZ;
  return true;
}

void SolidTube::SetInnerRadius(const double rmin) {

  if (rmin <= 0.) {
    std::cerr << "SolidTube::SetInnerRadius: Radius must be > 0.\n";
    return;
  }
  if (rmin >= m_rMax) {
    std::cerr << "SolidTube::SetInnerRadius:\n";
    std::cerr << "    Inner radius must be smaller than outer radius.\n";
    return;
  }
  m_rMin = rmin;
}

void SolidTube::SetOuterRadius(const double rmax) {

  if (rmax <= 0.) {
    std::cerr << "SolidTube::SetOuterRadius: Radius must be > 0.\n";
    return;
  }
  if (rmax <= m_rMin) {
    std::cerr << "SolidTube::SetOuterRadius:\n";
    std::cerr << "    Outer radius must be greater than inner radius.\n";
    return;
  }
  m_rMax = rmax;
}

void SolidTube::SetRadius(const double r) {

  if (r <= 0.) {
    std::cerr << "SolidTube::SetRadius: Radius must be > 0.\n";
    return;
  }
  m_r = r;
}

void SolidTube::SetHalfLength(const double lz) {

  if (lz <= 0.) {
    std::cerr << "SolidTube::SetHalfLength: Half-length must be > 0.\n";
    return;
  }
  m_lZ = lz;
}

void SolidTube::SetSectors(const unsigned int n) {

  if (n < 1) {
    std::cerr << "SolidTube::SetSectors: Number must be > 0.\n";
    return;
  }
  m_n = n;
}

bool SolidTube::SolidPanels(std::vector<Panel>& panels) {

  // AROT Rotation angle: m_rot
  // N    Number of sectors: m_n
  // R    Radius of cylinder: m_r
  // ZL   half-length m_lZ
  // X0   centre m_cX
  // Y0   centre m_cY
  // Z0   centre m_cZ
  // A    direction vector m_dX
  // B    direction vector m_dY
  // C    direction vector m_dZ

  const unsigned int nPanels = panels.size();
  // Direction vector.
  const double fnorm = sqrt(m_dX * m_dX + m_dY * m_dY + m_dZ * m_dZ);
  if (fnorm <= 0) {
    std::cerr << "SolidTube::SolidPanels:\n"
              << "    Zero norm direction vector; no panels generated.\n";
    return false;
  }
  
  // Set the mean or the outer radius.
  double r = m_r;
  if (m_average) {
    const double alpha = Pi / (4. * (m_n - 1.));
    r = 2 * m_r / (1. + asinh(tan(alpha)) * cos(alpha) / tan(alpha));
  }

  const unsigned int nPoints = 4 * (m_n - 1);
  // Create the top lid.
  if (m_toplid) {
    std::vector<double> xv;
    std::vector<double> yv;
    std::vector<double> zv;
    for (unsigned int i = 1; i <= nPoints; i++) {
      const double alpha = m_rot + HalfPi * (i - 1.) / (m_n - 1.);
      double x, y, z;
      ToGlobal(r * cos(alpha), r * sin(alpha), m_lZ, x, y, z);
      // Rotate into place.
      xv.push_back(x);
      yv.push_back(y);
      zv.push_back(z);
    }
    Panel newpanel;
    newpanel.a = m_cPhi * m_sTheta;
    newpanel.b = m_sPhi * m_sTheta;
    newpanel.c = m_cTheta;
    newpanel.xv = xv;
    newpanel.yv = yv;
    newpanel.zv = zv;
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // Create the bottom lid.
  if (m_bottomlid) {
    std::vector<double> xv;
    std::vector<double> yv;
    std::vector<double> zv;
    for (unsigned int i = 1; i <= nPoints; i++) {
      const double alpha = m_rot + HalfPi * (i - 1.) / (m_n - 1.);
      double x, y, z;
      ToGlobal(r * cos(alpha), r * sin(alpha), -m_lZ, x, y, z);
      // Rotate into place.
      xv.push_back(x);
      yv.push_back(y);
      zv.push_back(z);
    }
    Panel newpanel;
    newpanel.a = -m_cPhi * m_sTheta;
    newpanel.b = -m_sPhi * m_sTheta;
    newpanel.c = -m_cTheta;
    newpanel.xv = xv;
    newpanel.yv = yv;
    newpanel.zv = zv;
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // Create the side panels.
  double u = r * cos(m_rot);
  double v = r * sin(m_rot);
  // Rotate into place.
  double xv0, yv0, zv0;
  ToGlobal(u, v, -m_lZ, xv0, yv0, zv0);
  double xv1, yv1, zv1;
  ToGlobal(u, v, +m_lZ, xv1, yv1, zv1);
  // Go around the cylinder.
  for (unsigned int i = 2; i <= nPoints + 1; i++) {
    // Bottom and top of the line along the axis of the cylinder.
    double alpha = m_rot + HalfPi * (i - 1.) / (m_n - 1.);
    u = r * cos(alpha);
    v = r * sin(alpha);
    // Rotate into place.
    double xv2, yv2, zv2;
    ToGlobal(u, v, +m_lZ, xv2, yv2, zv2);
    double xv3, yv3, zv3;
    ToGlobal(u, v, -m_lZ, xv3, yv3, zv3);
    // Store the plane.
    Panel newpanel;
    alpha = m_rot + HalfPi * (i - 1.5) / (m_n - 1.);
    const double cAlpha = cos(alpha);
    const double sAlpha = sin(alpha);
    newpanel.a = m_cPhi * m_cTheta * cAlpha - m_sPhi * sAlpha;
    newpanel.b = m_sPhi * m_cTheta * cAlpha + m_cPhi * sAlpha;
    newpanel.c = -m_sTheta * cAlpha;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
    // Shift the points.
    xv0 = xv3;
    yv0 = yv3;
    zv0 = zv3;
    xv1 = xv2;
    yv1 = yv2;
    zv1 = zv2;
  }
  std::cout << "SolidTube::SolidPanels: "
            << panels.size() - nPanels << " panels.\n";
  return true;
}
}
