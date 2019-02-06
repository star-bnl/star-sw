#include <iostream>
#include <cmath>

#include "SolidSphere.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidSphere::SolidSphere(const double cx, const double cy, const double cz,
                         const double rmin, const double rmax)
    : Solid(cx, cy, cz, "SolidSphere"),
      m_rMin(rmin), m_rMax(rmax) {}

bool SolidSphere::IsInside(const double x, const double y, 
                           const double z) const {

  // Transform the point to local coordinates.
  const double dx = x - m_cX;
  const double dy = y - m_cY;
  const double dz = z - m_cZ;

  if (fabs(dx) > m_rMax || fabs(dy) > m_rMax || fabs(dz) > m_rMax) {
    if (m_debug) {
      std::cout << "SolidSphere::IsInside: (" << x << ", " << y << ", " << z 
                << ") is outside.\n";
    }
    return false;
  }

  const double r = sqrt(dx * dx + dy * dy + dz * dz);
  if (r >= m_rMin && r <= m_rMax) {
    if (m_debug) {
      std::cout << "SolidSphere::IsInside: (" << x << ", " << y << ", " << z 
                << ") is inside.\n";
    }
    return true;
  }

  if (m_debug) {
    std::cout << "SolidSphere::IsInside: (" << x << ", " << y << ", " << z 
              << ") is outside.\n";
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

bool SolidSphere::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_rMin;
  l2 = m_rMax;
  l3 = 0.;
  return true;
}

void SolidSphere::SetInnerRadius(const double rmin) {

  if (rmin <= 0.) {
    std::cerr << "SolidSphere::SetInnerRadius: Radius must be > 0.\n";
    return;
  }
  if (rmin >= m_rMax) {
    std::cerr << "SolidSphere::SetInnerRadius:\n";
    std::cerr << "    Inner radius must be smaller than outer radius.\n";
    return;
  }
  m_rMin = rmin;
}

void SolidSphere::SetOuterRadius(const double rmax) {

  if (rmax <= 0.) {
    std::cerr << "SolidSphere::SetOuterRadius: Radius must be > 0.\n";
    return;
  }
  if (rmax <= m_rMin) {
    std::cerr << "SolidSphere::SetOuterRadius:\n";
    std::cerr << "    Outer radius must be greater than inner radius.\n";
    return;
  }
  m_rMax = rmax;
}

void SolidSphere::SetMeridians(const unsigned int n) {

  if (n < 3) {
    std::cerr << "SolidSphere::SetMeridians: Number must be >= 3.\n";
    return;
  }
  m_n = n;
}

bool SolidSphere::SolidPanels(std::vector<Panel>& panels) {

  const unsigned int nPanels = panels.size();
  const double R = m_rMax;
  // Loop over the sphere.
  for (unsigned int i = 1; i <= m_n; ++i) {
    const double phi0 = TwoPi * (i - 1.) / m_n;
    const double phi1 = TwoPi * double(i) / m_n;
    const double cphi0 = cos(phi0);
    const double sphi0 = sin(phi0);
    const double cphi1 = cos(phi1);
    const double sphi1 = sin(phi1);
    for (unsigned int j = 1; j <= m_n; ++j) {
      const double theta0 = -HalfPi + Pi * (j - 1.) / m_n;
      const double theta1 = -HalfPi + Pi * double(j) / m_n;
      const double ctheta0 = cos(theta0);
      const double stheta0 = sin(theta0);
      const double ctheta1 = cos(theta1);
      const double stheta1 = sin(theta1);
      Panel newpanel;
      // Corners of this parcel.
      if (j == 1) {
        const double xv0 = m_cX + R * cphi0 * ctheta0;
        const double yv0 = m_cY + R * sphi0 * ctheta0;
        const double zv0 = m_cZ + R * stheta0;
        const double xv1 = m_cX + R * cphi1 * ctheta1;
        const double yv1 = m_cY + R * sphi1 * ctheta1;
        const double zv1 = m_cZ + R * stheta1;
        const double xv2 = m_cX + R * cphi0 * ctheta1;
        const double yv2 = m_cY + R * sphi0 * ctheta1;
        const double zv2 = m_cZ + R * stheta1;
        newpanel.xv = {xv0, xv1, xv2};
        newpanel.yv = {yv0, yv1, yv2};
        newpanel.zv = {zv0, zv1, zv2};
     } else if (j == m_n) {
        const double xv0 = m_cX + R * cphi0 * ctheta0;
        const double yv0 = m_cY + R * sphi0 * ctheta0;
        const double zv0 = m_cZ + R * stheta0;
        const double xv1 = m_cX + R * cphi1 * ctheta0;
        const double yv1 = m_cY + R * sphi1 * ctheta0;
        const double zv1 = m_cZ + R * stheta0;
        const double xv2 = m_cX + R * cphi1 * ctheta1;
        const double yv2 = m_cY + R * sphi1 * ctheta1;
        const double zv2 = m_cZ + R * stheta1;
        newpanel.xv = {xv0, xv1, xv2};
        newpanel.yv = {yv0, yv1, yv2};
        newpanel.zv = {zv0, zv1, zv2};
      } else {
        const double xv0 = m_cX + R * cphi0 * ctheta0;
        const double yv0 = m_cY + R * sphi0 * ctheta0;
        const double zv0 = m_cZ + R * stheta0;
        const double xv1 = m_cX + R * cphi1 * ctheta0;
        const double yv1 = m_cY + R * sphi1 * ctheta0;
        const double zv1 = m_cZ + R * stheta0;
        const double xv2 = m_cX + R * cphi1 * ctheta1;
        const double yv2 = m_cY + R * sphi1 * ctheta1;
        const double zv2 = m_cZ + R * stheta1;
        const double xv3 = m_cX + R * cphi0 * ctheta1;
        const double yv3 = m_cY + R * sphi0 * ctheta1;
        const double zv3 = m_cZ + R * stheta1;
        newpanel.xv = {xv0, xv1, xv2, xv3};
        newpanel.yv = {yv0, yv1, yv2, yv3};
        newpanel.zv = {zv0, zv1, zv2, zv3};
      }
      // Inclination angle in theta.
      const double alpha = atan2((ctheta0 - ctheta1) * sqrt((1. + cos(phi1 - phi0)) / 2), stheta1 - stheta0);
      const double calpha = cos(alpha);
      const double salpha = sin(alpha);
      // Store the panel.
      newpanel.a = cos(0.5 * (phi0 + phi1)) * calpha;
      newpanel.b = sin(0.5 * (phi0 + phi1)) * calpha;
      newpanel.c = salpha;
      panels.push_back(std::move(newpanel));
    }
  }
  std::cout << "SolidSphere::SolidPanels: "
            << panels.size() - nPanels << " panels.\n";
  return true;
}
}
