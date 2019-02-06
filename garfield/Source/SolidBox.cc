#include <iostream>
#include <cmath>

#include "SolidBox.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidBox::SolidBox(const double cx, const double cy, const double cz,
                   const double lx, const double ly, const double lz)
    : Solid(cx, cy, cz, "SolidBox"),
      m_lX(lx), m_lY(ly), m_lZ(lz) {}

SolidBox::SolidBox(const double cx, const double cy, const double cz,
                   const double lx, const double ly, const double lz,
                   const double dx, const double dy, const double dz)
    : SolidBox(cx, cy, cz, lx, ly, lz) {

  SetDirection(dx, dy, dz);
}

bool SolidBox::IsInside(const double x, const double y, 
                        const double z) const {

  // Transform the point to local coordinates.
  double u = x, v = y, w = z;
  ToLocal(x, y, z, u, v, w);

  // See whether the point is inside.
  if (fabs(u) > m_lX || fabs(v) > m_lY || fabs(w) > m_lZ) {
    if (m_debug) {
      std::cout << "SolidBox::IsInside: (" << x << ", " << y << ", " << z 
                << ") is outside.\n";
    }
    return false;
  }

  if (m_debug) {
    std::cout << "SolidBox::IsInside: (" << x << ", " << y << ", " << z 
              << ") is inside.\n";
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

bool SolidBox::GetDimensions(double& l1, double& l2, double& l3) const {

  l1 = m_lX;
  l2 = m_lY;
  l3 = m_lZ;
  return true;
}

void SolidBox::SetHalfLengthX(const double lx) {

  if (lx > 0.) {
    m_lX = lx;
  } else {
    std::cerr << "SolidBox::SetHalfLengthX: Half-length must be > 0.\n";
  }
}

void SolidBox::SetHalfLengthY(const double ly) {

  if (ly > 0.) {
    m_lY = ly;
  } else {
    std::cerr << "SolidBox::SetHalfLengthY: Half-length must be > 0.\n";
  }
}

void SolidBox::SetHalfLengthZ(const double lz) {

  if (lz > 0.) {
    m_lZ = lz;
  } else {
    std::cerr << "SolidBox::SetHalfLengthZ: Half-length must be > 0.\n";
  }
}


bool SolidBox::SolidPanels(std::vector<Panel>& panels) {

  const auto nPanels = panels.size();
  double xv0, yv0, zv0;
  double xv1, yv1, zv1;
  double xv2, yv2, zv2;
  double xv3, yv3, zv3;
  // Draw the 6 sides of the box, start with the x = xmin face.
  if (m_lY > 0 && m_lZ > 0) {
    ToGlobal(-m_lX, -m_lY, -m_lZ, xv0, yv0, zv0);
    ToGlobal(-m_lX, +m_lY, -m_lZ, xv1, yv1, zv1);
    ToGlobal(-m_lX, +m_lY, +m_lZ, xv2, yv2, zv2);
    ToGlobal(-m_lX, -m_lY, +m_lZ, xv3, yv3, zv3);
    // Colour(-m_cPhi * m_cTheta, -m_sPhi * m_cTheta, +m_sTheta, wcol);
    Panel newpanel;
    newpanel.a = -m_cPhi * m_cTheta;
    newpanel.b = -m_sPhi * m_cTheta;
    newpanel.c = +m_sTheta;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // The x = xmax face.
  if (m_lX > 0 && m_lY > 0 && m_lZ > 0) {
    ToGlobal(+m_lX, -m_lY, -m_lZ, xv0, yv0, zv0);
    ToGlobal(+m_lX, +m_lY, -m_lZ, xv1, yv1, zv1);
    ToGlobal(+m_lX, +m_lY, +m_lZ, xv2, yv2, zv2);
    ToGlobal(+m_lX, -m_lY, +m_lZ, xv3, yv3, zv3);
    Panel newpanel;
    newpanel.a = m_cPhi * m_cTheta;
    newpanel.b = m_sPhi * m_cTheta;
    newpanel.c = -m_sTheta;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // The y = ymin face.
  if (m_lX > 0 && m_lZ > 0) {
    ToGlobal(-m_lX, -m_lY, -m_lZ, xv0, yv0, zv0);
    ToGlobal(+m_lX, -m_lY, -m_lZ, xv1, yv1, zv1);
    ToGlobal(+m_lX, -m_lY, +m_lZ, xv2, yv2, zv2);
    ToGlobal(-m_lX, -m_lY, +m_lZ, xv3, yv3, zv3);
    Panel newpanel;
    newpanel.a = m_sPhi;
    newpanel.b = -m_cPhi;
    newpanel.c = 0;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // The y = ymax face.
  if (m_lX > 0 && m_lY > 0 && m_lZ > 0) {
    ToGlobal(-m_lX, +m_lY, -m_lZ, xv0, yv0, zv0);
    ToGlobal(+m_lX, +m_lY, -m_lZ, xv1, yv1, zv1);
    ToGlobal(+m_lX, +m_lY, +m_lZ, xv2, yv2, zv2);
    ToGlobal(-m_lX, +m_lY, +m_lZ, xv3, yv3, zv3);
    Panel newpanel;
    newpanel.a = -m_sPhi;
    newpanel.b = +m_cPhi;
    newpanel.c = 0;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // The z = zmin face.
  if (m_lX > 0 && m_lY > 0) {
    ToGlobal(-m_lX, -m_lY, -m_lZ, xv0, yv0, zv0);
    ToGlobal(-m_lX, +m_lY, -m_lZ, xv1, yv1, zv1);
    ToGlobal(+m_lX, +m_lY, -m_lZ, xv2, yv2, zv2);
    ToGlobal(+m_lX, -m_lY, -m_lZ, xv3, yv3, zv3);
    Panel newpanel;
    newpanel.a = -m_cPhi * m_sTheta;
    newpanel.b = -m_sPhi * m_sTheta;
    newpanel.c = -m_cTheta;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // The z = zmax face.
  if (m_lX > 0 && m_lY > 0 && m_lZ > 0) {
    ToGlobal(-m_lX, -m_lY, +m_lZ, xv0, yv0, zv0);
    ToGlobal(-m_lX, +m_lY, +m_lZ, xv1, yv1, zv1);
    ToGlobal(+m_lX, +m_lY, +m_lZ, xv2, yv2, zv2);
    ToGlobal(+m_lX, -m_lY, +m_lZ, xv3, yv3, zv3);
    Panel newpanel;
    newpanel.a = +m_cPhi * m_sTheta;
    newpanel.b = +m_sPhi * m_sTheta;
    newpanel.c = +m_cTheta;
    newpanel.xv = {xv0, xv1, xv2, xv3};
    newpanel.yv = {yv0, yv1, yv2, yv3};
    newpanel.zv = {zv0, zv1, zv2, zv3};
    newpanel.colour = 0;
    newpanel.volume = 0;
    panels.push_back(std::move(newpanel));
  }
  // Done, check panel count.
  std::cout << "SolidBox::SolidPanels: " 
            << panels.size() - nPanels << " panels.\n";
  return true;
}
}
