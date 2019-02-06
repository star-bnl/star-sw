#include <iostream>
#include <cmath>

#include "Solid.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

void Solid::SetDirection(const double dx, const double dy, const double dz) {

  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d < Small) {
    std::cerr << m_className << ": Direction vector has zero norm.\n";
    return;
  }
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

double Solid::NotImplemented(const std::string& fcn) const {

  std::cerr << m_className << "::" << fcn << ": function not implemented.\n";
  return 0.;
}
}
