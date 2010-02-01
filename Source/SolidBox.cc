#include <iostream>
#include <cmath>

#include "SolidBox.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

SolidBox::SolidBox(const double cx, const double cy, const double cz, 
                   const double lx, const double ly, const double lz) : 
  Solid(), 
  cX(cx), cY(cy), cZ(cz),
  lX(lx), lY(ly), lZ(lz),
  dX(0.), dY(0.), dZ(1.),
  cPhi(1.),   sPhi(0.),
  cTheta(1.), sTheta(0.) {
  
  if (debug) {
    std::cout << "SolidBox:" << std::endl;
    std::cout << "    " << cx - lx << " < x < " << cx + lx << std::endl;
    std::cout << "    " << cy - ly << " < y < " << cz + lz << std::endl;
    std::cout << "    " << cy - ly << " < z < " << cz + lz << std::endl;
  }
  
}

SolidBox::SolidBox(const double cx, const double cy, const double cz, 
                   const double lx, const double ly, const double lz,
                   const double dx, const double dy, const double dz) : 
  Solid(), 
  cX(cx), cY(cy), cZ(cz),
  lX(lx), lY(ly), lZ(lz),
  dX(0.), dY(0.), dZ(1.),
  cPhi(1.),   sPhi(0.),
  cTheta(1.), sTheta(0.) {
  
  const double d = sqrt(dx * dx + dy * dy + dz * dz);  
  if (d < Small) {
    if (warning || debug) {
      std::cerr << "SolidBox: Direction vector is not defined." << std::endl;
    }
  } else {
    dX = dx / Small; dY = dy / Small; dZ = dz / Small;
    double phi, theta;
    const double dt = sqrt(dx * dx + dy * dy);
    if (dt <= Small) {
      phi = 0.;    
      if (dZ < 0.) {
        theta = - HalfPi;
      } else {
        theta = HalfPi;
      }
    } else {
      phi = atan2(dY, dX);
      theta = acos(dZ);
    }
    cTheta = cos(theta); 
    sTheta = sin(theta);
    cPhi = cos(phi);
    sPhi = sin(phi);
  }

}

bool 
SolidBox::IsInside(const double x, const double y, const double z) {
  
  // Transform the point to local coordinates
  const double dx = x - cX;
  const double dy = y - cY;
  const double dz = z - cZ;
  const double u =  cPhi * cTheta * dx + sPhi * cTheta * dy - sTheta * dz;
  const double v = -sPhi          * dx + cPhi *          dy;
  const double w =  cPhi * sTheta * dx + sPhi * sTheta * dy + cTheta * dz;
  
  // See whether the point is inside
  if (fabs(u) > lX || fabs(v) > lY || fabs(w) > lZ) {
    if (debug) {
      std::cerr << "SolidBox::IsInside:" << std::endl;
      std::cerr << "    (" << x << ", " << y << ", " << z << ") " 
                << " is outside." << std::endl;
    }
    return false;
  }
  
  if (debug) {
    std::cerr << "SolidBox::IsInside:" << std::endl;
    std::cerr << "    (" << x << ", " << y << ", " << z << ") " 
              << " is inside." << std::endl;
  }
  
  return true;
  
}

bool
SolidBox::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                         double& xmax, double& ymax, double& zmax) {

  xmin = cX - lX; xmax = cX + lX;
  ymin = cY - lY; ymax = cY + lY;
  zmin = cZ - lZ; zmax = cZ + lZ;
  return true;

}

}

