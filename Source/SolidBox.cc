#include <iostream>
#include <cmath>

#include "SolidBox.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidBox::SolidBox(const double cx, const double cy, const double cz, 
                   const double lx, const double ly, const double lz) : 
  Solid(), 
  cX(cx), cY(cy), cZ(cz),
  lX(lx), lY(ly), lZ(lz),
  dX(0.), dY(0.), dZ(1.),
  cPhi(1.),   sPhi(0.),
  cTheta(1.), sTheta(0.) {
  
  std::cout << "SolidBox:\n";
  std::cout << "    " << cx - lx << " < x [cm] < " << cx + lx << "\n";
  std::cout << "    " << cy - ly << " < y [cm] < " << cy + ly << "\n";
  std::cout << "    " << cz - lz << " < z [cm] < " << cz + lz << "\n";
  
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
    std::cerr << "SolidBox: Direction vector has zero norm.\n";
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
      std::cout << "SolidBox::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ") " 
                << " is outside.\n";
    }
    return false;
  }
  
  if (debug) {
    std::cout << "SolidBox::IsInside:\n";
    std::cout << "    (" << x << ", " << y << ", " << z << ") " 
              << " is inside.\n";
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

bool
SolidBox::GetCenter(double& x, double& y, double& z) {

  x = cX; y = cY; z = cZ;
  return true;
  
}

bool
SolidBox::GetDimensions(double& l1, double& l2, double& l3) {

  l1 = lX; l2 = lY; l3 = lZ;
  return true;

}

bool
SolidBox::GetDirection(double& x, double& y, double& z) {

 x = sTheta * cPhi;
 y = sTheta * sPhi;
 z = cTheta;
 return true;

}

}

