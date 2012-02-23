#include <iostream>
#include <cmath>

#include "SolidTube.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

SolidTube::SolidTube(const double cx, const double cy, const double cz, 
                     const double rmin, const double rmax, const double lz) : 
  Solid(), 
  cX(cx), cY(cy), cZ(cz),
  rMin(rmin), rMax(rmax), lZ(lz),
  dX(0.), dY(0.), dZ(1.),
  cPhi(1.),   sPhi(0.),
  cTheta(1.), sTheta(0.) {
  
}

SolidTube::SolidTube(const double cx, const double cy, const double cz, 
                     const double rmin, const double rmax, const double lz,
                     const double dx, const double dy, const double dz) : 
  Solid(), 
  cX(cx), cY(cy), cZ(cz),
  rMin(rmin), rMax(rmax), lZ(lz),
  dX(0.), dY(0.), dZ(1.),
  cPhi(1.),   sPhi(0.),
  cTheta(1.), sTheta(0.) {
  
  const double d = sqrt(dx * dx + dy * dy + dz * dz);  
  if (d < Small) {
    std::cerr << "SolidTube: Direction vector has zero norm.\n";
  } else {
    dX = dx / d; dY = dy / d; dZ = dz / d;
    double phi, theta;
    const double dt = sqrt(dX * dX + dY * dY);
    if (dt < Small) {
      phi = 0.;    
      if (dZ > 0.) {
        theta = 0.;
      } else {
        theta = Pi;
      }
    } else {
      phi = atan2(dY, dX);
      theta = atan2(dt, dZ);
    }
    cTheta = cos(theta); 
    sTheta = sin(theta);
    cPhi = cos(phi);
    sPhi = sin(phi);
  }

}

bool 
SolidTube::IsInside(const double x, const double y, const double z) {
  
  // Transform the point to local coordinates
  const double dx = x - cX;
  const double dy = y - cY;
  const double dz = z - cZ;
  const double u =  cPhi * cTheta * dx + sPhi * cTheta * dy - sTheta * dz;
  const double v = -sPhi          * dx + cPhi *          dy;
  const double w =  cPhi * sTheta * dx + sPhi * sTheta * dy + cTheta * dz;
 
  if (fabs(w) > lZ) {
    if (debug) {
      std::cout << "SolidTube::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is outside.\n";
    }
    return false;
  }
  
  const double r = sqrt(u * u + v * v);
  if (r >= rMin && r <= rMax) {
    if (debug) {
      std::cout << "SolidTube::IsInside:\n";
      std::cout << "    (" << x << ", " << y << ", " << z << ")"
                << " is inside.\n";
    }
    return true;
  }

  if (debug) {
    std::cout << "SolidTube::IsInside:\n";
    std::cout << "    (" << x << ", " << y << ", " << z << ") " 
              << " is outside.\n";
  }  
  return false;
  
}

bool
SolidTube::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                          double& xmax, double& ymax, double& zmax) {

  if (cTheta == 1. && cPhi == 1.) {
    xmin = cX - rMax; xmax = cX + rMax;
    ymin = cY - rMax; ymax = cY + rMax;
    zmin = cZ - lZ;   zmax = cZ + lZ;
    return true;
  }

  const double dd = sqrt(rMax * rMax + lZ * lZ);
  xmin = cX - dd; xmax = cX + dd;
  ymin = cY - dd; ymax = cY + dd;
  zmin = cZ - dd; zmax = cZ + dd;
  return true;

}

bool
SolidTube::GetCenter(double& x, double& y, double& z) {

  x = cX; y = cY; z = cZ;
  return true;
  
}

bool
SolidTube::GetDimensions(double& l1, double& l2, double& l3) {

  l1 = rMin; l2 = rMax; l3 = lZ;
  return true;

}

bool
SolidTube::GetOrientation(double& ctheta, double& stheta,
                          double& cphi, double& sphi) {

  ctheta = cTheta; stheta = sTheta;
  cphi = cPhi; sphi = sPhi;
  return true;

}

void
SolidTube::SetInnerRadius(const double rmin) {

  if (rmin <= 0.) {
    std::cerr << "SolidTube::SetInnerRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmin >= rMax) {
    std::cerr << "SolidTube::SetInnerRadius:\n";
    std::cerr << "    Inner radius must be smaller than outer radius.\n";
    return;
  }
  rMin = rmin;

}

void
SolidTube::SetOuterRadius(const double rmax) {

  if (rmax <= 0.) {
    std::cerr << "SolidTube::SetOuterRadius:\n";
    std::cerr << "    Radius must be > 0.\n";
    return;
  }
  if (rmax <= rMin) {
    std::cerr << "SolidTube::SetOuterRadius:\n";
    std::cerr << "    Outer radius must be greater than inner radius.\n";
    return;
  }
  rMax = rmax;

}

void
SolidTube::SetHalfLengthZ(const double lz) {

  if (lz <= 0.) {
    std::cerr << "SolidTube::SetHalfLengthZ:\n";
    std::cerr << "    Half-length must be > 0.\n";
    return;
  }
  lZ = lz;

}

}
