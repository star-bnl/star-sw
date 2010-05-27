#include <iostream>

#include "ComponentConstant.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

ComponentConstant::ComponentConstant() : 
  ComponentBase(), 
  fx(0.), fy(0.), fz(0.),
  hasPotential(false), 
  x0(0.), y0(0.), z0(0.), v0(0.),
  hasWeightingField(false), wfield(""),
  fwx(0.), fwy(0.), fwz(0.),
  hasWeightingPotential(false),
  wx0(0.), wy0(0.), wz0(0.), w0(0.)  {
      
}

void 
ComponentConstant::ElectricField(
                    const double x, const double y, const double z,
                    double& ex, double& ey, double& ez, 
                    Medium*& m, int& status) {

  ex = fx; ey = fy; ez = fz;
  if (!GetMedium(x, y, z, m)) {
    if (debug) {
      std::cerr << "ComponentConstant::ElectricField:" << std::endl;
      std::cerr << "    (" << x << ", " << y << ", " << z << ")" 
                << " is not inside a medium." << std::endl;
    }
    status = -6;
    m = 0;
    return;
  }
  
  if (m->IsDriftable()) {
    status = 0;
  } else {
    status = -5;
  }

}

void 
ComponentConstant::ElectricField(
                    const double x, const double y, const double z, 
                    double& ex, double& ey, double& ez, double& v,
                    Medium*& m, int& status) {
                                 
  ex = fx; ey = fy; ez = fz;
  if (hasPotential) {
    v = v0 - (x - x0) * fx
           - (y - y0) * fy
           - (z - z0) * fz;
  } else {
    v = 0.;
    if (debug) {
      std::cerr << "ComponentConstant::ElectricField:" << std::endl;
      std::cerr << "    Potential is not defined." << std::endl;
    }
  }
  
  if (!GetMedium(x, y, z, m)) {
    if (debug) {
      std::cerr << "ComponentConstant::ElectricField:" << std::endl;
      std::cerr << "    (" << x << ", " << y << ", " << z << ")" 
                << " is not inside a medium." << std::endl;
    }
    status = -6;
    m = 0;
    return;
  }
  
  if (m->IsDriftable()) {
    status = 0;
  } else {
    status = -5;
  }
  
}

bool 
ComponentConstant::GetVoltageRange(double& vmin, double& vmax) {

  if (!hasPotential) return false;
 
  if (theGeometry == 0) {
    std::cerr << "ComponentConstant::GetVoltageRange:" << std::endl;
    std::cerr << "    Geometry is not defined." << std::endl;
    return false;
  }
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << "ComponentConstant::GetVoltageRange:" << std::endl;
    std::cerr << "    Could not determine bounding box." << std::endl;
    return false;
  }
  // Calculate potentials at each corner
  const double pxmin = v0 - (xmin - x0) * fx;
  const double pxmax = v0 - (xmax - x0) * fx;
  const double pymin = - (ymin - y0) * fy;
  const double pymax = - (ymax - y0) * fy;
  const double pzmin = - (zmin - z0) * fz;
  const double pzmax = - (zmax - z0) * fz;
  double p[8];
  p[0] = pxmin + pymin + pzmin;
  p[1] = pxmin + pymin + pzmax;
  p[2] = pxmin + pymax + pzmin;
  p[3] = pxmin + pymax + pzmax;
  p[4] = pxmax + pymin + pzmin;
  p[5] = pxmax + pymin + pzmax;
  p[6] = pxmax + pymax + pzmin;
  p[7] = pxmax + pymax + pzmax;
  vmin = vmax = p[7];
  for (int i = 7; i--;) {
    if (p[i] > vmax) vmax = p[i];
    if (p[i] < vmin) vmin = p[i];
  }       
  
  return true;
  
}

void 
ComponentConstant::WeightingField(
    const double x, const double y, const double z,
    double& wx, double& wy, double& wz,
    const std::string label) {

  if (!hasWeightingField || label != wfield) return;
  wx = fwx; wy = fwy; wz = fwz;

}

double
ComponentConstant::WeightingPotential(
    const double x, const double y, const double z, 
    const std::string label) {

  if (!hasWeightingPotential || label != wfield) return 0.;

  return w0 - (x - wx0) * fwx
            - (y - wy0) * fwy
            - (z - wz0) * fwz;

}

void 
ComponentConstant::SetElectricField(
                    const double ex, const double ey, const double ez) {

  fx = ex; fy = ey; fz = ez;
  if (fx * fx + fy * fy + fz * fz > Small) return;
  
  std::cerr << "ElectricFieldConstant::SetField:" << std::endl;
  std::cerr << "    Electric field is set to zero." << std::endl;  
  ready = true;
  
}

void 
ComponentConstant::SetPotential(
            const double x, const double y, const double z, const double v) {

  x0 = x; y0 = y; z0 = z;
  v0 = v;
  hasPotential = true;

}

void 
ComponentConstant::SetWeightingField(
            const double wx, const double wy, const double wz,
            const std::string label) {

  wfield = label;
  fwx = wx; fwy = wy; fwz = wz;
  hasWeightingField = true;

}

void 
ComponentConstant::SetWeightingPotential(
            const double x, const double y, const double z, const double v) {

  if (!hasWeightingField) {
    std::cerr << "ComponentConstant::SetWeightingPotential:" << std::endl;
    std::cerr << "    No weighting field specified." << std::endl;
    return;
  }
  wx0 = x; wy0 = y; wz0 = z;
  w0 = v;
  hasWeightingPotential = true;

}

void
ComponentConstant::Reset() {

  fx = fy = fz = 0.;
  hasPotential = false;
  ready = false;
  
}

void
ComponentConstant::UpdatePeriodicity() {
  
  if (debug) {
    std::cerr << "ComponentConstant::UpdatePeriodicity:" << std::endl;
    std::cerr << "    Periodicities are not supported." << std::endl;
  }

}

}
