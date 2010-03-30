#include <iostream>

#include "ComponentConstant.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

ComponentConstant::ComponentConstant() : 
  ComponentBase(), 
  fx(0.), fy(0.), fz(0.),
  hasPotential(false), 
  x0(0.), y0(0.), z0(0.), v0(0.),
  nWeightingFields(0) {
  
  wfields.clear();
      
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
  
  // Calculate potentials at each corner
  const double pxmin = v0 - (xMinBoundingBox - x0) * fx;
  const double pxmax = v0 - (xMaxBoundingBox - x0) * fx;
  const double pymin = - (yMinBoundingBox - y0) * fy;
  const double pymax = - (yMaxBoundingBox - y0) * fy;
  const double pzmin = - (zMinBoundingBox - z0) * fz;
  const double pzmax = - (zMaxBoundingBox - z0) * fz;
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

  wx = wy = wz = 0.;
  for (int i = nWeightingFields; i--;) {
    if (label == wfields[i].label) {
      wx += wfields[i].wx;
      wy += wfields[i].wy;
      wz += wfields[i].wz;
    }
  }

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
ComponentConstant::AddWeightingField(
            const double wx, const double wy, const double wz,
            const std::string label) {

  wfield newField;
  newField.wx = wx;
  newField.wy = wy;
  newField.wz = wz;
  newField.label = label;
  wfields.push_back(newField);
  ++nWeightingFields;

}

bool 
ComponentConstant::CheckSolidType(Solid* s) {

  if (s == 0) {
    std::cerr << "ComponentConstant::CheckSolidType:" << std::endl;
    std::cerr << "    Solid is not defined." << std::endl;
    return false;
  }
  return true;
  
}

void 
ComponentConstant::CheckBoundaryConditionType(int& bctype, double& bcval) {

  if (debug) {
    std::cerr << "ComponentConstant::CheckBoundaryConditionType:" << std::endl;
    std::cerr << "    Boundary conditions are ignored." << std::endl;
  }
  
  bctype = 0;
  bcval = 0.;

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
