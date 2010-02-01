#include <iostream>

#include "ComponentUser.hh"

namespace Garfield {

ComponentUser::ComponentUser() :
  ComponentBase(), 
  hasField(false), field(0), 
  hasPotential(false), potential(0),
  nWeightingFields(0)  {
  
  wfields.clear();

}

void 
ComponentUser::ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, 
                             Medium*& m, int& status) {

  if (!hasField) {
    ex = ey = ez = 0.;
    m = 0;
    status = -10;
    return;
  }
  
  field(x, y, z, ex, ey, ez);  
  if (!GetMedium(x, y, z, m)) {
    if (debug) {
      std::cerr << "ComponentUser::ElectricField:" << std::endl;
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
ComponentUser::ElectricField(const double x, const double y, const double z, 
                             double& ex, double& ey, double& ez, double& v, 
                             Medium*& m, int& status) {

  if (!hasField) {
    ex = ey = ez = v = 0.;  
    m = 0;
    status = -10;
    return;
  }
  field(x, y, z, ex, ey, ez);
  
  if (hasPotential) {
    potential(x, y, z, v);  
  } else {
    v = 0.;
  }  
    
  if (!GetMedium(x, y, z, m)) {
    if (debug) {
      std::cerr << "ComponentUser::ElectricField:" << std::endl;
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
ComponentUser::GetVoltageRange(double& vmin, double& vmax) {

  vmin = vmax = 0.;
  return false;

}

void 
ComponentUser::WeightingField(const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string label) {

  wx = wy = wz = 0.;
  double fx = 0., fy = 0., fz = 0.;
  for (int i = nWeightingFields; i--;) {
    if (label == wfields[i].label) {
      wfields[i].field(x, y, z, fx, fy, fz);
      wx += fx; wy += fy; wz += fz;
    }
  }

}

void 
ComponentUser::SetElectricField(void (*f)(const double, const double, const double, double&, double&, double&)) {

  if (f == 0) {
    std::cerr << "ComponentUser::SetField:" << std::endl;
    std::cerr << "    Function is not defined." << std::endl;
    return;
  }
  field = f;
  hasField = true;
  ready = true;
  
}

void 
ComponentUser::SetPotential(void (*f)(const double, const double, const double, double&)) {

  if (f == 0) {
    std::cerr << "ComponentUser::SetPotential:" << std::endl;
    std::cerr << "    Function is not defined." << std::endl;
    return;
  }
  potential = f;
  hasPotential = true;

}

void 
ComponentUser::AddWeightingField(void (*f)(const double, const double, const double, double&, double&, double&),
                                 const std::string label) {
                                 
  if (f == 0) {
    std::cerr << "ComponentUser::AddWeightingField:" << std::endl;
    std::cerr << "    Function is not defined." << std::endl;
    return;
  }
  wfield newField;
  newField.field = f;
  newField.label = label;
  wfields.push_back(newField);
  ++nWeightingFields;

}

bool 
ComponentUser::CheckSolidType(Solid* s) {

  if (s == 0) {
    std::cerr << "ComponentUser::CheckSolidType:" << std::endl;
    std::cerr << "    Solid is not defined." << std::endl;
    return false;
  }
  return true;

}

void 
ComponentUser::CheckBoundaryConditionType(int& bctype, double& bcval) {

  if (warning) {
    std::cerr << "ComponentUser::CheckBoundaryConditionType:" << std::endl;
    std::cerr << "    Boundary conditions are ignored." << std::endl;
  }
  
  bctype = 0;
  bcval = 0.;

}

void
ComponentUser::Reset() {

  field = 0; 
  potential = 0;
  hasField = false;
  hasPotential = false;
  ready = false;
  
}

void
ComponentUser::UpdatePeriodicity() {

  if (warning) {
    std::cerr << "ComponentUser::UpdatePeriodicity:" << std::endl;
    std::cerr << "    Periodicities are not supported." << std::endl;
  }

}

}
