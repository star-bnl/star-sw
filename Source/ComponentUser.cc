#include <iostream>

#include "ComponentUser.hh"

namespace Garfield {

ComponentUser::ComponentUser()
    : ComponentBase(),
      m_efield(NULL),
      m_potential(NULL),
      m_wfield(NULL),
      m_wpot(NULL),
      m_bfield(NULL) {

  m_className = "ComponentUser";
}

void ComponentUser::ElectricField(const double x, const double y,
                                  const double z, double& ex, double& ey,
                                  double& ez, Medium*& m, int& status) {

  if (!m_efield) {
    ex = ey = ez = 0.;
    m = NULL;
    status = -10;
    return;
  }

  m_efield(x, y, z, ex, ey, ez);
  m = GetMedium(x, y, z);
  if (!m) {
    if (m_debug) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    (" << x << ", " << y << ", " << z << ")"
                << " is not inside a medium.\n";
    }
    status = -6;
    return;
  }

  if (m->IsDriftable()) {
    status = 0;
  } else {
    status = -5;
  }
}

void ComponentUser::ElectricField(const double x, const double y,
                                  const double z, double& ex, double& ey,
                                  double& ez, double& v, Medium*& m,
                                  int& status) {

  if (!m_efield) {
    ex = ey = ez = v = 0.;
    m = NULL;
    status = -10;
    return;
  }
  m_efield(x, y, z, ex, ey, ez);

  if (m_potential) {
    m_potential(x, y, z, v);
  } else {
    v = 0.;
  }

  m = GetMedium(x, y, z);
  if (!m) {
    if (m_debug) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    (" << x << ", " << y << ", " << z << ")"
                << " is not inside a medium.\n";
    }
    status = -6;
    return;
  }

  if (m->IsDriftable()) {
    status = 0;
  } else {
    status = -5;
  }
}

bool ComponentUser::GetVoltageRange(double& vmin, double& vmax) {

  vmin = vmax = 0.;
  return false;
}

void ComponentUser::MagneticField(const double x, const double y, 
                                  const double z,
                                  double& bx, double& by, double& bz, 
                                  int& status) {

  if (!m_bfield) {
    bx = by = bz = 0.;
    status = -10;
    return;
  }
  m_bfield(x, y, z, bx, by, bz);
  status = 0;  
  
}

void ComponentUser::WeightingField(const double x, const double y,
                                   const double z, double& wx, double& wy,
                                   double& wz, const std::string& label) {

  wx = wy = wz = 0.;
  if (!m_wfield) return;
  m_wfield(x, y, z, wx, wy, wz, label);
}

double ComponentUser::WeightingPotential(const double x, const double y,
                                         const double z,
                                         const std::string& label) {

  double v = 0.;
  if (m_wpot) m_wpot(x, y, z, v, label);
  return v;
}

void ComponentUser::SetElectricField(void (*f)(const double, const double,
                                               const double, double&, double&,
                                               double&)) {

  if (!f) {
    std::cerr << m_className << "::SetElectricField:\n    Null pointer.\n";
    return;
  }
  m_efield = f;
  m_ready = true;
}

void ComponentUser::SetPotential(void (*f)(const double, const double,
                                           const double, double&)) {

  if (!f) {
    std::cerr << m_className << "::SetPotential:\n    Null pointer.\n";
    return;
  }
  m_potential = f;
}

void ComponentUser::SetWeightingField(void (*f)(const double, const double,
                                                const double, double&, double&,
                                                double&, const std::string)) {

  if (!f) {
    std::cerr << m_className << "::SetWeightingField:\n    Null pointer.\n";
    return;
  }
  m_wfield = f;
}

void ComponentUser::SetWeightingPotential(void (*f)(const double, const double,
                                                    const double, double&,
                                                    const std::string)) {

  if (!f) {
    std::cerr << m_className << "::SetWeightingPotential:\n    Null pointer.\n";
    return;
  }
  m_wpot = f;
}

void ComponentUser::SetMagneticField(void (*f)(const double, const double,
                                               const double, double&, double&,
                                               double&)) {

  if (!f) {
    std::cerr << m_className << "::SetMagneticField:\n    Null pointer.\n";
    return;
  }
  m_bfield = f;
}

void ComponentUser::Reset() {

  m_efield = NULL;
  m_potential = NULL;
  m_wfield = NULL;
  m_wpot = NULL;
  m_bfield = NULL;
  m_ready = false;
}

void ComponentUser::UpdatePeriodicity() {

  if (m_debug) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Periodicities are not supported.\n";
  }
}
}
