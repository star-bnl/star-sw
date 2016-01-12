#include <iostream>

#include "ComponentUser.hh"

namespace Garfield {

ComponentUser::ComponentUser()
    : ComponentBase(),
      m_hasField(false),
      m_field(0),
      m_hasPotential(false),
      m_potential(0),
      m_hasWeightingField(false),
      m_wfield(0),
      m_hasWeightingPotential(false),
      m_wpot(0) {

  m_className = "ComponentUser";
}

void ComponentUser::ElectricField(const double x, const double y,
                                  const double z, double& ex, double& ey,
                                  double& ez, Medium*& m, int& status) {

  if (!m_hasField) {
    ex = ey = ez = 0.;
    m = 0;
    status = -10;
    return;
  }

  m_field(x, y, z, ex, ey, ez);
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

  if (!m_hasField) {
    ex = ey = ez = v = 0.;
    m = 0;
    status = -10;
    return;
  }
  m_field(x, y, z, ex, ey, ez);

  if (m_hasPotential) {
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

void ComponentUser::WeightingField(const double x, const double y,
                                   const double z, double& wx, double& wy,
                                   double& wz, const std::string& label) {

  wx = wy = wz = 0.;
  if (!m_hasWeightingField) return;
  m_wfield(x, y, z, wx, wy, wz, label);
}

double ComponentUser::WeightingPotential(const double x, const double y,
                                         const double z,
                                         const std::string& label) {

  double v = 0.;
  if (m_hasWeightingPotential) {
    m_wpot(x, y, z, v, label);
  }
  return v;
}

void ComponentUser::SetElectricField(void (*f)(const double, const double,
                                               const double, double&, double&,
                                               double&)) {

  if (!f) {
    std::cerr << m_className << "::SetElectricField:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  m_field = f;
  m_hasField = true;
  m_ready = true;
}

void ComponentUser::SetPotential(void (*f)(const double, const double,
                                           const double, double&)) {

  if (!f) {
    std::cerr << m_className << "::SetPotential:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  m_potential = f;
  m_hasPotential = true;
}

void ComponentUser::SetWeightingField(void (*f)(const double, const double,
                                                const double, double&, double&,
                                                double&, const std::string)) {

  if (!f) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  m_wfield = f;
  m_hasWeightingField = true;
}

void ComponentUser::SetWeightingPotential(void (*f)(const double, const double,
                                                    const double, double&,
                                                    const std::string)) {

  if (!f) {
    std::cerr << m_className << "::SetWeightingPotential:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  m_wpot = f;
  m_hasWeightingPotential = true;
}

void ComponentUser::Reset() {

  m_field = 0;
  m_potential = 0;
  m_wfield = 0;
  m_wpot = 0;
  m_hasField = false;
  m_hasPotential = false;
  m_hasWeightingField = false;
  m_hasWeightingPotential = false;
  m_ready = false;
}

void ComponentUser::UpdatePeriodicity() {

  if (m_debug) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Periodicities are not supported.\n";
  }
}
}
