#include <iostream>

#include "ComponentConstant.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

ComponentConstant::ComponentConstant()
    : ComponentBase(),
      m_fx(0.),
      m_fy(0.),
      m_fz(0.),
      m_hasPotential(false),
      m_x0(0.),
      m_y0(0.),
      m_z0(0.),
      m_v0(0.),
      m_hasWeightingField(false),
      m_wfield(""),
      m_fwx(0.),
      m_fwy(0.),
      m_fwz(0.),
      m_hasWeightingPotential(false),
      m_wx0(0.),
      m_wy0(0.),
      m_wz0(0.),
      m_w0(0.) {

  m_className = "ComponentConstant";
}

void ComponentConstant::ElectricField(const double x, const double y,
                                      const double z, double& ex, double& ey,
                                      double& ez, Medium*& m, int& status) {

  ex = m_fx;
  ey = m_fy;
  ez = m_fz;
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

void ComponentConstant::ElectricField(const double x, const double y,
                                      const double z, double& ex, double& ey,
                                      double& ez, double& v, Medium*& m,
                                      int& status) {

  ex = m_fx;
  ey = m_fy;
  ez = m_fz;
  if (m_hasPotential) {
    v = m_v0 - (x - m_x0) * m_fx - (y - m_y0) * m_fy - (z - m_z0) * m_fz;
  } else {
    v = 0.;
    if (m_debug) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Potential is not defined.\n";
    }
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

bool ComponentConstant::GetVoltageRange(double& vmin, double& vmax) {

  if (!m_hasPotential) return false;

  if (!m_geometry) {
    std::cerr << m_className << "::GetVoltageRange:\n";
    std::cerr << "    Geometry pointer is null.\n";
    return false;
  }
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::GetVoltageRange:\n";
    std::cerr << "    Could not determine bounding box.\n";
    return false;
  }
  // Calculate potentials at each corner
  const double pxmin = m_v0 - (xmin - m_x0) * m_fx;
  const double pxmax = m_v0 - (xmax - m_x0) * m_fx;
  const double pymin = m_v0 - (ymin - m_y0) * m_fy;
  const double pymax = m_v0 - (ymax - m_y0) * m_fy;
  const double pzmin = m_v0 - (zmin - m_z0) * m_fz;
  const double pzmax = m_v0 - (zmax - m_z0) * m_fz;
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

void ComponentConstant::WeightingField(const double x, const double y,
                                       const double z, double& wx, double& wy,
                                       double& wz, const std::string& label) {

  if (!m_hasWeightingField || label != m_wfield) return;

  Medium* m = GetMedium(x, y, z);
  if (!m) {
    wx = wy = wz = 0.;
    if (m_debug) {
      std::cout << m_className << "::WeightingField:\n";
      std::cout << "    No medium at (" << x << ", " << y << ", " << z << ")\n";
    }
    return;
  }
  wx = m_fwx;
  wy = m_fwy;
  wz = m_fwz;
}

double ComponentConstant::WeightingPotential(const double x, const double y,
                                             const double z,
                                             const std::string& label) {

  if (!m_hasWeightingPotential || label != m_wfield) return 0.;

  Medium* m = GetMedium(x, y, z); 
  if (!m) return 0.;

  return m_w0 - (x - m_wx0) * m_fwx - (y - m_wy0) * m_fwy - (z - m_wz0) * m_fwz;
}

void ComponentConstant::SetElectricField(const double ex, const double ey,
                                         const double ez) {

  m_fx = ex;
  m_fy = ey;
  m_fz = ez;
  if (m_fx * m_fx + m_fy * m_fy + m_fz * m_fz > Small) return;

  std::cerr << m_className << "::SetField:\n";
  std::cerr << "    Electric field is set to zero.\n";
  m_ready = true;
}

void ComponentConstant::SetPotential(const double x, const double y,
                                     const double z, const double v) {

  m_x0 = x;
  m_y0 = y;
  m_z0 = z;
  m_v0 = v;
  m_hasPotential = true;
}

void ComponentConstant::SetWeightingField(const double wx, const double wy,
                                          const double wz,
                                          const std::string label) {

  m_wfield = label;
  m_fwx = wx;
  m_fwy = wy;
  m_fwz = wz;
  m_hasWeightingField = true;
}

void ComponentConstant::SetWeightingPotential(const double x, const double y,
                                              const double z, const double v) {

  if (!m_hasWeightingField) {
    std::cerr << m_className << "::SetWeightingPotential:\n";
    std::cerr << "    Set the weighting field first!\n";
    return;
  }
  m_wx0 = x;
  m_wy0 = y;
  m_wz0 = z;
  m_w0 = v;
  m_hasWeightingPotential = true;
}

void ComponentConstant::Reset() {

  m_fx = m_fy = m_fz = 0.;
  m_hasPotential = false;
  m_hasWeightingField = false;
  m_hasWeightingPotential = false;
  m_wfield = "";
  m_ready = false;
}

void ComponentConstant::UpdatePeriodicity() {

  if (m_debug) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Periodicities are not supported.\n";
  }
}
}
