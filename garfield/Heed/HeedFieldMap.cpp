#include <iostream>

#include "Sensor.hh"

#include "wcpplib/clhep_units/WSystemOfUnits.h"

#include "HeedFieldMap.h"

namespace Heed {

const double HeedFieldMap::conv = 1. / CLHEP::cm;

HeedFieldMap::HeedFieldMap() 
  : m_x(0.),
    m_y(0.),
    m_z(0.),
    m_sensor(NULL), 
    m_useEfield(false), 
    m_useBfield(false) {}

void HeedFieldMap::field_map(const point& pt, vec& efield, vec& bfield,
                             vfloat& mrange) const {

  const double x = pt.v.x * conv + m_x;
  const double y = pt.v.y * conv + m_y;
  const double z = pt.v.z * conv + m_z;

  // Initialise the electric and magnetic field.
  efield = vec(0., 0., 0.);
  bfield = vec(0., 0., 0.);
  mrange = DBL_MAX;

  if (!m_sensor) {
    std::cerr << "HeedFieldMap::field_map: Sensor not defined.\n";
    return;
  }

  if (m_useEfield) {
    double ex = 0., ey = 0., ez = 0.;
    int status = 0;
    Garfield::Medium* m = NULL;
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m, status);
    efield.x = ex * 1.e-7;
    efield.y = ey * 1.e-7;
    efield.z = ez * 1.e-7;
  }

  if (m_useBfield) {
    double bx = 0., by = 0., bz = 0.;
    int status = 0;
    m_sensor->MagneticField(x, y, z, bx, by, bz, status);
    bfield.x = bx * 1.e-3;
    bfield.y = by * 1.e-3;
    bfield.z = bz * 1.e-3;
  }
}

bool HeedFieldMap::inside(const point& pt) {

  const double x = pt.v.x * conv + m_x;
  const double y = pt.v.y * conv + m_y;
  const double z = pt.v.z * conv + m_z;
  // Check if the point is inside the drift area.
  if (!m_sensor->IsInArea(x, y, z)) return false;
  // Check if the point is inside a medium.
  Garfield::Medium* m = NULL;
  if (!m_sensor->GetMedium(x, y, z, m) || !m) return false;
  return m->IsIonisable();
}

}
