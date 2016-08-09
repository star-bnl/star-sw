#include <iostream>

#include "Sensor.hh"

#include "wcpplib/clhep_units/WSystemOfUnits.h" 

#include "HeedFieldMap.h"

namespace Heed {

void HeedFieldMap::field_map(const point& pt, vec& efield, vec& bfield, 
                         vfloat& mrange) const {

  const double x = pt.v.x / cm;
  const double y = pt.v.y / cm;
  const double z = pt.v.z / cm;

  // Initialise the electric and magnetic field.
  efield = vec(0., 0., 0.);
  bfield = vec(0., 0., 0.);
  mrange = DBL_MAX;

  if (!m_sensor) {
    std::cerr << "HeedFieldMap::field_map: Sensor not defined.\n";
    return;
  }

  // TODO: check correct dimensions of E and B fields
  if (m_useEfield) {
    double ex = 0., ey = 0., ez = 0.;
    int status = 0;
    Garfield::Medium* m = NULL;
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m, status);
    efield.x = ex * 1.e-5;
    efield.y = ey * 1.e-5;
    efield.z = ez * 1.e-5;
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

}
