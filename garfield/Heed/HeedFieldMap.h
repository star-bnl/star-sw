#ifndef G_HEED_FIELDMAP_H
#define G_HEED_FIELDMAP_H

#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/geometry/vec.h"

namespace Garfield {
class Sensor;
}

namespace Heed {

/// Retrieve electric and magnetic field from Sensor.

class HeedFieldMap {
 public:
  HeedFieldMap() = default;

  void SetSensor(Garfield::Sensor* sensor) { m_sensor = sensor; }
  void SetCentre(const double x, const double y, const double z) {
    m_x = x;
    m_y = y;
    m_z = z;
  }
  void UseEfield(const bool flag) { m_useEfield = flag; }
  void UseBfield(const bool flag) { m_useBfield = flag; }

  void field_map(const point& pt, vec& efield, vec& bfield,
                 vfloat& mrange) const;
  bool inside(const point& pt);

 private:
  /// Conversion factor from mm to cm.
  static constexpr double conv = 1. / CLHEP::cm;

  // Centre of the geometry.
  double m_x = 0.;
  double m_y = 0.;
  double m_z = 0.;

  Garfield::Sensor* m_sensor = nullptr;
  bool m_useEfield = false;
  bool m_useBfield = false;
};
}

#endif
