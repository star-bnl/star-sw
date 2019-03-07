
#ifndef G_HEED_FIELDMAP_H
#define G_HEED_FIELDMAP_H

#include "wcpplib/geometry/vec.h"

namespace Garfield {
class Sensor;
}

namespace Heed {

/// Retrieve electric and magnetic field from Sensor.

class HeedFieldMap {
 public:
  HeedFieldMap();

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
  static const double conv;

  // Centre of the geometry.
  double m_x;
  double m_y;
  double m_z;

  Garfield::Sensor* m_sensor;
  bool m_useEfield;
  bool m_useBfield;
};
}

#endif
