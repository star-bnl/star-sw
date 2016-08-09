
#ifndef G_HEED_FIELDMAP_H
#define G_HEED_FIELDMAP_H

#include "wcpplib/geometry/vec.h"

namespace Garfield {
  class Sensor;
}

namespace Heed {

class HeedFieldMap { 
 public:
  HeedFieldMap() : m_sensor(NULL), m_useEfield(false), m_useBfield(false) {}

  void SetSensor(Garfield::Sensor* sensor) { m_sensor = sensor; }
  void UseEfield(const bool flag) { m_useEfield = flag; }
  void UseBfield(const bool flag) { m_useBfield = flag; }

  void field_map(const point& pt, vec& efield, vec& bfield, 
                 vfloat& mrange) const;
 private:
  Garfield::Sensor* m_sensor;
  bool m_useEfield;
  bool m_useBfield;

};

}

#endif
