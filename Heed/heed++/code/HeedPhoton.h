#ifndef HEEDPHOTON_H
#define HEEDPHOTON_H

#include <vector>
#include "HeedFieldMap.h"
#include "heed++/code/HeedMatterDef.h"
#include "wcpplib/geometry/gparticle.h"

//#define SFER_PHOTOEL  // make direction of photoelectron absolutely random

namespace Heed {
extern long last_particle_number;

/// Definition of the photon which can be emitted at atomic relaxation cascades
/// and traced through the geometry.
/// 2003, I. Smirnov

class HeedPhoton : public gparticle {
 public:
  /// Default constructor.
  HeedPhoton() : gparticle(), m_fieldMap(NULL) {}
  /// Constructor.
  HeedPhoton(manip_absvol* primvol, const point& pt, const vec& vel,
             vfloat time, long fparent_particle_number, double fenergy,
             HeedFieldMap* fieldmap, const bool fs_print_listing = false);
  /// Destructor
  virtual ~HeedPhoton() {}

  virtual void physics_after_new_speed(std::vector<gparticle*>& secondaries);
  virtual void physics(std::vector<gparticle*>& secondaries);
  virtual void print(std::ostream& file, int l) const;
  virtual HeedPhoton* copy() const { return new HeedPhoton(*this); }

  long particle_number;
  long parent_particle_number;
  /// Photon energy [MeV]
  double energy;
  /// Flag whether the photon has been absorbed.,
  /// Used in physics_after_new_speed.
  bool s_photon_absorbed;
  /// Index of absorbing atom.
  long na_absorbing;
  /// Index of absorbing shell
  long ns_absorbing;

#ifdef SFER_PHOTOEL
  int s_sfer_photoel;
#endif
  /// Flag that delta-electrons are already generated (or cannot be created).
  bool s_delta_generated;

 private:
  /// Flag to print internal algorithms of a selected event
  bool s_print_listing;
  HeedFieldMap* m_fieldMap = nullptr;
};
}

#endif
