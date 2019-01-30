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
  HeedPhoton() = default;
  /// Constructor.
  HeedPhoton(manip_absvol* primvol, const point& pt, const vec& vel,
             vfloat time, long fparent_particle_number, double fenergy,
             HeedFieldMap* fieldmap, const bool fs_print_listing = false);
  /// Destructor
  virtual ~HeedPhoton() {}

  void print(std::ostream& file, int l) const override;
  HeedPhoton* copy() const override { return new HeedPhoton(*this); }

  long m_particle_number;
  long m_parent_particle_number;

  /// Photon energy [MeV]
  double m_energy;

  /// Flag whether the photon has been absorbed.
  /// Used in physics_after_new_speed.
  bool m_photon_absorbed = false;
  /// Index of absorbing atom.
  long m_na_absorbing;
  /// Index of absorbing shell
  long m_ns_absorbing;

#ifdef SFER_PHOTOEL
  int s_sfer_photoel;
#endif

  /// Flag that delta-electrons are already generated (or cannot be created).
  bool m_delta_generated = false;

 protected:
  void physics_after_new_speed(std::vector<gparticle*>& secondaries) override;
  void physics(std::vector<gparticle*>& secondaries) override;

 private:
  /// Flag to print internal algorithms of a selected event
  bool m_print_listing = false;

  HeedFieldMap* m_fieldMap = nullptr;
};
}

#endif
