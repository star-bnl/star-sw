#ifndef HEEDPHOTON_H
#define HEEDPHOTON_H

#include <list>
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
  HeedPhoton(void) : gparticle(), m_particleBank(NULL), m_fieldMap(NULL) {}
  /// Constructor.
  HeedPhoton(manip_absvol* primvol, const point& pt, const vec& vel,
             vfloat time, long fparent_particle_number, double fenergy,
             std::list<ActivePtr<gparticle> >& particleBank,
             HeedFieldMap* fieldmap, int fs_print_listing = 0);
  macro_copy_total(HeedPhoton);
  /// Destructor
  virtual ~HeedPhoton() {}

  void physics_after_new_speed(void);
  virtual void physics(void);
  virtual void print(std::ostream& file, int l) const;

  long particle_number;
  long parent_particle_number;
  double energy;          // MeV
  int s_photon_absorbed;  // used in physics_after_new_speed
  long na_absorbing;      // number of absorbing atom
  long ns_absorbing;      // number of absorbing shell
#ifdef SFER_PHOTOEL
  int s_sfer_photoel;
#endif
  /// Flag that delta-electrons are already generated (or cannot be created)
  int s_delta_generated;

 private:
  /// Flag to print internal algorithms of a selected event
  int s_print_listing;
  std::list<ActivePtr<gparticle> >* m_particleBank;
  HeedFieldMap* m_fieldMap;
};
}

#endif
