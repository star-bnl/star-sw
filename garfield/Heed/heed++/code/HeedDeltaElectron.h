#ifndef HEEDDELTAELECTRON_H
#define HEEDDELTAELECTRON_H

#include "wcpplib/particle/eparticle.h"
#include "heed++/code/HeedCondElectron.h"

namespace Heed {

class PairProd;
extern long last_particle_number;

/// Definition of delta-electron which can be traced through the geometry.
/// 2003, I. Smirnov

class HeedDeltaElectron : public eparticle {
 public:
  /// Default constructor.
  HeedDeltaElectron() : eparticle(), s_print_listing(false) {}
  /// Constructor.
  HeedDeltaElectron(manip_absvol* primvol, const point& pt, const vec& vel,
                    vfloat time, long fparent_particle_number,
                    HeedFieldMap* fieldmap, bool fs_print_listing = false);
  /// Destructor
  virtual ~HeedDeltaElectron() {}

  std::vector<HeedCondElectron> conduction_electrons;
  std::vector<HeedCondElectron> conduction_ions;

  long parent_particle_number;

  virtual void physics_mrange(double& fmrange);
  virtual void physics_after_new_speed(std::vector<gparticle*>& secondaries);
  virtual HeedDeltaElectron* copy() const {
    return new HeedDeltaElectron(*this);
  }
  virtual void print(std::ostream& file, int l) const;

 private:
  long particle_number;

  /// Flag to print internal algorithms of a selected event.
  bool s_print_listing;

  // The following things are done in physics_mrange.
  // Later mrange may be reduced by geometry.
  // the signature of this is prange < phys_mrange
  double phys_mrange;  // in internal units

  // Flag that the range is restricted by the loss of all energy to ionization.
  // It is to avoid additional little step due to limited precision at
  // subtraction of energy loss at step from kinetic energy.
  bool s_stop_eloss;

  // Flag that the step is restricted
  // by the condition that number of elastic scatterings with low angles
  // should be less or equal to hdecs->eesls->get_qscat()
  int s_mult_low_path_length;

  // Number of low angle scatterings
  double q_low_path_length;
  // Sign that the range is restricted by path length for large angle scattering
  bool s_path_length;

  // Necessary energy (in internal units) at next step to
  // produce a conduction electron. It is necessary energy, not the left energy
  // because it is randomly generated.
  // Attention: if 0.0, then the electron is already left.
  double necessary_energy;

  double total_Eloss;

  static bool s_low_mult_scattering;
  static bool s_high_mult_scattering;

  void ionisation(const double eloss, const double dedx, PairProd* pairprod);
};
}

#endif
