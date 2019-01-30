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
  HeedDeltaElectron() = default;
  /// Constructor.
  HeedDeltaElectron(manip_absvol* primvol, const point& pt, const vec& vel,
                    vfloat ftime, long fparent_particle_number,
                    HeedFieldMap* fieldmap, bool fs_print_listing = false);
  /// Destructor
  virtual ~HeedDeltaElectron() {}

  HeedDeltaElectron* copy() const override {
    return new HeedDeltaElectron(*this);
  }
  void print(std::ostream& file, int l) const override;

  std::vector<HeedCondElectron> conduction_electrons;
  std::vector<HeedCondElectron> conduction_ions;

  long parent_particle_number;

 protected:
  void physics_mrange(double& fmrange) override;
  void physics_after_new_speed(std::vector<gparticle*>& secondaries) override;

 private:
  long m_particle_number;

  /// Flag to print internal algorithms of a selected event.
  bool m_print_listing = false;

  /// Physics-based step length (in internal units).
  /// Later mrange may be reduced by geometry.
  double m_phys_mrange = 0.;

  /// Flag that the range is restricted by the loss of all energy to ionization.
  /// It is to avoid additional little step due to limited precision at
  /// subtraction of energy loss at step from kinetic energy.
  bool m_stop_eloss = false;

  /// Flag that the step is restricted by the condition that the number of 
  /// elastic scatterings with low angles should be less or equal to 
  /// hdecs->eesls->get_qscat()
  bool m_mult_low_path_length = false;

  /// Number of low angle scatterings
  double m_q_low_path_length = 0.;
  /// Flag that the range is restricted by path length for large angle scattering
  bool m_path_length = false;

  /// Necessary energy (in internal units) at next step to produce a
  /// conduction electron. 
  // It is not identical to the left energy because it is randomly generated.
  // Attention: if 0.0, then the electron is already finished.
  double m_necessary_energy = 0.;

  double m_total_eloss = 0.;

  static bool s_low_mult_scattering;
  static bool s_high_mult_scattering;

  void ionisation(const double eloss, const double dedx, PairProd* pairprod);
};
}

#endif
