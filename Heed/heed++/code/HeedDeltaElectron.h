#ifndef HEEDDELTAELECTRON_H
#define HEEDDELTAELECTRON_H

#include "wcpplib/particle/eparticle.h"

/*
Definition of delta-electron which can be traced
through the geometry

2003, I. Smirnov
*/

namespace Heed {
extern long last_particle_number;

class HeedDeltaElectron : public eparticle {
 public:
  static int s_low_mult_scattering;
  static int s_high_mult_scattering;

  long particle_number;
  long parent_particle_number;
  // static long last_particle_number;

  int s_print_listing;  // convenient to print internal algorithms
                        // of a selected event
  double total_Eloss;

  // The following things are done in physics_mrange.
  // Later mrange may be reduced by geometry.
  // the signature of this is prange < phys_mrange
  double phys_mrange;  // in internal units
  int s_stop_eloss;    // sign that the range is restricted
                       // by the loss of all energy to ionization.
                       // It is to avoid additional little step due to
  // limited precision at subtraction of energy loss at step from
  // kinetic energy.

  int s_mult_low_path_length;  // if 1 then the step is restricted
  // by the condition that number of elastic scatterings with low angles
  // should be less or equal to hdecs->eesls->get_qscat()
  double q_low_path_length;  // number of low angle scatterings
  int s_path_length;         // sign that the range is restricted by
                             // path length for large angle scattering

  double necessary_energy;  // ( internal units)
                            // at next step to left conduction electron.
                            // It is necessary energy, not the left energy
                            // because it is randomly generated.
  // Attention: if 0.0, then the electron is already left.



  // Constructors
  HeedDeltaElectron() : eparticle(), s_print_listing(0) {}
  HeedDeltaElectron(manip_absvol* primvol, const point& pt, const vec& vel,
                    vfloat time, long fparent_particle_number,
                    int fs_print_listing = 0);
  // Destructor
  virtual ~HeedDeltaElectron() {}

  virtual void physics_mrange(double& fmrange);
  virtual void physics_after_new_speed();
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(HeedDeltaElectron);

};

}

#endif
