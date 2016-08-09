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
  // Constructors
  HeedDeltaElectron() : eparticle(), s_print_listing(0) {}
  HeedDeltaElectron(manip_absvol* primvol, const point& pt, const vec& vel,
                    vfloat time, long fparent_particle_number,
                    HeedFieldMap* fieldmap,
                    int fs_print_listing = 0);
  macro_copy_total(HeedDeltaElectron);
  // Destructor
  virtual ~HeedDeltaElectron() {}

  long parent_particle_number;

  virtual void physics_mrange(double& fmrange);
  virtual void physics_after_new_speed();
  virtual void print(std::ostream& file, int l) const;

 private:
  long particle_number;

  // Flag to print internal algorithms of a selected event.
  int s_print_listing;

  // The following things are done in physics_mrange.
  // Later mrange may be reduced by geometry.
  // the signature of this is prange < phys_mrange
  double phys_mrange;  // in internal units
  // Sign that the range is restricted by the loss of all energy to ionization.
  // It is to avoid additional little step due to limited precision at 
  // subtraction of energy loss at step from kinetic energy.
  int s_stop_eloss;    
  int s_mult_low_path_length;  // if 1 then the step is restricted
  // by the condition that number of elastic scatterings with low angles
  // should be less or equal to hdecs->eesls->get_qscat()

  // Number of low angle scatterings
  double q_low_path_length;  
  // Sign that the range is restricted by path length for large angle scattering
  int s_path_length;         

  double necessary_energy;  // ( internal units)
                            // at next step to left conduction electron.
                            // It is necessary energy, not the left energy
                            // because it is randomly generated.
  // Attention: if 0.0, then the electron is already left.

  double total_Eloss;

  static int s_low_mult_scattering;
  static int s_high_mult_scattering;

};

}

#endif
