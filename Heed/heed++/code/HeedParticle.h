#ifndef HEEDPARTICLE_H
#define HEEDPARTICLE_H

#include "wcpplib/particle/eparticle.h"
#include "wcpplib/safetl/AbsList.h"
#include "wcpplib/safetl/BlkArr.h"
/*
Definition of the particle which can be traced through the
geometry. Also the definition of cluster (energy transfer),
and particle bank.

2003, I. Smirnov
*/

//#define SINGLE_TRANSFER // for debug
#ifdef SINGLE_TRANSFER
#include "heed++/code/EnTransfCS.h"
extern EnTransfCS* aetcs_single_transf;
extern long na_single_transf;
extern long ns_single_transf;
extern double ener_single_transf;
#endif

namespace Heed {

class HeedParticle : public eparticle {
 public:
  /// Constructors
  HeedParticle(void) : eparticle() {}
  HeedParticle(manip_absvol* primvol, const point& pt, const vec& vel,
               vfloat time, particle_def* fpardef, int fs_loss_only = 0,
               int fs_print_listing = 0);
  // If fs_loss_only == 1 only transferred energy
  // is simulated: no deposition of clusters,
  // no generation of virtual photons.
  macro_copy_total(HeedParticle);
  /// Destructor
  virtual ~HeedParticle() {}

  virtual void physics(void);
  virtual void print(std::ostream& file, int l) const;

  int s_print_listing;
  long particle_number;
  static long last_particle_number;

  double transferred_energy_in_step;
  long qtransfer;
  int s_loss_only;
  BlkArr<double> transferred_energy;
  BlkArr<long> natom;
  BlkArr<long> nshell;

};

}

#endif
