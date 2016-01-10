#ifndef HEEDPARTICLE_BGM_H
#define HEEDPARTICLE_BGM_H

#include <vector>
#include "HeedCluster.h"
#include "wcpplib/particle/eparticle.h"
#include "wcpplib/safetl/AbsList.h"
/*
Definition of the particle which can be traced through the
geometry. Also the definition of cluster (energy transfer),
and particle bank.

2003, I. Smirnov
*/

//#define SINGLE_TRANSFER          // for debug
#ifdef SINGLE_TRANSFER
#include "heed++/code/EnTransfCS.h"
extern EnTransfCS* aetcs_single_transf;
extern long na_single_transf;
extern long ns_single_transf;
extern double ener_single_transf;
#endif

namespace Heed {
extern long last_particle_number;

class HeedParticle_BGM : public eparticle {
 public:
  int s_print_listing;
  long particle_number;
  // static long last_particle_number;
  HeedParticle_BGM(manip_absvol* primvol, const point& pt, const vec& vel,
                   vfloat time, particle_def* fpardef, int fs_loss_only = 0,
                   int fs_print_listing = 0);
  // if fs_loss_only == 1 - only transfer energy and
  // no othet physics: no deposition of clusters,
  // no generation of virtual photons.
  // Thus it is just a PAI without even clusters

  virtual void physics(void);
  HeedParticle_BGM(void) : eparticle() {}
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(HeedParticle_BGM);
  virtual ~HeedParticle_BGM() { ; }
  double transferred_energy_in_step;  // internal units

  long qtransfer;
  int s_loss_only;
  std::vector<double> transferred_energy;  // internal units
  std::vector<long> natom;
  std::vector<long> nshell;
 
  std::vector<HeedCluster> cluster_bank;
};

}

#endif
