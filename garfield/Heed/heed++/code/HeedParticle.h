#ifndef HEEDPARTICLE_H
#define HEEDPARTICLE_H

#include <vector>
#include <list>
#include "wcpplib/particle/eparticle.h"
#include "HeedCluster.h"
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
extern long last_particle_number;

class HeedParticle : public eparticle {
 public:
  /// Constructors
  HeedParticle() : eparticle(), m_particleBank(NULL) {}
  HeedParticle(manip_absvol* primvol, const point& pt, const vec& vel,
               vfloat time, particle_def* fpardef, 
               std::list<ActivePtr<gparticle> >& particleBank,
               int fs_loss_only = 0,
               int fs_print_listing = 0);
  // If fs_loss_only == 1 only transferred energy
  // is simulated: no deposition of clusters,
  // no generation of virtual photons.
  macro_copy_total(HeedParticle);
  /// Destructor
  virtual ~HeedParticle() {}

  virtual void physics(void);
  virtual void print(std::ostream& file, int l) const;

 private:
  int s_print_listing;
  long particle_number;

  double transferred_energy_in_step;
  long qtransfer;
  int s_loss_only;
  std::vector<double> transferred_energy;
  std::vector<long> natom;
  std::vector<long> nshell;

  std::vector<HeedCluster> m_clusterBank;
  std::list<ActivePtr<gparticle> >* m_particleBank;
};

}

#endif
