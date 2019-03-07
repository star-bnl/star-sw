#ifndef HEEDPARTICLE_H
#define HEEDPARTICLE_H

#include <vector>
#include "wcpplib/particle/eparticle.h"
#include "HeedCluster.h"

namespace Heed {
extern long last_particle_number;

/// Definition of the particle which can be traced through the geometry.
/// Also the definition of cluster (energy transfer), and particle bank.
///
/// 2003, I. Smirnov

class HeedParticle : public eparticle {
 public:
  /// Default constructor
  HeedParticle() : eparticle() {}
  /// Constructor.
  /// If fs_loss_only == false only transferred energy
  /// is simulated: no deposition of clusters,
  /// no generation of virtual photons.
  HeedParticle(manip_absvol* primvol, const point& pt, const vec& vel,
               vfloat time, particle_def* fpardef, HeedFieldMap* fieldmap,
               const bool fs_loss_only = false,
               const bool fs_print_listing = false);
  /// Destructor
  virtual ~HeedParticle() {}

  virtual void physics(std::vector<gparticle*>& secondaries);
  virtual HeedParticle* copy() const { return new HeedParticle(*this); }
  virtual void print(std::ostream& file, int l) const;

 private:
  bool s_print_listing;
  long particle_number;

  bool s_loss_only;
  std::vector<double> etransf;
  std::vector<long> natom;
  std::vector<long> nshell;

  bool s_store_clusters;
  std::vector<HeedCluster> m_clusterBank;
};
}

#endif
