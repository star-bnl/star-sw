#ifndef HEEDPARTICLE_H
#define HEEDPARTICLE_H

#include <vector>
#include "wcpplib/particle/eparticle.h"
#include "HeedCluster.h"

namespace Heed {
extern long last_particle_number;

/// Charged particle which can be traced through the geometry.
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

  HeedParticle* copy() const override { return new HeedParticle(*this); }
  void print(std::ostream& file, int l) const override;

 protected:
  void physics(std::vector<gparticle*>& secondaries) override;

 private:
  bool m_print_listing = false;
  long m_particle_number;

  bool m_loss_only = false;
  std::vector<double> m_etransf;
  std::vector<long> m_natom;
  std::vector<long> m_nshell;

  bool m_store_clusters = false;
  std::vector<HeedCluster> m_clusterBank;
};
}

#endif
