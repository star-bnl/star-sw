#ifndef HEEDCLUSTER_H
#define HEEDCLUSTER_H

#include "wcpplib/geometry/vec.h"
#include "wcpplib/geometry/volume.h"

namespace Heed {

/// Cluster.
class HeedCluster {
 public:
  HeedCluster() = default;
  HeedCluster(double ftransferred_energy, long festimated_qel, const point& fpt,
              const point& fptloc, const manip_absvol_treeid& ftid, long fnatom,
              long fnshell)
      : transferred_energy(ftransferred_energy),
        estimated_qel(festimated_qel),
        pt(fpt),
        ptloc(fptloc),
        tid(ftid),
        natom(fnatom),
        nshell(fnshell) {}
  /// Energy transfer in internal units.
  double transferred_energy = 0.;

  long estimated_qel = 0;
  /// Coordinates in the first system of the tree.
  point pt;
  /// Coordinates in the local system (the last system in the tree).
  point ptloc;

  manip_absvol_treeid tid;
  long natom = 0;
  long nshell = 0;
  void print(std::ostream& file, int l) const;
};
}

#endif
