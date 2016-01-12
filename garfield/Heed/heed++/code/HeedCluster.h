#ifndef HEEDCLUSTER_H
#define HEEDCLUSTER_H

#include "wcpplib/geometry/vec.h"
#include "wcpplib/geometry/volume.h"

namespace Heed {

class HeedCluster : public RegPassivePtr {
 public:
  double transferred_energy;  // internal units
  long estimated_qel;
  point pt;     // in the first system from tid system
  point ptloc;  // in the local system, the last system from tid
  manip_absvol_treeid tid;
  long natom;
  long nshell;
  HeedCluster(void)
      : transferred_energy(0.0), estimated_qel(0), natom(0), nshell(0) {
    ;
  }
  HeedCluster(double ftransferred_energy, long festimated_qel, const point& fpt,
              const point& fptloc, const manip_absvol_treeid& ftid, long fnatom,
              long fnshell)
      : transferred_energy(ftransferred_energy),
        estimated_qel(festimated_qel),
        pt(fpt),
        ptloc(fptloc),
        tid(ftid),
        natom(fnatom),
        nshell(fnshell) {
    ;
  }
  virtual void print(std::ostream& file, int l) const;
};

}

#endif
