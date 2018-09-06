#ifndef ENTRANFCS_BGM_H
#define ENTRANFCS_BGM_H

#include "heed++/code/BGMesh.h"
#include "heed++/code/EnTransfCS.h"

namespace Heed {

/// Energy transfer cross-section
class EnTransfCS_BGM {
 public:
  /// Default constructor
  EnTransfCS_BGM() = default;
  /// Constructor
  EnTransfCS_BGM(double fparticle_mass, BGMesh* fmesh,
                 int fs_primary_electron, HeedMatterDef* fhmd,
                 long fparticle_charge = 1);

  // All data from EnTransfCS that do not depend on speed.
  // Particle mass [MeV]
  double particle_mass = 0.;
  /// Particle charge in units of electron charges.
  /// It is squared, therefore the sign does not matter.
  long particle_charge = 0;
  /// Sign that the primary particle is an electron
  int s_primary_electron = 0;

  HeedMatterDef* hmd = nullptr;
  BGMesh* mesh = nullptr;
  std::vector<EnTransfCS> etcs_bgm;

  EnTransfCS_BGM* copy() const { return new EnTransfCS_BGM(*this); }
  void print(std::ostream& file, int l) const;
};
}

#endif
