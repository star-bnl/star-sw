#include <iomanip>

#include "wcpplib/util/FunNameStack.h"
#include "heed++/code/EnTransfCS_BGM.h"

namespace Heed {

EnTransfCS_BGM::EnTransfCS_BGM(double fparticle_mass, BGMesh* fmesh,
                               int fs_primary_electron, HeedMatterDef* fhmd,
                               long fparticle_charge)
    : particle_mass(fparticle_mass),
      particle_charge(fparticle_charge),
      s_primary_electron(fs_primary_electron),
      hmd(fhmd),
      mesh(fmesh) {
  mfunnamep("EnTransfCS_BGM::EnTransfCS_BGM(...)");

  const long q = mesh->q;
  etcs_bgm.resize(q);
  for (long n = 0; n < q; n++) {
    double bg = mesh->x[n];
    // gamma - 1
    double gamma_1 = sqrt(1.0 + (bg * bg)) - 1.0;
    etcs_bgm[n] = EnTransfCS(fparticle_mass, gamma_1, fs_primary_electron, fhmd,
                             fparticle_charge);
  }
}

void EnTransfCS_BGM::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "EnTransfCS_BGM(l=" << l << "):\n";
  indn.n += 2;
  Ifile << "particle_mass=" << particle_mass
        << " particle_charge=" << particle_charge << std::endl;
  Ifile << "s_primary_electron=" << s_primary_electron << std::endl;
  Ifile << "hmd:\n";
  hmd->print(file, 1);
  Ifile << "mesh:\n";
  mesh->print(file, 1);

  Ifile << "Array of Cross Section:\n";
  Ifile << "Number of elements = " << etcs_bgm.size() << '\n';

  if (l >= 2) {
    const long q = mesh->q;
    for (long n = 0; n < q; n++) {
      Ifile << "n=" << std::setw(5) << n << " bg=" << std::setw(14)
            << mesh->x[n] << " quan=" << std::setw(14) << etcs_bgm[n].quanC;
#ifndef EXCLUDE_MEAN
      file << " mean=" << std::setw(14) << etcs_bgm[n].meanC;
#endif
      file << '\n';
    }
  }
  indn.n -= 2;
}
}
