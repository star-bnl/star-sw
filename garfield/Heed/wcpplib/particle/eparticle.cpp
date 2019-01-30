#include "wcpplib/particle/eparticle.h"

// 1998 - 2004,   I. Smirnov

namespace Heed {

eparticle::eparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat ftime, particle_def* fpardef, HeedFieldMap* fieldmap)
    : mparticle(primvol, pt, vel, ftime, fpardef->mass), 
      particle_type(fpardef), m_fieldMap(fieldmap) {
}

int eparticle::force(const point& pt, vec& f, vec& f_perp, vfloat& mrange) {
  vec efield(0., 0., 0.);
  vec hfield(0., 0., 0.);
  if (!m_fieldMap) {
    std::cerr << "Field map not defined.\n";
    return 1;
  }
  m_fieldMap->field_map(pt, efield, hfield, mrange);
  f = pardef->charge * efield;
  f_perp = pardef->charge * hfield;
  return 1;
}

void eparticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "eparticle: particle is ";
  print_notation(file);
  file << '\n';
  mparticle::print(file, l);
}
}
