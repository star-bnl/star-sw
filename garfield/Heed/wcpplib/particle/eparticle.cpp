#include "wcpplib/particle/eparticle.h"
/*
1998 - 2004,   I. Smirnov
*/

namespace Heed {

void field_map(const point& pt, vec& Efield, vec& Hfield, vfloat& mrange);
// defined anywhere outside

eparticle::eparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat time, particle_def* fpardef, HeedFieldMap* fieldmap)
    : mparticle(), particle_type(fpardef), m_fieldMap(fieldmap) {
  gparticle gp(primvol, pt, vel, time);
  static_cast<mparticle&>(*this) = mparticle(gp, fpardef->mass);
}

eparticle::eparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat time, particle_def* fpardef, 
                     HeedFieldMap* fieldmap, const double gamma_1)
    : mparticle(primvol, pt, vel, time, fpardef->mass, gamma_1),
      particle_type(fpardef), m_fieldMap(fieldmap) {}

int eparticle::force(const point& pt, vec& f, vec& f_perp, vfloat& mrange) {
  vec efield(0., 0., 0.);
  vec hfield(0., 0., 0.);
  if (!m_fieldMap) {
    std::cerr << "WE HAVE A PROBLEMO!!!" << std::endl;
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
