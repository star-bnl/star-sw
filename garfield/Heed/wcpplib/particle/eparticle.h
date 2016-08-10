#ifndef EPARTICLE_H
#define EPARTICLE_H
#include "wcpplib/geometry/mparticle.h"
#include "wcpplib/particle/particle_def.h"
#include "HeedFieldMap.h"

/*
Charged particle, combination of features of massive geometrical
particle and specification of concrete particle as one of types
known by science.

1998 - 2004, I. Smirnov.
*/

namespace Heed {

class eparticle : public mparticle, public particle_type {
 public:
  /// Constructors
  eparticle(void) : mparticle(), particle_type(), m_fieldMap(NULL) {}
  eparticle(manip_absvol* primvol, const point& pt, const vec& vel, vfloat time,
            particle_def* fpardef, HeedFieldMap* fieldmap);
  eparticle(manip_absvol* primvol, const point& pt,
            const vec& vel,  // length does not have meaning
            vfloat time, particle_def* fpardef, HeedFieldMap* fieldmap, 
            const double gamma_1);
  AnyType_copy(eparticle, gparticle);
  /// Destructor
  virtual ~eparticle() {}
  virtual void print(std::ostream& file, int l) const;

  virtual int force(const point& pt, vec& f, vec& f_perp, vfloat& mrange);
  // if returns 0 then no force, but it should fill zero to f anyway
  // mrange - distance at which the force should not change much

 protected:
  HeedFieldMap* m_fieldMap;
};

}

#endif
