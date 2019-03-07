#ifndef EPARTICLE_H
#define EPARTICLE_H
#include "wcpplib/geometry/mparticle.h"
#include "wcpplib/particle/particle_def.h"
#include "HeedFieldMap.h"

// 1998 - 2004, I. Smirnov.

namespace Heed {

/// Charged particle. Combination of features of massive geometrical
/// particle and specification of concrete particle as one of types
/// known by science.

class eparticle : public mparticle, public particle_type {
 public:
  /// Default constructor
  eparticle() : mparticle(), particle_type(), m_fieldMap(NULL) {}
  /// Constructor using velocity vector.
  eparticle(manip_absvol* primvol, const point& pt, const vec& vel, vfloat time,
            particle_def* fpardef, HeedFieldMap* fieldmap);
  /// Destructor
  virtual ~eparticle() {}

  virtual eparticle* copy() const { return new eparticle(*this); }
  virtual void print(std::ostream& file, int l) const;

  /// Calculate force components.
  virtual int force(const point& pt, vec& f, vec& f_perp, vfloat& mrange);
  // mrange - distance at which the force should not change much

 protected:
  HeedFieldMap* m_fieldMap;
};
}

#endif
