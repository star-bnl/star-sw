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
  eparticle() = default;
  /// Constructor using velocity vector.
  eparticle(manip_absvol* primvol, const point& pt, const vec& vel, vfloat time,
            particle_def* fpardef, HeedFieldMap* fieldmap);
  /// Destructor
  virtual ~eparticle() {}

  eparticle* copy() const override { return new eparticle(*this); }
  void print(std::ostream& file, int l) const override;

 protected:
  /// Calculate force components.
  int force(const point& pt, vec& f, vec& f_perp, vfloat& mrange) override;
  // mrange - distance at which the force should not change much

  /// Pointer to field map.
  HeedFieldMap* m_fieldMap = nullptr;
};
}

#endif
