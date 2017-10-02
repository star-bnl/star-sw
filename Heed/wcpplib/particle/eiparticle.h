#ifndef EIPARTICLE_H
#define EIPARTICLE_H
#include "wcpplib/particle/eparticle.h"
#include "wcpplib/particle/particle_def.h"

// 1999 - 2002  I. Smirnov.

namespace Heed {

/// Charged ionizing particle.

class eiparticle : public eparticle {
 public:
  /// Default constructor
  eiparticle() : eparticle() {}
  /// Constructor
  eiparticle(manip_absvol* primvol, const point& pt, const vec& vel,
             vfloat time, particle_def* fpardef, HeedFieldMap* fieldmap,
             int fs_add_loss = 0)
      : eparticle(primvol, pt, vel, time, fpardef, fieldmap),
        s_add_loss(fs_add_loss),
        total_loss(0.0) {}
  /// Destructor
  virtual ~eiparticle() {}

  virtual eiparticle* copy() const { return new eiparticle(*this); }
  virtual void print(std::ostream& file, int l) const;
  virtual void physics_after_new_speed(std::vector<gparticle*>& secondaries);

 private:
  /// Energy loss per unit length.
  double Bethe_Bloch_en_loss();

  /// Flag for handling energy loss.
  /// 0 - loss is subtracted from current particle energy
  /// 1 - loss is added (simulation of particle passage to opposite
  ///      direction at integration of field equations).
  ///      Do not forget to assign opposite charge.
  int s_add_loss;
  double total_loss;
};
}

#endif
