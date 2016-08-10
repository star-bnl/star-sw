#ifndef EIPARTICLE_H
#define EIPARTICLE_H
#include "wcpplib/particle/eparticle.h"
#include "wcpplib/particle/particle_def.h"

/*
Charged ionizing particle.
Very simple and crude class - for start and for debug (comparison with more
accurate but complicted future classes).

1999 - 2002  I. Smirnov.
*/

namespace Heed {

class eiparticle : public eparticle {
 public:
  /// Constructors
  eiparticle(void) : eparticle() {}
  eiparticle(manip_absvol* primvol, const point& pt, const vec& vel,
             vfloat time, particle_def* fpardef, HeedFieldMap* fieldmap,
             int fs_add_loss = 0)
      : eparticle(primvol, pt, vel, time, fpardef, fieldmap),
        s_add_loss(fs_add_loss),
        total_loss(0.0) {}
  AnyType_copy(eiparticle, gparticle);
  /// Destructor
  virtual ~eiparticle() {}
  virtual void print(std::ostream& file, int l) const;
  virtual void physics_after_new_speed(void);

 private:
  /// Energy loss per unit length
  double Bethe_Bloch_en_loss(void);

  /// Flag for handling energy loss
  /// 0 - loss is subtracted from current particle energy
  /// 1 - loss is added (simulation of particle passage to opposite
  //      direction at intergation of field equations).
  //      Do not forget to assign opposite charge.
  int s_add_loss;
  double total_loss;

};

}

#endif
