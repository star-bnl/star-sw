#ifndef EIPARTICLE_H
#define EIPARTICLE_H
#include "wcpplib/particle/eparticle.h" 
#include "wcpplib/particle/particle_def.h"

/*
Charged ionizing particle.
Very simple and rude class - for start and for debug (comparison with more
accurate but complicted future classes).

1999 - 2002  I. Smirnov.
*/


class eiparticle: public eparticle
{
  double Bethe_Bloch_en_loss(void); // per unit length
public:
  //virtual void step(void);
  virtual void physics_after_new_speed(void);
  //virtual void ionization_loss(void);
  int s_add_loss;  // 0 - the Eloss is subtracted from current particle energy
  // 1 - Eloss is added ( simulation of particle passage to opposite 
  //     direction at intergation of field equations).
  //     Do not forget to assign opposite charge.  
  double total_Eloss;

  eiparticle(manip_absvol* primvol, const point& pt, 
	     const vec& vel, vfloat time, particle_def* fpardef, 
	     int fs_add_loss=0):
    eparticle(primvol, pt, vel, time, fpardef), s_add_loss(fs_add_loss),
    total_Eloss(0.0) {;}
  eiparticle(void): eparticle(){;}
  virtual void print(ostream& file, int l) const ;
  AnyType_copy(eiparticle, gparticle);
  virtual ~eiparticle() {;}
};

#endif
