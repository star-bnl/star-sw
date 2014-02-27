#ifndef MPARTICLE_H
#define MPARTICLE_H
#include <iostream>
#include "wcpplib/geometry/gparticle.h"
#include "wcpplib/math/lorgamma.h"

/* massive particle. A force can be applied.

Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
 */

class mparticle : public gparticle {
 public:
  /// Mass (not mass * speed_of_light^2)
  double mass;

  double orig_kin_energy;
  double orig_gamma_1;  // gamma-1
  double prev_kin_energy;
  double prev_gamma_1;  // gamma-1
  double curr_kin_energy;
  double curr_gamma_1;  // gamma-1

  void check_consistency(void) const;  // checks that kin_energy,
  // gamma_1, speed, speed_of_light and mass correspond to each other

  virtual void step(void);

  virtual void curvature(int& fs_cf, vec& frelcen, vfloat& fmrange,
                         vfloat prec);
  // Allows to set curvature.
  // Calls force().
  // If force is zero, returns fs_cf=0; frelcen=dv0;
  // If force is zero, and currpos.dir==dv0, makes, in addition,  fmrange=0;
  // If currpos.dir==dv0, makes currpos.dir=unit_vec(f);
  // If force is parallel or anti-parallel to dir, makes fs_cf=0; frelcen=dv0;
  // If force is anti-parallel to dir, restrics range till exceeding
  // of kinetic energy.

  virtual void physics_after_new_speed(void) { ; }
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.

  virtual void physics(void) { ; }
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.

  virtual int force(const point& pt, vec& f, vec& f_perp, vfloat& mrange);
  // Force is considered to be split to two components.
  // One component, namely f,
  // is directed to any direction and capable to do the work.
  // The other one is always normal to dir and it cannot do the work.
  // The latter can represent magnetic component of Lorentz force.
  // This splitting improve precision of calculation of kinetic energy.
  // But the latter component is not the true force.
  // To derive the force one should
  // do vector multiplication of speed by f_perp.:
  // f_perp2=currpos.speed * (currpos.dir||f_perp_fl2);
  //
  // if returns 0 then no force, but it should fill zero to f anyway
  // mrange - distance at which the force should not change much
  //
  // The dimension of f is [weight] * [lenght] / [time]^2
  // The dimsnsion of f_perp is [weight] / [time];

  void new_speed(void);
  // Set new speed, direction and time for currpos.

  mparticle(void) : gparticle(), mass(0.0) { ; }
  mparticle(gparticle const& gp, double fmass);  // dengerous,
  // only not for very fast particles, since gamma-1 is computed from speed.
  mparticle(gparticle const& gp, double fmass, double gamma_1);
  // safe, but only current gamma_1 is given.
  // It should be equal to origin one.
  // The previous and the next speed should be zero.
  // So this constructor is good when gparticle is in origin point.
  // But gamma_1 should correspond to velocity given in gp.
  // It is automatically checked.
  // In the following constructor the length of velocity has no affect.
  mparticle(manip_absvol* primvol, const point& pt, const vec& vel, vfloat time,
            double fmass, double gamma_1);

  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(gparticle);
  virtual ~mparticle() { ; }
};

std::ostream& operator<<(std::ostream& file, const mparticle& f);

#endif
