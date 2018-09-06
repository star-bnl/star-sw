#ifndef MPARTICLE_H
#define MPARTICLE_H
#include <iostream>
#include "wcpplib/geometry/gparticle.h"
#include "wcpplib/math/lorgamma.h"

/*
Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
 */

namespace Heed {

/// Massive particle. A force can be applied.

class mparticle : public gparticle {
 public:
  /// Mass (not mass * speed_of_light^2)
  double mass = 0.;

  double orig_kin_energy;
  double orig_gamma_1;  // gamma-1
  double prev_kin_energy;
  double prev_gamma_1;  // gamma-1
  double curr_kin_energy;
  double curr_gamma_1;  // gamma-1

  /// Check consistency of kin_energy, gamma_1, speed, speed_of_light and mass.
  void check_consistency() const;

  void step(std::vector<gparticle*>& secondaries) override;

  /// Set curvature. Calls force().
  /// If force is zero, returns fs_cf=0; frelcen=dv0;
  /// If force is zero, and currpos.dir==dv0, makes, in addition,  fmrange=0;
  /// If currpos.dir==dv0, makes currpos.dir=unit_vec(f);
  /// If force is parallel or anti-parallel to dir, makes fs_cf=0; frelcen=dv0;
  /// If force is anti-parallel to dir, restricts range till exceeding
  /// kinetic energy.
  void curvature(int& fs_cf, vec& frelcen, vfloat& fmrange, 
                 vfloat prec) override;

  /// The force is considered to be split in two components.
  /// One component, namely f, can be in any direction and is
  /// capable of doing work. The other one is always normal to dir
  /// and cannot do work. The latter can represent the magnetic component of 
  /// the Lorentz force.
  /// This splitting improve precision of calculation of kinetic energy.
  /// But the latter component is not the true force. To derive the force
  /// one should do vector multiplication of speed by f_perp,
  /// f_perp2 = currpos.speed * (currpos.dir || f_perp_fl2);
  /// Return 0 if there is no force, f is initialised to zero anyway.
  /// mrange is the distance at which the force should not change much.
  /// The dimension of f is [weight] * [lenght] / [time]^2
  /// The dimension of f_perp is [weight] / [time];
  virtual int force(const point& pt, vec& f, vec& f_perp, vfloat& mrange);

  /// Set new speed, direction and time for currpos.
  void new_speed();

  /// Default constructor.
  mparticle() = default;
  /// Constructor, \f$\gamma - 1\f$ calculated from the from velocity vector.
  mparticle(manip_absvol* primvol, const point& pt, const vec& vel, vfloat time,
            double fmass);
  /// Destructor.
  virtual ~mparticle() {}

  void print(std::ostream& file, int l) const override;
  mparticle* copy() const override { return new mparticle(*this); }
};

std::ostream& operator<<(std::ostream& file, const mparticle& f);
}

#endif
