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
  /// Default constructor.
  mparticle() = default;
  /// Constructor, \f$\gamma - 1\f$ calculated from the from velocity vector.
  mparticle(manip_absvol* primvol, const point& pt, const vec& vel, 
            vfloat ftime, double fmass);
  /// Destructor.
  virtual ~mparticle() {}

  /// Get the current kinetic energy.
  double kinetic_energy() const { return m_curr_ekin; }

  void print(std::ostream& file, int l) const override;
  mparticle* copy() const override { return new mparticle(*this); }


 protected:
  void step(std::vector<gparticle*>& secondaries) override;

  /// Set curvature. Calls force().
  /// - If force is zero, set curved = false, frelcen = (0, 0, 0).
  ///   - If, in addition, currpos.dir == (0, 0, 0), set fmrange = 0.
  /// - If currpos.dir == (0, 0, 0), set currpos.dir = unit_vec(f).
  /// - If force is parallel or anti-parallel to dir, 
  ///   set curved = false, frelcen = (0, 0, 0).
  /// - If force is anti-parallel to dir, restrict range till exceeding
  ///   kinetic energy.
  void curvature(bool& curved, vec& frelcen, vfloat& fmrange, 
                 vfloat prec) override;

  /// The force is considered to be split in two components.
  /// One component, f, can be in any direction and is
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

  /// Mass (not mass * speed_of_light^2)
  double m_mass = 0.;

  /// Current kinetic energy
  double m_curr_ekin = 0.;
  /// Original kinetic energy
  double m_orig_ekin = 0.;
  /// Previous kinetic energy
  double m_prev_ekin = 0.;

  /// Current \f$\gamma - 1\f$
  double m_curr_gamma_1 = 0.;
  /// Original \f$\gamma - 1\f$
  double m_orig_gamma_1 = 0.;
  /// Previous \f$\gamma - 1\f$
  double m_prev_gamma_1 = 0.;

 private:
  /// Check consistency of kinetic energy, \f$\gamma - 1\f$, speed, and mass.
  void check_consistency() const;
  /// Set new speed, direction and time for the current position.
  void new_speed();

};

std::ostream& operator<<(std::ostream& file, const mparticle& f);
}

#endif
