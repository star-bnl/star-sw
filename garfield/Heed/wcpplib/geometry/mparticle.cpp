#include "wcpplib/geometry/mparticle.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
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

using CLHEP::c_light;
using CLHEP::c_squared;

mparticle::mparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat ftime, double fmass) 
    : gparticle(primvol, pt, vel, ftime),
      m_mass(fmass) {

  mfunname("mparticle::mparticle(...)");

  const double mc2 = m_mass * c_squared;
  m_orig_gamma_1 = lorgamma_1(m_origin.speed / c_light);
  m_orig_ekin = m_orig_gamma_1 * mc2;
  m_prev_gamma_1 = lorgamma_1(m_prevpos.speed / c_light);
  m_prev_ekin = m_prev_gamma_1 * mc2;
  m_curr_gamma_1 = lorgamma_1(m_currpos.speed / c_light);
  m_curr_ekin = m_curr_gamma_1 * mc2;
  check_consistency();
}

void mparticle::check_consistency() const {
  mfunname("void mparticle::check_consistency() const");
  check_econd11(vecerror, != 0, mcerr);

  double v0 = c_light * lorbeta(m_orig_gamma_1);
  double v1 = m_origin.speed;
  check_econd11a(fabs(v0 - v1) / (v0 + v1), > 1.0e-10, (*this), mcerr);

  v0 = c_light * lorbeta(m_prev_gamma_1);
  v1 = m_prevpos.speed;
  check_econd11a(fabs(v0 - v1) / (v0 + v1), > 1.0e-10, (*this), mcerr);

  v0 = c_light * lorbeta(m_curr_gamma_1);
  v1 = m_currpos.speed;
  check_econd11a(fabs(v0 - v1) / (v0 + v1), > 1.0e-10, (*this), mcerr);

  const double mc2 = m_mass * c_squared;
  double ek = m_orig_gamma_1 * mc2;
  if (ek > 1000.0 * DBL_MIN) {
    check_econd11a(fabs(m_orig_ekin - ek) / (m_orig_ekin + ek), > 1.0e-9, 
        "ek=" << ek << '\n' << (*this), mcerr);
  }
  ek = m_prev_gamma_1 * mc2;
  if (ek > 1000.0 * DBL_MIN) {
    check_econd11a(fabs(m_prev_ekin - ek) / (m_prev_ekin + ek), > 1.0e-9, 
        "ek=" << ek << '\n' << (*this), mcerr);
  }
  ek = m_curr_gamma_1 * mc2;
  if (ek > 1000.0 * DBL_MIN) {
    check_econd11a(fabs(m_curr_ekin - ek) / (m_curr_ekin + ek), > 1.0e-9, 
        "ek=" << ek << '\n' << (*this), mcerr);
  }
}

void mparticle::step(std::vector<gparticle*>& secondaries) {
  // Make step to nextpos and calculate new step to border
  mfunname("void mparticle::step(...)");
  m_prevpos = m_currpos;
  m_prev_ekin = m_curr_ekin;
  m_prev_gamma_1 = m_curr_gamma_1;
  m_currpos = m_nextpos;
  m_total_range_from_origin += m_currpos.prange;
  m_nstep++;
  if (m_currpos.prange == 0) {
    m_nzero_step++;
    check_econd12a(m_nzero_step, >, m_max_qzero_step,
                   "too many zero steps, possible infinite loop\n";
                   print(mcout, 10);, mcerr);
  } else {
    m_nzero_step = 0;
  }
  // Calculate new current speed, direction and time.
  new_speed();
  physics_after_new_speed(secondaries);

  if (m_alive) {
    if (m_prevpos.tid != m_currpos.tid) change_vol();
    m_nextpos = calc_step_to_bord();
  }
}

void mparticle::curvature(bool& curved, vec& frelcen, vfloat& fmrange,
                          vfloat prec) {

  pvecerror("void mparticle::curvature(...)");
  vec f;
  vec f_perp_fl;
  int i = force(m_currpos.pt, f, f_perp_fl, fmrange);
  vec f_perp = m_currpos.speed * (m_currpos.dir || f_perp_fl);
  f += f_perp;
  if (i == 0 || f == dv0) {
    curved = false;
    frelcen = dv0;
    if (m_currpos.dir == dv0) fmrange = 0;  // to stay in the place
    return;
  }
  if (m_currpos.dir == dv0) {
    // starting to move in the direction of force
    m_currpos.dir = unit_vec(f);
  }
  const int j = check_par(m_currpos.dir, f, prec);
  if (j != 0) {
    curved = false;
    frelcen = dv0;
    if (j == -1) {
      // decelerate, search for stop point
      const double ran = m_curr_ekin / f.length();
      if (fmrange > ran) fmrange = ran;
    }
  } else {
    curved = true;
    vec fn = project_to_plane(f, m_currpos.dir);  // normal component
    frelcen = unit_vec(fn);
    double len = fn.length();
    vfloat rad =
        (m_currpos.speed * m_currpos.speed * (m_curr_gamma_1 + 1) * m_mass) / len;
    frelcen *= rad;
  }
  m_currpos.dirloc = m_currpos.dir;
  m_currpos.tid.up_absref(&m_currpos.dirloc);
}

int mparticle::force(const point& /*pt*/, vec& f, vec& f_perp, vfloat& mrange) {
  f.x = f_perp.x = 0.;
  f.y = f_perp.y = 0.;
  f.z = f_perp.z = 0.;
  mrange = max_vfloat;
  return 0;
}

void mparticle::new_speed() {
  pvecerror("void mparticle::new_speed(void)");
  if (m_currpos.prange == 0.0) {
    check_consistency();
    return;
  }
  vec f1, f2, f_perp1, f_perp2, f_perp_fl1, f_perp_fl2;
  vfloat r1, r2;  // ranges, do not need here
  int i = force(m_prevpos.pt, f1, f_perp_fl1, r1);
  int j = force(m_currpos.pt, f2, f_perp_fl2, r2);
  check_econd11a(vecerror, != 0, "position 1, after computing force\n", mcerr);
  f_perp1 = m_prevpos.speed * (m_prevpos.dir || f_perp_fl1);
  f_perp2 = m_currpos.speed * (m_currpos.dir || f_perp_fl2);
  // Later f_perp are ignored since they can not do the work;
  vec f_mean = 0.5 * (f1 + f2);
  check_econd11a(vecerror, != 0, "position 2, after computing f_perp\n", mcerr);

  if ((i == 0 && j == 0) || f_mean == dv0) {
    m_curr_ekin = m_prev_ekin;
    m_curr_gamma_1 = m_prev_gamma_1;
    // speed is preserved by gparticle
    m_currpos.speed = m_prevpos.speed;  
  } else {
    vec r = m_currpos.pt - m_prevpos.pt;
    double W = 0;  // force * range * cos() = work * cos() (may be negative)
    if (r != dv0) W = f_mean * r;
    // This is work which should lead to increase or decrease the speed
    if (W == 0) {
      m_curr_ekin = m_prev_ekin;
      m_curr_gamma_1 = m_prev_gamma_1;
      m_currpos.speed = m_prevpos.speed;
    } else {
      m_curr_ekin = m_prev_ekin + W;
      if (m_curr_ekin <= 0) {
        m_curr_ekin = 0;
        m_currpos.speed = 0;
        m_curr_gamma_1 = 0;
        m_currpos.dir = dv0;
      } else {
        double resten = m_mass * c_squared;
        m_curr_gamma_1 = m_curr_ekin / resten;
        m_currpos.speed = c_light * lorbeta(m_curr_gamma_1);
      }
    }
  }
  if (!(i == 0 && j == 0)) {
    vec fn1 = project_to_plane(f1, m_prevpos.dir);  // normal component
    vec fn2 = project_to_plane(f2, m_currpos.dir);  // normal component
    check_econd11a(vecerror, != 0, "position 3, after computing fn2\n", mcerr);
    // mean ortogonal component of working force
    vec mean_fn = 0.5 * (fn1 + fn2);  
    double mean_fn_len = mean_fn.length();
    vec fdir = m_prevpos.dir;
    if (mean_fn_len > 0.0) {
      vec relcen = unit_vec(mean_fn);
      double mean_speed = (m_prevpos.speed + m_currpos.speed) * 0.5;
      vfloat new_rad = (mean_speed * mean_speed *
                        ((m_prev_gamma_1 + m_curr_gamma_1) * 0.5 + 1) * m_mass) /
                       mean_fn_len;
      if (new_rad > 0.0) {
        vfloat ang = m_currpos.prange / new_rad;  // angle to turn
        fdir.turn(m_prevpos.dir || relcen, ang);  // direction at the end
      }
    }
    check_econd11a(vecerror, != 0, "position 4\n", mcerr);
    vec mean_f_perp_fl = 0.5 * (f_perp_fl1 + f_perp_fl2);
    double len_mean_f_perp_fl = mean_f_perp_fl.length();
    f_perp2 = m_currpos.speed * (m_currpos.dir || f_perp_fl2);
    double mean_f_perp = 0.5 * (f_perp1.length() + f_perp2.length());
    check_econd11a(vecerror, != 0, "position 5\n", mcerr);
    if (len_mean_f_perp_fl > 0.0) {
      vec fdir_proj = project_to_plane(m_prevpos.dir, mean_f_perp_fl);
      if (!apeq(fdir_proj.length(), 0.0)) {
        check_econd11a(vecerror, != 0, "position 6\n", mcerr);
        double length_proj = m_currpos.prange * cos2vec(m_prevpos.dir, fdir_proj);
        check_econd11a(vecerror, != 0, "position 7\n", mcerr);
        double acc =
            mean_f_perp / (((m_prev_gamma_1 + m_curr_gamma_1) * 0.5 + 1) * m_mass);
        double mean_speed = (m_prevpos.speed + m_currpos.speed) * 0.5;
        double new_rad = pow(mean_speed * fdir_proj.length(), 2) / acc;
        double ang = length_proj / new_rad;
        if (new_rad > 0 && ang > 0) {
          fdir.turn(mean_f_perp_fl, -ang);  // direction at the end
          check_econd11a(vecerror, != 0, "position 8\n", mcerr);
        }
      }
    }
    m_currpos.dir = fdir;
    check_econd11a(vecerror, != 0, "position 9, after turn\n", mcerr);
  }
  m_currpos.dirloc = m_currpos.dir;
  m_currpos.tid.up_absref(&m_currpos.dirloc);
  const double mean_speed = 0.5 * (m_prevpos.speed + m_currpos.speed);
  m_currpos.time = m_prevpos.time + m_currpos.prange / mean_speed; 
  check_consistency();
}
void mparticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "mparticle: mass=" << m_mass << " (" << m_mass / CLHEP::kg << " kg, "
          << m_mass * c_squared / CLHEP::GeV << " GeV)\n";
  Ifile << "orig_ekin=" << m_orig_ekin << " ("
        << m_orig_ekin / CLHEP::GeV << " GeV)"
        << " orig_gamma_1=" << m_orig_gamma_1 << '\n';
  Ifile << "prev_ekin=" << m_prev_ekin << " ("
        << m_prev_ekin / CLHEP::GeV << " GeV)"
        << " prev_gamma_1=" << m_prev_gamma_1 << '\n';
  Ifile << "curr_kin_energy=" << m_curr_ekin << " ("
        << m_curr_ekin / CLHEP::GeV << " GeV)"
        << " curr_gamma_1=" << m_curr_gamma_1 << '\n';
  gparticle::print(file, l);
}

std::ostream& operator<<(std::ostream& file, const mparticle& f) {
  (&f)->print(file, 10);
  return file;
}
}
