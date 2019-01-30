#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/random/rnorm.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"

// 2003, I. Smirnov

#define USE_ADJUSTED_W
#define RANDOM_POIS
#define DIRECT_LOW_IF_LITTLE

namespace {

long findInterval(Heed::EnergyMesh* emesh, const double energy) {

  const long n = emesh->get_interval_number_between_centers(energy);
  return std::min(std::max(n, 0L), emesh->get_q() - 2);
}

double interpolate(Heed::EnergyMesh* emesh, const double x,
                   const std::vector<double>& y) {

  const long n = findInterval(emesh, x);
  const double x1 = emesh->get_ec(n);
  const double x2 = emesh->get_ec(n + 1);
  const double y1 = y[n];
  const double y2 = y[n + 1];
  return y1 + (x - x1) * (y2 - y1) / (x2 - x1);
}

}

namespace Heed {

using CLHEP::degree;
using CLHEP::cm;
using CLHEP::eV;
using CLHEP::keV;
using CLHEP::MeV;
using CLHEP::c_light;
using CLHEP::c_squared;

bool HeedDeltaElectron::s_low_mult_scattering = true;
bool HeedDeltaElectron::s_high_mult_scattering = true;

HeedDeltaElectron::HeedDeltaElectron(manip_absvol* primvol, const point& pt,
                                     const vec& vel, vfloat ftime,
                                     long fparent_particle_number,
                                     HeedFieldMap* fieldmap,
                                     bool fprint_listing)
    : eparticle(primvol, pt, vel, ftime, &electron_def, fieldmap),
      parent_particle_number(fparent_particle_number),
      m_particle_number(last_particle_number++),
      m_print_listing(fprint_listing) {
  mfunname("HeedDeltaElectron::HeedDeltaElectron(...)");
}

void HeedDeltaElectron::physics_mrange(double& fmrange) {
  mfunname("void HeedDeltaElectron::physics_mrange(double& fmrange)");
  if (m_print_listing) mcout << "HeedDeltaElectron::physics_mrange\n";

  m_mult_low_path_length = false;
  m_q_low_path_length = 0.0;
  m_path_length = false;
  if (fmrange <= 0.0) return;
  if (m_curr_ekin <= 0.0) {
    fmrange = 0.0;
    return;
  }
  // Get local volume and convert it to a cross-section object.
  const absvol* av = m_currpos.tid.G_lavol();
  auto hdecs = dynamic_cast<const HeedDeltaElectronCS*>(av);
  if (!hdecs) return;
  if (m_print_listing) Iprintnf(mcout, fmrange);
  const double ek = m_curr_ekin / MeV;
  // Get the dE/dx at this kinetic energy.
  EnergyMesh* emesh = hdecs->hmd->energy_mesh;
  const double dedx = interpolate(emesh, ek, hdecs->eLoss);
  // Min. loss 50 eV.
  double eloss = std::max(0.1 * ek, 0.00005);
  if (eloss > ek) {
    eloss = ek;
    m_stop_eloss = true;
  } else {
    m_stop_eloss = false;
  }
  fmrange = std::min(fmrange, (eloss / dedx) * cm);
  if (m_print_listing) Iprint2nf(mcout, fmrange, ek);
  const double ek_restr = std::max(ek, 0.0005);
  if (m_print_listing) Iprintnf(mcout, ek_restr / keV);

  double low_path_length = 0.;  // in internal units
  if (s_low_mult_scattering) {
    low_path_length = interpolate(emesh, ek_restr, hdecs->low_lambda) * cm;
    if (m_print_listing) Iprintnf(mcout, low_path_length / cm);
    long qscat = hdecs->eesls->get_qscat();
    const double sigma_ctheta = hdecs->get_sigma(ek_restr, qscat);
    // Reduce the number of scatterings, if the angle is too large.
    if (sigma_ctheta > 0.3) qscat = long(qscat * 0.3 / sigma_ctheta);
    const double mult_low_path_length = qscat * low_path_length;
    if (m_print_listing) Iprintnf(mcout, mult_low_path_length);
    if (fmrange > mult_low_path_length) {
      fmrange = mult_low_path_length;
      m_mult_low_path_length = true;
      m_q_low_path_length = hdecs->eesls->get_qscat();
      m_stop_eloss = false;
    } else {
      m_mult_low_path_length = false;
      m_q_low_path_length = fmrange / low_path_length;
    }
    if (m_print_listing) Iprint2nf(mcout, fmrange, m_q_low_path_length);
  }

  if (s_high_mult_scattering) {
    const double mean_path = interpolate(emesh, ek_restr, hdecs->lambda);
    if (m_print_listing) Iprintnf(mcout, mean_path);
    const double path_length = -mean_path * cm * log(1.0 - SRANLUX());
    if (m_print_listing) Iprintnf(mcout, path_length);
    if (fmrange > path_length) {
      fmrange = path_length;
      m_path_length = true;
      m_mult_low_path_length = true;
      if (s_low_mult_scattering) {
        m_q_low_path_length = fmrange / low_path_length;
        if (m_print_listing) Iprintnf(mcout, m_q_low_path_length);
      }
      m_stop_eloss = false;
    } else {
      m_path_length = false;
    }
    if (m_print_listing) Iprintnf(mcout, fmrange);
  }
  m_phys_mrange = fmrange;
}

void HeedDeltaElectron::physics_after_new_speed(
    std::vector<gparticle*>& /*secondaries*/) {
  mfunname("void HeedDeltaElectron::physics_after_new_speed()");
  if (m_print_listing) {
    mcout << "HeedDeltaElectron::physics_after_new_speed\n";
    Iprint2n(mcout, m_currpos.prange, m_curr_ekin);
  }
  check_econd11(vecerror, != 0, mcerr);
  if (m_currpos.prange <= 0.0) {
    if (m_curr_ekin <= 0.0) {
      // Get local volume.
      absvol* av = m_currpos.tid.G_lavol();
      if (av && av->s_sensitive && m_fieldMap->inside(m_currpos.ptloc)) {
        if (m_print_listing) mcout << "Convert to conduction electron.\n";
        conduction_electrons.emplace_back(
            HeedCondElectron(m_currpos.ptloc, m_currpos.time));
      }
      m_alive = false;
    }
    if (m_print_listing) mcout << "exit due to currpos.prange <= 0.0\n";
    return;
  }
  // Get local volume and convert it to a cross-section object.
  const absvol* av = m_currpos.tid.G_lavol();
  auto hdecs = dynamic_cast<const HeedDeltaElectronCS*>(av);
  if (!hdecs) return;
  double ek = m_curr_ekin / MeV;
  if (m_print_listing) {
    Iprintnf(mcout, ek);
    Iprint3n(mcout, m_stop_eloss, m_phys_mrange, m_currpos.prange);
  }
  // Calculate dE/dx and energy loss. Update the kinetic energy.
  double dedx;
  double Eloss = 0.;
  if (m_stop_eloss && m_phys_mrange == m_currpos.prange) {
    Eloss = m_curr_ekin;
    m_curr_ekin = 0.0;
    dedx = Eloss / m_currpos.prange / (MeV / cm);
  } else {
    EnergyMesh* emesh = hdecs->hmd->energy_mesh;
    dedx = interpolate(emesh, ek, hdecs->eLoss);
    Eloss = std::min(m_currpos.prange * dedx * MeV / cm, m_curr_ekin);
    m_total_eloss += Eloss;
    m_curr_ekin -= Eloss;
  }
  if (m_print_listing)
    Iprint3nf(mcout, m_prev_ekin / eV, m_curr_ekin / eV, Eloss / eV);
  if (m_curr_ekin <= 0.0) {
    if (m_print_listing) mcout << "m_curr_ekin <= 0.0\n";
    m_curr_ekin = 0.0;
    m_curr_gamma_1 = 0.0;
    m_currpos.speed = 0.0;
    m_alive = false;
  } else {
    const double resten = m_mass * c_squared;
    m_curr_gamma_1 = m_curr_ekin / resten;
    m_currpos.speed = c_light * lorbeta(m_curr_gamma_1);
  }
  absvol* vav = m_currpos.tid.G_lavol();
  if (vav && vav->s_sensitive) {
    if (m_print_listing) {
      mcout << "volume is sensitive\n";
      Iprint2nf(mcout, Eloss / eV, m_necessary_energy / eV);
    }
    if (Eloss > 0.0) ionisation(Eloss, dedx, hdecs->pairprod);
  }
  if (m_print_listing) {
    mcout << '\n';
    Iprintn(mcout, m_alive);
  }
  if (!m_alive) {
    // Done tracing the delta electron. Create the last conduction electron.
    vav = m_currpos.tid.G_lavol();
    if (vav && vav->s_sensitive && m_fieldMap->inside(m_currpos.ptloc)) {
      if (m_print_listing) mcout << "Last conduction electron\n";
      conduction_electrons.emplace_back(
          HeedCondElectron(m_currpos.ptloc, m_currpos.time));
    }
    return;
  }

  if (m_print_listing) mcout << "\nstart to rotate by low angle\n";
  double ek_restr = std::max(ek, 0.0005);
  if (m_print_listing) Iprint2nf(mcout, m_currpos.prange, m_phys_mrange);
  if (m_currpos.prange < m_phys_mrange) {
    // recalculate scatterings
    m_path_length = false;
    if (s_low_mult_scattering) {
      EnergyMesh* emesh = hdecs->hmd->energy_mesh;
      const double low_path_length = interpolate(emesh, ek_restr, hdecs->low_lambda) * cm;
      if (m_print_listing) Iprintnf(mcout, low_path_length / cm);
      m_mult_low_path_length = false;
      m_q_low_path_length = m_currpos.prange / low_path_length;
      if (m_print_listing) Iprintnf(mcout, m_q_low_path_length);
    }
  }
  if (m_print_listing) Iprintnf(mcout, m_q_low_path_length);
#ifdef RANDOM_POIS
  if (m_q_low_path_length > 0.0) {
    int ierror = 0;
    long random_q_low_path_length = pois(m_q_low_path_length, ierror);
    check_econd11a(ierror, == 1,
                   " q_low_path_length=" << m_q_low_path_length << '\n', mcerr);
    m_q_low_path_length = long(random_q_low_path_length);
    if (m_print_listing) {
      mcout << "After pois:\n";
      Iprintnf(mcout, m_q_low_path_length);
    }
  }
#endif
  if (m_q_low_path_length > 0) {
#ifdef DIRECT_LOW_IF_LITTLE
    const long max_q_low_path_length_for_direct = 5;
    if (m_q_low_path_length < max_q_low_path_length_for_direct) {
      // direct modeling
      if (m_print_listing) {
        mcout << "direct modeling of low scatterings\n";
        Iprint(mcout, m_currpos.dir);
      }
      EnergyMesh* emesh = hdecs->hmd->energy_mesh;
      const long n1r = findInterval(emesh, ek_restr);
      for (long nscat = 0; nscat < m_q_low_path_length; ++nscat) {
        if (m_print_listing) Iprintn(mcout, nscat);
        double theta_rot =
            hdecs->low_angular_points_ran[n1r].ran(SRANLUX()) * degree;
        if (m_print_listing) Iprintnf(mcout, theta_rot);
        vec dir = m_currpos.dir;
        basis temp(dir, "temp");
        vec vturn;
        vturn.random_round_vec();
        vturn = vturn * sin(theta_rot);
        vec new_dir(vturn.x, vturn.y, cos(theta_rot));
        new_dir.down(&temp);
        m_currpos.dir = new_dir;
        if (m_print_listing) Iprint(mcout, new_dir);
      }
      m_currpos.dirloc = m_currpos.dir;
      m_currpos.tid.up_absref(&m_currpos.dirloc);
    } else {
#endif
      double sigma_ctheta = hdecs->get_sigma(ek_restr, m_q_low_path_length);
      // actually it is mean(1-cos(theta)) or
      // sqrt( mean( square(1-cos(theta) ) ) ) depending on USE_MEAN_COEF

      if (m_print_listing) Iprintnf(mcout, sigma_ctheta);
      // Gauss (but actually exponential distribution fits better).
      // double ctheta = 1.0 - fabs(rnorm_improved() * sigma_ctheta);
      // Exponential:
      double ctheta = 0.0;
      {
#ifdef USE_MEAN_COEF
#else
      double sq2 = sqrt(2.0);
#endif
        do {
          double y = 0.0;
          do {  // in order to avoid SRANLUX() = 1
            y = SRANLUX();
            if (m_print_listing) Iprintnf(mcout, y);
          } while (y == 1.0);
#ifdef USE_MEAN_COEF
          double x = sigma_ctheta * (-log(1.0 - y));
#else
        double x = sigma_ctheta * 1.0 / sq2 * (-log(1.0 - y));
#endif
          ctheta = 1.0 - x;
          if (m_print_listing) Iprint2nf(mcout, x, ctheta);
        } while (ctheta <= -1.0);  // avoid absurd cos(theta)
        check_econd21(ctheta, < -1.0 ||, > 1.0, mcerr);
      }
      if (m_print_listing) Iprintnf(mcout, ctheta);
      double theta_rot = acos(ctheta);
      if (m_print_listing) Iprint2nf(mcout, theta_rot, theta_rot / degree);
      vec dir = m_currpos.dir;
      basis temp(dir, "temp");
      vec vturn;
      vturn.random_round_vec();
      vturn = vturn * sin(theta_rot);
      vec new_dir(vturn.x, vturn.y, cos(theta_rot));
      new_dir.down(&temp);
      m_currpos.dir = new_dir;
      m_currpos.dirloc = m_currpos.dir;
      m_currpos.tid.up_absref(&m_currpos.dirloc);
    }
#ifdef DIRECT_LOW_IF_LITTLE
  }
#endif
  if (m_path_length) {
    if (m_print_listing) {
      mcout << "\nstarting to rotate by large angle" << std::endl;
      Iprintnf(mcout, m_path_length);
    }
    EnergyMesh* emesh = hdecs->hmd->energy_mesh;
    const long n1r = findInterval(emesh, ek_restr);
    double theta_rot = hdecs->angular_points_ran[n1r].ran(SRANLUX()) * degree;
    if (m_print_listing) Iprintnf(mcout, theta_rot);
    vec dir = m_currpos.dir;
    basis temp(dir, "temp");
    vec vturn;
    vturn.random_round_vec();
    vturn = vturn * sin(theta_rot);
    vec new_dir(vturn.x, vturn.y, cos(theta_rot));
    new_dir.down(&temp);
    m_currpos.dir = new_dir;
    m_currpos.dirloc = m_currpos.dir;
    m_currpos.tid.up_absref(&m_currpos.dirloc);
  }
  if (m_print_listing) Iprint2nf(mcout, m_currpos.dir, m_currpos.dirloc);
}

void HeedDeltaElectron::ionisation(const double eloss, const double dedx,
                                   PairProd* pairprod) {

  if (eloss < m_necessary_energy) {
    m_necessary_energy -= eloss;
    return;
  }

  if (m_print_listing) mcout << "\nstart to leave conduction electrons\n";
  if (m_necessary_energy <= 0.0) {
#ifdef USE_ADJUSTED_W
    m_necessary_energy = pairprod->get_eloss(m_prev_ekin / eV) * eV;
#else
    m_necessary_energy = pairprod->get_eloss() * eV;
#endif
  }
  if (m_print_listing) Iprintnf(mcout, m_necessary_energy / eV);
  double eloss_left = eloss;
  point curpt = m_prevpos.pt;
  vec dir = m_prevpos.dir;  // this approximation ignores curvature
  double ekin = m_prev_ekin;
  if (m_print_listing) Iprintnf(mcout, curpt);
  while (eloss_left >= m_necessary_energy) {
    const double step_length = m_necessary_energy / (dedx * MeV / cm);
    if (m_print_listing) Iprintnf(mcout, step_length);
    curpt = curpt + dir * step_length;
    if (m_print_listing) Iprintf(mcout, curpt);
    point ptloc = curpt;
    m_prevpos.tid.up_absref(&ptloc);
    if (m_print_listing) mcout << "New conduction electron\n";
    if (m_fieldMap->inside(ptloc)) {
      conduction_electrons.emplace_back(HeedCondElectron(ptloc, m_currpos.time));
      conduction_ions.emplace_back(HeedCondElectron(ptloc, m_currpos.time));
    }
    eloss_left -= m_necessary_energy;
    ekin -= m_necessary_energy;
    if (ekin < 0.) break;
    // Generate next random energy
#ifdef USE_ADJUSTED_W
    m_necessary_energy = eV * pairprod->get_eloss(ekin / eV);
#else
    m_necessary_energy = pairprod->get_eloss() * eV;
#endif
    if (m_print_listing) {
      Iprintnf(mcout, eloss_left / eV);
      Iprint2nf(mcout, ekin / eV, m_necessary_energy / eV);
    }
  }
  m_necessary_energy -= eloss_left;
  if (m_print_listing) Iprintnf(mcout, m_necessary_energy / eV);
}

void HeedDeltaElectron::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedDeltaElectron (l=" << l
        << "): particle_number=" << m_particle_number << "\n";
  if (l == 1) return;
  indn.n += 2;
  Ifile << "s_low_mult_scattering=" << s_low_mult_scattering
        << " s_high_mult_scattering=" << s_high_mult_scattering << '\n';
  Ifile << "phys_mrange=" << m_phys_mrange << " stop_eloss=" << m_stop_eloss
        << " mult_low_path_length=" << m_mult_low_path_length << '\n';
  Ifile << "q_low_path_length=" << m_q_low_path_length
        << " path_length=" << m_path_length
        << " necessary_energy/eV=" << m_necessary_energy / eV << '\n';
  Ifile << " parent_particle_number=" << parent_particle_number << '\n';

  mparticle::print(file, l - 1);
  indn.n -= 2;
}
}
