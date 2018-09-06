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
                                     const vec& vel, vfloat time,
                                     long fparent_particle_number,
                                     HeedFieldMap* fieldmap,
                                     bool fs_print_listing)
    : eparticle(primvol, pt, vel, time, &electron_def, fieldmap),
      parent_particle_number(fparent_particle_number),
      particle_number(last_particle_number++),
      s_print_listing(fs_print_listing),
      phys_mrange(0.0),
      s_stop_eloss(false),
      s_mult_low_path_length(false),
      q_low_path_length(0.0),
      s_path_length(false),
      necessary_energy(0.0),
      total_Eloss(0.) {
  mfunname("HeedDeltaElectron::HeedDeltaElectron(...)");
}

void HeedDeltaElectron::physics_mrange(double& fmrange) {
  mfunname("void HeedDeltaElectron::physics_mrange(double& fmrange)");
  if (s_print_listing) mcout << "HeedDeltaElectron::physics_mrange\n";

  s_mult_low_path_length = false;
  q_low_path_length = 0.0;
  s_path_length = false;
  if (fmrange <= 0.0) return;
  if (curr_kin_energy <= 0.0) {
    fmrange = 0.0;
    return;
  }
  // Get local volume and convert it to a cross-section object.
  const absvol* av = currpos.tid.G_lavol();
  auto hdecs = dynamic_cast<const HeedDeltaElectronCS*>(av);
  if (!hdecs) return;
  if (s_print_listing) Iprintnf(mcout, fmrange);
  const double ek = curr_kin_energy / MeV;
  // Get the dE/dx at this kinetic energy.
  EnergyMesh* emesh = hdecs->hmd->energy_mesh;
  const double dedx = interpolate(emesh, ek, hdecs->eLoss);
  // Min. loss 50 eV.
  double eloss = std::max(0.1 * ek, 0.00005);
  if (eloss > ek) {
    eloss = ek;
    s_stop_eloss = true;
  } else {
    s_stop_eloss = false;
  }
  fmrange = std::min(fmrange, (eloss / dedx) * cm);
  if (s_print_listing) Iprint2nf(mcout, fmrange, ek);
  const double ek_restr = std::max(ek, 0.0005);
  if (s_print_listing) Iprintnf(mcout, ek_restr / keV);

  double low_path_length = 0.;  // in internal units
  if (s_low_mult_scattering) {
    low_path_length = interpolate(emesh, ek_restr, hdecs->low_lambda) * cm;
    if (s_print_listing) Iprintnf(mcout, low_path_length / cm);
    long qscat = hdecs->eesls->get_qscat();
    const double sigma_ctheta = hdecs->get_sigma(ek_restr, qscat);
    // Reduce the number of scatterings, if the angle is too large.
    if (sigma_ctheta > 0.3) qscat = long(qscat * 0.3 / sigma_ctheta);
    const double mult_low_path_length = qscat * low_path_length;
    if (s_print_listing) Iprintnf(mcout, mult_low_path_length);
    if (fmrange > mult_low_path_length) {
      fmrange = mult_low_path_length;
      s_mult_low_path_length = true;
      q_low_path_length = hdecs->eesls->get_qscat();
      s_stop_eloss = false;
    } else {
      s_mult_low_path_length = false;
      q_low_path_length = fmrange / low_path_length;
    }
    if (s_print_listing) Iprint2nf(mcout, fmrange, q_low_path_length);
  }

  if (s_high_mult_scattering) {
    const double mean_path = interpolate(emesh, ek_restr, hdecs->lambda);
    if (s_print_listing) Iprintnf(mcout, mean_path);
    const double path_length = -mean_path * cm * log(1.0 - SRANLUX());
    if (s_print_listing) Iprintnf(mcout, path_length);
    if (fmrange > path_length) {
      fmrange = path_length;
      s_path_length = true;
      s_mult_low_path_length = true;
      if (s_low_mult_scattering) {
        q_low_path_length = fmrange / low_path_length;
        if (s_print_listing) Iprintnf(mcout, q_low_path_length);
      }
      s_stop_eloss = false;
    } else {
      s_path_length = false;
    }
    if (s_print_listing) Iprintnf(mcout, fmrange);
  }
  phys_mrange = fmrange;
}

void HeedDeltaElectron::physics_after_new_speed(
    std::vector<gparticle*>& /*secondaries*/) {
  mfunname("void HeedDeltaElectron::physics_after_new_speed()");
  if (s_print_listing) {
    mcout << "HeedDeltaElectron::physics_after_new_speed\n";
    Iprint2n(mcout, currpos.prange, curr_kin_energy);
  }
  check_econd11(vecerror, != 0, mcerr);
  if (currpos.prange <= 0.0) {
    if (curr_kin_energy <= 0.0) {
      // Get local volume.
      absvol* av = currpos.tid.G_lavol();
      if (av && av->s_sensitive && m_fieldMap->inside(currpos.ptloc)) {
        if (s_print_listing) mcout << "Convert to conduction electron.\n";
        conduction_electrons.emplace_back(
            HeedCondElectron(currpos.ptloc, currpos.time));
      }
      s_life = false;
    }
    if (s_print_listing) mcout << "exit due to currpos.prange <= 0.0\n";
    return;
  }
  // Get local volume and convert it to a cross-section object.
  const absvol* av = currpos.tid.G_lavol();
  auto hdecs = dynamic_cast<const HeedDeltaElectronCS*>(av);
  if (!hdecs) return;
  double ek = curr_kin_energy / MeV;
  if (s_print_listing) {
    Iprintnf(mcout, ek);
    Iprint3n(mcout, s_stop_eloss, phys_mrange, currpos.prange);
  }
  // Calculate dE/dx and energy loss. Update the kinetic energy.
  double dedx;
  double Eloss = 0.;
  if (s_stop_eloss && phys_mrange == currpos.prange) {
    Eloss = curr_kin_energy;
    curr_kin_energy = 0.0;
    dedx = Eloss / currpos.prange / (MeV / cm);
  } else {
    EnergyMesh* emesh = hdecs->hmd->energy_mesh;
    dedx = interpolate(emesh, ek, hdecs->eLoss);
    Eloss = std::min(currpos.prange * dedx * MeV / cm, curr_kin_energy);
    total_Eloss += Eloss;
    curr_kin_energy -= Eloss;
  }
  if (s_print_listing)
    Iprint3nf(mcout, prev_kin_energy / eV, curr_kin_energy / eV, Eloss / eV);
  if (curr_kin_energy <= 0.0) {
    if (s_print_listing) mcout << "curr_kin_energy <= 0.0\n";
    curr_kin_energy = 0.0;
    curr_gamma_1 = 0.0;
    currpos.speed = 0.0;
    s_life = false;
  } else {
    const double resten = mass * c_squared;
    curr_gamma_1 = curr_kin_energy / resten;
    currpos.speed = c_light * lorbeta(curr_gamma_1);
  }
  absvol* vav = currpos.tid.G_lavol();
  if (vav && vav->s_sensitive) {
    if (s_print_listing) {
      mcout << "volume is sensitive\n";
      Iprint2nf(mcout, Eloss / eV, necessary_energy / eV);
    }
    if (Eloss > 0.0) ionisation(Eloss, dedx, hdecs->pairprod);
  }
  if (s_print_listing) {
    mcout << '\n';
    Iprintn(mcout, s_life);
  }
  if (!s_life) {
    // Done tracing the delta electron. Create the last conduction electron.
    vav = currpos.tid.G_lavol();
    if (vav && vav->s_sensitive && m_fieldMap->inside(currpos.ptloc)) {
      if (s_print_listing) mcout << "Last conduction electron\n";
      conduction_electrons.emplace_back(
          HeedCondElectron(currpos.ptloc, currpos.time));
    }
    return;
  }

  if (s_print_listing) mcout << "\nstart to rotate by low angle\n";
  double ek_restr = std::max(ek, 0.0005);
  if (s_print_listing) Iprint2nf(mcout, currpos.prange, phys_mrange);
  if (currpos.prange < phys_mrange) {
    // recalculate scatterings
    s_path_length = false;
    if (s_low_mult_scattering) {
      EnergyMesh* emesh = hdecs->hmd->energy_mesh;
      const double low_path_length = interpolate(emesh, ek_restr, hdecs->low_lambda) * cm;
      if (s_print_listing) Iprintnf(mcout, low_path_length / cm);
      s_mult_low_path_length = false;
      q_low_path_length = currpos.prange / low_path_length;
      if (s_print_listing) Iprintnf(mcout, q_low_path_length);
    }
  }
  if (s_print_listing) Iprintnf(mcout, q_low_path_length);
#ifdef RANDOM_POIS
  if (q_low_path_length > 0.0) {
    int ierror = 0;
    long random_q_low_path_length = pois(q_low_path_length, ierror);
    check_econd11a(ierror, == 1,
                   " q_low_path_length=" << q_low_path_length << '\n', mcerr);
    q_low_path_length = long(random_q_low_path_length);
    if (s_print_listing) {
      mcout << "After pois:\n";
      Iprintnf(mcout, q_low_path_length);
    }
  }
#endif
  if (q_low_path_length > 0) {
#ifdef DIRECT_LOW_IF_LITTLE
    const long max_q_low_path_length_for_direct = 5;
    if (q_low_path_length < max_q_low_path_length_for_direct) {
      // direct modeling
      if (s_print_listing) {
        mcout << "direct modeling of low scatterings\n";
        Iprint(mcout, currpos.dir);
      }
      EnergyMesh* emesh = hdecs->hmd->energy_mesh;
      const long n1r = findInterval(emesh, ek_restr);
      for (long nscat = 0; nscat < q_low_path_length; ++nscat) {
        if (s_print_listing) Iprintn(mcout, nscat);
        double theta_rot =
            hdecs->low_angular_points_ran[n1r].ran(SRANLUX()) * degree;
        if (s_print_listing) Iprintnf(mcout, theta_rot);
        vec dir = currpos.dir;
        basis temp(dir, "temp");
        vec vturn;
        vturn.random_round_vec();
        vturn = vturn * sin(theta_rot);
        vec new_dir(vturn.x, vturn.y, cos(theta_rot));
        new_dir.down(&temp);
        currpos.dir = new_dir;
        if (s_print_listing) Iprint(mcout, new_dir);
      }
      currpos.dirloc = currpos.dir;
      currpos.tid.up_absref(&currpos.dirloc);
    } else {
#endif
      double sigma_ctheta = hdecs->get_sigma(ek_restr, q_low_path_length);
      // actually it is mean(1-cos(theta)) or
      // sqrt( mean( square(1-cos(theta) ) ) ) depending on USE_MEAN_COEF

      if (s_print_listing) Iprintnf(mcout, sigma_ctheta);
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
            if (s_print_listing) Iprintnf(mcout, y);
          } while (y == 1.0);
#ifdef USE_MEAN_COEF
          double x = sigma_ctheta * (-log(1.0 - y));
#else
        double x = sigma_ctheta * 1.0 / sq2 * (-log(1.0 - y));
#endif
          ctheta = 1.0 - x;
          if (s_print_listing) Iprint2nf(mcout, x, ctheta);
        } while (ctheta <= -1.0);  // avoid absurd cos(theta)
        check_econd21(ctheta, < -1.0 ||, > 1.0, mcerr);
      }
      if (s_print_listing) Iprintnf(mcout, ctheta);
      double theta_rot = acos(ctheta);
      if (s_print_listing) Iprint2nf(mcout, theta_rot, theta_rot / degree);
      vec dir = currpos.dir;
      basis temp(dir, "temp");
      vec vturn;
      vturn.random_round_vec();
      vturn = vturn * sin(theta_rot);
      vec new_dir(vturn.x, vturn.y, cos(theta_rot));
      new_dir.down(&temp);
      currpos.dir = new_dir;
      currpos.dirloc = currpos.dir;
      currpos.tid.up_absref(&currpos.dirloc);
    }
#ifdef DIRECT_LOW_IF_LITTLE
  }
#endif
  if (s_path_length) {
    if (s_print_listing) {
      mcout << "\nstarting to rotate by large angle" << std::endl;
      Iprintnf(mcout, s_path_length);
    }
    EnergyMesh* emesh = hdecs->hmd->energy_mesh;
    const long n1r = findInterval(emesh, ek_restr);
    double theta_rot = hdecs->angular_points_ran[n1r].ran(SRANLUX()) * degree;
    if (s_print_listing) Iprintnf(mcout, theta_rot);
    vec dir = currpos.dir;
    basis temp(dir, "temp");
    vec vturn;
    vturn.random_round_vec();
    vturn = vturn * sin(theta_rot);
    vec new_dir(vturn.x, vturn.y, cos(theta_rot));
    new_dir.down(&temp);
    currpos.dir = new_dir;
    currpos.dirloc = currpos.dir;
    currpos.tid.up_absref(&currpos.dirloc);
  }
  if (s_print_listing) Iprint2nf(mcout, currpos.dir, currpos.dirloc);
}

void HeedDeltaElectron::ionisation(const double eloss, const double dedx,
                                   PairProd* pairprod) {

  if (eloss < necessary_energy) {
    necessary_energy -= eloss;
    return;
  }

  if (s_print_listing) mcout << "\nstart to leave conduction electrons\n";
  if (necessary_energy <= 0.0) {
#ifdef USE_ADJUSTED_W
    necessary_energy = pairprod->get_eloss(prev_kin_energy / eV) * eV;
#else
    necessary_energy = pairprod->get_eloss() * eV;
#endif
  }
  if (s_print_listing) Iprintnf(mcout, necessary_energy / eV);
  double eloss_left = eloss;
  point curpt = prevpos.pt;
  vec dir = prevpos.dir;  // this approximation ignores curvature
  double ekin = prev_kin_energy;
  if (s_print_listing) Iprintnf(mcout, curpt);
  while (eloss_left >= necessary_energy) {
    const double step_length = necessary_energy / (dedx * MeV / cm);
    if (s_print_listing) Iprintnf(mcout, step_length);
    curpt = curpt + dir * step_length;
    if (s_print_listing) Iprintf(mcout, curpt);
    point ptloc = curpt;
    prevpos.tid.up_absref(&ptloc);
    if (s_print_listing) mcout << "New conduction electron\n";
    if (m_fieldMap->inside(ptloc)) {
      conduction_electrons.emplace_back(HeedCondElectron(ptloc, currpos.time));
      conduction_ions.emplace_back(HeedCondElectron(ptloc, currpos.time));
    }
    eloss_left -= necessary_energy;
    ekin -= necessary_energy;
    if (ekin < 0.) break;
    // Generate next random energy
#ifdef USE_ADJUSTED_W
    necessary_energy = eV * pairprod->get_eloss(ekin / eV);
#else
    necessary_energy = pairprod->get_eloss() * eV;
#endif
    if (s_print_listing) {
      Iprintnf(mcout, eloss_left / eV);
      Iprint2nf(mcout, ekin / eV, necessary_energy / eV);
    }
  }
  necessary_energy -= eloss_left;
  if (s_print_listing) Iprintnf(mcout, necessary_energy / eV);
}

void HeedDeltaElectron::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "HeedDeltaElectron (l=" << l
        << "): particle_number=" << particle_number << "\n";
  if (l == 1) return;
  indn.n += 2;
  Ifile << "s_low_mult_scattering=" << s_low_mult_scattering
        << " s_high_mult_scattering=" << s_high_mult_scattering << '\n';
  Ifile << "phys_mrange=" << phys_mrange << " s_stop_eloss=" << s_stop_eloss
        << " s_mult_low_path_length=" << s_mult_low_path_length << '\n';
  Ifile << "q_low_path_length=" << q_low_path_length
        << " s_path_length=" << s_path_length
        << " necessary_energy/eV=" << necessary_energy / eV << '\n';
  Ifile << " parent_particle_number=" << parent_particle_number << '\n';

  mparticle::print(file, l - 1);
  indn.n -= 2;
}
}
