#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/random/chisran.h"
#include "wcpplib/random/pois.h"
#include "wcpplib/random/rnorm.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/HeedCondElectron.h"

/*
2003, I. Smirnov
*/

#define USE_ADJUSTED_W
#define RANDOM_POIS
#define DIRECT_LOW_IF_LITTLE

namespace Heed {

const long max_q_low_path_length_for_direct = 5;

int HeedDeltaElectron::s_low_mult_scattering = 1;
int HeedDeltaElectron::s_high_mult_scattering = 1;

HeedDeltaElectron::HeedDeltaElectron(manip_absvol* primvol, const point& pt,
                                     const vec& vel, vfloat time,
                                     long fparent_particle_number,
                                     HeedFieldMap* fieldmap,
                                     int fs_print_listing)
    : eparticle(primvol, pt, vel, time, &electron_def, fieldmap),
      parent_particle_number(fparent_particle_number),
      particle_number(last_particle_number++),
      s_print_listing(fs_print_listing),
      phys_mrange(0.0),
      s_stop_eloss(0),
      s_mult_low_path_length(0),
      q_low_path_length(0.0),
      s_path_length(0),
      necessary_energy(0.0),
      total_Eloss(0.) {
  mfunname("HeedDeltaElectron::HeedDeltaElectron(...)");

}

void HeedDeltaElectron::physics_mrange(double& fmrange) {
  mfunname("void HeedDeltaElectron::physics_mrange(double& fmrange)");
  if (s_print_listing == 1) {
    mcout << "void HeedDeltaElectron::physics_mrange(double& fmrange)"
          << std::endl;
  }
  s_mult_low_path_length = 0;
  q_low_path_length = 0.0;
  s_path_length = 0;
  if (fmrange <= 0.0) return;
  if (curr_kin_energy == 0.0) {
    fmrange = 0.0;
    return;
  }
  const absvol* av = currpos.G_lavol();  // get least address of volume
  const HeedDeltaElectronCSType* hmecst =
      dynamic_cast<const HeedDeltaElectronCSType*>(av);
  if (hmecst == NULL) return;
  if (s_print_listing == 1) Iprintnf(mcout, fmrange);
  // calculate eloss and mrange as follows from eloss
  HeedDeltaElectronCS* hdecs = hmecst->hdecs.getver();
  double ek = curr_kin_energy / MeV;
  EnergyMesh* emesh = hdecs->hmd->energy_mesh.get();
  long n1 = emesh->get_interval_number_between_centers(ek);
  long qener = emesh->get_q();
  if (n1 < 0) n1 = 0;
  if (n1 > qener - 2) n1 = qener - 2;
  long n2 = n1 + 1;
  double dedx = hdecs->eLoss[n1] + (hdecs->eLoss[n2] - hdecs->eLoss[n1]) /
                                       (emesh->get_ec(n2) - emesh->get_ec(n1)) *
                                       (ek - emesh->get_ec(n1));
  //double ek_reduced = ek * 0.9;
  double eloss = 0.1 * ek;               // MeV
  if (eloss < 0.00005) eloss = 0.00005;  // loss by 50eV
  if (eloss > ek) {
    eloss = ek;
    s_stop_eloss = 1;
  } else {
    s_stop_eloss = 0;
  }
  double mrange = (eloss / dedx) * cm;
  if (fmrange > mrange) fmrange = mrange;
  if (s_print_listing == 1) {
    Iprintnf(mcout, fmrange);
    Iprintnf(mcout, ek);
  }
  double ek_restricted = ek;
  if (ek_restricted < 0.0005) ek_restricted = 0.0005;
  if (s_print_listing == 1) Iprintnf(mcout, ek_restricted / keV);

  long n1r = emesh->get_interval_number_between_centers(ek_restricted);
  if (n1r < 0) n1r = 0;
  if (n1r > qener - 2) n1r = qener - 2;
  long n2r = n1r + 1;
  double low_path_length = 0.;  // in internal units
  if (s_low_mult_scattering == 1) {
    low_path_length = (hdecs->low_lambda[n1r] +
                       (hdecs->low_lambda[n2r] - hdecs->low_lambda[n1r]) /
                           (emesh->get_ec(n2r) - emesh->get_ec(n1r)) *
                           (ek_restricted - emesh->get_ec(n1r))) * cm;
    if (s_print_listing == 1) Iprintnf(mcout, low_path_length / cm);
    long qscat = hdecs->eesls->get_qscat();
    double sigma_ctheta = hdecs->get_sigma(ek_restricted, qscat);
    // Reduce the number of scatterings, if the angle is too large.
    if (sigma_ctheta > 0.3) qscat = long(qscat * 0.3 / sigma_ctheta);
    double mult_low_path_length = qscat * low_path_length;
    if (s_print_listing == 1) Iprintnf(mcout, mult_low_path_length);
    if (fmrange > mult_low_path_length) {
      fmrange = mult_low_path_length;
      s_mult_low_path_length = 1;
      q_low_path_length = hdecs->eesls->get_qscat();
      s_stop_eloss = 0;
    } else {
      s_mult_low_path_length = 0;
      q_low_path_length = fmrange / low_path_length;
    }
    if (s_print_listing == 1) {
      Iprintnf(mcout, fmrange);
      Iprintnf(mcout, q_low_path_length);
    }
  }

  if (s_high_mult_scattering == 1) {
    if (s_print_listing == 1) {
      Iprintf(mcout, currpos.pt);
      Iprintnf(mcout, n1r);
      Iprintnf(mcout, n2r);
      Iprintnf(mcout, ek_restricted);
      Iprintnf(mcout, emesh->get_ec(n1r));
      Iprintnf(mcout, emesh->get_ec(n2r));
    }
    double mean_path_length =
        (hdecs->lambda[n1r] + (hdecs->lambda[n2r] - hdecs->lambda[n1r]) /
                                  (emesh->get_ec(n2r) - emesh->get_ec(n1r)) *
                                  (ek_restricted - emesh->get_ec(n1r))) * cm;
    if (s_print_listing == 1) {
      Iprintnf(mcout, mean_path_length);
      Iprintnf(mcout, mean_path_length / cm);
    }
    double path_length = -mean_path_length * log(1.0 - SRANLUX());
    if (s_print_listing == 1) Iprintnf(mcout, path_length);
    if (fmrange > path_length) {
      fmrange = path_length;
      s_path_length = 1;
      s_mult_low_path_length = 0;
      if (s_low_mult_scattering == 1) {
        q_low_path_length = fmrange / low_path_length;
        if (s_print_listing == 1) Iprintnf(mcout, q_low_path_length);
      }
      s_stop_eloss = 0;
    } else {
      s_path_length = 0;
    }
    if (s_print_listing == 1) Iprintnf(mcout, fmrange);
  }
  phys_mrange = fmrange;
}

void HeedDeltaElectron::physics_after_new_speed(void) {
  mfunname("void HeedDeltaElectron::physics_after_new_speed(void)");
  if (s_print_listing == 1) {
    mcout << "HeedDeltaElectron::physics_after_new_speed is started\n";
    Iprint2n(mcout, currpos.prange, curr_kin_energy);
  }
  check_econd11(vecerror, != 0, mcerr);
  if (currpos.prange <= 0.0) {
    if (curr_kin_energy == 0.0) {
      // Get least address of volume.
      absvol* av = currpos.G_lavol();
      SensitiveVolume* asv = dynamic_cast<SensitiveVolume*>(av);
      if (asv != NULL) {
        if (s_print_listing == 1) {
          mcout << "HeedDeltaElectron::physics_after_new_speed: \n";
          mcout << "This is converted to conduction\n";
        }
        // TODO: replace push_back by emplace_back.
        asv->conduction_electron_bank.push_back(
            HeedCondElectron(currpos.ptloc, currpos.time));
      }
      s_life = 0;
    }
    if (s_print_listing == 1) mcout << "exit due to currpos.prange <= 0.0\n";
    return;
  }
  // Get least address of volume
  const absvol* av = currpos.G_lavol();
  const HeedDeltaElectronCSType* hmecst =
      dynamic_cast<const HeedDeltaElectronCSType*>(av);
  if (s_print_listing == 1)
    mcout << "physics_after_new_speed: started" << std::endl;
  if (hmecst == NULL) return;
  HeedDeltaElectronCS* hdecs = hmecst->hdecs.get();
  double ek = curr_kin_energy / MeV;
  EnergyMesh* emesh = hdecs->hmd->energy_mesh.get();
  long n1 = emesh->get_interval_number_between_centers(ek);
  long qener = emesh->get_q();
  if (n1 < 0) n1 = 0;
  if (n1 > qener - 2) n1 = qener - 2;
  long n2 = n1 + 1;
  if (s_print_listing == 1) {
    Iprintnf(mcout, ek);
    Iprint2n(mcout, n1, n2);
    Iprint3n(mcout, s_stop_eloss, phys_mrange, currpos.prange);
  }
  /*
  double dedx = hdecs->eLoss[n1] +
    (hdecs->eLoss[n2] - hdecs->eLoss[n1])/
    (emesh->get_ec(n2) - emesh->get_ec(n1)) *
    (ek - emesh->get_ec(n1));
  double Eloss = dedx * MeV/cm;
  Eloss *= currpos.prange;
  if(s_print_listing == 1) Iprintn(mcout, Eloss/eV);
  total_Eloss += Eloss;
  curr_kin_energy -= Eloss;
  */
  double dedx;
  double Eloss;
  if (s_stop_eloss == 1 && phys_mrange == currpos.prange) {
    Eloss = curr_kin_energy;
    curr_kin_energy = 0.0;
    dedx = Eloss / currpos.prange / (MeV / cm);
  } else {
    dedx = hdecs->eLoss[n1] + (hdecs->eLoss[n2] - hdecs->eLoss[n1]) /
                                  (emesh->get_ec(n2) - emesh->get_ec(n1)) *
                                  (ek - emesh->get_ec(n1));
    Eloss = dedx * MeV / cm;
    Eloss *= currpos.prange;
    total_Eloss += Eloss;
    curr_kin_energy -= Eloss;
  }
  if (s_print_listing == 1)
    Iprint3nf(mcout, prev_kin_energy / eV, curr_kin_energy / eV, Eloss / eV);
  if (curr_kin_energy <= 0.0) {
    if (s_print_listing == 1) {
      mcout << "curr_kin_energy <= 0.0, curr_kin_energy=" << curr_kin_energy
            << " curr_kin_energy/MeV=" << curr_kin_energy / MeV << '\n';
    }
    curr_kin_energy = 0.0;
    curr_gamma_1 = 0.0;
    currpos.speed = 0.0;
    s_life = 0;
  } else {
    double resten = mass * c_squared;
    curr_gamma_1 = curr_kin_energy / resten;
    currpos.speed = c_light * lorbeta(curr_gamma_1);
  }
  absvol* vav = currpos.G_lavol();
  SensitiveVolume* asv = dynamic_cast<SensitiveVolume*>(vav);
  if (asv != NULL) {
    if (s_print_listing == 1) {
      mcout << "volume is sensitive\n";
      Iprintnf(mcout, Eloss / eV);
      Iprintnf(mcout, necessary_energy / eV);
    }
    if (Eloss > 0.0) {
      if (Eloss >= necessary_energy) {
        // can leave electrons
        // there is no need to recalculate mean energy loss per 1 cm,
        // since necessary_energy is not an addition
        if (s_print_listing == 1) {
          mcout << "\nstart to leave conduction electrons" << std::endl;
          //Iprintnf(mcout, Eloss/eV);
          //Iprintnf(mcout, necessary_energy/eV);
          Iprintnf(mcout, dedx);
        }
        if (necessary_energy <= 0.0) {
#ifdef USE_ADJUSTED_W
          necessary_energy =
              hdecs->pairprod->get_eloss(prev_kin_energy / eV) * eV;
#else
          necessary_energy = hdecs->pairprod->get_eloss() * eV;
#endif
        }
        if (s_print_listing == 1) Iprintnf(mcout, necessary_energy / eV);
        double Eloss_left = Eloss;
        point curpt = prevpos.pt;
        vec dir = prevpos.dir;  // this approximation ignores curvature
        double curr_kin_energy_for_cond = prev_kin_energy;
        if (s_print_listing == 1) Iprintnf(mcout, curpt);
        // then at each step necessary_energy is energy due to expend till
        // next conduction electron
        while (Eloss_left >= necessary_energy) {
          // this condition provides
          // also that the current electron energy is non negative
          double step_length = necessary_energy / (dedx * MeV / cm);
          if (s_print_listing == 1) Iprintnf(mcout, step_length);
          curpt = curpt + dir * step_length;
          if (s_print_listing == 1) Iprintf(mcout, curpt);
          point ptloc = curpt;
          prevpos.tid.up_absref(&ptloc);
          if (s_print_listing == 1) mcout << "New conduction electron\n";
          // TODO: replace push_back by emplace_back.
          asv->conduction_electron_bank.push_back(
              HeedCondElectron(ptloc, currpos.time));
          Eloss_left -= necessary_energy;
          curr_kin_energy_for_cond -= necessary_energy;
// generate next random energy
#ifdef USE_ADJUSTED_W
          necessary_energy =
              eV * hdecs->pairprod->get_eloss(curr_kin_energy_for_cond / eV);
#else
          necessary_energy = hdecs->pairprod->get_eloss() * eV;
#endif
          if (s_print_listing == 1) {
            Iprintnf(mcout, Eloss_left / eV);
            Iprint2nf(mcout, curr_kin_energy_for_cond / eV,
                      necessary_energy / eV);
          }
        }
        necessary_energy -= Eloss_left;
        if (s_print_listing == 1) Iprintnf(mcout, necessary_energy / eV);
      } else {
        necessary_energy -= Eloss;
      }
    }
  }
  if (s_print_listing == 1) {
    mcout << '\n';
    Iprintn(mcout, s_life);
  }
  if (s_life == 1) {
    if (s_print_listing == 1) mcout << "\nstart to rotate by low angle\n";
    double ek_restricted = ek;
    if (ek_restricted < 0.0005) ek_restricted = 0.0005;
    if (s_print_listing == 1) {
      Iprintnf(mcout, currpos.prange);
      Iprintnf(mcout, phys_mrange);
    }
    if (currpos.prange < phys_mrange) {
      // recalculate scatterings
      s_path_length = 0;
      if (s_low_mult_scattering == 1) {
        long n1r = emesh->get_interval_number_between_centers(ek_restricted);
        if (n1r < 0) n1r = 0;
        if (n1r > qener - 2) n1r = qener - 2;
        long n2r = n1r + 1;
        double low_path_length;  // in internal units
        if (s_low_mult_scattering == 1) {
          low_path_length = (hdecs->low_lambda[n1r] +
                             (hdecs->low_lambda[n2r] - hdecs->low_lambda[n1r]) /
                                 (emesh->get_ec(n2r) - emesh->get_ec(n1r)) *
                                 (ek_restricted - emesh->get_ec(n1r))) * cm;
          if (s_print_listing == 1) Iprintnf(mcout, low_path_length / cm);
          s_mult_low_path_length = 0;
          q_low_path_length = currpos.prange / low_path_length;
          if (s_print_listing == 1) Iprintnf(mcout, q_low_path_length);
        }
      }
    }
    if (s_print_listing == 1) Iprintnf(mcout, q_low_path_length);
#ifdef RANDOM_POIS
    long random_q_low_path_length = 0;
    if (q_low_path_length > 0.0) {
      int ierror = 0;
      random_q_low_path_length = pois(q_low_path_length, ierror);
      check_econd11a(ierror, == 1,
                     " q_low_path_length=" << q_low_path_length << '\n', mcerr);
      q_low_path_length = long(random_q_low_path_length);
      if (s_print_listing == 1) {
        mcout << "After pois:\n";
        Iprintnf(mcout, q_low_path_length);
      }
    }
#endif
    if (q_low_path_length > 0) {
#ifdef DIRECT_LOW_IF_LITTLE
      if (q_low_path_length < max_q_low_path_length_for_direct) {
        // direct modeling
        if (s_print_listing == 1) {
          mcout << "direct modeling of low scatterings\n";
          Iprint(mcout, currpos.dir);
        }
        long n1r = emesh->get_interval_number_between_centers(ek_restricted);
        if (n1r < 0) n1r = 0;
        if (n1r > qener - 1) n1r = qener - 1;
        for (long nscat = 0; nscat < q_low_path_length; ++nscat) {
          if (s_print_listing == 1) Iprintn(mcout, nscat);
          double theta_rot =
              hdecs->low_angular_points_ran[n1r].ran(SRANLUX()) / 180.0 * M_PI;
          if (s_print_listing == 1)
            Iprint2nf(mcout, theta_rot, theta_rot / M_PI * 180.0);
          vec dir = currpos.dir;
          //Iprint(mcout, dir);
          basis temp(dir, "temp");
          vec vturn;
          vturn.random_round_vec();
          vturn = vturn * sin(theta_rot);
          vec new_dir(vturn.x, vturn.y, cos(theta_rot));
          new_dir.down(&temp);
          currpos.dir = new_dir;
          if (s_print_listing == 1) Iprint(mcout, new_dir);
        }
        currpos.dirloc = currpos.dir;
        currpos.tid.up_absref(&currpos.dirloc);
      } else {
#endif
        double sigma_ctheta =
            hdecs->get_sigma(ek_restricted, q_low_path_length);
        // actually it is mean(1-cos(theta)) or
        // sqrt( mean( square(1-cos(theta) ) ) ) depending on USE_MEAN_COEF

        if (s_print_listing == 1) Iprintnf(mcout, sigma_ctheta);
        /*
        This is for Gauss.
        But actually exponential distribution fits better.
        float r1 = SRANLUX();
        float r2 = SRANLUX();
        float x1, x2;
        rnorm(r1, r2, x1, x2);
        //Iprintn(mcout, x1);
        double ctheta = 1.0 - fabs(x1 * sigma_ctheta);
        // By the way,
        // it seems that there were no condition > -1.0, this is error.
        */
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
              if (s_print_listing == 1) Iprintnf(mcout, y);
            } while (y == 1.0);
#ifdef USE_MEAN_COEF
            double x = sigma_ctheta * (-log(1.0 - y));
#else
            double x = sigma_ctheta * 1.0 / sq2 * (-log(1.0 - y));
#endif
            ctheta = 1.0 - x;
            if (s_print_listing == 1) Iprint2nf(mcout, x, ctheta);
          } while (ctheta <= -1.0);  // avoid absurd cos(theta)
          check_econd21(ctheta, < -1.0 ||, > 1.0, mcerr);
        }
        if (s_print_listing == 1) Iprintnf(mcout, ctheta);
        double theta_rot = acos(ctheta);
        if (s_print_listing == 1) {
          Iprint2nf(mcout, theta_rot, theta_rot / M_PI * 180.0);
        }
        vec dir = currpos.dir;
        //Iprint(mcout, dir);
        basis temp(dir, "temp");
        //long n1r = emesh->get_interval_number_between_centers(ek_restricted);
        //double theta_rot = angular_points_ran[nr1].ran(SRANLUX());
        //Iprintn(mcout, theta_rot);
        //double phi = 2.0 * M_PI * SRANLUX();
        vec vturn;
        vturn.random_round_vec();
        vturn = vturn * sin(theta_rot);
        vec new_dir(vturn.x, vturn.y, cos(theta_rot));
        new_dir.down(&temp);
        currpos.dir = new_dir;
        //Iprint(mcout, new_dir);
        currpos.dirloc = currpos.dir;
        currpos.tid.up_absref(&currpos.dirloc);
      }
#ifdef DIRECT_LOW_IF_LITTLE
    }
#endif
    if (s_path_length == 1) {
      if (s_print_listing == 1) {
        mcout << "\nstarting to rotate by large angle" << std::endl;
        Iprintnf(mcout, s_path_length);
      }
      long n1r = emesh->get_interval_number_between_centers(ek_restricted);
      if (n1r < 0) n1r = 0;
      if (n1r > qener - 1) n1r = qener - 1;
      double theta_rot =
          hdecs->angular_points_ran[n1r].ran(SRANLUX()) / 180.0 * M_PI;
      if (s_print_listing == 1) Iprintnf(mcout, theta_rot);
      vec dir = currpos.dir;
      //Iprint(mcout, dir);
      basis temp(dir, "temp");
      vec vturn;
      vturn.random_round_vec();
      vturn = vturn * sin(theta_rot);
      vec new_dir(vturn.x, vturn.y, cos(theta_rot));
      new_dir.down(&temp);
      currpos.dir = new_dir;
      //Iprint(mcout, new_dir);
      currpos.dirloc = currpos.dir;
      currpos.tid.up_absref(&currpos.dirloc);
    }
  } else {
    // no need to scater
    absvol* vav = currpos.G_lavol();  // get least address of volume
    SensitiveVolume* asv = dynamic_cast<SensitiveVolume*>(vav);
    if (asv != NULL) {
      if (s_print_listing == 1) mcout << "Last conduction electron\n";
      // TODO: replace push_back by emplace_back.
      asv->conduction_electron_bank.push_back(
          HeedCondElectron(currpos.ptloc, currpos.time));
    }
  }
  if (s_print_listing == 1) {
    Iprintf(mcout, currpos.dir);
    Iprintf(mcout, currpos.dirloc);
  }
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
