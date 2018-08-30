#include <iomanip>
#include <fstream>
#include <algorithm>
#include "wcpplib/clhep_units/WSystemOfUnits.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedMatterDef.h"

// 2003, I. Smirnov

namespace {

double integrate(const Heed::PointCoorMesh<double, const double*>& mesh,
                 const std::vector<double>& y,
                 double x1, double x2, const unsigned int xpower) {

  if (xpower > 1) return 0.;
  if (x1 >= x2) return 0.;
  const long qi = mesh.get_qi();
  if (qi < 1) return 0.;
  const double xmin = mesh.get_xmin();
  const double xmax = mesh.get_xmax();
  if (x2 <= xmin || x1 >= xmax) return 0.;

  double s = 0.;
  long ibeg = 0;
  if (x1 <= xmin) {
    x1 = xmin;
  } else {
    long n1, n2;
    double b1, b2;
    if (mesh.get_interval(x1, n1, b1, n2, b2) != 1) return 0.;
    if (b2 - x1 > 0) {
      if (x2 <= b2) {
        if (xpower == 0) {
          s = (x2 - x1) * y[n1];
        } else {
          s = 0.5 * (x2 * x2 - x1 * x1) * y[n1];
        }
        return s;
      }
      if (xpower == 0) {
        s += (b2 - x1) * y[n1];
     } else {
        s += 0.5 * (b2 * b2 - x1 * x1) * y[n1];
      }
    }
    ibeg = n2;
  }
  long iend = qi;
  if (x2 >= xmax) {
    x2 = xmax;
  } else {
    long n1, n2;
    double b1, b2;
    if (mesh.get_interval(x2, n1, b1, n2, b2) != 1) return 0.;
    if (x2 - b1 > 0) {
      if (xpower == 0) {
        s += (x2 - b1) * y[n1];
      } else {
        s += 0.5 * (x2 * x2 - b1 * b1) * y[n1];
      }
    }
    iend = n1;
  }
  double b;
  mesh.get_scoor(ibeg, b);
  for (long i = ibeg; i < iend; ++i) {
    const double a = b;
    mesh.get_scoor(i + 1, b);
    if (xpower == 0) {
      s += (b - a) * y[i];
    } else {
      s += 0.5 * (b * b - a * a) * y[i];
    }
  }
  return s;
}

double cdf(const Heed::PointCoorMesh<double, const double*>& mesh,
          const std::vector<double>& y, std::vector<double>& integ_y) {

  const long qi = mesh.get_qi();
  if (qi < 1) return 0.;

  double s = 0.0;
  double xp2 = 0.0;
  mesh.get_scoor(0, xp2);
  for (long n = 0; n < qi; n++) {
    const double xp1 = xp2;
    mesh.get_scoor(n + 1, xp2);
    const double step = xp2 - xp1;
    if (y[n] < 0.0) return 0.;
    s += y[n] * step;
    integ_y[n] = s;
  }
  for (long n = 0; n < qi; n++) integ_y[n] /= s;
  return s;
}

}

namespace Heed {

using CLHEP::twopi;
using CLHEP::electron_mass_c2;
using CLHEP::fine_structure_const;
using CLHEP::hbarc;
using CLHEP::cm;

EnTransfCS::EnTransfCS(double fparticle_mass, double fgamma_1,
                       bool fs_primary_electron, HeedMatterDef* fhmd,
                       long fparticle_charge)
    : particle_mass(fparticle_mass),
      particle_charge(fparticle_charge),
      gamma_1(fgamma_1),
      s_primary_electron(fs_primary_electron),
      hmd(fhmd) {
  mfunnamep("EnTransfCS::EnTransfCS(...)");

  const double beta = lorbeta(fgamma_1);
  const double beta2 = beta * beta;
  const double beta12 = 1.0 - beta2;
  const double gamma = fgamma_1 + 1.;
  // Particle kinetic energy.
  const double tkin = particle_mass * gamma_1;
  const double ener = particle_mass * gamma;
  // Calculate the max. energy transfer.
  if (s_primary_electron) {
    max_etransf = 0.5 * tkin;
  } else {
    double rm2 = particle_mass * particle_mass;
    double rme = electron_mass_c2;
    if (beta12 > 1.0e-10) {
      max_etransf =
          2.0 * rm2 * electron_mass_c2 * beta2 /
          ((rm2 + rme * rme + 2.0 * rme * gamma * particle_mass) * beta12);
      if (max_etransf > tkin) max_etransf = tkin;
    } else {
      max_etransf = tkin;
    }
  }
  const long qe = hmd->energy_mesh->get_q();
  log1C.assign(qe, 0.0);
  log2C.assign(qe, 0.0);
  chereC.assign(qe, 0.0);
  chereCangle.assign(qe, 0.0);
  Rruth.assign(qe, 0.0);
  addaC.assign(qe, 0.0);
#ifndef EXCLUDE_A_VALUES
  addaC_a.assign(qe, 0.0);
#endif

  const long qa = hmd->matter->qatom();
  cher.resize(qa);
  fruth.resize(qa);
  adda.resize(qa);
  fadda.resize(qa);
  quan.resize(qa);
  mean.resize(qa);
#ifndef EXCLUDE_A_VALUES
  cher_a.resize(qa);
  adda_a.resize(qa);
  fadda_a.resize(qa);
  quan_a.resize(qa);
  mean_a.resize(qa);
#endif

  for (long na = 0; na < qa; na++) {
    const long qs = hmd->apacs[na]->get_qshell();
    cher[na].assign(qs, std::vector<double>(qe, 0.));
    fruth[na].assign(qs, std::vector<double>(qe, 0.));
    adda[na].assign(qs, std::vector<double>(qe, 0.));
    fadda[na].assign(qs, std::vector<double>(qe, 0.));
    quan[na].assign(qs, 0.);
    mean[na].assign(qs, 0.);
#ifndef EXCLUDE_A_VALUES
    cher_a[na].assign(qs, std::vector<double>(qe, 0.));
    adda_a[na].assign(qs, std::vector<double>(qe, 0.));
    fadda_a[na].assign(qs, std::vector<double>(qe, 0.));
    quan_a[na].assign(qs, 0.);
    mean_a[na].assign(qs, 0.);
#endif
  }

  const long q2 = particle_charge * particle_charge;
  double coefpa = fine_structure_const * q2 / (beta2 * CLHEP::pi);
  const double coefCh = coefpa / hmd->eldens;
  for (long ne = 0; ne < qe; ne++) {
    const double eps1 = hmd->epsi1[ne];
    const double eps2 = hmd->epsi2[ne];
    const double a0 = 1. + eps1;
    const double a1 = -eps1 + a0 * beta12;
    const double a2 = beta2 * eps2;
    log1C[ne] = log(1. / sqrt(a1 * a1 + a2 * a2));

    const double ec = hmd->energy_mesh->get_ec(ne);
    const double a3 = 2. * electron_mass_c2 * beta2 / ec;
    log2C[ne] = a3 > 0. ? log(a3) : 0.;

    double a4 = atan(a2 / a1);
    if (a1 < 0) a4 += CLHEP::pi;
    chereCangle[ne] = a4;

    const double a5 = eps2 * eps2;
    const double a6 = (-a0 * a1 + beta2 * a5) / (a0 * a0 + a5);
    chereC[ne] = coefCh * a6 * a4;

    if (s_simple_form) {
      Rruth[ne] = 1. / (ec * ec);
      if (!s_primary_electron) {
        Rruth[ne] *= (1. - beta2 * ec / max_etransf);
      }
    } else {
      if (!s_primary_electron) {
        Rruth[ne] = 1. / (ec * ec) * (1. - beta2 * ec / max_etransf +
                                      ec * ec / (2. * ener * ener));
      } else {
        const double delta = ec / particle_mass;
        const double pg2 = gamma * gamma;
        const double dgd = delta * (gamma_1 - delta);
        Rruth[ne] = beta2 / (particle_mass * particle_mass) * 1.0 /
                    (pg2 - 1.0) * (gamma_1 * gamma_1 * pg2 / (dgd * dgd) -
                                   (2.0 * pg2 + 2.0 * gamma - 1.0) / dgd + 1.0);
      }
    }
  }

  double Z_mean = hmd->matter->Z_mean();
  const double ethr = hmd->min_ioniz_pot;
  for (long na = 0; na < qa; na++) {
    auto pacs = hmd->apacs[na];
    const double awq = hmd->matter->weight_quan(na);
    const long qs = pacs->get_qshell();
    for (long ns = 0; ns < qs; ns++) {
      for (long ne = 0; ne < qe; ne++) {
        double e1 = hmd->energy_mesh->get_e(ne);
        double e2 = hmd->energy_mesh->get_e(ne + 1);
        double ics = 0.;
        if (hmd->s_use_mixture_thresholds == 1) {
          ics = pacs->get_integral_TICS(ns, e1, e2, ethr) / (e2 - e1);
        } else {
          ics = pacs->get_integral_ICS(ns, e1, e2) / (e2 - e1);
        }
        check_econd11a(ics, < 0,
                       "na=" << na << " ns=" << ns << " ne=" << ne << '\n',
                       mcerr);
        const double tacs = hmd->ACS[ne];
        if (tacs <= 0.0) continue;
        cher[na][ns][ne] = chereC[ne] * awq * ics / tacs;
#ifndef EXCLUDE_A_VALUES
        double acs = pacs->get_integral_ACS(ns, e1, e2) / (e2 - e1);
        cher_a[na][ns][ne] = chereC[ne] * awq * acs / tacs;
#endif
      }
      // Calculate the integral.
      const double cR = C1_MEV2_MBN * awq * coefpa / Z_mean;
      double s = 0.;
      for (long ne = 0; ne < qe; ne++) {
        const double e1 = hmd->energy_mesh->get_e(ne);
        const double ec = hmd->energy_mesh->get_ec(ne);
        const double e2 = hmd->energy_mesh->get_e(ne + 1);
        const double r = pacs->get_integral_ACS(ns, e1, e2) * cR;
        // Here it must be ACS to satisfy sum rule for Rutherford
        check_econd11a(r, < 0.0, "na=" << na << " ns=" << ns << " ne=" << ne,
                       mcerr);
        if (ec > ethr && ec < max_etransf) {
          fruth[na][ns][ne] = (s + 0.5 * r) * Rruth[ne];
          check_econd11a(fruth[na][ns][ne], < 0,
                         "na=" << na << " ns=" << ns << " na=" << na, mcerr);
        } 
        s += r;
      }
    }
  }
  for (long ne = 0; ne < qe; ++ne) {
    double s = 0.0;
#ifndef EXCLUDE_A_VALUES
    double s_a = 0.0;
#endif
    double e1 = hmd->energy_mesh->get_e(ne);
    double ec = hmd->energy_mesh->get_ec(ne);
    double e2 = hmd->energy_mesh->get_e(ne + 1);
    const double eps1 = hmd->epsi1[ne];
    const double eps2 = hmd->epsi2[ne];
    const double eps11 = 1. + eps1;
    const double sqepsi = eps11 * eps11 + eps2 * eps2;
    const double cL = C1_MEV2_MBN * coefpa * (log1C[ne] + log2C[ne]) / 
                      (ec * Z_mean * sqepsi); 
    for (long na = 0; na < qa; na++) {
      double awq = hmd->matter->weight_quan(na);
      auto pacs = hmd->apacs[na];
      const long qs = pacs->get_qshell();
      for (long ns = 0; ns < qs; ns++) {
        double ics = 0.;
        if (hmd->s_use_mixture_thresholds == 1) {
          ics = pacs->get_integral_TICS(ns, e1, e2, ethr) / (e2 - e1);
        } else {
          ics = pacs->get_integral_ICS(ns, e1, e2) / (e2 - e1);
        }
        double r = std::max(cL * awq * ics + fruth[na][ns][ne], 0.);
#ifndef EXCLUDE_A_VALUES
        double acs = pacs->get_integral_ACS(ns, e1, e2) / (e2 - e1);
        double r_a = std::max(cL * awq * acs + fruth[na][ns][ne], 0.);
#endif
        if (ec > ethr) {
          r += cher[na][ns][ne];
          if (r < 0.0) {
            funnw.whdr(mcout);
            mcout << "negative adda\n";
            mcout << "na=" << na << " ns=" << ns << " ne=" << ne
                  << ": " << r << '\n';
            r = 0.;
          }
        }
#ifndef EXCLUDE_A_VALUES
        r_a += cher[na][ns][ne];
        check_econd11a(r_a, < 0,
                       "na=" << na << " ns=" << ns << " na=" << na, mcerr);
#endif
        adda[na][ns][ne] = r;
        s += r;
#ifndef EXCLUDE_A_VALUES
        adda_a[na][ns][ne] = r_a;
        s_a += r_a;
#endif
      }
    }
    addaC[ne] = s;
#ifndef EXCLUDE_A_VALUES
    addaC_a[ne] = s_a;
#endif
  }

  const double* aetemp = hmd->energy_mesh->get_ae();
  PointCoorMesh<double, const double*> pcm_e(qe + 1, &(aetemp));
  double emin = hmd->energy_mesh->get_emin();
  double emax = hmd->energy_mesh->get_emax();

  const double rho = hmd->xeldens;
  quanC = integrate(pcm_e, addaC, emin, emax, 0) * rho;
  meanC = integrate(pcm_e, addaC, emin, emax, 1) * rho;

#ifndef EXCLUDE_A_VALUES
  quanC_a = integrate(pcm_e, addaC_a, emin, emax, 0) * rho;
  meanC_a = integrate(pcm_e, addaC_a, emin, emax, 1) * rho;
#endif
  meanC1 = meanC;
  const double coef = fine_structure_const * fine_structure_const * q2 * twopi /
                      (electron_mass_c2 * beta2) * rho;
  if (s_simple_form) {
    if (!s_primary_electron) {
      if (max_etransf > hmd->energy_mesh->get_e(qe)) {
        double e1 = hmd->energy_mesh->get_e(qe);
        double e2 = max_etransf;
        meanC1 += coef * (log(e2 / e1) - beta2 / max_etransf * (e2 - e1));
      }
    } else {
      if (max_etransf > hmd->energy_mesh->get_e(qe)) {
        double e1 = hmd->energy_mesh->get_e(qe);
        double e2 = max_etransf;
        meanC1 += coef * log(e2 / e1);
      }
    }
  } else {
    if (!s_primary_electron) {
      if (max_etransf > hmd->energy_mesh->get_e(qe)) {
        double e1 = hmd->energy_mesh->get_e(qe);
        double e2 = max_etransf;
        meanC1 += coef *
                  (log(e2 / e1) - beta2 / max_etransf * (e2 - e1) +
                   (e2 * e2 - e1 * e1) / (4.0 * ener * ener));
      }
#ifndef EXCLUDE_A_VALUES
      meanC1_a = meanC_a;
      if (max_etransf > hmd->energy_mesh->get_e(qe)) {
        double e1 = hmd->energy_mesh->get_e(qe);
        double e2 = max_etransf;
        meanC1_a += coef * (log(e2 / e1) - beta2 / max_etransf * (e2 - e1) +
                            (e2 * e2 - e1 * e1) /
                                (4.0 * ener * ener));
      }
#endif
    }
  }

  for (long na = 0; na < qa; na++) {
    const long qs = hmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ns++) {
      quan[na][ns] = integrate(pcm_e, adda[na][ns], emin, emax, 0) * rho;
      mean[na][ns] = integrate(pcm_e, adda[na][ns], emin, emax, 1) * rho;
#ifndef EXCLUDE_A_VALUES
      quan_a[na][ns] = integrate(pcm_e, adda_a[na][ns], emin, emax, 0) * rho;
      mean_a[na][ns] = integrate(pcm_e, adda_a[na][ns], emin, emax, 1) * rho;
#endif
    }
  }

  for (long na = 0; na < qa; na++) {
    const long qs = hmd->apacs[na]->get_qshell();
    for (long ns = 0; ns < qs; ns++) {
      if (quan[na][ns] > 0.0) {
        const double s = cdf(pcm_e, adda[na][ns], fadda[na][ns]);
        if (fabs(s * rho - quan[na][ns]) > 1.e-10) {
          std::cerr << "Heed::EnTransfCS: Integrals differ (warning).\n";
        }
      }
#ifndef EXCLUDE_A_VALUES
      if (quan_a[na][ns] > 0.0) {
        const double s = cdf(pcm_e, adda_a[na][ns], fadda_a[na][ns]);
        if (fabs(s * rho - quan_a[na][ns]) > 1.e-10) {
          std::cerr << "Heed::EnTransfCS: Integrals differ (warning).\n";
        }
      }
#endif
    }
  }

  length_y0.resize(qe, 0.);
  for (long ne = 0; ne < qe; ne++) {
    const double k0 = hmd->energy_mesh->get_ec(ne) / (hbarc / cm);
    const double det_value = 1.0 / (gamma * gamma) - hmd->epsi1[ne] * beta2;
    length_y0[ne] = det_value > 0. ? beta / k0 * 1.0 / sqrt(det_value) : 0.;
  }

  /*
  std::ofstream dcsfile;
  dcsfile.open("dcs.txt", std::ios::out);
  dcsfile << "# energy [MeV] vs. diff. cs per electron [Mbarn / MeV]\n";
  for (int i = 0; i < qe; ++i) {
    double sumR = 0.;
    double sumC = 0.;
    double sumL = 0.; 
    for (long na = 0; na < qa; ++na) {
      const long qs = hmd->apacs[na]->get_qshell();
      for (long ns = 0; ns < qs; ++ns) {
        sumR += fruth[na][ns][i];
        sumC += cher[na][ns][i];
        sumL += adda[na][ns][i] - cher[na][ns][i] - fruth[na][ns][i];
      }
    }
    const double f1 = log1C[i] / (log1C[i] + log2C[i]);
    const double f2 = 1. - f1;
    sumR /= C1_MEV2_MBN;
    sumC /= C1_MEV2_MBN;
    sumL /= C1_MEV2_MBN;
    const double sumL1 = f1 * sumL;
    const double sumL2 = f2 * sumL;
    dcsfile << hmd->energy_mesh->get_ec(i) << "  " << addaC[i] / C1_MEV2_MBN
            << "  " << sumL1 << "  " << "  " << sumC << "  " << sumL2 << "  " << sumR 
            << "\n";
  }
  dcsfile.close();
  //*/

  log1C.clear();
  log2C.clear();
  chereC.clear();
  chereCangle.clear();
  Rruth.clear();

  addaC.clear();
  cher.clear();
  fruth.clear();
  adda.clear();
  mean.clear();
#ifndef EXCLUDE_A_VALUES
  addaC_a.clear();
  cher_a.clear();
  adda_a.clear();
  fadda_a.clear();
  mean_a.clear();
#endif
}

void EnTransfCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "EnTransfCS(l=" << l << "):\n";
  indn.n += 2;
  Ifile << "particle_mass=" << particle_mass
        << "particle_ener=" << particle_mass * (gamma_1 + 1.)
        << " particle_charge=" << particle_charge << std::endl;
  Ifile << "max_etransf=" << max_etransf << std::endl;
  Ifile << "s_primary_electron=" << s_primary_electron << std::endl;
  Ifile << "hmd:\n";
  hmd->print(file, 1);
#ifndef EXCLUDE_A_VALUES
  Ifile << "quanC=" << quanC << " quanC_a=" << quanC_a << '\n';
  Ifile << "meanC=" << meanC << " meanC_a=" << meanC_a << '\n';
  Ifile << "meanC1=" << meanC1 << " meanC1_a=" << meanC1_a << '\n';
#else
  Ifile << "quanC=" << quanC << '\n';
  Ifile << "meanC=" << meanC << '\n';
  Ifile << "meanC1=" << meanC1 << '\n';
#endif
  if (l > 2) {
    long qe = hmd->energy_mesh->get_q();
    long ne;
    if (l > 4) {
      Ifile << "       enerc,      log1C,      log2C,      chereC,     addaC, "
               "chereCangle   Rruth   length_y0\n";
      for (ne = 0; ne < qe; ne++) {
        Ifile << std::setw(12) << hmd->energy_mesh->get_ec(ne) << std::setw(12)
              << log1C[ne] << std::setw(12) << log2C[ne] << std::setw(12)
              << chereC[ne] << std::setw(12) << addaC[ne] << std::setw(12)
              << chereCangle[ne] << std::setw(12) << Rruth[ne]
              << std::setw(12) << length_y0[ne] << '\n';
      }
    }
    if (l > 3) {
      long qa = hmd->matter->qatom();
      long na;
      Iprintn(file, hmd->matter->qatom());
      for (na = 0; na < qa; na++) {
        Iprintn(file, na);
        long qs = hmd->apacs[na]->get_qshell();
        long ns;
        Iprintn(file, hmd->apacs[na]->get_qshell());
        for (ns = 0; ns < qs; ns++) {
          Iprintn(file, ns);
          Ifile << "quan      =" << std::setw(13) << quan[na][ns]
                << " mean  =" << std::setw(13) << mean[na][ns] << '\n';
#ifndef EXCLUDE_A_VALUES
          Ifile << "quan_a    =" << std::setw(13) << quan_a[na][ns]
                << " mean_a=" << std::setw(13) << mean_a[na][ns] << '\n';
#endif
          if (l > 5) {
            Ifile << "   enerc,        cher,       cher_a,     fruth,   adda, "
                     "  adda_a,  fadda,   fadda_a\n";
            for (ne = 0; ne < qe; ne++) {
              Ifile << std::setw(12)
                    << hmd->energy_mesh->get_ec(ne)
                    << std::setw(12) << cher[na][ns][ne]
#ifndef EXCLUDE_A_VALUES
                    << std::setw(12) << cher_a[na][ns][ne]
#endif
                    << std::setw(12) << fruth[na][ns][ne] << std::setw(12)
                    << adda[na][ns][ne]
#ifndef EXCLUDE_A_VALUES
                    << std::setw(12) << adda_a[na][ns][ne]
#endif
                    << std::setw(12) << fadda[na][ns][ne]
#ifndef EXCLUDE_A_VALUES
                    << std::setw(12) << fadda_a[na][ns][ne]
#endif
                    << '\n';
            }
          }
        }
      }
    }
  }
  indn.n -= 2;
}
}
