#include <iomanip>
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/ElElasticScat.h"
#include "wcpplib/ioniz/e_cont_enloss.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/util/FunNameStack.h"

// 2003, I. Smirnov

namespace Heed {

using CLHEP::twopi;
using CLHEP::degree;
using CLHEP::electron_mass_c2;
using CLHEP::eV;
using CLHEP::MeV;
using CLHEP::cm;
using CLHEP::cm3;
using CLHEP::gram;
using CLHEP::mole;
using CLHEP::Avogadro;

HeedDeltaElectronCS::HeedDeltaElectronCS(HeedMatterDef* fhmd,
                                         ElElasticScat* fees,
                                         ElElasticScatLowSigma* feesls,
                                         PairProd* fpairprod, int fsruth,
                                         double fmlambda, double fmthetac)
    : hmd(fhmd),
      ees(fees),
      eesls(feesls),
      pairprod(fpairprod),
      mlambda(fmlambda),
      sruth(fsruth),
      mthetac(fmthetac) {
  mfunname("HeedDeltaElectronCS::HeedDeltaElectronCS(...)");

  const long qe = hmd->energy_mesh->get_q();
  eLoss.resize(qe, 0.);
  beta.resize(qe, 0.);
  momentum.resize(qe, 0.);
  std::vector<double> beta2(qe, 0.);
  std::vector<double> momentum2(qe, 0.);

  const double rho = hmd->matter->density();
  const double zmean = hmd->matter->Z_mean();
  const double amean = hmd->matter->A_mean();
  const double ZA = zmean / amean;
  const double I_eff = 15.8 * eV * zmean;
  double smax = 0.0;
  for (long ne = 0; ne < qe; ne++) {
    const double ec = hmd->energy_mesh->get_ec(ne) * MeV;
    const double gamma_1 = ec / electron_mass_c2;
    beta[ne] = lorbeta(gamma_1);
    beta2[ne] = beta[ne] * beta[ne];
    const double en = electron_mass_c2 + ec;
    momentum2[ne] =
        (en * en - electron_mass_c2 * electron_mass_c2) / (MeV * MeV);
    momentum[ne] = sqrt(momentum2[ne]);
    const double dedx = e_cont_enloss(ZA, I_eff, rho, ec, DBL_MAX, -1);
    if (smax < dedx) smax = dedx;
    eLoss[ne] = dedx / (MeV / cm);
  }
  smax = smax / (MeV / cm);
  // looking for minimal (maximal by value but negative) derivative
  double deriv_min = 0.0;
  long n_deriv_min = 0;
  for (long ne = 1; ne < qe; ne++) {
    double d =
        (eLoss[ne] * beta[ne] - eLoss[ne - 1] * beta[ne - 1]) /
        (hmd->energy_mesh->get_ec(ne) - hmd->energy_mesh->get_ec(ne - 1));
    if (deriv_min > d) {
      deriv_min = d;
      n_deriv_min = ne - 1;
    }
  }
  for (long ne = 0; ne < n_deriv_min - 1; ne++) {
    eLoss[ne] = (eLoss[n_deriv_min - 1] * beta[n_deriv_min - 1] +
                 deriv_min * (hmd->energy_mesh->get_ec(ne) -
                              hmd->energy_mesh->get_ec(n_deriv_min - 1))) /
                beta[ne];
  }

  lambda.resize(qe);
  if (sruth == 2) {
    angular_mesh_c.resize(q_angular_mesh);
    angular_mesh_c[0] = 0.0;
    const double amin = 0.3;
    const double amax = 180.0;
    const double rk = pow(amax / amin, (1.0 / (q_angular_mesh - 2.)));
    double ar = amin;
    angular_mesh_c[1] = ar;
    for (long n = 2; n < q_angular_mesh; n++) {
      angular_mesh_c[n] = angular_mesh_c[n - 1] * rk;
    }
    angular_mesh_c[q_angular_mesh - 1] = 180.0;
    angular_points_ran.resize(qe);
    low_angular_points_ran.resize(qe);
    low_lambda.resize(qe);
    const long qes = eesls->get_ees()->get_qe();
#ifdef USE_MEAN_COEF
    mean_coef_low_sigma.resize(qes);
#else
    coef_low_sigma.resize(qes);
#endif
    for (long ne = 0; ne < qes; ne++) {
      long qat = hmd->matter->qatom();
      double s = 0.0;
      for (long nat = 0; nat < qat; nat++) {
#ifdef USE_MEAN_COEF
        s += eesls->get_mean_coef(hmd->matter->atom(nat)->Z(), ne) *
             hmd->matter->weight_quan(nat);
#else
        s += eesls->get_coef(hmd->matter->atom(nat)->Z(), ne) *
             hmd->matter->weight_quan(nat);
#endif
      }
#ifdef USE_MEAN_COEF
      mean_coef_low_sigma[ne] = s;
#else
      coef_low_sigma[ne] = s;
#endif
    }
  }

  for (long ne = 0; ne < qe; ne++) {
    double rr;
    double ek = hmd->energy_mesh->get_ec(ne) * 1000.0;
    if (ek <= 10.0) {
      rr = 1.0e-3 * amean / (gram / mole) / zmean * 3.872e-3 * pow(ek, 1.492);
    } else {
      rr = 1.0e-3 * 6.97e-3 * pow(ek, 1.6);
    }
    rr = rr / (rho / (gram / cm3));
    rr = rr * 0.1;
    // Iprintn(mcout, rr);
    double cor = 1.0;
    {
      // b-k*(x-a)**2 = 0  =>  x= a +- sqrt(b/k)
      // k = b / (x - a)**2
      double a = 2.5;
      double b = 4;
      // k=1.0/4.0
      double x = 0.0;
      double k = b / ((x - a) * (x - a));
      x = ek * 1000.0;
      double r = b - k * (x - a) * (x - a);
      if (r < 0.0)
        r = 1;
      else
        r = r + 1;
      cor = r;
    }
    if (sruth == 1) {
      lambda[ne] = mlambda / (rho / (gram / cm3));
      if (lambda[ne] < rr) lambda[ne] = rr;
      lambda[ne] = lambda[ne] * cor;
      // Calculate the minimum angle for restriction of field by atomic shell
      double mT = 2.0 * asin(1.0 / (2.0 * momentum[ne] * zmean * 5.07e2));
      // Throw out too slow interactions. They do not have any influence.
      if (mT < mthetac) mT = mthetac;
      // Calculate the cut angle due to mean free part
      double A = hmd->Rutherford_const / cor / (momentum2[ne] * beta2[ne]) /
                 pow(5.07e10, 2);
      double B = lambda[ne] * A;
      B = sqrt(B / (B + 1.0));
      // Threshold turn angle
      double thetac = 2.0 * asin(B);

      // If it too little, reset it. It will lead to increasing
      // of lambda and decreasing of calculation time.
      if (thetac < mT) {
        thetac = mT;
        B = mT;  // B is double precision
        double r = sin(0.5 * B);
        lambda[ne] = 1 / A * 2.0 * r * r / (1 + cos(B));
        // r=cos(TetacBdel(nen,nm))
        // lamBdel=A*(1.0+r)/(1.0-r)
        // lamBdel=1.0/lamBdel
        // lamBdel=(p2*bet2*sin(TetacBdel/2.0)**2) / A
      }
      // const double CosThetac12 = cos(0.5 * thetac);
      // const dobule SinThetac12 = sin(0.5 * thetac);
      // Flag that scattering is spherical.
      // const bool sisfera = thetac > 1.5 ? true : false;
    } else if (sruth == 0) {
      // Gauss formula
      const double msig = sqrt(2.0) * 13.6 / (beta[ne] * momentum[ne]);
      double x = mthetac / msig;
      x = x * x;
      x = x * hmd->radiation_length * cor;
      lambda[ne] = mlambda / (rho / (gram / cm3));
      if (lambda[ne] < rr) lambda[ne] = rr;
      lambda[ne] = lambda[ne] * cor;
      if (lambda[ne] < x) lambda[ne] = x;
    } else if (sruth == 2) {
      // Cross section for material per one av. atom, in cm^2/rad.
      std::vector<double> smat(q_angular_mesh, 0.);
      const double energy = hmd->energy_mesh->get_ec(ne);
      const long qat = hmd->matter->qatom();
      for (long nan = 0; nan < q_angular_mesh; nan++) {
        const double angle = angular_mesh_c[nan] * degree;
        double s = 0.0;
        for (long nat = 0; nat < qat; nat++) {
          const int z = hmd->matter->atom(nat)->Z();
          const double w = hmd->matter->weight_quan(nat);
          s += ees->get_CS(z, energy, angle) * w;
        }
        s = s * 1.0E-16;
        // s = s * 1.0E-16 * C1_MEV_CM * C1_MEV_CM;
        // Angstrem**2 -> cm**2
        // cm**2 -> MeV**-2
        smat[nan] = s * twopi * sin(angle);  // sr -> dtheta
      }
      angular_points_ran[ne] =
          PointsRan(angular_mesh_c, smat, low_cut_angle_deg, 180);
      low_angular_points_ran[ne] =
          PointsRan(angular_mesh_c, smat, 0., low_cut_angle_deg);
      const double coef =
          Avogadro * rho / (gram / cm3) / (amean / (gram / mole));
      lambda[ne] =
          1. / (angular_points_ran[ne].get_integ_active() * degree * coef);
      low_lambda[ne] =
          1. / (low_angular_points_ran[ne].get_integ_active() * degree * coef);
    }
  }
}

double HeedDeltaElectronCS::get_sigma(double energy, double nscat) const {
  mfunname("double HeedDeltaElectronCS::get_sigma(...)");
  check_econd11(nscat, < 0, mcerr);
  // check_econd21(nscat , < 0 || , > eesls->get_qscat() , mcerr);
  // ^ not compatible with Poisson
  const long qe = ees->get_qe();
  double energyKeV = energy * 1000.0;
  energyKeV = std::max(energyKeV, ees->get_energy_mesh(0));
  energyKeV = std::min(energyKeV, ees->get_energy_mesh(qe - 1));
  long n1 = 0;
  long n2 = qe - 1;
  while (n2 - n1 > 1) {
    const long n3 = n1 + (n2 - n1) / 2;
    if (energyKeV < ees->get_energy_mesh(n3))
      n2 = n3;
    else
      n1 = n3;
  }
  const double e1 = ees->get_energy_mesh(n1);
  const double e2 = ees->get_energy_mesh(n2);
#ifdef USE_MEAN_COEF
  const double v1 = nscat * mean_coef_low_sigma[n1];
  const double v2 = nscat * mean_coef_low_sigma[n2];
#else
  const double v1 = nscat * coef_low_sigma[n1];
  const double v2 = nscat * coef_low_sigma[n2];
#endif
  return v1 + (v2 - v1) / (e2 - e1) * (energyKeV - e1);
}

void HeedDeltaElectronCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "HeedDeltaElectronCS(l=" << l << "):";
  long qe = hmd->energy_mesh->get_q();
  // Iprintn(mcout, qe);
  // mcout<<std::endl;
  Iprintn(file, mlambda);
  Iprintn(file, mthetac);
  Iprintn(file, sruth);
  Ifile << "         get_ec,        beta,      momentum,    eLoss,    lambda,  "
           " low_lambda:" << std::endl;
  indn.n += 2;
  for (long ne = 0; ne < qe; ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << hmd->energy_mesh->get_ec(ne) << ' ' << std::setw(12) << beta[ne]
          << ' ' << std::setw(12) << momentum[ne] << ' ' << std::setw(12)
          << eLoss[ne] << ' ' << std::setw(12) << lambda[ne] << ' '
          << std::setw(12) << low_lambda[ne] << '\n';
  }
  indn.n -= 2;
  Ifile << "na, angular_mesh_c:" << std::endl;
  indn.n += 2;
  for (long na = 0; na < q_angular_mesh; na++) {
    Ifile << na << ' ' << std::setw(12) << angular_mesh_c[na] << '\n';
  }
  indn.n -= 2;
  Iprintn(file, eesls->get_ees()->get_qe());
  indn.n += 2;
#ifdef USE_MEAN_COEF
  Ifile << "ne, energy_mesh(ne), mean_coef_low_sigma:" << std::endl;
  for (long ne = 0; ne < eesls->get_ees()->get_qe(); ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << eesls->get_ees()->get_energy_mesh(ne) << " KeV " << std::setw(12)
          << mean_coef_low_sigma[ne] << '\n';
  }
#else
  Ifile << "ne, energy_mesh(ne), coef_low_sigma:" << std::endl;
  for (long ne = 0; ne < eesls->get_ees()->get_qe(); ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << eesls->get_ees()->get_energy_mesh(ne) << " KeV " << std::setw(12)
          << coef_low_sigma[ne] << '\n';
  }
#endif
  indn.n -= 2;
}
}
