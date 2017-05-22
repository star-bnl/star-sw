#include <iomanip>
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/ElElasticScat.h"
#include "wcpplib/ioniz/e_cont_enloss.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/math/lorgamma.h"
/*
2003, I. Smirnov
*/

namespace Heed {

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
  //mcout<<"starting to inite HeedDeltaElectronCS\n";
  long qe = hmd->energy_mesh->get_q();
  long ne;
  eLoss = DynLinArr<double>(qe);
  beta = DynLinArr<double>(qe);
  beta2 = DynLinArr<double>(qe);
  momentum = DynLinArr<double>(qe);
  momentum2 = DynLinArr<double>(qe);
  double smax = 0.0;
  for (ne = 0; ne < qe; ne++) {
    double ec = hmd->energy_mesh->get_ec(ne);
    double gamma_1 = ec * MeV / electron_mass_c2;
    beta[ne] = lorbeta(gamma_1);
    beta2[ne] = beta[ne] * beta[ne];
    momentum2[ne] = (pow(electron_mass_c2 + ec * MeV, 2.0) -
                     electron_mass_c2 * electron_mass_c2) / (MeV * MeV);
    momentum[ne] = sqrt(momentum2[ne]);
    double ZA = hmd->matter->Z_mean() / hmd->matter->A_mean();
    double I_eff = 15.8 * eV * hmd->matter->Z_mean();
    double dedx =
        e_cont_enloss(ZA, I_eff, hmd->matter->density(), ec * MeV, DBL_MAX, -1);
    if (smax < dedx) smax = dedx;
    eLoss[ne] = dedx / (MeV / cm);
  }
  //smax = smax / (MeV/cm) / 2.0;  // otherwise the ranges are too long
  smax = smax / (MeV / cm);
  double deriv_min = 0.0;  // looking for minimal(maximal by value but negative)
                           // derivative
  long n_deriv_min = 0;
  for (ne = 1; ne < qe; ne++) {
    double d =
        (eLoss[ne] * beta[ne] - eLoss[ne - 1] * beta[ne - 1]) /
        (hmd->energy_mesh->get_ec(ne) - hmd->energy_mesh->get_ec(ne - 1));
    if (deriv_min > d) {
      deriv_min = d;
      n_deriv_min = ne - 1;
    }
  }
  //Iprintn(mcout, n_deriv_min);
  for (ne = 0; ne < n_deriv_min - 1; ne++) {
    eLoss[ne] =
        (eLoss[n_deriv_min - 1] * beta[n_deriv_min - 1] +
         deriv_min * (hmd->energy_mesh->get_ec(ne) -
                      hmd->energy_mesh->get_ec(n_deriv_min - 1))) / beta[ne];
  }

  /*
  for(ne=0; ne<qe; ne++)
  {
    if(eLoss[ne] < smax) eLoss[ne] = smax;
    else break;
  }
  */
  lambda = DynLinArr<double>(qe);
  //low_lambda = DynLinArr< double >(qe);
  rthetac = DynLinArr<double>(qe);
  thetac = DynLinArr<double>(qe);
  CosThetac12 = DynLinArr<double>(qe);
  SinThetac12 = DynLinArr<double>(qe);
  sisfera = DynLinArr<int>(qe, 0);
  msig = DynLinArr<double>(qe);
  if (sruth == 2) {
    angular_mesh_c = DynLinArr<double>(q_angular_mesh);
    long n;
    angular_mesh_c[0] = 0.0;
    double amin = 0.3;
    double amax = 180.0;
    double rk = pow(amax / amin, (1.0 / double(q_angular_mesh - 2)));
    double ar = amin;
    angular_mesh_c[1] = ar;
    for (n = 2; n < q_angular_mesh; n++) {
      angular_mesh_c[n] = angular_mesh_c[n - 1] * rk;
    }
    angular_mesh_c[q_angular_mesh - 1] = 180.0;

    //print_DynLinArr_double(mcout, angular_mesh_c);
    smat = DynLinArr<DynLinArr<double> >(qe);
    //ismat = DynLinArr< DynLinArr< double > >(qe);
    //mean_free_path = DynLinArr< double >(qe);
    angular_points_ran = DynLinArr<PointsRan>(qe);
    low_angular_points_ran = DynLinArr<PointsRan>(qe);
    low_lambda = DynLinArr<double>(qe);
#ifdef USE_MEAN_COEF
    mean_coef_low_sigma = DynLinArr<double>(eesls->get_ees()->get_qe());
#else
    coef_low_sigma = DynLinArr<double>(eesls->get_ees()->get_qe());
#endif
    // long qat = hmd->matter->qatom();
    // long nat;
    for (ne = 0; ne < eesls->get_ees()->get_qe(); ne++) {
      //double energy = eesls->ees->get_energy_mesh(ne) * 0.001;
      long qat = hmd->matter->qatom();
      long nat;
      double s = 0.0;

      for (nat = 0; nat < qat; nat++) {
//s += pow( eesls->get_coef(hmd->matter->atom(nat)->Z(), ne), 2.0) *
//hmd->matter->weight_quan(nat);
#ifdef USE_MEAN_COEF
        s += eesls->get_mean_coef(hmd->matter->atom(nat)->Z(), ne) *
             hmd->matter->weight_quan(nat);
#else
        s += eesls->get_coef(hmd->matter->atom(nat)->Z(), ne) *
             hmd->matter->weight_quan(nat);
#endif
      }
//s = sqrt(s);
#ifdef USE_MEAN_COEF
      mean_coef_low_sigma[ne] = s;
#else
      coef_low_sigma[ne] = s;
#endif
    }
  }
  for (ne = 0; ne < qe; ne++) {
    double rr;
    double ek = hmd->energy_mesh->get_ec(ne) * 1000.0;
    if (ek <= 10.0) {
      rr = 1.0e-3 * hmd->matter->A_mean() / (g / mole) / hmd->matter->Z_mean() *
           3.872e-3 * pow(ek, 1.492);
      rr = rr / (hmd->matter->density() / (gram / cm3));
    } else {
      rr = 1.0e-3 * 6.97e-3 * pow(ek, 1.6);
      rr = rr / (hmd->matter->density() / (gram / cm3));
    }
    rr = rr * 0.1;
    //Iprintn(mcout, rr);
    double cor = 1.0;
    {
      //                b-k*(x-a)**2 = 0  =>  x= a +- sqrt(b/k)
      //                k = b / (x - a)**2
      double a = 2.5;
      double b = 4;
      //k=1.0/4.0
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
      lambda[ne] = mlambda / (hmd->matter->density() / (gram / cm3));
      if (lambda[ne] < rr) lambda[ne] = rr;
      lambda[ne] = lambda[ne] * cor;
      // Calculate the minimum angle for restriction of field by
      // atomic shell
      double mT =
          2.0 *
          asin(1.0 / (2.0 * momentum[ne] * hmd->matter->Z_mean() * 5.07e2));
      rthetac[ne] = mT;
      if (mT < mthetac) mT = mthetac;  // Throw out too slow interaction. They
                                       // do not influent to anything

      //           Calculate the cut angle due to mean free part
      double A = hmd->Rutherford_const / cor / (momentum2[ne] * beta2[ne]) /
                 pow(5.07e10, 2.0);
      double B = lambda[ne] * A;
      B = sqrt(B / (B + 1.0));
      thetac[ne] = 2.0 * asin(B);

      // If it too little, reset it. It will lead to increasing
      // of lambda and decriasing of calculation time.
      if (thetac[ne] < mT) {
        thetac[ne] = mT;
        B = mT;  // B is double precision
        double r = sin(B / 2.0);
        lambda[ne] = 1 / A * 2.0 * r * r / (1 + cos(B));
        //r=cos(TetacBdel(nen,nm))
        //lamBdel=A*(1.0+r)/(1.0-r)
        //lamBdel=1.0/lamBdel
        //lamBdel=(p2*bet2*sin(TetacBdel/2.0)**2) / A
      }
      B = thetac[ne];
      CosThetac12[ne] = cos(B / 2.0);
      SinThetac12[ne] = sin(B / 2.0);
      if (thetac[ne] > 1.5)
        sisfera[ne] = 1;
      else
        sisfera[ne] = 0;

      //c       debug mode:
      //c        lamaBdel(nen,nm)=2.0*lamaBdel(nen,nm)
    } else if (sruth == 0) {  // gaus formula

      //  calculate path length from mTetacBdel
      double msig_loc = mthetac;
      double x = msig_loc / (sqrt(2.0) * 13.6 / (beta[ne] * momentum[ne]));
      x = x * x;

      //x=x/DensMatDS(nMatVol(nVolBdel))
      x = x * hmd->radiation_length * cor;
      lambda[ne] = mlambda / (hmd->matter->density() / (gram / cm3));
      if (lambda[ne] < rr) lambda[ne] = rr;
      lambda[ne] = lambda[ne] * cor;
      //c        write(oo,*)' x=',x,' rleng=',rleng
      //c                reset if it is too large
      if (lambda[ne] < x) lambda[ne] = x;
      msig[ne] = sqrt(2.0) * 13.6 / (beta[ne] * momentum[ne]);

      //c        debug mode:
      //c        lamaBdel(nen,nm)=2.0*lamaBdel(nen,nm)
      //c        msigBdel(nen)=0.5*msigBdel(nen)
    } else if (sruth == 2) {
      smat[ne] = DynLinArr<double>(q_angular_mesh);
      //ismat[ne] = DynLinArr< double >(q_angular_mesh);
      double energy = hmd->energy_mesh->get_ec(ne);
      long qat = hmd->matter->qatom();
      long nat;
      long nan;
      for (nan = 0; nan < q_angular_mesh; nan++) {
        double angle = angular_mesh_c[nan] / 180.0 * M_PI;
        double s = 0.0;
        for (nat = 0; nat < qat; nat++) {
          s += ees->get_CS(hmd->matter->atom(nat)->Z(), energy, angle) *
               hmd->matter->weight_quan(nat);
        }
        s = s * 1.0E-16;
        //s = s * 1.0E-16 * C1_MEV_CM * C1_MEV_CM;
        //      Angstrem**2 -> cm**2
        //                cm**2 -> MeV**-2
        s = s * 2.0 * M_PI * sin(angle);  // sr -> dtheta

        smat[ne][nan] = s;
      }
      angular_points_ran[ne] =
          PointsRan(angular_mesh_c, smat[ne], low_cut_angle_deg, 180.0);
      low_angular_points_ran[ne] =
          PointsRan(angular_mesh_c, smat[ne], 0.0, low_cut_angle_deg);
      //lambda[ne] = angular_points_ran[ne].get_integ()/180.0 * M_PI;
      //Iprintn(mcout, hmd->matter->density()/(gram/cm3));
      //Iprintn(mcout, hmd->matter->A_mean()/(gram/mole));
      lambda[ne] = 1.0 / (angular_points_ran[ne].get_integ_active() / 180.0 *
                          M_PI * AVOGADRO *
                          //(AVOGADRO/(C1_MEV_CM * C1_MEV_CM)) *
                          hmd->matter->density() / (gram / cm3) /
                          (hmd->matter->A_mean() / (gram / mole)));
      //PointsRan low_angular_points_ran(angular_mesh_c, smat[ne],
      //                                       0.0, low_cut_angle_deg);
      low_lambda[ne] =
          1.0 / (low_angular_points_ran[ne].get_integ_active() / 180.0 * M_PI *
                 AVOGADRO *
                 //                //(AVOGADRO/(C1_MEV_CM * C1_MEV_CM)) *
                 hmd->matter->density() / (gram / cm3) /
                 (hmd->matter->A_mean() / (gram / mole)));
    }
  }

  //mcout<<"finishing HeedDeltaElectronCS\n";

}

double HeedDeltaElectronCS::get_sigma(double energy, double nscat) const {
  mfunname("double HeedDeltaElectronCS::get_sigma(...)");
  check_econd11(nscat, < 0, mcerr);
  //check_econd21(nscat , < 0 || , > eesls->get_qscat() , mcerr);
  // ^ not compartible with Poisson
  double energyKeV = energy * 1000.0;
  if (energyKeV < ees->get_energy_mesh(0)) energyKeV = ees->get_energy_mesh(0);
  if (energyKeV > ees->get_energy_mesh(ees->get_qe() - 1))
    energyKeV = ees->get_energy_mesh(ees->get_qe() - 1);
  long n1 = 0;
  long n2 = ees->get_qe() - 1;
  long n3;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (energyKeV < ees->get_energy_mesh(n3))
      n2 = n3;
    else
      n1 = n3;
  }
//Iprintn(mcout, n1);
//Iprintn(mcout, n2);
#ifdef USE_MEAN_COEF
  double v1 = nscat * mean_coef_low_sigma[n1];
  double v2 = nscat * mean_coef_low_sigma[n2];
#else
  double v1 = nscat * coef_low_sigma[n1];
  double v2 = nscat * coef_low_sigma[n2];
#endif
  //Iprintn(mcout, v1);
  //Iprintn(mcout, v2);
  double r =
      v1 + (v2 - v1) / (ees->get_energy_mesh(n2) - ees->get_energy_mesh(n1)) *
               (energyKeV - ees->get_energy_mesh(n1));
  return r;
}

void HeedDeltaElectronCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "HeedDeltaElectronCS(l=" << l << "):";
  long qe = hmd->energy_mesh->get_q();
  //Iprintn(mcout, qe);
  //mcout<<std::endl;
  Iprintn(file, mlambda);
  Iprintn(file, mthetac);
  Iprintn(file, sruth);
  long ne;
  Ifile << "         get_ec,        beta,      momentum,    eLoss,    lambda,  "
           " low_lambda:" << std::endl;
  indn.n += 2;
  for (ne = 0; ne < qe; ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << hmd->energy_mesh->get_ec(ne) << ' ' << std::setw(12) << beta[ne]
          << ' ' << std::setw(12) << momentum[ne] << ' ' << std::setw(12)
          << eLoss[ne] << ' ' << std::setw(12) << lambda[ne] << ' '
          << std::setw(12) << low_lambda[ne] << '\n';
  }
  indn.n -= 2;
  Ifile << "         get_ec,        rthetac,    thetac,    sisfera,     msig:"
        << std::endl;
  indn.n += 2;
  for (ne = 0; ne < qe; ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << hmd->energy_mesh->get_ec(ne) << ' ' << std::setw(12) << rthetac[ne]
          << ' ' << std::setw(12) << thetac[ne] << ' ' << std::setw(12)
          << sisfera[ne] << ' ' << std::setw(12) << msig[ne] << '\n';
  }
  indn.n -= 2;
  Ifile << "na, angular_mesh_c:" << std::endl;
  indn.n += 2;
  long na;
  for (na = 0; na < q_angular_mesh; na++) {
    Ifile << na << ' ' << std::setw(12) << angular_mesh_c[na] << '\n';
  }
  indn.n -= 2;
  Iprintn(file, eesls->get_ees()->get_qe());
  indn.n += 2;
#ifdef USE_MEAN_COEF
  Ifile << "ne, energy_mesh(ne), mean_coef_low_sigma:" << std::endl;
  for (ne = 0; ne < eesls->get_ees()->get_qe(); ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << eesls->get_ees()->get_energy_mesh(ne) << " KeV " << std::setw(12)
          << mean_coef_low_sigma[ne] << '\n';
  }
#else
  Ifile << "ne, energy_mesh(ne), coef_low_sigma:" << std::endl;
  for (ne = 0; ne < eesls->get_ees()->get_qe(); ne++) {
    Ifile << std::setw(3) << ne << ' ' << std::setw(12)
          << eesls->get_ees()->get_energy_mesh(ne) << " KeV " << std::setw(12)
          << coef_low_sigma[ne] << '\n';
  }
#endif
  indn.n -= 2;
}

std::ostream& operator<<(std::ostream& file, const HeedDeltaElectronCSType& f) {
  mfunname("std::ostream& operator << (std::ostream& file, const "
           "HeedDeltaElectronCSType& f)");
  if (f.hdecs.get() == NULL) {
    Ifile << "HeedDeltaElectronCSType: type is not initialized\n";
  } else {
    Ifile << "HeedDeltaElectronCSType: =";
    f.hdecs->print(file, 1);
  }
  return file;
}

}
