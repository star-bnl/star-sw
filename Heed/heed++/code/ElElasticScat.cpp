#include <fstream>
#include <iomanip>
#include <cmath>

#include "wcpplib/stream/findmark.h"
#include "wcpplib/math/PolLeg.h"
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"
#include "wcpplib/random/PointsRan.h"
#include "wcpplib/geometry/vec.h"
#include "wcpplib/matter/AtomDef.h"  // to find atomic weights for histogramms
#include "wcpplib/random/ranluxint.h"

#include "heed++/code/PhysicalConstants.h"
#include "heed++/code/ElElasticScat.h"

/*
2003, I. Smirnov
*/

namespace Heed {

double ElElasticScatDataStruct::CS(double theta) {
  if (A[0] == -1.0) return -1.0;
  double s = 0.0;
  const double ctheta = cos(theta);
  for (long n = 0; n < 4; ++n) {
    s += A[n] / (pow(1.0 - ctheta + 2.0 * B, double(n + 1)));
  }
  for (long n = 0; n < 7; ++n) {
    s += C[n] * polleg(n, ctheta);
  }
  return s;
}

ElElasticScat::ElElasticScat(const String& file_name) : atom(0) {
  mfunnamep("ElElasticScat::ElElasticScat(const String& filename)");
  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  int i = findmark(file, "#");
  check_econd11a(i, != 1, "cannot find sign #, wrong file format", mcerr);
  file >> qe;
  energy_mesh = DynLinArr<double>(qe);
  gamma_beta2 = DynLinArr<double>(qe);
  for (long ne = 0; ne < qe; ++ne) {
    file >> energy_mesh[ne];
    if (!file.good()) {
      funnw.ehdr(mcerr);
      mcerr << "error at reading energy_mesh, ne=" << ne << '\n';
      spexit(mcerr);
    }
    double gamma = 1.0 + 0.001 * energy_mesh[ne] / ELMAS;
    // energy_mesh[ne] in KeV
    double beta2 =
        (2.0 * 0.001 * energy_mesh[ne] / ELMAS +
         pow(0.001 * energy_mesh[ne] / ELMAS, 2.0)) / pow(gamma, 2.0);
    gamma_beta2[ne] = gamma * beta2;
  }
  while (findmark(file, "$") == 1) {
    atom.increment();
    long na = atom.get_qel() - 1;
    long Z;
    file >> Z;
    check_econd21(Z, < 1 ||, > 110, mcerr);
    atom[na] = ElElasticScatData(Z, qe);
    for (int nc = 0; nc < 4; ++nc) {
      for (long ne = 0; ne < qe; ++ne) {
        file >> atom[na].data[ne].A[nc];
        if (!file.good()) {
          funnw.ehdr(mcerr);
          mcerr << "error at reading A, Z=" << Z << " nc=" << nc << " ne=" << ne
                << '\n';
          spexit(mcerr);
        }
      }
    }
    for (int nc = 0; nc < 7; ++nc) {
      for (long ne = 0; ne < qe; ++ne) {
        file >> atom[na].data[ne].C[nc];
        if (!file.good()) {
          funnw.ehdr(mcerr);
          mcerr << "error at reading C, Z=" << Z << " nc=" << nc << " ne=" << ne
                << '\n';
          spexit(mcerr);
        }
      }
    }
    for (long ne = 0; ne < qe; ++ne) {
      file >> atom[na].data[ne].B;
      if (!file.good()) {
        funnw.ehdr(mcerr);
        mcerr << "error at reading B, Z=" << Z << " ne=" << ne << '\n';
        spexit(mcerr);
      }
    }
  }
}

double ElElasticScat::get_CS_for_presented_atom(long na, double energy,
                                                double angle) {
  mfunnamep("double ElElasticScat::get_CS_for_presented_atom(long na, double "
            "energy, double angle)");
  long ne;
  double enKeV = energy * 1000.0;
  double gamma = 1.0 + energy / ELMAS;
  double beta2 =
      (2.0 * energy / ELMAS + pow(energy / ELMAS, 2.0)) / pow(gamma, 2.0);
  double gamma_beta2_t = gamma * beta2;
  double coe = atom[na].Z / (FSCON * FSCON) / gamma_beta2_t;
  if (enKeV < energy_mesh[0]) {
    double r = -1.;
    // looking for valid data
    for (ne = 0; ne < qe; ne++) {
      r = atom[na].data[ne].CS(angle);
      if (r >= 0.0) break;
    }
    check_econd11(r, < 0.0, mcerr);
    r = r * coe * coe;
    return r;
  } else {
    if (enKeV >= energy_mesh[qe - 1]) {
      double r = -1.;
      // looking for valid data
      for (ne = qe - 1; ne >= 0; ne--) {
        r = atom[na].data[ne].CS(angle);
        if (r >= 0.0) break;
      }
      check_econd11(r, < 0.0, mcerr);
      r = r * coe * coe;
      return r;
      //double coe = atom[na].Z / (FSCON * FSCON) / gamma_beta2[0];
      //return atom[na].data[qe-1].CS(angle) * coe * coe;
    } else {
      for (ne = 1; ne < qe; ne++) {
        if (energy_mesh[ne] > enKeV) {
          break;
        }
      }
      //Iprintn(mcout, ne);
      double cs[2] = { -1., -1. };
      //double coe[2];
      // starting points
      long ne_left = ne - 1;
      long ne_right = ne;
      // looking for valid data
      for (ne = ne_left; ne >= 0; ne--) {
        cs[0] = atom[na].data[ne].CS(angle);
        if (cs[0] >= 0.0) break;
      }
      //cs[0] = atom[na].data[ne-1].CS(angle);
      //coe[0] = atom[na].Z / (FSCON * FSCON) / gamma_beta2[ne - 1];
      //cs[0] = cs[0] * coe[0] * coe[0];
      for (ne = ne_right; ne < qe; ne++) {
        cs[1] = atom[na].data[ne].CS(angle);
        if (cs[1] >= 0.0) break;
      }
      //cs[1] = atom[na].data[ne].CS(angle);
      //coe[1] = atom[na].Z / (FSCON * FSCON) / gamma_beta2[ne];
      //cs[1] = cs[1] * coe[1] * coe[1];
      //Iprintn(mcout, cs[0]);
      //Iprintn(mcout, cs[1]);
      double r = cs[0];
      if (cs[0] >= 0.0 && cs[1] >= 0.0) {
        r = cs[0] + (cs[1] - cs[0]) / (energy_mesh[ne] - energy_mesh[ne - 1]) *
                        (enKeV - energy_mesh[ne - 1]);
      } else {
        if (cs[0] >= 0.0) {
          r = cs[0];
        } else if (cs[1] >= 0.0) {
          r = cs[1];
        } else {
          funnw.ehdr(mcerr);
          mcerr << "not implemented case\n";
          spexit(mcerr);
        }
      }
      r = r * coe * coe;
      //Iprintn(mcout, r);
      return r;
    }
  }
}

double ElElasticScat::get_CS(long Z, double energy, double angle,
                             int s_interp) {
  mfunname("double ElElasticScat::get_CS(long Z, double energy, double angle, "
           "int s_interp)");
  long qa = atom.get_qel();
  long na;
  long na_left = 0;
  long Z_left = -100;
  long na_right = qa - 1;
  long Z_right = 10000;
  for (na = 0; na < qa; na++) {
    if (atom[na].Z == Z && s_interp == 0) {
      //mcout << "ElElasticScat::get_CS: atom[na].Z=" << atom[na].Z
      //      << " energy=" << energy
      //      << " angle=" << angle << '\n';
      return get_CS_for_presented_atom(na, energy, angle);
    } else {
      if (atom[na].Z > Z_left && atom[na].Z < Z) {
        Z_left = atom[na].Z;
        na_left = na;
      } else if (atom[na].Z < Z_right && atom[na].Z > Z) {
        Z_right = atom[na].Z;
        na_right = na;
      }
    }
  }
  check_econd11a(Z_left, == -100, " have not found previous atom", mcerr);
  check_econd11a(Z_right, == 10000, " have not found next atom", mcerr);
  double f1 = get_CS_for_presented_atom(na_left, energy, angle);
  double f2 = get_CS_for_presented_atom(na_right, energy, angle);
  double z1 = atom[na_left].Z;
  double z2 = atom[na_right].Z;
  double c = (f1 * pow(2, 2.0) - f2 * pow(z1, 2.0)) / (f2 * z1 - f1 * z2);
  double k = f1 / (z1 * (z1 + c));
  double r = k * Z * (Z + c);
  if (r < 0.0) r = 0.0;
  return r;
}

double ElElasticScat::get_CS_Rutherford(long Z, double energy, double angle) {
  mfunname("double ElElasticScat::get_CS_Rutherford(long Z, double energy, "
           "double angle)");
  double gamma_1 = energy / ELMAS;
  double beta2 = lorbeta2(gamma_1);
  double momentum2 = energy * energy + 2.0 * ELMAS * energy;
  double r = (1 / 4.) * Z * Z * ELRAD * ELRAD * ELMAS * ELMAS /
             (momentum2 * beta2 * pow(sin(angle / 2.0), 4.0)) /
             (pow(5.07E10, 2.0)) * 1.0e16;
  return r;
}

#ifndef EXCLUDE_FUNCTIONS_WITH_HISTDEF

void ElElasticScat::fill_hist(void) {
  mfunname("double ElElasticScat::fill_hist(void)");
  const long qh = 100;
  long qa = atom.get_qel();
  long na;
  long ne;
  DynArr<histdef> raw_hist(qa, qe);
  DynArr<histdef> cor_hist(qa, qe);
  DynArr<histdef> corpol_hist(qa, qe);
  DynArr<histdef> corpola_hist(qa, qe);
  DynLinArr<histdef> path_length_cor_hist(qa);
  DynArr<histdef> int_hist(qa, qe);
  DynArr<histdef> rut_hist(qa, qe);
  DynArr<histdef> rutpol_hist(qa, qe);
  DynLinArr<histdef> path_length_rut_hist(qa);
  for (na = 0; na < qa; na++) {
    String name;
    name = "path_length_cor_" + long_to_String(atom[na].Z);
    path_length_cor_hist[na] = histdef(name, qe, 0.0, qe);
    path_length_cor_hist[na].init();
    name = "path_length_rut_" + long_to_String(atom[na].Z);
    path_length_rut_hist[na] = histdef(name, qe, 0.0, qe);
    path_length_rut_hist[na].init();
    for (ne = 0; ne < qe; ne++) {
      double energyKeV = energy_mesh[ne];
      double energyMeV = 0.001 * energyKeV;
      name = "raw_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      raw_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      raw_hist.ac(na, ne).init();
      name = "cor_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      cor_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      cor_hist.ac(na, ne).init();
      name = "corpol_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      corpol_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      corpol_hist.ac(na, ne).init();
      name = "corpola_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      corpola_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      corpola_hist.ac(na, ne).init();
      name = "int_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      int_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      int_hist.ac(na, ne).init();
      name = "rut_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      rut_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      rut_hist.ac(na, ne).init();
      name = "rutpol_" + long_to_String(atom[na].Z) + "_" + long_to_String(ne);
      rutpol_hist.ac(na, ne) = histdef(name, qh, 0.0, 180.0);
      rutpol_hist.ac(na, ne).init();
      double s_cor = 0;
      double s_rut = 0;
      //double coef = Avogadro / (1.0*gram/mole) * 8.31896e-05*gram/cm3;
      //Iprintn(mcout, coef/cm3);
      double coef = Avogadro / (1.0 * g / mole) * 1.0 * g / cm3;
      // for A = 1 and for unit density
      // real values are not known in this program
      double angle_step = M_PI / qh;  // in rad
      long nan;
      for (nan = 0; nan < qh; nan++) {
        double angle = raw_hist.ac(na, ne).get_bin_center(0, nan);
        double anglerad = angle / 180.0 * M_PI;
        double gamma = 1.0 + energyMeV / ELMAS;
        double beta2 = (2.0 * energyMeV / ELMAS + pow(energyMeV / ELMAS, 2.0)) /
                       pow(gamma, 2.0);
        double gamma_beta2_t = gamma * beta2;
        double coe = atom[na].Z / (FSCON * FSCON) / gamma_beta2_t;
        double r = atom[na].data[ne].CS(anglerad);
        raw_hist.ac(na, ne).fill(angle, 0.0, r * coe * coe);
        double t;
        cor_hist.ac(na, ne)
            .fill(angle, 0.0, (t = get_CS(atom[na].Z, energyMeV, anglerad)));
        corpol_hist.ac(na, ne).fill(angle, 0.0, 2.0 * M_PI * sin(anglerad) * t);
        corpola_hist.ac(na, ne)
            .fill(angle, 0.0, 2.0 * M_PI * sin(anglerad) * t /
                                  (AtomDef::get_A(atom[na].Z) / (gram / mole)));
        s_cor += 2.0 * M_PI * sin(anglerad) * t;
        if (na != 0 && na < qa - 1)  // bypass not implemented
            {
          int_hist.ac(na, ne).fill(angle, 0.0, get_CS(atom[na].Z, energyMeV,
                                                      angle / 180.0 * M_PI, 1));
        }
        rut_hist.ac(na, ne)
            .fill(angle, 0.0, (t = get_CS_Rutherford(atom[na].Z, energyMeV,
                                                     angle / 180.0 * M_PI)));
        rutpol_hist.ac(na, ne).fill(angle, 0.0, 2.0 * M_PI * sin(anglerad) * t);
        s_rut += 2.0 * M_PI * sin(anglerad) * t;
      }
      //path_length_cor_hist[na].fill
      //        (ne, 0.0,
      //         angle_step * s_cor);
      path_length_cor_hist[na].fill(
          ne, 0.0, 1.0 / (coef * angle_step * s_cor * 1.e-20 * meter2) / cm);
      path_length_rut_hist[na].fill(
          ne, 0.0, 1.0 / (coef * angle_step * s_rut * 1.e-20 * meter2) / cm);
    }
  }
}

void ElElasticScat::fill_hist_low_scat(const String& file_name,
                                       const String& file_name_dist) {
  mfunnamep("double ElElasticScat::fill_hist_low_scat(const String& file_name, "
            "const String& file_name_dist)");
  int s_write_dist = 0;
  if (file_name_dist != String("") && file_name_dist != String("none"))
    s_write_dist = 1;
  const long qh = 100;
  long qa = atom.get_qel();
  //long na;
  long ne;
  long nq;
  const long qquan = 4;  // quantity of different quantities and histogramms
  long quan[qquan] = { 5, 10, 20, 40 };  // used for selection of hist.
  // mean and rms are computed by all collisions up to quan[qquan-1]

  //long quan[qquan]={5, 10, 20, 40, 100, 200, 400};
  long zmax = atom[qa - 1].Z;
  //DynArr< histdef > ang_hist(qa, qe, qquan);
  DynArr<histdef> ang_hist(zmax, qe, qquan);
  DynArr<histdef> mean_hist(zmax, qe);
  DynArr<histdef> sigma_hist(zmax, qe);
  DynLinArr<histdef> sigma_coef_hist(zmax);
  // two arrays where mean and sigma is stored. They are used
  // to write file file_name_dist
  DynArr<double> mea_ang_hist(zmax, qe, qquan);
  DynArr<double> sig_ang_hist(zmax, qe, qquan);

  const long q_angular_mesh = 50;
  DynLinArr<double> angular_mesh_c(q_angular_mesh);
  // angular mesh, centers
  long n;
  /*
  double amin = 1.0;
  double amax = 180.0;
  double rk = pow(amax/amin,(1.0/double(q_angular_mesh)));
  double ar = amin;
  angular_mesh_c[0] = ar;
  for( n=1; n<q_angular_mesh; n++)
  {
    angular_mesh_c[n] = angular_mesh_c[n-1] * rk;
  }
  */
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

  std::ofstream ofile(file_name.c_str());
  if (!ofile) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << endl;
    spexit(mcerr);
  }
  long za;
  ofile << "this file is created by function void "
           "ElElasticScat::fill_hist_low_scat(void)\n";
  ofile << "This is new format, in which the mean of 1 - cos(theta) is "
           "inserted\n";
  ofile << " format:\nnumber of atoms maximal number of interactions,\nthen "
           "loop by atoms:\nZ of atom\n";
  //ofile<<"number of energies\n"
  ofile << "ne energy_mesh[ne]  (mean of 1 - cos(theta)) (sqrt(mean of "
           "(1-coef)^2)\n";
  //ofile<<"number of interactions, mean cos of scattering angle, sigma of cos
  //of scattering angle(thinking that mean is zero)\n";
  ofile << "dollar sign means starting of this format\n";
  ofile << "$\n" << zmax << ' ' << quan[qquan - 1] << '\n';
  for (za = 1; za <= zmax; za++) {
    //for(na=0; na<qa; na++)
    //{
    //mcout<<"starting calculate na="<<na<<'\n';
    mcout << "starting calculate za=" << za << endl;
    ofile << za << '\n';
    //ofile<<na<<' '<<atom[na].Z<<'\n';
    String name;
    //name = "ang_" + long_to_String(atom[na].Z) + '_' +
    name = "sigma_coef" + long_to_String(za);
    sigma_coef_hist[za - 1] = histdef(name, qe, 0.0, qe);
    sigma_coef_hist[za - 1].init();
    // run events
    for (ne = 0; ne < qe; ne++) {
      mcout << "starting calculate ne=" << ne << endl;
      //ofile<<ne<<' '<<energy_mesh[ne]<<'\n';
      double energy = energy_mesh[ne] * 0.001;
      DynLinArr<double> cs(q_angular_mesh);
      long nan;
      for (nan = 0; nan < q_angular_mesh; nan++) {
        double angle = angular_mesh_c[nan] / 180.0 * M_PI;
        double s = get_CS(za, energy, angle);
        //double s = get_CS(atom[na].Z,
        //                  energy,
        //                  angle);
        s = s * 2.0 * M_PI * sin(angle);  // sr -> dtheta

        cs[nan] = s;
      }
      PointsRan angular_points_ran(angular_mesh_c, cs, 0.0, low_cut_angle_deg);
      DynLinArr<double> mean(quan[qquan - 1], 0.0);  // for all collisions
      DynLinArr<double> disp(quan[qquan - 1], 0.0);  // for all collisions
      for (nq = 0; nq < qquan; nq++) {
        String name;
        //name = "ang_" + long_to_String(atom[na].Z) + '_' +
        name = "ang_" + long_to_String(za) + '_' + long_to_String(ne) + '_' +
               long_to_String(nq);
        ang_hist.ac(za - 1, ne, nq) = histdef(name, 1000, -1.0, 1.0);
        ang_hist.ac(za - 1, ne, nq).init();
        //ang_hist.ac(na,ne,nq) = histdef(name, 100, -1.0, 1.0);
        //ang_hist.ac(na,ne,nq).init();
      }
      String name;
      //name = "ang_" + long_to_String(atom[na].Z) + '_' +
      name = "mean_" + long_to_String(za) + '_' + long_to_String(ne);
      // Comparing with similar statement below this is better
      // because the linear interpolation (h/pl ... l)
      // goes precisely through zero at the plot.
      // It also have correct number of bins and good right border.
      mean_hist.ac(za - 1, ne) =
          histdef(name, quan[qquan - 1] + 1, -0.5, quan[qquan - 1] + 0.5);
      //mean_hist.ac(za-1,ne) = histdef
      //        (name, quan[qquan-1], 0.0, quan[qquan-1]);
      mean_hist.ac(za - 1, ne).init();
      name = "sigma_" + long_to_String(za) + '_' + long_to_String(ne);
      sigma_hist.ac(za - 1, ne) =
          histdef(name, quan[qquan - 1] + 1, -0.5, quan[qquan - 1] + 0.5);
      //sigma_hist.ac(za-1,ne) = histdef
      //        (name, quan[qquan-1], 0.0, quan[qquan-1]);
      sigma_hist.ac(za - 1, ne).init();
      // run events
      long qev = 100000;
      long nev;
      long ncs;
      for (nev = 0; nev < qev; nev++) {
        nq = 0;
        vec dir(0, 0, 1);  // current direction
        for (ncs = 0; ncs < quan[qquan - 1]; ncs++) {
          //mcout<<"nev="<<nev<<" dir="<<dir;
          basis temp(dir, "temp");
          double theta_rot = angular_points_ran.ran(SRANLUX());
          //double phi = 2.0 * M_PI * SRANLUX();
          vec vturn;
          vturn.random_round_vec();
          vturn = vturn * sin(theta_rot / 180.0 * M_PI);
          vec new_dir(vturn.x, vturn.y, cos(theta_rot / 180.0 * M_PI));
          new_dir.down(&temp);
          //Iprint(mcout, new_dir);
          double theta = asin(sqrt(pow(new_dir.x, 2) + pow(new_dir.y, 2)) /
                              length(new_dir));
          if (new_dir.z < 0.0) theta = M_PI - theta;
          //Iprintn(mcout, theta);
          dir = new_dir;
          double ctheta = cos(theta);
          if (ncs == quan[nq] - 1)  // ncs is starting from 0
              {
            ang_hist.ac(za - 1, ne, nq).fill(ctheta, 0.0, 1.0);
            //ang_hist.ac(na,ne,nq).fill(ctheta, 0.0, 1.0);
            nq++;
          }
          mean[ncs] += 1.0 - ctheta;
          disp[ncs] += (ctheta - 1.0) * (ctheta - 1.0);
        }
      }
      nq = 0;
      for (ncs = 0; ncs < quan[qquan - 1]; ncs++) {
        mean[ncs] = mean[ncs] / qev;
        disp[ncs] = sqrt(disp[ncs] / (qev - 1));
        //ofile<<setw(5)<<ncs<<' '
        //     <<setw(12)<<mean[ncs]<<' '<<setw(12)<<disp[ncs]<<'\n';
        mean_hist.ac(za - 1, ne).fill(ncs + 1, 0.0, mean[ncs]);
        sigma_hist.ac(za - 1, ne).fill(ncs + 1, 0.0, disp[ncs]);
        if (ncs == quan[nq] - 1)  // ncs is starting from 0
            {
          mea_ang_hist.ac(za - 1, ne, nq) = mean[ncs];
          sig_ang_hist.ac(za - 1, ne, nq) = disp[ncs];
          nq++;
        }
      }
      // computing means for all collisions.
      double mean_coef = 0.0;
      long ns = 0;
      for (ncs = 0; ncs < quan[qquan - 1]; ncs++) {
        if (mean[ncs] < 0.4) {
          mean_coef += mean[ncs] / (ncs + 1.0);
          ns++;
        }
      }
      mean_coef = mean_coef / ns;
      double coef = 0.0;
      ns = 0;
      for (ncs = 0; ncs < quan[qquan - 1]; ncs++) {
        if (disp[ncs] < 0.4) {
          coef += disp[ncs] / (ncs + 1.0);
          ns++;
        }
      }
      coef = coef / ns;
      sigma_coef_hist[za - 1].fill(ne, 0.0, coef);
      ofile << ne << ' ' << setw(12) << energy_mesh[ne] << ' ' << setw(12)
            << mean_coef << ' ' << setw(12) << coef << '\n';
    }
  }
  if (s_write_dist == 1) {
    std::ofstream ofile(file_name_dist.c_str());
    if (!ofile) {
      funnw.ehdr(mcerr);
      mcerr << "cannot open file " << file_name_dist << endl;
      spexit(mcerr);
    }
    ofile << setw(5) << zmax << setw(5) << qe << setw(5) << qquan << '\n';
    for (za = 1; za <= zmax; za++) {
      for (ne = 0; ne < qe; ne++) {
        for (nq = 0; nq < qquan; nq++) {
          DynLinArr<float> hi;
          ang_hist.ac(za - 1, ne, nq).unpack(hi);
          long q = hi.get_qel();
          ofile << "\n# " << setw(5) << za << ' ' << setw(5) << ne << ' '
                << setw(5) << nq << ' ' << setw(5) << q << ' ' << setw(15)
                << mea_ang_hist.ac(za - 1, ne, nq) << ' ' << setw(15)
                << sig_ang_hist.ac(za - 1, ne, nq) << '\n';
          long n;
          for (n = 0; n < q; n++) {
            //ofile<<setw(5)<<n<<setw(12)<<hi[n]<<'\n';
            ofile << hi[n] << '\n';
          }
        }
      }
    }

  }

}

#endif

void ElElasticScat::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "ElElasticScat(l=" << l << "): qe=" << qe
        << " atom.get_gel()=" << atom.get_qel() << std::endl;
  if (l <= 1) return;
  indn.n += 2;
  Ifile << "energy_mesh=";
  for (long ne = 0; ne < qe; ++ne) {
    file << std::setw(12) << energy_mesh[ne];
  }
  file << std::endl;
  Ifile << "gamma_beta2=";
  for (long ne = 0; ne < qe; ++ne) {
    file << std::setw(12) << gamma_beta2[ne];
  }
  file << std::endl;
  indn.n -= 2;
  long qa = atom.get_qel();
  for (long na = 0; na < qa; ++na) {
    Ifile << "atom[na].Z=" << atom[na].Z << '\n';
    Ifile << "     ";
    for (long ne = 0; ne < qe; ++ne) {
      file << std::setw(12) << energy_mesh[ne];
    }
    file << std::endl;
    for (long n = 0; n < 4; ++n) {
      Ifile << "A[" << n << "]";
      for (long ne = 0; ne < qe; ++ne) {
        file << std::setw(12) << atom[na].data[ne].A[n];
      }
      file << std::endl;
    }
    for (int n = 0; n < 7; ++n) {
      Ifile << "C[" << n << "]";
      for (long ne = 0; ne < qe; ++ne) {
        file << std::setw(12) << atom[na].data[ne].C[n];
      }
      file << std::endl;
    }
    Ifile << "B     ";
    for (long ne = 0; ne < qe; ++ne) {
      file << std::setw(12) << atom[na].data[ne].B;
    }
    file << std::endl;
    /*
    for (int n = 0; n < 2; ++n) {
      Ifile << "D[" << n << "]";
      for (long ne = 0; ne < qe; ++ne) {
        file << std::setw(12) << atom[na].data[ne].D[n];
      }
      file << std::endl;
    }
    */
  }
}

ElElasticScatLowSigma::ElElasticScatLowSigma(ElElasticScat* fees,
                                             const String& file_name)
    : ees(fees) {
  mfunnamep("ElElasticScatLowSigma::ElElasticScatLowSigma(...)");
  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  int i = findmark(file, "$");
  check_econd11(i, != 1, mcerr);
  file >> qat >> qscat;
  check_econd11(qat, <= 0, mcerr);
  check_econd11(qscat, <= 0, mcerr);
  mean_coef = DynLinArr<DynLinArr<double> >(qat);
  coef = DynLinArr<DynLinArr<double> >(qat);
  for (long nat = 0; nat < qat; ++nat) {
    mean_coef[nat] = DynLinArr<double>(ees->get_qe());
    coef[nat] = DynLinArr<double>(ees->get_qe());
    long z;
    file >> z;
    check_econd12(z, !=, nat + 1, mcerr);
    for (long ne = 0; ne < ees->get_qe(); ++ne) {
      long fne;
      double e;
      mean_coef[nat][ne] = 0.0;
      coef[nat][ne] = 0.0;
      file >> fne >> e >> mean_coef[nat][ne] >> coef[nat][ne];
      // file >> fne >> e >> coef[nat][ne];  // old format
      check_econd12(fne, !=, ne, mcerr);
      check_econd12(e, !=, ees->get_energy_mesh(ne), mcerr);
      check_econd11(mean_coef[nat][ne], <= 0, mcerr);
      check_econd11(coef[nat][ne], <= 0, mcerr);
    }
  }
}

}
