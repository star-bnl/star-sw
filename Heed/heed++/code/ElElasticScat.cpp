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

// 2003, I. Smirnov

namespace Heed {

using CLHEP::electron_mass_c2;
using CLHEP::fine_structure_const;

double ElElasticScatDataStruct::CS(const double theta) const {
  if (A[0] == -1.0) return -1.0;
  double s = 0.0;
  const double ctheta = cos(theta);
  for (long n = 0; n < 4; ++n) {
    s += A[n] / (pow(1.0 - ctheta + 2.0 * B, n + 1));
  }
  for (long n = 0; n < 7; ++n) {
    s += C[n] * polleg(n, ctheta);
  }
  return s;
}

ElElasticScat::ElElasticScat(const std::string& file_name) : atom(0) {
  mfunnamep("ElElasticScat::ElElasticScat(const string& filename)");
  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  int i = findmark(file, "#");
  check_econd11a(i, != 1, "cannot find sign #, wrong file format", mcerr);
  file >> qe;
  energy_mesh.resize(qe);
  gamma_beta2.resize(qe);
  for (long ne = 0; ne < qe; ++ne) {
    file >> energy_mesh[ne];
    if (!file.good()) {
      funnw.ehdr(mcerr);
      mcerr << "error at reading energy_mesh, ne=" << ne << '\n';
      spexit(mcerr);
    }
    // energy mesh in keV
    const double rm = 0.001 * energy_mesh[ne] / electron_mass_c2;
    const double gamma = 1. + rm;
    const double beta2 = (2 * rm + rm * rm) / (gamma * gamma);
    gamma_beta2[ne] = gamma * beta2;
  }
  while (findmark(file, "$") == 1) {
    long Z;
    file >> Z;
    check_econd21(Z, < 1 ||, > 110, mcerr);
    atom.emplace_back(ElElasticScatData(Z, qe));
    for (int nc = 0; nc < 4; ++nc) {
      for (long ne = 0; ne < qe; ++ne) {
        file >> atom.back().data[ne].A[nc];
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
        file >> atom.back().data[ne].C[nc];
        if (!file.good()) {
          funnw.ehdr(mcerr);
          mcerr << "error at reading C, Z=" << Z << " nc=" << nc << " ne=" << ne
                << '\n';
          spexit(mcerr);
        }
      }
    }
    for (long ne = 0; ne < qe; ++ne) {
      file >> atom.back().data[ne].B;
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
  mfunnamep(
      "double ElElasticScat::get_CS_for_presented_atom(long na, double "
      "energy, double angle)");
  const double enKeV = energy * 1000.0;
  const double rm = energy / electron_mass_c2;
  const double gamma = 1. + rm;
  const double beta2 = (2. * rm + rm * rm) / (gamma * gamma);
  const double gamma_beta2_t = gamma * beta2;
  const double coe =
      fine_structure_const * fine_structure_const * atom[na].Z / gamma_beta2_t;
  if (enKeV < energy_mesh[0]) {
    double r = -1.;
    // looking for valid data
    for (long ne = 0; ne < qe; ne++) {
      r = atom[na].data[ne].CS(angle);
      if (r >= 0.0) break;
    }
    check_econd11(r, < 0.0, mcerr);
    return r * coe * coe;
  }
  if (enKeV >= energy_mesh[qe - 1]) {
    double r = -1.;
    // looking for valid data
    for (long ne = qe - 1; ne >= 0; ne--) {
      r = atom[na].data[ne].CS(angle);
      if (r >= 0.0) break;
    }
    check_econd11(r, < 0.0, mcerr);
    return r * coe * coe;
  }
  long ne = 1;
  for (ne = 1; ne < qe; ne++) {
    if (energy_mesh[ne] > enKeV) break;
  }
  double cs[2] = {-1., -1.};
  // starting points
  long ne_left = ne - 1;
  long ne_right = ne;
  // looking for valid data
  for (ne = ne_left; ne >= 0; ne--) {
    cs[0] = atom[na].data[ne].CS(angle);
    if (cs[0] >= 0.0) break;
  }
  for (ne = ne_right; ne < qe; ne++) {
    cs[1] = atom[na].data[ne].CS(angle);
    if (cs[1] >= 0.0) break;
  }
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
  return r * coe * coe;
}

double ElElasticScat::get_CS(long Z, double energy, double angle,
                             int s_interp) {
  mfunname(
      "double ElElasticScat::get_CS(long Z, double energy, double angle, "
      "int s_interp)");
  const long qa = atom.size();
  long na_left = 0;
  long Z_left = -100;
  long na_right = qa - 1;
  long Z_right = 10000;
  for (long na = 0; na < qa; na++) {
    if (atom[na].Z == Z && s_interp == 0) {
      return get_CS_for_presented_atom(na, energy, angle);
    }
    if (atom[na].Z > Z_left && atom[na].Z < Z) {
      Z_left = atom[na].Z;
      na_left = na;
    } else if (atom[na].Z < Z_right && atom[na].Z > Z) {
      Z_right = atom[na].Z;
      na_right = na;
    }
  }
  check_econd11a(Z_left, == -100, " have not found previous atom", mcerr);
  check_econd11a(Z_right, == 10000, " have not found next atom", mcerr);
  const double f1 = get_CS_for_presented_atom(na_left, energy, angle);
  const double f2 = get_CS_for_presented_atom(na_right, energy, angle);
  const double z1 = atom[na_left].Z;
  const double z2 = atom[na_right].Z;
  const double c = (f1 * 4 - f2 * z1 * z1) / (f2 * z1 - f1 * z2);
  const double k = f1 / (z1 * (z1 + c));
  double r = k * Z * (Z + c);
  if (r < 0.0) r = 0.0;
  return r;
}

double ElElasticScat::get_CS_Rutherford(long Z, double energy, double angle) {
  mfunname(
      "double ElElasticScat::get_CS_Rutherford(long Z, double energy, "
      "double angle)");
  const double gamma_1 = energy / electron_mass_c2;
  const double beta2 = lorbeta2(gamma_1);
  const double momentum2 = energy * energy + 2.0 * electron_mass_c2 * energy;
  // TODO
  double r = 0.25 * Z * Z * fine_structure_const * fine_structure_const /
             (momentum2 * beta2 * pow(sin(0.5 * angle), 4)) /
             (pow(5.07E10, 2)) * 1.0e16;
  return r;
}

void ElElasticScat::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "ElElasticScat(l=" << l << "): qe=" << qe
        << " atom.size()=" << atom.size() << std::endl;
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
  const long qa = atom.size();
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
  }
}

ElElasticScatLowSigma::ElElasticScatLowSigma(ElElasticScat* fees,
                                             const std::string& file_name)
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
  mean_coef.resize(qat);
  coef.resize(qat);
  for (long nat = 0; nat < qat; ++nat) {
    mean_coef[nat].resize(ees->get_qe());
    coef[nat].resize(ees->get_qe());
    long z;
    file >> z;
    check_econd12(z, !=, nat + 1, mcerr);
    for (long ne = 0; ne < ees->get_qe(); ++ne) {
      long fne;
      double e;
      mean_coef[nat][ne] = 0.0;
      coef[nat][ne] = 0.0;
      file >> fne >> e >> mean_coef[nat][ne] >> coef[nat][ne];
      check_econd12(fne, !=, ne, mcerr);
      check_econd12(e, !=, ees->get_energy_mesh(ne), mcerr);
      check_econd11(mean_coef[nat][ne], <= 0, mcerr);
      check_econd11(coef[nat][ne], <= 0, mcerr);
    }
  }
}
}
