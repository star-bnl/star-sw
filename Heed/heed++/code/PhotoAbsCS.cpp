#include <cmath>
#include <fstream>
#include <iomanip>
#include "wcpplib/stream/findmark.h"
#include "wcpplib/random/ranluxint.h"
#include "wcpplib/math/tline.h"
#include "heed++/code/PhotoAbsCS.h"

// 2004, I. Smirnov

//#define DEBUG_PRINT_get_escape_particles
//#define DEBUG_ignore_non_standard_channels

//#define ALWAYS_LINEAR_INTERPOLATION  // how the paper was computed

namespace {

/// Determine whether to use linear or nonlinear interpolation.
/// Usually photoabsorption cross sections decrease as inverse power function
/// of power like -2.75. Its linear interpolation inside large energy
/// intervals is remarkably not precise. This function is designed
/// to determine the cases common for the total program when the inverse power
/// function is applied in such intervals.
/// Conditions are empirical.
/// true - nonlinear, false - linear.
/// Energies and threshold are given in MeV.
/// e1 should be < e2.
bool sign_nonlinear_interpolation(double e1, double cs1, double e2, double cs2,
                                  double threshold) {
#ifdef ALWAYS_LINEAR_INTERPOLATION
  return false;
#else
  // normal case:
  if (cs2 >= cs1 || cs2 <= 0 || e1 < 300.0e-6 || e1 < 1.5 * threshold) {
    return false;
  }
  const double pw = log(cs1 / cs2) / log(e1 / e2);
  if (pw >= -1.0) {
    // good case for linear interpolation
    return false;
  } else if (pw < -5.0) {
    // unclear odd case, power would be dangerous
    return false;
  }
  // non-linear interpolation
  return true;
#endif
}

/// Fit table by a straight line or by inverse power function
/// (see sign_nonlinear_interpolation) and integrate the area below it.
/// The function (and the result) are assumed to be non-negative.
/// The tail is not added right here, but added after the call of this function.
/// The theshold is used to restrict this function from the left.
/// If threshold is less than e[0], the function is extrapolated
/// by the straight line till threshold.
/// If this line crosses zero, it is extrapolated only till this point.
double my_integr_fun(double xp1, double yp1, double xp2, double yp2,
                     double xmin, double /*xmax*/, double x1, double x2) {
  const bool nonlin = sign_nonlinear_interpolation(xp1, yp1, xp2, yp2, xmin);
  return nonlin ? Heed::t_integ_power_2point<double>(xp1, yp1, xp2, yp2, x1, x2)
                : Heed::t_integ_straight_2point<double>(xp1, yp1, xp2, yp2, x1,
                                                        x2, 0, 1);
}

double my_val_fun(double xp1, double yp1, double xp2, double yp2, double xmin,
                  double /*xmax*/, double x) {
  const bool nonlin = sign_nonlinear_interpolation(xp1, yp1, xp2, yp2, xmin);
  return nonlin
             ? Heed::t_value_power_2point<double>(xp1, yp1, xp2, yp2, x)
             : Heed::t_value_straight_2point<double>(xp1, yp1, xp2, yp2, x, 1);
}
}

namespace Heed {

//---------------------------------------------------------

PhotoAbsCS::PhotoAbsCS() : name(""), number(-1), Z(0), threshold(0.0) {}

PhotoAbsCS::PhotoAbsCS(const std::string& fname, int fZ, double fthreshold)
    : name(fname), number(-1), Z(fZ), threshold(fthreshold) {

  // Try to get the (shell) number from the name.
  std::istringstream ss(name);
  int i = -100;
  ss >> i;
  if (i >= 1 && i <= 50) number = i;
}

void PhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "PhotoAbsCS: name=" << name << " Z = " << Z
        << " threshold = " << threshold << std::endl;
}

//---------------------------------------------------------

AveragePhotoAbsCS::AveragePhotoAbsCS(PhotoAbsCS* apacs, double fwidth,
                                     double fstep, long fmax_q_step)
//    : real_pacs(apacs, do_clone),
    : width(fwidth),
      max_q_step(fmax_q_step),
      step(fstep) {
  mfunname("AveragePhotoAbsCS::AveragePhotoAbsCS(...)");
  check_econd11(apacs, == nullptr, mcerr);
  real_pacs.reset(apacs);
  // Check the parameters (step = 0.5 * width is bad but OK).
  if (fwidth > 0.0) check_econd11(fstep, >= 0.6 * fwidth, mcerr);
  // Copy the parameters of the "real" cross-section.
  name = real_pacs->get_name();
  number = real_pacs->get_number();
  Z = real_pacs->get_Z();
  threshold = real_pacs->get_threshold();
}

double AveragePhotoAbsCS::get_CS(double energy) const {
  mfunname("double AveragePhotoAbsCS::get_CS(double energy) const");
  // In case of zero width, return the unmodified "real" cross-section.
  if (width == 0.0) return real_pacs->get_CS(energy);
  const double w2 = width * 0.5;
  const double e1 = std::max(energy - w2, 0.);
  return real_pacs->get_integral_CS(e1, energy + w2) / width;
}

double AveragePhotoAbsCS::get_integral_CS(double energy1,
                                          double energy2) const {
  mfunname("double AveragePhotoAbsCS::get_integral_CS(...) const");
  if (width == 0.0 || energy1 >= energy2) {
    // Return the integral of the unmodified "real" cross-section.
    return real_pacs->get_integral_CS(energy1, energy2);
  }
  long q = long((energy2 - energy1) / step);
  if (q > max_q_step) {
    return real_pacs->get_integral_CS(energy1, energy2);
  }
  q++;
  const double rstep = (energy2 - energy1) / q;
  double x0 = energy1 + 0.5 * rstep;
  double s = 0.0;
  for (long n = 0; n < q; n++) s += get_CS(x0 + rstep * n);
  return s * rstep;
}

void AveragePhotoAbsCS::scale(double fact) {
  mfunname("void AveragePhotoAbsCS::scale(double fact)");
  real_pacs->scale(fact);
}

void AveragePhotoAbsCS::print(std::ostream& file, int l) const {
  mfunname("void PhotoAbsCS::print(std::ostream& file, int l) const");
  Ifile << "AveragePhotoAbsCS: width = " << width << " step=" << step
        << " max_q_step=" << max_q_step << '\n';
  indn.n += 2;
  real_pacs->print(file, l);
  indn.n -= 2;
}

//---------------------------------------------------------

HydrogenPhotoAbsCS::HydrogenPhotoAbsCS()
    : PhotoAbsCS("H", 1, 15.43e-6) {
  number = 1;
}

double HydrogenPhotoAbsCS::get_CS(double energy) const {
  if (energy < threshold || energy == DBL_MAX) return 0.0;
  // The factor 0.5 is needed because we have one atom instead of two.
  return 0.5 * prefactor * 0.0535 * (pow(100.0e-6 / energy, 3.228));
}

double HydrogenPhotoAbsCS::get_integral_CS(double e1,
                                           double e2) const {
  if (e2 < threshold) return 0.;
  if (e1 < threshold) e1 = threshold;
  const double c1 = 0.5 * 0.0535 * pow(100.0e-6, 3.228) / 2.228;
  if (e2 == DBL_MAX) {
    return prefactor * c1 * (1. / pow(e1, 2.228));
  } else {
    return prefactor * c1 * (1. / pow(e1, 2.228) - 1. / pow(e2, 2.228));
  }
}

void HydrogenPhotoAbsCS::scale(double fact) { prefactor = fact; }

void HydrogenPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "HydrogenPhotoAbsCS: name=" << name << " Z = " << Z
        << " threshold = " << threshold << std::endl;
}

//---------------------------------------------------------

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const std::string& fname, int fZ,
                                             double fthreshold,
                                             const std::string& ffile_name)
    : PhotoAbsCS(fname, fZ, fthreshold), file_name(ffile_name) {
  mfunnamep("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(...)");
  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  ener.reserve(20);
  cs.reserve(20);
  do {
    // Read the energy.
    double x = 0.;
    file >> x;
    if (!file.good()) break;
    // Make sure it is non-negative and in ascending order.
    check_econd11(x, < 0.0, mcerr);
    if (!ener.empty()) check_econd12(x, <, ener.back(), mcerr);
    // Read the cross-section.
    double y = 0.;
    file >> y;
    if (!file.good()) break;
    check_econd11(y, < 0.0, mcerr);
    // Add the point to the table.
    ener.push_back(x * 1.e-6);
    cs.push_back(y);
  } while (1);
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const std::string& fname, int fZ,
                                             double fthreshold,
                                             const std::vector<double>& fener,
                                             const std::vector<double>& fcs)
    : PhotoAbsCS(fname, fZ, fthreshold),
      file_name("none"),
      ener(fener),
      cs(fcs) {
  mfunname("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(...)");
  check_econd12(ener.size(), !=, cs.size(), mcerr);
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const std::string& fname, int fZ,
                                             double fthreshold, int l,
                                             double E0, double yw, double ya,
                                             double P, double sigma)
    : PhotoAbsCS(fname, fZ, fthreshold) {
  mfunname("SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(...)");
  const long q = 1000;
  // Make a logarithmic energy mesh.
  ener.resize(q, 0.);
  const double emin = 2.e-6;
  const double emax = 2.e-1;
  const double rk = pow(emax / emin, (1.0 / double(q)));
  double e2 = emin;
  for (long n = 0; n < q; n++) {
    const double e1 = e2;
    e2 = e2 * rk;
    ener[n] = (e1 + e2) * 0.5;
  }
  cs.assign(q, 0.);
  for (long n = 0; n < q; n++) {
    double energy = ener[n];
    if (energy < threshold) continue;
    const double Q = 5.5 + l - 0.5 * P;
    const double y = energy / E0;
    const double Fpasc = ((y - 1) * (y - 1) + yw * yw) * pow(y, (-Q)) *
                         pow((1.0 + sqrt(y / ya)), (-P));
    cs[n] = Fpasc * sigma;
  }
  remove_leading_zeros();
}

SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const SimpleTablePhotoAbsCS& total,
                                             const SimpleTablePhotoAbsCS& part,
                                             double emax_repl) {
  mfunname(
      "SimpleTablePhotoAbsCS::SimpleTablePhotoAbsCS(const "
      "SimpleTablePhotoAbsCS& total,...)");

  *this = total;  // to assure that all is preserved

  long qe_i = total.ener.size();

  const std::vector<double>& ener_r = part.get_arr_ener();
  const std::vector<double>& cs_r = part.get_arr_CS();
  long qe_r = ener_r.size();
  std::vector<double> new_ener;
  std::vector<double> new_cs;
  // first write replacements
  for (long ne = 0; ne < qe_r; ne++) {
    if (ener_r[ne] >= total.get_threshold() && ener_r[ne] <= emax_repl) {
      new_ener.push_back(ener_r[ne]);
      new_cs.push_back(cs_r[ne]);
    }
  }
  for (long ne = 0; ne < qe_i; ne++) {
    if (ener[ne] >= total.get_threshold() && ener[ne] > emax_repl) {
      new_ener.push_back(total.ener[ne]);
      new_cs.push_back(total.cs[ne]);
    }
  }
  ener.swap(new_ener);
  cs.swap(new_cs);
}

void SimpleTablePhotoAbsCS::remove_leading_zeros() {
  const long q = ener.size();
  long ne = 0;
  for (ne = 0; ne < q; ne++) {
    if (cs[ne] > 0.0) break;
  }
  if (ne <= 0) return;
  const long qn = q - ne;
  std::vector<double> enern(qn);
  std::vector<double> csn(qn);
  for (long nez = ne; nez < q; nez++) {
    enern[nez - ne] = ener[nez];
    csn[nez - ne] = cs[nez];
  }
  ener = enern;
  cs = csn;
}
void SimpleTablePhotoAbsCS::remove_leading_tiny(double level) {
  const long q = ener.size();
  long ne = 0;
  for (ne = 0; ne < q; ne++) {
    if (cs[ne] > level) break;
  }
  if (ne <= 0) return;
  const long qn = q - ne;
  std::vector<double> enern(qn);
  std::vector<double> csn(qn);
  for (long nez = ne; nez < q; nez++) {
    enern[nez - ne] = ener[nez];
    csn[nez - ne] = cs[nez];
  }
  ener = enern;
  cs = csn;
}

double SimpleTablePhotoAbsCS::get_CS(double energy) const {
  mfunname("double SimpleTablePhotoAbsCS::get_CS(double energy) const");
  long q = ener.size();
  if (q == 0) return 0.0;
  check_econd11(q, == 1, mcerr);
  if (energy < threshold) return 0.;
  if (energy <= ener[q - 1]) {
    PointCoorMesh<double, const std::vector<double> > pcm(q, &ener);
    return t_value_generic_point_ar<
        double, std::vector<double>,
        PointCoorMesh<double, const std::vector<double> > >(
        pcm, cs, &my_val_fun, energy, 1, threshold, 0, DBL_MAX);
  } else {
    if (energy == DBL_MAX)
      return 0.0;
    else
      return cs[q - 1] * pow(energy, -2.75) / pow(ener[q - 1], -2.75);
  }
}

double SimpleTablePhotoAbsCS::get_integral_CS(double energy1,
                                              double energy2) const {
  mfunname("double SimpleTablePhotoAbsCS::get_integral_CS(...)");

  const long q = ener.size();
  if (q == 0) return 0.0;
  check_econd11(q, == 1, mcerr);
  if (energy2 < threshold) return 0.0;
  if (energy1 < threshold) energy1 = threshold;
  double s = 0.0;
  double energy21 = ener[q - 1];
  if (energy1 < energy21) {
    if (energy21 > energy2) energy21 = energy2;
    check_econd12(energy1, >, energy21, mcerr);
    PointCoorMesh<double, const std::vector<double> > pcm(q, &ener);
    s = t_integ_generic_point_ar<
        double, std::vector<double>,
        PointCoorMesh<double, const std::vector<double> > >(
        pcm, cs, &my_integr_fun, energy1, energy21, 1, threshold, 0, DBL_MAX);
  }
  // print(mcout, 3);
  // mcout << "energy1="<<energy1
  //      << " energy21="<<energy21
  //      << " ener[q-1]="<<ener[q-1]
  //      << " threshold="<<threshold
  //      << " s="<<s<<'\n';
  check_econd11(s, < 0.0, mcout);
  if (energy2 > ener[q - 1]) {
    // add tail
    if (energy2 == DBL_MAX) {
      if (energy1 < ener[q - 1]) energy1 = ener[q - 1];
      double c =
          cs[q - 1] / (1.75 * pow(ener[q - 1], -2.75)) * pow(energy1, -1.75);
      // check_econd11(c , < 0.0, mcout);
      s += c;
    } else {
      if (energy1 < ener[q - 1]) energy1 = ener[q - 1];
      double c = cs[q - 1] / (1.75 * pow(ener[q - 1], -2.75)) *
                 (pow(energy1, -1.75) - pow(energy2, -1.75));
      // check_econd11(c , < 0.0, mcout);
      s += c;
    }
  }
  return s;
}

void SimpleTablePhotoAbsCS::scale(double fact) {
  mfunnamep("void SimpleTablePhotoAbsCS::scale(double fact)");
  const long q = ener.size();
  for (long n = 0; n < q; ++n) cs[n] *= fact;
}

void SimpleTablePhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "SimpleTablePhotoAbsCS: name=" << name << " Z = " << Z << "\n";
  Ifile << " threshold = " << threshold << " file_name=" << file_name << "\n";
  if (l > 1) {
    indn.n += 2;
    const long q = ener.size();
    for (long n = 0; n < q; ++n) {
      Ifile << "n=" << n << " ener=" << ener[n] << " cs=" << cs[n] << "\n";
    }
    indn.n -= 2;
  }
}

//---------------------------------------------------------

PhenoPhotoAbsCS::PhenoPhotoAbsCS() : PhotoAbsCS("none", 0, 0.0), power(0.0) {}

PhenoPhotoAbsCS::PhenoPhotoAbsCS(const std::string& fname, int fZ,
                                 double fthreshold, double fpower)
    : PhotoAbsCS(fname, fZ, fthreshold), power(fpower) {
  mfunname("PhenoPhotoAbsCS::PhenoPhotoAbsCS(...)");
  check_econd11a(power, <= 2, " value not allowed, integral would be infinite",
                 mcerr);
  const double a = power - 1.;
  factor = pow(threshold, a) * Thomas_sum_rule_const_Mb * Z * a;
}

double PhenoPhotoAbsCS::get_CS(double energy) const {
  if (energy < threshold || energy == DBL_MAX) return 0.0;
  return factor * pow(energy, -power);
}

double PhenoPhotoAbsCS::get_integral_CS(double energy1, double energy2) const {
  if (energy2 < threshold) return 0.0;
  if (energy1 < threshold) energy1 = threshold;
  const double a = power - 1.;
  double s;
  if (energy2 == DBL_MAX) {
    s = factor / a * (1. / pow(energy1, a));
  } else {
    s = factor / a * (1. / pow(energy1, a) - 1. / pow(energy2, a));
  }
  return s;
}

void PhenoPhotoAbsCS::scale(double fact) {
  mfunnamep("void PhenoPhotoAbsCS::scale(double fact)");
  factor *= fact;
}

void PhenoPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "PhenoPhotoAbsCS: name=" << name << " Z = " << Z << std::endl;
  Ifile << " threshold = " << threshold << " power=" << power
        << " factor=" << factor << std::endl;
}

//------------------------------------------------------------------------

void AtomicSecondaryProducts::add_channel(
    double fchannel_prob_dens, const std::vector<double>& felectron_energy,
    const std::vector<double>& fphoton_energy, int s_all_rest) {
  mfunnamep("void AtomicSecondaryProducts::add_channel(...)");
  check_econd21(fchannel_prob_dens, < 0.0 ||, > 1.0, mcerr);
  long q_old = channel_prob_dens.size();
  long q_new = q_old + 1;
  channel_prob_dens.resize(q_new);
  electron_energy.resize(q_new);
  photon_energy.resize(q_new);
  if (s_all_rest == 1) {
    double s = 0.0;
    for (long n = 0; n < q_old; ++n) {
      s += channel_prob_dens[n];
    }
    check_econd21(s, < 0.0 ||, > 1.0, mcerr);
    fchannel_prob_dens = 1.0 - s;
  }
  channel_prob_dens[q_old] = fchannel_prob_dens;
  electron_energy[q_old] = felectron_energy;
  photon_energy[q_old] = fphoton_energy;
  double s = 0.0;
  for (long n = 0; n < q_new; ++n) {
    s += channel_prob_dens[n];
  }
  if (s > 1.0) {
    funnw.ehdr(mcerr);
    mcerr << "s > 1.0, s=" << s << '\n';
    Iprintn(mcerr, q_new);
    for (long n = 0; n < q_new; ++n) {
      mcerr << "n=" << n << " channel_prob_dens[n]=" << channel_prob_dens[n]
            << '\n';
    }
    spexit(mcerr);
  }
}

int AtomicSecondaryProducts::get_channel(std::vector<double>& felectron_energy,
                                         std::vector<double>& fphoton_energy)
    const {
  mfunname("int AtomicSecondaryProducts::get_channel(...)");
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomicSecondaryProducts::get_channel is started\n";
  Iprintn(mcout, channel_prob_dens.size());
#endif
  if (channel_prob_dens.empty()) return 0;
  int ir = 0;
  double rn = SRANLUX();
#ifdef DEBUG_PRINT_get_escape_particles
  Iprintn(mcout, rn);
#endif
  if (channel_prob_dens.size() == 1) {
    if (rn < channel_prob_dens[0]) {
      felectron_energy = electron_energy[0];
      fphoton_energy = photon_energy[0];
      ir = 1;
    }
  } else {
    long q = channel_prob_dens.size();
    double s = 0.0;
    for (long n = 0; n < q; ++n) {
      s += channel_prob_dens[n];
      if (rn <= s) {
        felectron_energy = electron_energy[n];
        fphoton_energy = photon_energy[n];
        ir = 1;
#ifdef DEBUG_PRINT_get_escape_particles
        Iprint2n(mcout, n, s);
#endif
        break;
      }
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomicSecondaryProducts::get_channel is finishing\n";
  Iprintn(mcout, ir);
#endif
  return ir;
}

void AtomicSecondaryProducts::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "AtomicSecondaryProducts(l=" << l << "):\n";
  const long q = channel_prob_dens.size();
  Ifile << "number of channels=" << q << '\n';
  indn.n += 2;
  for (long n = 0; n < q; ++n) {
    Ifile << "n_channel=" << n << " probability=" << channel_prob_dens[n]
          << '\n';
    indn.n += 2;
    long qel = electron_energy[n].size();
    Ifile << "number of electrons=" << qel << '\n';
    indn.n += 2;
    for (long nel = 0; nel < qel; ++nel) {
      Ifile << "nel=" << nel << " electron_energy=" << electron_energy[n][nel]
            << '\n';
    }
    indn.n -= 2;
    long qph = photon_energy[n].size();
    Ifile << "number of photons=" << qph << '\n';
    indn.n += 2;
    for (long nph = 0; nph < qph; ++nph) {
      Ifile << "nph=" << nph << " photon_energy=" << photon_energy[n][nph]
            << '\n';
    }
    indn.n -= 2;
    indn.n -= 2;
  }
  indn.n -= 2;
}

AtomPhotoAbsCS::AtomPhotoAbsCS() : name("none"), Z(0), qshell(0) {}

double AtomPhotoAbsCS::get_TICS(double energy,
                                double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_TICS(...) const");
  if (factual_minimal_threshold <= energy) {
    // Above threshold, the ionization cross-section is assumed to be 
    // idential to the absorption cross-section.
    return get_ACS(energy);
  }
  return 0.0;
}

double AtomPhotoAbsCS::get_integral_TICS(
    double energy1, double energy2, double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_integral_TICS(...) const");
  if (factual_minimal_threshold > energy2) return 0.;
  energy1 = std::max(energy1, factual_minimal_threshold);
  return get_integral_ACS(energy1, energy2);
}

double AtomPhotoAbsCS::get_TICS(int nshell, double energy,
                                double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_TICS(...) const");
  if (s_ignore_shell[nshell]) return 0.;
  if (factual_minimal_threshold <= energy) {
    return get_integral_ACS(nshell, energy);
  }
  return 0.;
}

double AtomPhotoAbsCS::get_integral_TICS(
    int nshell, double energy1, double energy2,
    double factual_minimal_threshold) const {
  mfunname("double AtomPhotoAbsCS::get_integral_TICS(...) const");
  if (s_ignore_shell[nshell]) return 0.;
  if (factual_minimal_threshold <= energy1) {
    return get_integral_ACS(nshell, energy1, energy2);
  }
  if (factual_minimal_threshold >= energy2) return 0.0;
  return get_integral_ACS(nshell, factual_minimal_threshold, energy2);
}

void AtomPhotoAbsCS::remove_shell(int nshell) {
  mfunname("void AtomPhotoAbsCS::remove_shell(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  s_ignore_shell[nshell] = true;
}

void AtomPhotoAbsCS::restore_shell(int nshell) {
  mfunname("void AtomPhotoAbsCS::restore_shell(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  s_ignore_shell[nshell] = false;
}

void AtomPhotoAbsCS::print(std::ostream& file, int l) const {
  mfunnamep("void AtomPhotoAbsCS::print(std::ostream& file, int l) const");
  if (l <= 0) return;
  Ifile << "AtomPhotoAbsCS(l=" << l << "): name=" << name << " Z = " << Z
        << " qshell = " << qshell << std::endl;
  Iprintn(mcout, asp.size());
  long q = asp.size();
  if (q == 0) {
    q = s_ignore_shell.size();
    indn.n += 2;
    for (long n = 0; n < q; ++n) {
      Ifile << "n=" << n << " s_ignore_shell[n] = " << s_ignore_shell[n]
            << '\n';
    }
    indn.n -= 2;
  } else {
    check_econd12(asp.size(), !=, s_ignore_shell.size(), mcerr);
    indn.n += 2;
    for (long n = 0; n < q; ++n) {
      Ifile << "n=" << n << " s_ignore_shell[n] = " << s_ignore_shell[n]
            << '\n';
      asp[n].print(mcout, l);
    }
    indn.n -= 2;
  }
}

std::ostream& operator<<(std::ostream& file, const AtomPhotoAbsCS& f) {
  f.print(file, 1);
  return file;
}

double AtomPhotoAbsCS::get_I_min() const {
  mfunname("double AtomPhotoAbsCS::get_I_min() const");
  double st = DBL_MAX;
  // The minimal shell is normally the last, but to be safe we check all.
  for (int n = 0; n < qshell; ++n) st = std::min(st, get_threshold(n));
  return st;
}

void AtomPhotoAbsCS::get_escape_particles(
    const int nshell, double energy, std::vector<double>& el_energy,
    std::vector<double>& ph_energy) const {
  mfunname("void AtomPhotoAbsCS::get_escape_particles(...)");
#ifdef DEBUG_PRINT_get_escape_particles
  mcout << "AtomPhotoAbsCS::get_escape_particles is started\n";
  Iprintn(mcout, nshell);
  Iprintn(mcout, energy);
#endif
  // In principle, the energy is allowed to be slightly less than threshold
  // due to unprecision of definition of point-wise cross sections.
  // To keep correct norm it is better not to ignore such events.
  // They usually can be treated quite well.
  // The factor 0.5 is put there just as arbitrary check for full stupidity.
  const double thrShell = get_threshold(nshell);
  check_econd12(energy, <, 0.5 * thrShell, mcerr);

  el_energy.clear();
  ph_energy.clear();

  // Find the shell with the lowest threshold (should be the last).
  int n_min = 0;
  double thrMin = DBL_MAX;
  for (int n = 0; n < qshell; ++n) {
    if (get_threshold(n) < thrMin) {
      n_min = n;
      thrMin = get_threshold(n);
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  Iprintn(mcout, n_min);
#endif
  if (nshell == n_min) {
    // Outermost (valence) shell. Only generate the delta electron.
    const double en = std::max(energy - thrMin, 0.);
    el_energy.push_back(en);
    return;
  }
  // Energy of photo-electron
  double en = energy - thrShell;
  double hdist = 0.0;  // used to preserve the balance of energy
                       // virtual gamma are generated by energy mesh
  // and their energy could be little less than the shell energy.
  // To avoid generation of electrons with negative energy
  // their energy is corrected. The value of correction is hdist.
  // To preserve energy this hdist is then distributed over
  // the other secondary products if they exist.
  if (en < 0.0) {
    hdist = -en;
    en = 0.0;
  }
  int is = 0;
  std::vector<double> felectron_energy;
  std::vector<double> fphoton_energy;
#ifdef DEBUG_PRINT_get_escape_particles
  Iprint2n(mcout, asp.size(), get_qshell());
#endif
#ifndef DEBUG_ignore_non_standard_channels
  if (asp.size() == get_qshell()) {
    // works only in this case?
    is = asp[nshell].get_channel(felectron_energy, fphoton_energy);
    // Here zero can be if the shell is not included in database
    // or if not standard channel is not chosen by random way.
    // In both cases the standard way should be invoked.
  }
#endif
  int main_n = get_main_shell_number(nshell);
#ifdef DEBUG_PRINT_get_escape_particles
  Iprint2n(mcout, nshell, main_n);
  Iprintn(mcout, is);
  Iprint(mcout, felectron_energy);
  Iprint(mcout, fphoton_energy);
#endif

  if (is != 0) {
    // Generate photo-electron and just copy all what is proposed by
    // get_channel with corrections by hdist.
    el_energy.resize(1 + felectron_energy.size());
    el_energy[0] = en;
    long q = felectron_energy.size();
    for (long n = 0; n < q; ++n) {
      check_econd21(felectron_energy[n], < 0 ||, > thrShell, mcerr);
      el_energy[1 + n] = felectron_energy[n] - hdist;
      if (el_energy[1 + n] < 0) {
        hdist = -el_energy[1 + n];
        el_energy[1 + n] = 0.0;
      } else {
        hdist = 0.0;
      }
    }
    ph_energy.resize(fphoton_energy.size());
    q = fphoton_energy.size();
    for (long n = 0; n < q; ++n) {
      check_econd21(fphoton_energy[n], < 0 ||, > thrShell, mcerr);
      ph_energy[n] = fphoton_energy[n] - hdist;
      if (ph_energy[n] < 0) {
        hdist = -ph_energy[n];
        ph_energy[n] = 0.0;
      } else {
        hdist = 0.0;
      }
    }
    return;
  }

  // Generate default channel.
  if (main_n <= 0) {
    // Principal numbers are not available. Generate Auger to outmost shell.
    const double en1 = thrShell - hdist - 2 * thrMin;
    el_energy.push_back(en);
    if (en1 >= 0.0) el_energy.push_back(en1);
    return;
  }
  // First find the principal quantum number of the deepest shell.
  int main_n_largest = 0;
  for (int n = 0; n < qshell; ++n) {
    main_n_largest = std::max(main_n_largest, get_main_shell_number(n));
  }
#ifdef DEBUG_PRINT_get_escape_particles
  Iprintn(mcout, main_n_largest);
#endif
  if (main_n_largest - main_n < 2) {
    // Generate Auger from the outermost shell.
    double en1 = thrShell - hdist - 2 * thrMin;
    el_energy.push_back(en);
    if (en1 >= 0.0) el_energy.push_back(en1);
    return;
  }
  // At least K, l, M shells exist.
  // In this case we use more advanced scheme.
  // Look for shell with larger main number and with less energy
  int n_chosen = -1;
  double thr = DBL_MAX;  // this will be the least threshold
                         // among the shells with next principal number
  for (int n = 0; n < qshell; ++n) {
    // currently the minimal shell is the last,
    // but to avoid this assumption we check all
    int main_n_t = get_main_shell_number(n);
    if (main_n_t > 0 && main_n_t == main_n + 1) {
      if (thr > get_threshold(n)) {
        n_chosen = n;
        thr = get_threshold(n);
      }
    }
  }
#ifdef DEBUG_PRINT_get_escape_particles
  Iprint2n(mcout, n_chosen, thr);
#endif
  check_econd11(n_chosen, < 0, mcerr);
  double en1 = thrShell - hdist - 2 * get_threshold(n_chosen);
  if (en1 > 0.) {
    // Photo-electron
    el_energy.push_back(en);
    // First Auger from chosen shell
    el_energy.push_back(en1);
    // Then filling two vacancies at the next (chosen) shell
    // from the outermost one
    const double en2 = get_threshold(n_chosen) - 2 * thrMin;
    if (en2 > 0.) {
      el_energy.push_back(en2);
      el_energy.push_back(en2);
      check_econd11(el_energy[2], < 0.0, mcerr);
    }
    return;
  }
  en1 = thrShell - hdist - get_threshold(n_chosen) - thrMin;
  if (en1 > 0.) {
    // Photo-electron
    el_energy.push_back(en);
    el_energy.push_back(en1);
    // Filling initially ionized level from chosen
    // and emittance of Auger from outermost.
    check_econd11(el_energy[1], < 0.0, mcerr);
    const double en2 = get_threshold(n_chosen) - 2 * thrMin;
    if (en2 > 0.) el_energy.push_back(en2);
  }
}

AtomicSecondaryProducts* AtomPhotoAbsCS::get_asp(int nshell) {
  mfunnamep("AtomicSecondaryProducts* AtomPhotoAbsCS::get_asp(int nshell)");
  check_econd21(nshell, < 0 ||, >= qshell, mcerr);
  return &(asp[nshell]);
}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS() : AtomPhotoAbsCS() {}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ,
                                           const std::string& ffile_name)
    : file_name(ffile_name) {
  mfunnamep("SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(...)");
  check_econd11(fZ, < 1, mcerr);
  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  while (findmark(file, "#") == 1) {
    file >> Z;
    if (Z != fZ) continue;
    file >> qshell;
    check_econd21(qshell, < 1 ||, > 10000, mcerr);
    s_ignore_shell.resize(qshell, false);
    file >> name;
    m_acs.resize(qshell);
    asp.resize(qshell);
    std::vector<double> fl(qshell);
    int sZshell = 0;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      double thr = 0.0;
      int Zshell = 0;
      std::string shell_name;
      file >> thr;
      check_econd11(thr, <= 0.0, mcerr);
      file >> Zshell;
      check_econd11(Zshell, <= 0, mcerr);
      sZshell += Zshell;
      file >> fl[nshell];
      findmark(file, "!");
      file >> shell_name;
      m_acs[nshell].reset(new PhenoPhotoAbsCS(shell_name, Zshell, thr * 1.0e-6));
    }
    check_econd12(sZshell, !=, Z, mcerr);

    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      // currently the minimal shell is the last,
      // but to avoid this assumption we check all
      if (get_threshold(nshell) < st) n_min = nshell;
    }
    for (int nshell = 0; nshell < qshell; ++nshell) {
      if (fl[nshell] <= 0) continue;
      check_econd12(nshell, ==, n_min, mcerr);
      std::vector<double> felectron_energy;
      std::vector<double> fphoton_energy;
      fphoton_energy.push_back(get_threshold(nshell) - get_threshold(n_min));
      asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
    }
    return;
  }
  funnw.ehdr(mcerr);
  mcerr << "there is no element Z=" << fZ << " in file " << file_name << '\n';
  spexit(mcerr);
}

SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(int fZ, std::shared_ptr<PhotoAbsCS> facs) {
  mfunname("SimpleAtomPhotoAbsCS::SimpleAtomPhotoAbsCS(...)");
  check_econd11(facs, == nullptr, mcerr);
  check_econd11(fZ, <= 0, mcerr);
  check_econd12(fZ, !=, facs->get_Z(), mcerr);
  Z = fZ;
  qshell = 1;
  s_ignore_shell.resize(qshell, false);
  name = facs->get_name();
  m_acs.resize(1);
  m_acs[0] = std::move(facs);
}

double SimpleAtomPhotoAbsCS::get_threshold(int nshell) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_threshold(int nshell) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return m_acs[nshell]->get_threshold();
}

double SimpleAtomPhotoAbsCS::get_ACS(double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (!s_ignore_shell[n]) s += m_acs[n]->get_CS(energy);
  }
  return s;
}
double SimpleAtomPhotoAbsCS::get_integral_ACS(double energy1,
                                              double energy2) const {
  mfunnamep("double SimpleAtomPhotoAbsCS::get_integral_ACS(...) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n]) continue;
    const double t = m_acs[n]->get_integral_CS(energy1, energy2);
    if (t < 0) {
      funnw.ehdr(mcout);
      mcout << "t < 0\n";
      Iprintn(mcout, t);
      print(mcout, 4);
      spexit(mcout);
    }
    s += t;
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_ACS(int nshell, double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ACS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return s_ignore_shell[nshell] ? 0. : m_acs[nshell]->get_CS(energy);
}

double SimpleAtomPhotoAbsCS::get_integral_ACS(int nshell, double en1,
                                              double en2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ACS(...) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return s_ignore_shell[nshell] ? 0. : m_acs[nshell]->get_integral_CS(en1, en2);
}

double SimpleAtomPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ICS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (!s_ignore_shell[n]) s += m_acs[n]->get_CS(energy);
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_integral_ICS(double energy1,
                                              double energy2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ICS(...) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (!s_ignore_shell[n]) s += m_acs[n]->get_integral_CS(energy1, energy2);
  }
  return s;
}

double SimpleAtomPhotoAbsCS::get_ICS(int nshell, double energy) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_ICS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return s_ignore_shell[nshell] ? 0. : m_acs[nshell]->get_CS(energy);
}

double SimpleAtomPhotoAbsCS::get_integral_ICS(int nshell, double en1,
                                              double en2) const {
  mfunname("double SimpleAtomPhotoAbsCS::get_integral_ICS(...) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  return s_ignore_shell[nshell] ? 0. : m_acs[nshell]->get_integral_CS(en1, en2);
}

void SimpleAtomPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "SimpleAtomPhotoAbsCS(l=" << l << "): name=" << name << " Z = " << Z
        << " qshell = " << qshell << " file_name=" << file_name << std::endl;
  l--;
  if (l <= 0) return;
  indn.n += 2;
  for (int n = 0; n < qshell; ++n) {
    Ifile << "nshell=" << n << std::endl;
    m_acs[n]->print(file, l);
  }
  AtomPhotoAbsCS::print(file, l);
  indn.n -= 2;
}

//----------------------------------------------------------------------

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ,
                                   const std::string& fthreshold_file_name,
                                   const std::string& fsimple_table_file_name,
                                   const std::string& fname,
                                   double fminimal_threshold)
    : threshold_file_name(fthreshold_file_name),
      simple_table_file_name(fsimple_table_file_name),
      BT_file_name("none"),
      minimal_threshold(fminimal_threshold) {
  mfunnamep("ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(...) const");
  check_econd11(fZ, < 1, mcerr);
  std::ifstream threshold_file(threshold_file_name.c_str());
  if (!threshold_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << threshold_file_name << std::endl;
    spexit(mcerr);
  }
  std::vector<double> thr;
  std::vector<int> Zshell;
  std::vector<double> fl;
  std::vector<std::string> shell_name;
  bool foundZ = false;
  while (findmark(threshold_file, "#") == 1) {
    threshold_file >> Z;
    if (Z != fZ) continue;
    threshold_file >> qshell;
    check_econd21(qshell, < 1 ||, > 10000, mcerr);
    s_ignore_shell.resize(qshell, false);
    // Iprintn(mcout, qshell);
    thr.resize(qshell, 0.0);
    Zshell.resize(qshell, 0);
    fl.resize(qshell, 0.0);
    shell_name.resize(qshell);
    m_acs.resize(qshell);
    asp.resize(qshell);
    std::string temp_name;
    threshold_file >> temp_name;
    name = fname == "none" ? temp_name : fname;
    int sZshell = 0;
    for (int nshell = 0; nshell < qshell; nshell++) {
      threshold_file >> thr[nshell];
      check_econd11(thr[nshell], <= 0.0, mcerr);
      thr[nshell] *= 1.0e-6;
      threshold_file >> Zshell[nshell];
      check_econd11(Zshell[nshell], <= 0, mcerr);
      sZshell += Zshell[nshell];
      threshold_file >> fl[nshell];
      findmark(threshold_file, "!");
      threshold_file >> shell_name[nshell];
    }
    check_econd12(sZshell, !=, Z, mcerr);
    // currently the minimal shell is the last,
    // but to avoid this assumption, we check all.
    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; nshell++) {
      if (thr[nshell] < st) {
        n_min = nshell;
        st = thr[nshell];
      }
    }
    for (int nshell = 0; nshell < qshell; nshell++) {
      if (fl[nshell] <= 0) continue;
      check_econd12(nshell, ==, n_min, mcerr);
      std::vector<double> felectron_energy;
      std::vector<double> fphoton_energy;
      fphoton_energy.push_back(thr[nshell] - thr[n_min]);
      asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
    }
    foundZ = true;
    break;
  }
  if (!foundZ) {
    funnw.ehdr(mcerr);
    mcerr << "there is no element Z=" << fZ << " in file "
          << threshold_file_name << std::endl;
    spexit(mcerr);
  }
  // Here it reads the PACS as an one shell curve:
  SimpleTablePhotoAbsCS stpacs(name, Z, 0.0, fsimple_table_file_name);
  const std::vector<double>& ener = stpacs.get_arr_ener();
  const std::vector<double>& CS = stpacs.get_arr_CS();
  std::vector<double> left_CS = CS;  // used in sequencial algorithm
  const long qe = ener.size();
  // here cs is saved
  std::vector<std::vector<double> > SCS(qshell, std::vector<double>(qe, 0.));
  int nct = qshell - 1;   // "current" threshold index
  unsigned long nce = 0;  // "current" energy index
  // We ignore values below the lowest threshold.
  // It is not clear whether it is right, perhaps this is one of possible ways
  if (ener[0] < thr[qshell - 1]) {
    for (long ne = 0; ne < qe && (ener[ne] < thr[qshell - 1] ||
                                  (ener[ne] >= thr[qshell - 1] && ne > 1 &&
                                   CS[ne - 1] <= CS[ne - 2]));
         ne++) {
      if (ne > 0) left_CS[ne - 1] = 0.0;
      nce = ne;
    }
  }
  int s_more;
  int nt2 = 0;  // < nt1
  int s_spes = 0;
  // Actually this is a loop by the group of thresholds
  do {
    // Find all thresholds which are less then the current energy
    int nt;
    // sign that there are thresholds more than the current energy
    s_more = 0;
    for (nt = nct; nt >= 0; nt--) {
      if (s_spes == 0) {
        if (thr[nt] > ener[nce]) {
          s_more = 1;
          break;
        }
      } else {
        if (thr[nt] > ener[nce + 1]) {
          s_more = 1;
          break;
        }
      }
    }
    // nt is now index of the next threshold or -1, if the thresholds are
    // made up.
    int nt1 = nct;
    int nce_next = ener.size();
    nt2 = nt + 1;
    // mcout<<"nt="<<nt<<" nt1="<<nt1<<" nt2="<<nt2<<" s_more="<<s_more<<'\n';
    if (s_more == 1) {
      // if(nt >= 0)  // so if there are other larger thresholds,
      //{        // we should check how far we can pass at this step
      unsigned long ne;
      // finding energy larger than the next threshold
      for (ne = nce; ne < ener.size(); ne++) {
        if (thr[nt] <= ener[ne]) {
          nce_next = ne;
          s_spes = 0;
          break;
        }
        // At the following condition energy could be less than threshold,
        // but this point will anyway be associated with the next shell
        // corresponding to this threshold.
        // This is related to not precise measurement of cross section
        // and not precise knowledge of shell energies.
        // Occurence of this condition is marked by s_spes = 1.
        // At the next passing of this loop the thresholds are compared with
        // the next energy.
        if (ne > 1 && ne < ener.size() - 1 && ne > nce + 2 &&
            thr[nt] <= ener[ne + 1] &&
            (thr[nt] - ener[ne]) / (ener[ne + 1] - ener[ne]) < 0.1 &&
            CS[ne] > CS[ne - 1]) {
          // mcout<<"special condition is satisf.\n";
          nce_next = ne;
          s_spes = 1;
          break;
        }
      }
      if (ne == ener.size())  // threshold is larger then energy mesh
        s_more = 0;           // to finish the loop
    }
    // Iprintn(mcout, nce_next);
    // Iprintn(mcout, ener[nce_next-1]);
    int qt = nt1 - nt2 + 1;  // quantity of the new thresholds
    // Iprintn(mcout, qt);

    // Calculate sum of Z.
    int s = 0;
    for (nt = 0; nt < qt; nt++) {
      const int nshell = nct - nt;
      s += Zshell[nshell];
    }
    // Weights according to charges
    std::vector<double> w(qt);
    for (nt = 0; nt < qt; nt++) {
      const int nshell = nct - nt;
      w[nt] = double(Zshell[nshell]) / s;
    }
    double save_left_CS = left_CS[nce_next - 1];
    for (long ne = 0; ne < nce_next; ne++) {
      for (nt = 0; nt < qt; nt++) {
        int nshell = nct - nt;
        SCS[nshell][ne] = left_CS[ne] * w[nt];
      }
      left_CS[ne] = 0.0;
    }
    for (unsigned long ne = nce_next; ne < ener.size(); ne++) {
      double extrap_CS =
          save_left_CS * pow(ener[nce_next - 1], 2.75) / pow(ener[ne], 2.75);
      if (extrap_CS > left_CS[ne]) extrap_CS = left_CS[ne];
      for (nt = 0; nt < qt; nt++) {
        int nshell = nct - nt;
        SCS[nshell][ne] = extrap_CS * w[nt];
      }
      left_CS[ne] -= extrap_CS;
    }
    nce = nce_next;
    nct = nt2 - 1;
  } while (s_more != 0);
  // now nt2 will be index of last filled shell
  // Now to fill the shells which are absent in the input table.
  // They will be initialized phenomenologically, based on the sum rule.
  for (int ns = 0; ns < nt2; ns++) {
    m_acs[ns].reset(new PhenoPhotoAbsCS(shell_name[ns], Zshell[ns], thr[ns]));
  }
  // Initialization of input shells.
  for (int ns = nt2; ns < qshell; ns++) {
    auto adr = new SimpleTablePhotoAbsCS(shell_name[ns], Zshell[ns], thr[ns], ener, SCS[ns]);
    adr->remove_leading_zeros();
    m_acs[ns].reset(adr);
  }
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  // Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  // Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      const double threshold = m_acs[qshell - 1]->get_threshold();
      // add excitation
      exener[0] = low_boundary_of_excitations * threshold;
      exener[1] = threshold;
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > threshold) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - threshold;
          exener[1] += minimal_threshold - threshold;
        }
      }
    }
  } else if (pred_integ < integ) {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        m_acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                                   const std::string& fBT_file_name, int id,
                                   double fminimal_threshold)
    : threshold_file_name("none"),
      simple_table_file_name("none"),
      BT_file_name(fBT_file_name),
      minimal_threshold(fminimal_threshold) {
  mfunnamep(
      "ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const std::string& fname, "
      "const std::string& fBT_file_name, int id, double fminimal_threshold)");
  check_econd11(fZ, < 1, mcerr);
  check_econd21(id, < 1 ||, > 2, mcerr);

  name = fname;
  std::ifstream BT_file(BT_file_name.c_str());
  if (!BT_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << BT_file_name << std::endl;
    spexit(mcerr);
  }
  std::vector<double> thresh;
  std::vector<double> fl;
  Z = fZ;
  int i = findmark(BT_file, "NUCLEAR CHARGE =");
  check_econd11a(i, != 1, "wrong file format", mcerr);
  int Z_from_file;
  BT_file >> Z_from_file;
  check_econd12(Z_from_file, !=, Z, mcerr);
  qshell = 0;
  while ((i = findmark(BT_file, "Z =")) == 1) {
    BT_file >> i;
    check_econd11(i, != Z, mcerr);
    std::string shellname;
    BT_file >> shellname;
    // Iprintn(mcout, shellname);
    i = findmark(BT_file, "$");
    check_econd11(i, != 1, mcerr);
    long qen;
    BT_file >> qen;
    check_econd11(qen, <= 0, mcerr);
    std::vector<double> fener(qen, 0.0);
    std::vector<double> fcs(qen, 0.0);
    double thr = 0.0;
    BT_file >> thr;
    check_econd11(thr, <= 0, mcerr);
    thr *= 1.0e-3;  // pass from keV to MeV
    if (id == 2) {
      thresh.push_back(thr);
      fl.resize(fl.size() + 1);
      BT_file >> fl[qshell];
      check_econd21(fl[qshell], < 0.0 ||, > 1.0, mcerr);
      // Iprintn(mcout, fl[qshell]);
    }
    long nen;
    for (nen = 0; nen < qen; nen++) {
      BT_file >> fener[nen] >> fcs[nen];
      check_econd11(fener[nen], <= 0.0, mcerr);
      check_econd11(fcs[nen], < 0.0, mcerr);
      fener[nen] *= 1.0e-3;  // pass from keV to MeV
    }
    qshell++;
    m_acs.resize(qshell);
    m_acs.back().reset(new SimpleTablePhotoAbsCS(shellname, 0, thr, fener, fcs));
  }
  if (id == 2) {
    // a copy of similar thing from subroutine above
    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      // currently the minimal shell is the last,
      // but to avoid this assumption we check all
      if (thresh[nshell] < st) {
        n_min = nshell;
        st = thresh[nshell];
      }
    }
    asp.resize(qshell);
    for (int nshell = 0; nshell < qshell; ++nshell) {
      if (fl[nshell] > 0) {
        check_econd12(nshell, ==, n_min, mcerr);
        std::vector<double> felectron_energy;
        std::vector<double> fphoton_energy;
        fphoton_energy.push_back(thresh[nshell] - thresh[n_min]);
        asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
      }
    }
  }

  check_econd11(qshell, <= 0, mcerr);
  s_ignore_shell.resize(qshell, false);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  // Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  // Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      const double thr = m_acs[qshell - 1]->get_threshold();
      // add excitation
      exener[0] = low_boundary_of_excitations * thr;
      exener[1] = thr;
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > thr) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - thr;
          exener[1] += minimal_threshold - thr;
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        m_acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

#define READ_FILE_WITH_PRINCIPAL_NUMBERS

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const std::string& fname,
                                   const std::string& fFitBT_file_name,
                                   int id, 
                                   int s_no_scale, double fminimal_threshold)
    : threshold_file_name("none"),
      simple_table_file_name("none"),
      BT_file_name(fFitBT_file_name),
      minimal_threshold(fminimal_threshold) {
  mfunnamep(
      "ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(int fZ, const std::string& fname, "
      "const "
      "std::string& fFitBT_file_name, int id, int id1, double "
      "fminimal_threshold)");
  check_econd11(fZ, < 1, mcerr);
  check_econd21(id, < 1 ||, > 2, mcerr);
  Z = fZ;
  name = fname;
  std::ifstream BT_file(fFitBT_file_name.c_str());
  if (!BT_file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << BT_file_name << std::endl;
    spexit(mcerr);
  }
  std::vector<double> thresh;
  std::vector<double> fl;
  int i = 0;
  while ((i = findmark(BT_file, "$")) == 1) {
    long iZ;
    BT_file >> iZ;
    if (iZ != Z) continue;
    BT_file >> qshell;
    // Iprintn(mcout, qshell);
    check_econd11(qshell, <= 0, mcerr);
    check_econd11(qshell, > 1000, mcerr);
    m_acs.resize(qshell);
    if (id == 2) {
      thresh.resize(qshell);
      fl.resize(qshell);
    }
    for (int nshell = 0; nshell < qshell; ++nshell) {
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
      int n_princ = 0;
#endif
      int l;
      double threshold;
      double E0;
      double yw;
      double ya;
      double P;
      double sigma;
      if (BT_file.eof()) {
        mcerr << "unexpected end of file " << BT_file_name << '\n';
        spexit(mcerr);
      }
      if (!BT_file.good()) {
        mcerr << "bad format of file " << BT_file_name << '\n';
        spexit(mcerr);
      }
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
      BT_file >> n_princ;
      if (!BT_file.good()) {
        mcerr << "bad format of file " << BT_file_name << '\n';
        spexit(mcerr);
      }
      check_econd21(n_princ, < 0 ||, > 10, mcerr);
#endif
      BT_file >> l >> threshold >> E0 >> sigma >> ya >> P >> yw;
      check_econd11(l, < 0, mcerr);
      check_econd11(l, > 20, mcerr);
      threshold *= 1.0e-6;
      E0 *= 1.0e-6;

      check_econd11a(threshold, <= 2.0e-6,
                     "n_princ=" << n_princ << " l=" << l << '\n', mcerr);
      check_econd11(E0, <= 0, mcerr);
      double flu = 0.0;
      if (id == 2) {
        if (BT_file.eof()) {
          mcerr << "unexpected end of file " << BT_file_name << '\n';
          spexit(mcerr);
        }
        if (!BT_file.good()) {
          mcerr << "bad format of file " << BT_file_name << '\n';
          spexit(mcerr);
        }
        BT_file >> flu;
        check_econd11(flu, < 0.0, mcerr);
        check_econd11(flu, > 1.0, mcerr);
        thresh[nshell] = threshold;
        fl[nshell] = flu;
      }
#ifdef READ_FILE_WITH_PRINCIPAL_NUMBERS
      // necessary for generation escape products
      std::string shellname(std::to_string(n_princ) + " shell number " +
                            std::to_string(nshell));
#else
      std::string shellname("shell number " + std::to_string(nshell));
#endif
      m_acs[nshell].reset(new SimpleTablePhotoAbsCS(shellname, 0, threshold, 
            l, E0, yw, ya, P, sigma));
      // Iprintn(mcout, nshell);
      // Iprint3n(mcout, l, threshold, E0);
      // Iprint4n(mcout, yw, ya, P, sigma);
      // acs[nshell]->print(mcout, 5);
    }
    goto mark1;
  }
  funnw.ehdr(mcerr);
  mcerr << "there is no element Z=" << fZ << " in file " << fFitBT_file_name
        << std::endl;
  spexit(mcerr);
mark1:
  if (id == 2) {
    // a copy of similar thing from subroutine above
    int n_min = 0;
    double st = DBL_MAX;
    for (int nshell = 0; nshell < qshell; ++nshell) {
      // currently the minimal shell is the last,
      // but to avoid this assumption we check all
      if (thresh[nshell] < st) {
        n_min = nshell;
        st = thresh[nshell];
      }
    }
    asp.resize(qshell);
    for (int nshell = 0; nshell < qshell; ++nshell) {
      if (fl[nshell] > 0) {
        check_econd12(nshell, ==, n_min, mcerr);
        std::vector<double> felectron_energy;
        std::vector<double> fphoton_energy;
        fphoton_energy.push_back(thresh[nshell] - thresh[n_min]);
        asp[nshell].add_channel(fl[nshell], felectron_energy, fphoton_energy);
      }
    }
  }

  check_econd11(qshell, <= 0, mcerr);
  s_ignore_shell.resize(qshell, false);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  // Iprintn(mcout, integ);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  // Iprintn(mcout, pred_integ);
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      const double thr = m_acs[qshell - 1]->get_threshold();
      // add excitation
      exener[0] = low_boundary_of_excitations * thr;
      exener[1] = thr;
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > thr) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - thr;
          exener[1] += minimal_threshold - thr;
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1 && s_no_scale == 0) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        m_acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(
    int fZ, const std::string& fname, const std::string& fFitBT_file_name,
    const std::string& fsimple_table_file_name, double emax_repl,
    int id,  // to distinguish it from constructor above
    double fminimal_threshold) {
  mfunname("ExAtomPhotoAbsCS::ExAtomPhotoAbsCS(...)");
  Z = fZ;
  name = fname;
  int s_no_scale = 1;
  *this = ExAtomPhotoAbsCS(fZ, fname, fFitBT_file_name, id, s_no_scale,
                           fminimal_threshold);

  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;

  double thrmin = DBL_MAX;
  long nsmin = -1;
  // Look for minimal shell (usually the last).
  for (long ns = 0; ns < qshell; ++ns) {
    if (thrmin > m_acs[ns]->get_threshold()) {
      nsmin = ns;
      thrmin = m_acs[ns]->get_threshold();
    }
  }
  check_econd11(nsmin, < 0, mcerr);
  check_econd11(nsmin, != qshell - 1, mcerr);
  
  PhotoAbsCS* apacs = m_acs[nsmin].get();
  auto first_shell = dynamic_cast<SimpleTablePhotoAbsCS*>(apacs);
  check_econd11(first_shell, == nullptr, mcerr);

  SimpleTablePhotoAbsCS stpacs(name, Z, 0.0, fsimple_table_file_name);
  stpacs.remove_leading_tiny(1.0e-10);

  // Merging shells:
  SimpleTablePhotoAbsCS* merged = new SimpleTablePhotoAbsCS(*first_shell, stpacs, emax_repl); 
  m_acs[nsmin].reset(merged);

  s_ignore_shell.resize(qshell, false);
  height_of_excitation = 0.0;
  exener[0] = exener[1] = 0.0;
  double integ = get_integral_ACS(0.0, DBL_MAX);
  integ_abs_before_corr = integ;
  double pred_integ = Thomas_sum_rule_const_Mb * Z;
  if (pred_integ > integ) {
    if (s_add_excitations_to_normalize == 1) {
      const double thr = m_acs[qshell - 1]->get_threshold();
      // add excitation
      exener[0] = low_boundary_of_excitations * thr;
      exener[1] = thr;
      height_of_excitation = (pred_integ - integ) / (exener[1] - exener[0]);
      if (minimal_threshold > 0.0) {
        if (minimal_threshold > thr) {
          // currently the minimal shell is the last one
          exener[0] += minimal_threshold - thr;
          exener[1] += minimal_threshold - thr;
        }
      }
    }
  } else {
    if (s_scale_to_normalize_if_more == 1) {
      const double fact = pred_integ / integ;
      for (int nshell = 0; nshell < qshell; ++nshell) {
        m_acs[nshell]->scale(fact);
      }
    }
  }
  integ_abs_after_corr = get_integral_ACS(0.0, DBL_MAX);
  integ_ioniz_after_corr = get_integral_ICS(0.0, DBL_MAX);
}

double ExAtomPhotoAbsCS::get_threshold(int nshell) const {
  mfunname("double ExAtomPhotoAbsCS::get_threshold(int nshell) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  double r = m_acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (r < minimal_threshold) r = minimal_threshold;
  }
  return r;
}

double ExAtomPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n]) continue;
    double shift = 0.0;
    const double t = m_acs[n]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    s += m_acs[n]->get_CS(energy - shift);
  }
  return s;
}

double ExAtomPhotoAbsCS::get_integral_ICS(double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ICS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n]) continue;
    double shift = 0.0;
    const double t = m_acs[n]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    s += m_acs[n]->get_integral_CS(energy1 - shift, energy2 - shift);
  }
  return s;
}

double ExAtomPhotoAbsCS::get_ICS(int nshell, double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ICS(int nshell, double energy) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell]) return 0.;
  double shift = 0.0;
  const double t = m_acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (t < minimal_threshold) shift = minimal_threshold - t;
  }
  return m_acs[nshell]->get_CS(energy - shift);
}

double ExAtomPhotoAbsCS::get_integral_ICS(int nshell, double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ICS(int nshell, ...) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell]) return 0.;
  double shift = 0.0;
  const double t = m_acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (t < minimal_threshold) shift = minimal_threshold - t;
  }
  return m_acs[nshell]->get_integral_CS(energy1 - shift, energy2 - shift);
}

double ExAtomPhotoAbsCS::get_ACS(double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(double energy) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n]) continue;
    double shift = 0.0;
    const double t = m_acs[n]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    s += m_acs[n]->get_CS(energy - shift);
  }
  if (energy >= exener[0] && energy <= exener[1]) s += height_of_excitation;
  return s;
}

double ExAtomPhotoAbsCS::get_integral_ACS(double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ACS(...) const");
  double s = 0.0;
  for (int n = 0; n < qshell; ++n) {
    if (s_ignore_shell[n]) continue;
    double shift = 0.0;
    const double t = m_acs[n]->get_threshold();
    if (minimal_threshold > 0.0) {
      if (t < minimal_threshold) shift = minimal_threshold - t;
    }
    s += m_acs[n]->get_integral_CS(energy1 - shift, energy2 - shift);
  }
  double b[2] = {std::max(exener[0], energy1), std::min(exener[1], energy2)};
  if (b[1] >= b[0]) s += height_of_excitation * (b[1] - b[0]);
  return s;
}

double ExAtomPhotoAbsCS::get_ACS(int nshell, double energy) const {
  mfunname("double ExAtomPhotoAbsCS::get_ACS(int nshell, double energy)");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell]) return 0.;
  double shift = 0.0;
  const double t = m_acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (t < minimal_threshold) shift = minimal_threshold - t;
  }
  double s = m_acs[nshell]->get_CS(energy - shift);
  if (nshell == qshell - 1 && energy >= exener[0] && energy <= exener[1]) {
    s += height_of_excitation;
  }
  return s;
}

double ExAtomPhotoAbsCS::get_integral_ACS(int nshell, double energy1,
                                          double energy2) const {
  mfunname("double ExAtomPhotoAbsCS::get_integral_ACS(int nshell, ...) const");
  check_econd21(nshell, < 0 ||, > qshell, mcerr);
  if (s_ignore_shell[nshell]) return 0.;
  double shift = 0.0;
  const double t = m_acs[nshell]->get_threshold();
  if (minimal_threshold > 0.0) {
    if (t < minimal_threshold) shift = minimal_threshold - t;
  }
  double s = m_acs[nshell]->get_integral_CS(energy1 - shift, energy2 - shift);
  if (nshell == qshell - 1) {
    double b[2] = {std::max(exener[0], energy1), std::min(exener[1], energy2)};
    if (b[1] >= b[0]) s += height_of_excitation * (b[1] - b[0]);
  }
  return s;
}

void ExAtomPhotoAbsCS::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "ExAtomPhotoAbsCS(l=" << l << "): name=" << name << " Z = " << Z
        << " qshell = " << qshell << std::endl;
  indn.n += 2;
  Ifile << "threshold_file_name=" << threshold_file_name << '\n';
  Ifile << "simple_table_file_name=" << simple_table_file_name << '\n';
  Ifile << "BT_file_name=" << BT_file_name << std::endl;
  Ifile << "Thomas_sum_rule_const_Mb * Z = " << Thomas_sum_rule_const_Mb* Z
        << '\n';
  Ifile << "integ_abs_before_corr        = " << integ_abs_before_corr << '\n';
  Ifile << "integ_abs_after_corr         = " << integ_abs_after_corr << '\n';
  Ifile << "integ_ioniz_after_corr       = " << integ_ioniz_after_corr << '\n';
  Ifile << "height_of_excitation=" << height_of_excitation
        << " exener=" << exener[0] << ' ' << exener[1] << '\n';
  Iprintn(file, minimal_threshold);
  Ifile << "integrals by shells:\n";
  Ifile << "nshell, int(acs), int(ics)\n";
  for (long n = 0; n < qshell; n++) {
    double ainteg = get_integral_ACS(n, 0.0, DBL_MAX);
    double iinteg = get_integral_ICS(n, 0.0, DBL_MAX);
    Ifile << n << "    " << ainteg << "    " << iinteg << '\n';
  }

  if (l > 1) {
    l--;
    indn.n += 2;
    for (long n = 0; n < qshell; ++n) {
      Ifile << "nshell=" << n << std::endl;
      m_acs[n]->print(file, l);
    }
    AtomPhotoAbsCS::print(file, l);
    indn.n -= 2;
  }
  indn.n -= 2;
}

void ExAtomPhotoAbsCS::replace_shells_by_average(double fwidth, double fstep,
                                                 long fmax_q_step) {
  mfunname("void ExAtomPhotoAbsCS::replace_shells_by_average(...)");
  for (long n = 0; n < qshell; n++) {
    if (!m_acs[n]) continue;
    PhotoAbsCS* a =
        new AveragePhotoAbsCS(m_acs[n].get(), fwidth, fstep, fmax_q_step);
    m_acs[n].reset(a);
  }
}

//---------------------------------------------------------

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS* fatom, int fqatom,
                                 double fW, double fF)
    : qatom(fqatom), W(fW), F(fF) {
  qatom_ps.push_back(qatom);
  atom.push_back(fatom);
  if (W == 0.0) W = coef_I_to_W * atom[0]->get_I_min();
}

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS* fatom1, int fqatom_ps1,
                                 const AtomPhotoAbsCS* fatom2, int fqatom_ps2,
                                 double fW, double fF)
    : qatom(fqatom_ps1 + fqatom_ps2), W(fW), F(fF) {
  qatom_ps.push_back(fqatom_ps1);
  qatom_ps.push_back(fqatom_ps2);
  atom.push_back(fatom1);
  atom.push_back(fatom2);
  if (W != 0.0) return;
#ifdef CALC_W_USING_CHARGES
  W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_Z() * atom[0]->get_I_min() +
                     qatom_ps[1] * atom[1]->get_Z() * atom[1]->get_I_min()) /
      (qatom_ps[0] * atom[0]->get_Z() + qatom_ps[1] * atom[1]->get_Z());
#else
  W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_I_min() +
                     qatom_ps[1] * atom[1]->get_I_min()) /
      qatom;
#endif
}

MolecPhotoAbsCS::MolecPhotoAbsCS(const AtomPhotoAbsCS* fatom1, int fqatom_ps1,
                                 const AtomPhotoAbsCS* fatom2, int fqatom_ps2,
                                 const AtomPhotoAbsCS* fatom3, int fqatom_ps3,
                                 double fW, double fF)
    : qatom(fqatom_ps1 + fqatom_ps2 + fqatom_ps3), W(fW), F(fF) {
  qatom_ps.push_back(fqatom_ps1);
  qatom_ps.push_back(fqatom_ps2);
  qatom_ps.push_back(fqatom_ps3);
  atom.push_back(fatom1);
  atom.push_back(fatom2);
  atom.push_back(fatom3);
  if (W != 0.0) return;
#ifdef CALC_W_USING_CHARGES
  W = coef_I_to_W * (qatom_ps[0] * atom[0]->get_Z() * atom[0]->get_I_min() +
                     qatom_ps[1] * atom[1]->get_Z() * atom[1]->get_I_min() +
                     qatom_ps[2] * atom[2]->get_Z() * atom[2]->get_I_min()) /
      (qatom_ps[0] * atom[0]->get_Z() + qatom_ps[1] * atom[1]->get_Z() +
       qatom_ps[2] * atom[2]->get_Z());
#else
  W = coef_I_to_W *
      (qatom_ps[0] * atom[0]->get_I_min() + qatom_ps[1] * atom[1]->get_I_min() +
       qatom_ps[2] * atom[2]->get_I_min()) /
      qatom;
#endif
}

double MolecPhotoAbsCS::get_ACS(const double energy) const {
  mfunname("double MolecPhotoAbsCS::get_ACS(double energy) const");
  const long q = qatom_ps.size();
  double s = 0.0;
  for (long n = 0; n < q; n++) s += qatom_ps[n] * atom[n]->get_ACS(energy);
  return s;
}

double MolecPhotoAbsCS::get_integral_ACS(double en1, double en2) const {
  mfunname("double MolecPhotoAbsCS::get_integral_ACS(double e1, double e2)");
  const long q = qatom_ps.size();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_integral_ACS(en1, en2);
  }
  return s;
}

double MolecPhotoAbsCS::get_ICS(double energy) const {
  mfunname("double MolecPhotoAbsCS::get_ICS(double energy) const");
  const long q = qatom_ps.size();
  double s = 0.0;
  for (long n = 0; n < q; n++) s += qatom_ps[n] * atom[n]->get_ICS(energy);
  return s;
}

double MolecPhotoAbsCS::get_integral_ICS(double en1, double en2) const {
  mfunname("double MolecPhotoAbsCS::get_integral_ICS(double e1, double e2)");
  const long q = qatom_ps.size();
  double s = 0.0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_integral_ICS(en1, en2);
  }
  return s;
}

int MolecPhotoAbsCS::get_total_Z() const {
  mfunname("int MolecPhotoAbsCS::get_total_Z() const");
  const long q = qatom_ps.size();
  int s = 0;
  for (long n = 0; n < q; n++) {
    s += qatom_ps[n] * atom[n]->get_Z();
  }
  return s;
}

void MolecPhotoAbsCS::print(std::ostream& file, int l) const {
  Ifile << "MolecPhotoAbsCS (l=" << l << "):\n";
  Iprintn(file, qatom);
  Iprintn(file, W);
  Iprintn(file, F);
  const long q = qatom_ps.size();
  Ifile << "number of sorts of atoms is " << q << '\n';
  indn.n += 2;
  for (long n = 0; n < q; n++) {
    Ifile << "n=" << n << " qatom_ps[n]=" << qatom_ps[n] << " atom:\n";
    atom[n]->print(file, l);
  }
  indn.n -= 2;
}

std::ostream& operator<<(std::ostream& file, const MolecPhotoAbsCS& f) {
  f.print(file, 1);
  return file;
}
}
