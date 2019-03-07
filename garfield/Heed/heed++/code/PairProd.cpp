#include <fstream>
#include <cmath>
#include <cfloat>
#include <climits>
#include "wcpplib/random/ranluxint.h"
#include "heed++/code/PairProd.h"

// 2003, I. Smirnov

//#define USE_GET_ELOSS_CUT

namespace Heed {

PairProd::PairProd(const std::string& file_name, double fwa, double ffactorFano)
    : wa(fwa), factorFano(ffactorFano) {
  mfunnamep(
      "PairProd::PairProd(const std::string& file_name, double fwa, double "
      "ffactorFano)");

  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  long q;
  file >> wa_table >> I_table >> J_table >> factorFano_table >> q;
  if (!file.good()) {
    funnw.ehdr(mcerr);
    mcerr << "error at reading file" << std::endl;
    spexit(mcerr);
  }
  std::vector<double> xx(q);
  std::vector<double> yy(q);
  for (long n = 0; n < q; n++) file >> xx[n] >> yy[n];
  pran = PointsRan(xx, yy, I_table, J_table);
  k = sqrt(factorFano * wa * wa / (factorFano_table * wa_table * wa_table));
  s = wa - k * wa_table;
  // s = wa - wa / factorFano * ( 2.0 * factorFano - factorFano_table);
  // k = wa / (factorFano * wa_table) * ( 2.0 * factorFano - factorFano_table);
}

double PairProd::get_eloss() const {
  mfunname("double PairProd::get_eloss() const");
  return k * pran.ran(SRANLUX()) + s;
}

#ifdef USE_GET_ELOSS_CUT

double PairProd::get_eloss(const double e_cur) const {
  mfunname("double PairProd::get_eloss(const double ecur) const");
  const double e_loss = k * pran.ran(SRANLUX()) + s;
  const double w_cut_ratio = 0.2;
  return e_cur - e_loss < w_cut_ratio * wa ? 1.0e20 : eloss;
}

#else

double PairProd::get_eloss(const double e_cur) const {
  mfunname("double PairProd::get_eloss(const double ecur) const");
  const double e_loss = k * pran.ran(SRANLUX()) + s;
  const double V_ratio = 0.5;
  const double v = V_ratio * wa / e_cur; 
  // const double c = 1. / (1. - v);
  const double c = v < 1. ? 1. / (1. - v * v) : DBL_MAX;
  return e_loss * c;
}

#endif

void PairProd::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "PairProd:\n";
  indn.n += 2;
  Ifile << "wa=" << wa << " factorFano=" << factorFano << '\n';
  Ifile << "wa_table=" << wa_table << " factorFano_table=" << factorFano_table
        << '\n';
  Ifile << " I_table=" << I_table << " J_table=" << J_table << " k=" << k
        << " s=" << s << '\n';
  pran.print(file);
  indn.n -= 2;
}
}
