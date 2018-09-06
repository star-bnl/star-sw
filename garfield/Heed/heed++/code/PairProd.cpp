#include <fstream>
#include <cmath>
#include <cfloat>
#include <climits>

#include "wcpplib/util/FunNameStack.h"
#include "wcpplib/random/ranluxint.h"
#include "heed++/code/PairProd.h"

// 2003, I. Smirnov

//#define USE_GET_ELOSS_CUT

namespace Heed {

PairProd::PairProd(const std::string& file_name, double fw, double ffano)
    : m_w(fw), m_f(ffano) {
  mfunnamep("PairProd::PairProd(const std::string&, double, double)");

  std::ifstream file(file_name.c_str());
  if (!file) {
    funnw.ehdr(mcerr);
    mcerr << "cannot open file " << file_name << std::endl;
    spexit(mcerr);
  }
  long q;
  file >> m_wtable >> m_i >> m_j >> m_ftable >> q;
  if (!file.good()) {
    funnw.ehdr(mcerr);
    mcerr << "error at reading file" << std::endl;
    spexit(mcerr);
  }
  std::vector<double> xx(q);
  std::vector<double> yy(q);
  for (long n = 0; n < q; n++) file >> xx[n] >> yy[n];
  pran = PointsRan(xx, yy, m_i, m_j);
  m_k = sqrt(m_f * m_w * m_w / (m_ftable * m_wtable * m_wtable));
  m_s = m_w - m_k * m_wtable;
  // m_s = m_w - m_w / m_f * (2. * m_f - m_ftable);
  // m_k = m_w / (m_f * m_wtable) * (2. * m_f - m_ftable);
}

double PairProd::get_eloss() const {
  mfunname("double PairProd::get_eloss() const");
  return m_k * pran.ran(SRANLUX()) + m_s;
}

#ifdef USE_GET_ELOSS_CUT

double PairProd::get_eloss(const double e_cur) const {
  mfunname("double PairProd::get_eloss(const double ecur) const");
  const double e_loss = m_k * pran.ran(SRANLUX()) + m_s;
  constexpr double w_cut_ratio = 0.2;
  return e_cur - e_loss < w_cut_ratio * m_w ? 1.0e20 : eloss;
}

#else

double PairProd::get_eloss(const double e_cur) const {
  mfunname("double PairProd::get_eloss(const double ecur) const");
  const double e_loss = m_k * pran.ran(SRANLUX()) + m_s;
  constexpr double V_ratio = 0.5;
  const double v = V_ratio * m_w / e_cur; 
  // const double c = 1. / (1. - v);
  const double c = v < 1. ? 1. / (1. - v * v) : DBL_MAX;
  return e_loss * c;
}

#endif

void PairProd::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  Ifile << "PairProd:\n";
  indn.n += 2;
  Ifile << "W=" << m_w << " Fano factor=" << m_f << '\n';
  Ifile << "W table=" << m_wtable << " Fano factor table=" << m_ftable << '\n';
  Ifile << "I=" << m_i << " J=" << m_j << " k=" << m_k << " s=" << m_s << '\n';
  pran.print(file);
  indn.n -= 2;
}
}
