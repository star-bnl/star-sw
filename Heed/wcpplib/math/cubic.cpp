#include <cfloat>
#include <iomanip>
#include "wcpplib/math/cubic.h"
#include "wcpplib/math/parabol.h"
#include "wcpplib/util/FunNameStack.h"

/*
Copyright (c) 2005 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

namespace Heed {

const Cubic::double_complex Cubic::iu(0, 1);

void Cubic::find_zero(double_complex& z1, double_complex& z2,
                      double_complex& z3) const {
  mfunname("void Cubic::find_zero(...) const");
  const Cubic& t = (*this);
  if (s_dxzero != 0) {
    z1 = dz1;
    z2 = dz2;
    z3 = dz3;
    return;
  }

  check_econd11a(da, == 0.0, "this is not cubic polynomial!", mcerr);
  double a2 = db / da;
  double a1 = dc / da;
  double a0 = dd / da;
  double Q = (3.0 * a1 - a2 * a2) / 9.0;
  double R = (9.0 * a2 * a1 - 27.0 * a0 - 2.0 * a2 * a2 * a2) / 54.0;
  double D = Q * Q * Q + R * R;
  double sD = sqrt(fabs(D));
  double_complex S;
  double_complex T;
  if (D >= 0.0) {
    double tt = R + sD;
    if (tt > 0.0) {
      S = pow(tt, 1 / 3.0);
    } else if (tt < 0.0) {
      S = -pow(-tt, 1 / 3.0);
    } else {
      S = 0.0;
    }
    tt = R - sD;
    if (tt > 0.0) {
      T = pow(tt, 1 / 3.0);
    } else if (tt < 0.0) {
      T = -pow(-tt, 1 / 3.0);
    } else {
      T = 0.0;
    }
  } else {
    S = pow(R + iu * sD, 1 / 3.0);
    T = pow(R - iu * sD, 1 / 3.0);
  }
  z1 = -a2 / 3.0 + (S + T);
  z2 = -a2 / 3.0 - (S + T) / 2.0 + 0.5 * iu * std::sqrt(3.0) * (S - T);
  z3 = -a2 / 3.0 - (S + T) / 2.0 - 0.5 * iu * std::sqrt(3.0) * (S - T);
  t.dz1 = z1;
  t.dz2 = z2;
  t.dz3 = z3;
  t.s_dxzero = 3;
}

int Cubic::find_real_zero(double z[3]) const {
  mfunname("int Cubic::find_real_zero(double z[3]) const");
  double_complex zc1;
  double_complex zc2;
  double_complex zc3;
  find_zero(zc1, zc2, zc3);
  double thresh = 10.0 * DBL_MIN;
  int q = 0;
  if (fabs(zc1.imag()) < thresh ||
      (zc1.real() != 0.0 && fabs(zc1.imag() / zc1.real()) < thresh)) {
    z[q] = zc1.real();
    q++;
  }
  if (fabs(zc2.imag()) < thresh ||
      (zc2.real() != 0.0 && fabs(zc2.imag() / zc2.real()) < thresh)) {
    z[q] = zc2.real();
    q++;
  }
  if (fabs(zc3.imag()) < thresh ||
      (zc3.real() != 0.0 && fabs(zc3.imag() / zc3.real()) < thresh)) {
    z[q] = zc3.real();
    q++;
  }
  int n2 = 0;
  for (int n1 = 0; n1 < q - 1; n1++) {
    for (n2 = n1; n2 < q; n2++) {
      if (z[n1] > z[n2]) {
        double t = z[n1];
        z[n1] = z[n2];
        z[n2] = t;
      }
    }
  }
  for (int n1 = 0; n1 < q - 1; n1++) {
    // TODO: debug.
    if ((fabs(z[n1]) < thresh && fabs(z[n2]) < thresh) ||
        fabs((z[n1] - z[n1 + 1]) / (z[n1] + z[n1 + 1])) < thresh) {
      for (n2 = n1 + 1; n2 < q - 1; n2++) {
        z[n2] = z[n2 + 1];
      }
      q--;
      n1--;
    }
  }
  return q;
}

int Cubic::find_maxmin(double xmm[2], double ymm[2], int s_mm[2]) const {
  mfunname(
      "int Cubic::find_maxmin(double xmm[2], double ymm[2], int s_mm[2]) "
      "const");
  double ap = 3 * da;
  double bp = 2 * db;
  double cp = dc;
  Parabol par(ap, bp, cp);
  s_mm[0] = 0;
  s_mm[1] = 0;
  int qz = par.find_zero(xmm);
  if (qz == 1) {
    s_mm[0] = 0;
  }
  if (qz == 2) {
    if (a() > 0) {
      s_mm[0] = 1;
      s_mm[1] = -1;
    } else {
      s_mm[0] = -1;
      s_mm[1] = 1;
    }
  }
  for (int n = 0; n < qz; ++n) {
    ymm[n] = y(xmm[n]);
  }
  return qz;
}

std::ostream& operator<<(std::ostream& file, const Cubic& f) {
  Cubic::double_complex z1;
  Cubic::double_complex z2;
  Cubic::double_complex z3;
  Ifile << "Cubic: s_xzero=" << f.s_xzero() << '\n';
  indn.n += 2;
  f.find_zero(z1, z2, z3);
  Ifile << "Cubic: a=" << f.a() << " b=" << f.b() << " c=" << f.c()
        << " d=" << f.d() << '\n';
  file << " z1,2,3=" << z1 << ' ' << z2 << ' ' << z3 << '\n';
  double r[3];
  int q;
  q = f.find_real_zero(r);
  Ifile << "The number of real zeros =" << q << '\n';
  int n;
  Ifile << "Solutions=";
  for (n = 0; n < q; n++) file << ' ' << r[n];
  file << '\n';
  double xmm[2];
  double ymm[2];
  int s_mm[2];
  q = f.find_maxmin(xmm, ymm, s_mm);
  Ifile << "Max/Min, q=" << q << '\n';
  indn.n += 2;
  for (n = 0; n < q; n++) {
    Ifile << "n=" << n << " xmm[n]=" << std::setw(13) << xmm[n]
          << " ymm[n]=" << std::setw(13) << ymm[n]
          << " s_mm[n]=" << std::setw(13) << s_mm[n] << '\n';
  }
  indn.n -= 2;
  indn.n -= 2;

  return file;
}
}
