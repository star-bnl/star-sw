#ifndef CUBIC_H
#define CUBIC_H

#include <complex>

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

using std::complex;
typedef complex<float> float_complex;
typedef complex<double> double_complex;
typedef complex<long double> long_double_complex;

class Cubic {
 public:
  inline double a(void) const { return da; }
  inline double b(void) const { return db; }
  inline double c(void) const { return dc; }
  inline double d(void) const { return dd; }
  inline double s_xzero(void) const {
    return s_dxzero;
  }  // for debug
  inline void put_a(double fa) {
    da = fa;
    s_dxzero = 0;
  }
  inline void put_b(double fb) {
    db = fb;
    s_dxzero = 0;
  }
  inline void put_c(double fc) {
    dc = fc;
    s_dxzero = 0;
  }
  inline void put_d(double fd) {
    dd = fd;
    s_dxzero = 0;
  }

  Cubic(void) : da(0.0), db(0.0), dc(0.0), dd(0.0), s_dxzero(0) {}
  Cubic(double fa, double fb, double fc, double fd)
      : da(fa), db(fb), dc(fc), dd(fd), s_dxzero(0) {}

  inline double y(double x) const {
    return da * x * x * x + db * x * x + dc * x + dd;
  }

  void find_zero(double_complex& z1, double_complex& z2,
                 double_complex& z3) const;
  // They are not ordered and analysed
  int find_real_zero(double z[3]) const;
  // returns number of solutions
  // Analysed and ordered real solutions

  // returns number of solutions
  // first is the least.
  int find_maxmin(double xmm[2], double ymm[2],
                  int s_mm[2]) const;  // 1 - maximum, -1 - minimum, 0 - non
 private:
  double da, db, dc, dd;
  mutable int s_dxzero;
  mutable double_complex dz1;
  mutable double_complex dz2;
  mutable double_complex dz3;
};

std::ostream& operator<<(std::ostream& file, const Cubic& f);

#endif
