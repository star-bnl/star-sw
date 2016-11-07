#include <iostream>
#include <cmath>

#include "Numerics.hh"

namespace Garfield {

namespace Numerics {

void Dfact(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir, int& ifail, double& det, int& jfail) {

  const double g1 = 1.e-19;
  const double g2 = 1.e-19;

  double tf, p, q, t, s11, s12;
  int k;

  ifail = jfail = 0;

  int nxch = 0;
  det = 1.;

  for (int j = 1; j <= n; ++j) {
    k = j;
    p = fabs(a[j - 1][j - 1]);
    if (j == n) {
      if (p <= 0.) {
        det = 0.;
        ifail = -1;
        jfail = 0;
        return;
      }
      det *= a[j - 1][j - 1];
      a[j - 1][j - 1] = 1. / a[j - 1][j - 1];
      t = fabs(det);
      if (t < g1) {
        det = 0.;
        if (jfail == 0) jfail = -1;
      } else if (t > g2) {
        det = 1.;
        if (jfail == 0) jfail = +1;
      }
      continue;
    }
    for (int i = j + 1; i <= n; ++i) {
      q = fabs(a[i - 1][j - 1]);
      if (q <= p) continue;
      k = i;
      p = q;
    }
    if (k != j) {
      for (int l = 1; l <= n; ++l) {
        tf = a[j - 1][l - 1];
        a[j - 1][l - 1] = a[k - 1][l - 1];
        a[k - 1][l - 1] = tf;
      }
      ++nxch;
      ir[nxch - 1] = j * 4096 + k;
    } else if (p <= 0.) {
      det = 0.;
      ifail = -1;
      jfail = 0;
      return;
    }
    det *= a[j - 1][j - 1];
    a[j - 1][j - 1] = 1. / a[j - 1][j - 1];
    t = fabs(det);
    if (t < g1) {
      det = 0.;
      if (jfail == 0) jfail = -1;
    } else if (t > g2) {
      det = 1.;
      if (jfail == 0) jfail = +1;
    }
    for (k = j + 1; k <= n; ++k) {
      s11 = -a[j - 1][k - 1];
      s12 = -a[k - 1][j];
      if (j == 1) {
        a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
        a[k - 1][j] = -(s12 + a[j - 1][j] * a[k - 1][j - 1]);
        continue;
      }
      for (int i = 1; i <= j - 1; ++i) {
        s11 += a[i - 1][k - 1] * a[j - 1][i - 1];
        s12 += a[i - 1][j] * a[k - 1][i - 1];
      }
      a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
      a[k - 1][j] = -a[j - 1][j] * a[k - 1][j - 1] - s12;
    }
  }

  if (nxch % 2 != 0) det = -det;
  if (jfail != 0) det = 0.;
  ir[n - 1] = nxch;
}

void Dfeqn(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir, std::vector<double>& b) {

  double te;
  double s21, s22;

  if (n <= 0) return;

  int nxch = ir[n - 1];
  if (nxch != 0) {
    for (int m = 1; m <= nxch; ++m) {
      int ij = ir[m - 1];
      int i = ij / 4096;
      int j = ij % 4096;
      te = b[i - 1];
      b[i - 1] = b[j - 1];
      b[j - 1] = te;
    }
  }

  b[0] *= a[0][0];
  if (n == 1) return;

  for (int i = 2; i <= n; ++i) {
    s21 = -b[i - 1];
    for (int j = 1; j <= i - 1; ++j) {
      s21 += a[i - 1][j - 1] * b[j - 1];
    }
    b[i - 1] = -a[i - 1][i - 1] * s21;
  }

  for (int i = 1; i <= n - 1; ++i) {
    s22 = -b[n - i - 1];
    for (int j = 1; j <= i; ++j) {
      s22 += a[n - i - 1][n - j] * b[n - j];
    }
    b[n - i - 1] = -s22;
  }
}

void Dfinv(const int n, std::vector<std::vector<double> >& a,
           std::vector<int>& ir) {

  double ti;
  double s31, s32, s33, s34;

  if (n <= 1) return;
  a[1][0] = -a[1][1] * a[0][0] * a[1][0];
  a[0][1] = -a[0][1];
  if (n > 2) {
    for (int i = 3; i <= n; ++i) {
      for (int j = 1; j <= i - 2; ++j) {
        s31 = 0.;
        s32 = a[j - 1][i - 1];
        for (int k = j; k <= i - 2; ++k) {
          s31 += a[k - 1][j - 1] * a[i - 1][k - 1];
          s32 += a[j - 1][k] * a[k][i - 1];
        }
        a[i - 1][j - 1] =
            -a[i - 1][i - 1] * (s31 + a[i - 2][j - 1] * a[i - 1][i - 2]);
        a[j - 1][i - 1] = -s32;
      }
      a[i - 1][i - 2] = -a[i - 1][i - 1] * a[i - 2][i - 2] * a[i - 1][i - 2];
      a[i - 2][i - 1] = -a[i - 2][i - 1];
    }
  }

  for (int i = 1; i <= n - 1; ++i) {
    for (int j = 1; j <= i; ++j) {
      s33 = a[i - 1][j - 1];
      for (int k = 1; k <= n - i; ++k) {
        s33 += a[i + k - 1][j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][j - 1] = s33;
    }
    for (int j = 1; j <= n - i; ++j) {
      s34 = 0.;
      for (int k = j; k <= n - i; ++k) {
        s34 += a[i + k - 1][i + j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][i + j - 1] = s34;
    }
  }

  int nxch = ir[n - 1];
  if (nxch == 0) return;

  for (int m = 1; m <= nxch; ++m) {
    int k = nxch - m + 1;
    int ij = ir[k - 1];
    int i = ij / 4096;
    int j = ij % 4096;
    for (k = 1; k <= n; ++k) {
      ti = a[k - 1][i - 1];
      a[k - 1][i - 1] = a[k - 1][j - 1];
      a[k - 1][j - 1] = ti;
    }
  }
}

//   ******************************************************************
//
//   REPLACES B BY THE SOLUTION X OF A*X=B, AND REPLACES A BY ITS IN-
//   VERSE.
//
//   n            ORDER OF THE SQUARE MATRIX IN ARRAY A.
//   A            (DOUBLE PRECISION) TWO-DIMENSIONAL ARRAY CONTAINING
//                AN n BY n MATRIX.
//
//   IFAIL        OUTPUT PARAMETER.   IFAIL= 0 ... NORMAL EXIT.
//                                    IFAIL=-1 ... SINGULAR MATRIX.
//
//   B            (DOUBLE PRECISION) ONE-DIMENSIONAL ARRAY
//
//   CALLS ... DFACT, DFINV.
//
//   ******************************************************************

void Deqinv(const int n, std::vector<std::vector<double> >& a, int& ifail,
            std::vector<double>& b) {

  double t1, t2, t3;
  double det, temp, s;
  double b1, b2, c11, c12, c13, c21, c22, c23, c31, c32, c33;

  std::vector<int> ir;
  ir.clear();
  ir.resize(n);
  for (int i = 0; i < n; ++i) ir[i] = 0;

  // TEST FOR PARAMETER ERRORS.
  if (n < 1) {
    ifail = +1;
    return;
  }

  ifail = 0;
  int jfail = 0;

  if (n > 3) {
    // n > 3 CASES.
    // FACTORIZE MATRIX, INVERT AND SOLVE SYSTEM.
    Dfact(n, a, ir, ifail, det, jfail);
    if (ifail != 0) return;
    Dfeqn(n, a, ir, b);
    Dfinv(n, a, ir);
  } else if (n == 3) {
    // n = 3 CASE.
    // COMPUTE COFACTORS.
    c11 = a[1][1] * a[2][2] - a[1][2] * a[2][1];
    c12 = a[1][2] * a[2][0] - a[1][0] * a[2][2];
    c13 = a[1][0] * a[2][1] - a[1][1] * a[2][0];
    c21 = a[2][1] * a[0][2] - a[2][2] * a[0][1];
    c22 = a[2][2] * a[0][0] - a[2][0] * a[0][2];
    c23 = a[2][0] * a[0][1] - a[2][1] * a[0][0];
    c31 = a[0][1] * a[1][2] - a[0][2] * a[1][1];
    c32 = a[0][2] * a[1][0] - a[0][0] * a[1][2];
    c33 = a[0][0] * a[1][1] - a[0][1] * a[1][0];
    t1 = fabs(a[0][0]);
    t2 = fabs(a[1][0]);
    t3 = fabs(a[2][0]);

    // SET temp = PIVOT AND det = PIVOT * det.
    if (t1 >= t2) {
      if (t3 >= t1) {
        // PIVOT IS A31
        temp = a[2][0];
        det = c23 * c12 - c22 * c13;
      } else {
        // PIVOT IS A11
        temp = a[0][0];
        det = c22 * c33 - c23 * c32;
      }
    } else {
      if (t3 >= t2) {
        // PIVOT IS A31
        temp = a[2][0];
        det = c23 * c12 - c22 * c13;
      } else {
        // PIVOT IS A21
        temp = a[1][0];
        det = c13 * c32 - c12 * c33;
      }
    }

    // SET ELEMENTS OF INVERSE IN A.
    if (det == 0.) {
      ifail = -1;
      return;
    }
    s = temp / det;
    a[0][0] = s * c11;
    a[0][1] = s * c21;
    a[0][2] = s * c31;
    a[1][0] = s * c12;
    a[1][1] = s * c22;
    a[1][2] = s * c32;
    a[2][0] = s * c13;
    a[2][1] = s * c23;
    a[2][2] = s * c33;

    // REPLACE B BY AINV*B.
    b1 = b[0];
    b2 = b[1];
    b[0] = a[0][0] * b1 + a[0][1] * b2 + a[0][2] * b[2];
    b[1] = a[1][0] * b1 + a[1][1] * b2 + a[1][2] * b[2];
    b[2] = a[2][0] * b1 + a[2][1] * b2 + a[2][2] * b[2];
  } else if (n == 2) {
    // n = 2 CASE BY CRAMERS RULE.
    det = a[0][0] * a[1][1] - a[0][1] * a[1][0];
    if (det == 0.) {
      ifail = -1;
      return;
    }
    s = 1. / det;
    c11 = s * a[1][1];
    a[0][1] = -s * a[0][1];
    a[1][0] = -s * a[1][0];
    a[1][1] = s * a[0][0];
    a[0][0] = c11;

    b1 = b[0];
    b[0] = c11 * b1 + a[0][1] * b[1];
    b[1] = a[1][0] * b1 + a[1][1] * b[1];
  } else {
    // n = 1 CASE.
    if (a[0][0] == 0.) {
      ifail = -1;
      return;
    }
    a[0][0] = 1. / a[0][0];
    b[0] = a[0][0] * b[0];
  }
}

void Cfact(const int n, std::vector<std::vector<std::complex<double> > >& a,
           std::vector<int>& ir, int& ifail, std::complex<double>& det,
           int& jfail) {

  const double g1 = 1.e-19;
  const double g2 = 1.e-19;

  std::complex<double> tf;
  double p, q, t;
  std::complex<double> s11, s12;
  int k;

  ifail = jfail = 0;

  int nxch = 0;
  det = std::complex<double>(1., 0.);

  for (int j = 1; j <= n; ++j) {
    k = j;
    p = std::max(fabs(real(a[j - 1][j - 1])), fabs(imag(a[j - 1][j - 1])));
    if (j == n) {
      if (p <= 0.) {
        det = std::complex<double>(0., 0.);
        ifail = -1;
        jfail = 0;
        return;
      }
      det *= a[j - 1][j - 1];
      a[j - 1][j - 1] = std::complex<double>(1., 0.) / a[j - 1][j - 1];
      t = std::max(fabs(real(det)), fabs(imag(det)));
      if (t < g1) {
        det = std::complex<double>(0., 0.);
        if (jfail == 0) jfail = -1;
      } else if (t > g2) {
        det = std::complex<double>(1., 0.);
        if (jfail == 0) jfail = +1;
      }
      continue;
    }
    for (int i = j + 1; i <= n; ++i) {
      q = std::max(fabs(real(a[i - 1][j - 1])), fabs(imag(a[i - 1][j - 1])));
      if (q <= p) continue;
      k = i;
      p = q;
    }
    if (k != j) {
      for (int l = 1; l <= n; ++l) {
        tf = a[j - 1][l - 1];
        a[j - 1][l - 1] = a[k - 1][l - 1];
        a[k - 1][l - 1] = tf;
      }
      ++nxch;
      ir[nxch - 1] = j * 4096 + k;
    } else if (p <= 0.) {
      det = std::complex<double>(0., 0.);
      ifail = -1;
      jfail = 0;
      return;
    }
    det *= a[j - 1][j - 1];
    a[j - 1][j - 1] = 1. / a[j - 1][j - 1];
    t = std::max(fabs(real(det)), fabs(imag(det)));
    if (t < g1) {
      det = std::complex<double>(0., 0.);
      if (jfail == 0) jfail = -1;
    } else if (t > g2) {
      det = std::complex<double>(1., 0.);
      if (jfail == 0) jfail = +1;
    }
    for (k = j + 1; k <= n; ++k) {
      s11 = -a[j - 1][k - 1];
      s12 = -a[k - 1][j];
      if (j == 1) {
        a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
        a[k - 1][j] = -(s12 + a[j - 1][j] * a[k - 1][j - 1]);
        continue;
      }
      for (int i = 1; i <= j - 1; ++i) {
        s11 += a[i - 1][k - 1] * a[j - 1][i - 1];
        s12 += a[i - 1][j] * a[k - 1][i - 1];
      }
      a[j - 1][k - 1] = -s11 * a[j - 1][j - 1];
      a[k - 1][j] = -a[j - 1][j] * a[k - 1][j - 1] - s12;
    }
  }

  if (nxch % 2 != 0) det = -det;
  if (jfail != 0) det = std::complex<double>(0., 0.);
  ir[n - 1] = nxch;
}

void Cfinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
           std::vector<int>& ir) {

  std::complex<double> ti;
  std::complex<double> s31, s32, s33, s34;

  if (n <= 1) return;
  a[1][0] = -a[1][1] * a[0][0] * a[1][0];
  a[0][1] = -a[0][1];
  if (n > 2) {
    for (int i = 3; i <= n; ++i) {
      for (int j = 1; j <= i - 2; ++j) {
        s31 = std::complex<double>(0., 0.);
        s32 = a[j - 1][i - 1];
        for (int k = j; k <= i - 2; ++k) {
          s31 += a[k - 1][j - 1] * a[i - 1][k - 1];
          s32 += a[j - 1][k] * a[k][i - 1];
        }
        a[i - 1][j - 1] =
            -a[i - 1][i - 1] * (s31 + a[i - 2][j - 1] * a[i - 1][i - 2]);
        a[j - 1][i - 1] = -s32;
      }
      a[i - 1][i - 2] = -a[i - 1][i - 1] * a[i - 2][i - 2] * a[i - 1][i - 2];
      a[i - 2][i - 1] = -a[i - 2][i - 1];
    }
  }

  for (int i = 1; i <= n - 1; ++i) {
    for (int j = 1; j <= i; ++j) {
      s33 = a[i - 1][j - 1];
      for (int k = 1; k <= n - i; ++k) {
        s33 += a[i + k - 1][j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][j - 1] = s33;
    }
    for (int j = 1; j <= n - i; ++j) {
      s34 = std::complex<double>(0., 0.);
      for (int k = j; k <= n - i; ++k) {
        s34 += a[i + k - 1][i + j - 1] * a[i - 1][i + k - 1];
      }
      a[i - 1][i + j - 1] = s34;
    }
  }

  int nxch = ir[n - 1];
  if (nxch == 0) return;

  for (int m = 1; m <= nxch; ++m) {
    int k = nxch - m + 1;
    int ij = ir[k - 1];
    int i = ij / 4096;
    int j = ij % 4096;
    for (k = 1; k <= n; ++k) {
      ti = a[k - 1][i - 1];
      a[k - 1][i - 1] = a[k - 1][j - 1];
      a[k - 1][j - 1] = ti;
    }
  }
}

//    ******************************************************************
//
//     REPLACES A BY ITS INVERSE.
//
//     (PARAMETERS AS FOR CEQINV.)
//
//     CALLS ... CFACT, CFINV.
//
//     ******************************************************************

void Cinv(const int n, std::vector<std::vector<std::complex<double> > >& a,
          int& ifail) {

  double t1, t2, t3;
  std::complex<double> det, temp, s;
  std::complex<double> c11, c12, c13, c21, c22, c23, c31, c32, c33;

  std::vector<int> ir;
  ir.clear();
  ir.resize(n);
  for (int i = 0; i < n; ++i) ir[i] = 0;

  // TEST FOR PARAMETER ERRORS.
  if (n < 1) {
    ifail = 1;
    return;
  }

  ifail = 0;
  int jfail = 0;

  if (n > 3) {
    // n > 3 CASES.
    // FACTORIZE MATRIX AND INVERT.
    Cfact(n, a, ir, ifail, det, jfail);
    if (ifail != 0) return;
    Cfinv(n, a, ir);
  } else if (n == 3) {
    // n = 3 CASE.
    // COMPUTE COFACTORS.
    c11 = a[1][1] * a[2][2] - a[1][2] * a[2][1];
    c12 = a[1][2] * a[2][0] - a[1][0] * a[2][2];
    c13 = a[1][0] * a[2][1] - a[1][1] * a[2][0];
    c21 = a[2][1] * a[0][2] - a[2][2] * a[0][1];
    c22 = a[2][2] * a[0][0] - a[2][0] * a[0][2];
    c23 = a[2][0] * a[0][1] - a[2][1] * a[0][0];
    c31 = a[0][1] * a[1][2] - a[0][2] * a[1][1];
    c32 = a[0][2] * a[1][0] - a[0][0] * a[1][2];
    c33 = a[0][0] * a[1][1] - a[0][1] * a[1][0];
    t1 = fabs(real(a[0][0])) + fabs(imag(a[0][0]));
    t2 = fabs(real(a[1][0])) + fabs(imag(a[1][0]));
    t3 = fabs(real(a[2][0])) + fabs(imag(a[2][0]));

    // SET temp = PIVOT AND det = PIVOT * det.
    if (t1 >= t2) {
      if (t3 >= t1) {
        // PIVOT IS A31
        temp = a[2][0];
        det = c23 * c12 - c22 * c13;
      } else {
        // PIVOT IS A11
        temp = a[0][0];
        det = c22 * c33 - c23 * c32;
      }
    } else {
      if (t3 >= t2) {
        // PIVOT IS A31
        temp = a[2][0];
        det = c23 * c12 - c22 * c13;
      } else {
        // PIVOT IS A21
        temp = a[1][0];
        det = c13 * c32 - c12 * c33;
      }
    }
    // SET ELEMENTS OF INVERSE IN A.
    if (real(det) == 0. && imag(det) == 0.) {
      ifail = -1;
      return;
    }
    s = temp / det;
    a[0][0] = s * c11;
    a[0][1] = s * c21;
    a[0][2] = s * c31;
    a[1][0] = s * c12;
    a[1][1] = s * c22;
    a[1][2] = s * c32;
    a[2][0] = s * c13;
    a[2][1] = s * c23;
    a[2][2] = s * c33;
  } else if (n == 2) {
    // n=2 CASE BY CRAMERS RULE.
    det = a[0][0] * a[1][1] - a[0][1] * a[1][0];
    if (real(det) == 0. && imag(det) == 0.) {
      ifail = -1;
      return;
    }
    s = std::complex<double>(1., 0.) / det;
    c11 = s * a[1][1];
    a[0][1] = -s * a[0][1];
    a[1][0] = -s * a[1][0];
    a[1][1] = s * a[0][0];
    a[0][0] = c11;
  } else {
    // n = 1 CASE.
    if (real(a[0][0]) == 0. && imag(a[0][0]) == 0.) {
      ifail = -1;
      return;
    }
    a[0][0] = std::complex<double>(1., 0.) / a[0][0];
  }
}

// Numerical integration using 15-point Gauss-Kronrod algorithm
// Origin: QUADPACK
double GaussKronrod15(double (*f)(const double), const double a,
                      const double b) {

  // Abscissae of the 15-point Kronrod rule
  // xGK[1], xGK[3], ... abscissae of the 7-point Gauss rule
  // xGK[0], xGK[2], ... abscissae which are optimally added
  //                     to the 7-point Gauss rule
  const double xGK[8] = {9.914553711208126e-01, 9.491079123427585e-01,
                         8.648644233597691e-01, 7.415311855993944e-01,
                         5.860872354676911e-01, 4.058451513773972e-01,
                         2.077849550078985e-01, 0.0e+00};
  // Weights of the 15-point Kronrod rule
  const double wGK[8] = {2.293532201052922e-02, 6.309209262997855e-02,
                         1.047900103222502e-01, 1.406532597155259e-01,
                         1.690047266392679e-01, 1.903505780647854e-01,
                         2.044329400752989e-01, 2.094821410847278e-01};
  // Weights of the 7-point Gauss rule
  const double wG[4] = {1.294849661688697e-01, 2.797053914892767e-01,
                        3.818300505051189e-01, 4.179591836734694e-01};

  // Mid-point of the interval
  const double center = 0.5 * (a + b);
  // Half-length of the interval
  const double halfLength = 0.5 * (b - a);

  double fC = f(center);
  // Result of the 7-point Gauss formula
  double resG = fC * wG[3];
  // Result of the 15-point Kronrod formula
  double resK = fC * wGK[7];

  for (int j = 0; j < 3; ++j) {
    const int i = j * 2 + 1;
    // Abscissa
    const double x = halfLength * xGK[i];
    // Function value
    const double fSum = f(center - x) + f(center + x);
    resG += wG[j] * fSum;
    resK += wGK[i] * fSum;
  }

  for (int j = 0; j < 4; ++j) {
    const int i = j * 2;
    const double x = halfLength * xGK[i];
    const double fSum = f(center - x) + f(center + x);
    resK += wGK[i] * fSum;
  }

  return resK * halfLength;
}

double Divdif(const std::vector<double>& f, const std::vector<double>& a,
              int nn, double x, int mm) {

  // C++ version of DIVDIF (CERN program library E105) which performs
  // tabular interpolation using symmetrically placed argument points.

  double t[20], d[20];

  const int mmax = 10;

  // Check the arguments.
  if (nn < 2) {
    std::cerr << "Divdif:\n";
    std::cerr << "    Array length < 2.\n";
    return 0.;
  }
  if (mm < 1) {
    std::cerr << "Divdif:\n";
    std::cerr << "    Interpolation order < 1.\n";
    return 0.;
  }

  // Deal with the case that X is located at A(1) or A(N).
  if (fabs(x - a[0]) < 1.e-6 * (fabs(a[0]) + fabs(a[nn - 1]))) {
    return f[0];
  }
  if (fabs(x - a[nn - 1]) < 1.e-6 * (fabs(a[0]) + fabs(a[nn - 1]))) {
    return f[nn - 1];
  }

  // Find subscript IX of X in array A.
  int n = nn;
  int m;
  if (mm <= mmax && mm <= n - 1) {
    m = mm;
  } else {
    if (mmax <= n - 1) {
      m = mmax;
    } else {
      m = n - 1;
    }
  }
  int mplus = m + 1;
  int ix = 0;
  int iy = n + 1;
  int mid;
  if (a[0] > a[n - 1]) {
    // Search decreasing arguments.
    do {
      mid = (ix + iy) / 2;
      if (x > a[mid - 1]) {
        iy = mid;
      } else {
        ix = mid;
      }
    } while (iy - ix > 1);
  } else {
    // Search increasing arguments.
    do {
      mid = (ix + iy) / 2;
      if (x < a[mid - 1]) {
        iy = mid;
      } else {
        ix = mid;
      }
    } while (iy - ix > 1);
  }
  //  Copy reordered interpolation points into (T[I],D[I]), setting
  //  EXTRA to True if M+2 points to be used.
  int npts = m + 2 - (m % 2);
  int ip = 0;
  int l = 0;
  int isub;
  do {
    isub = ix + l;
    if ((1 > isub) || (isub > n)) {
      // Skip point.
      npts = mplus;
    } else {
      // Insert point.
      ip++;
      t[ip - 1] = a[isub - 1];
      d[ip - 1] = f[isub - 1];
    }
    if (ip < npts) {
      l = -l;
      if (l >= 0) {
        l++;
      }
    }
  } while (ip < npts);

  bool extra = npts != mplus;
  // Replace d by the leading diagonal of a divided-difference table,
  // supplemented by an extra line if EXTRA is True.
  for (int l = 1; l <= m; l++) {
    if (extra) {
      isub = mplus - l;
      d[m + 1] = (d[m + 1] - d[m - 1]) / (t[m + 1] - t[isub - 1]);
    }
    int i = mplus;
    for (int j = l; j <= m; j++) {
      isub = i - l;
      d[i - 1] = (d[i - 1] - d[i - 1 - 1]) / (t[i - 1] - t[isub - 1]);
      i--;
    }
  }
  // Evaluate the Newton interpolation formula at X, averaging two values
  // of last difference if EXTRA is True.
  double sum = d[mplus - 1];
  if (extra) {
    sum = 0.5 * (sum + d[m + 1]);
  }
  int j = m;
  for (int l = 1; l <= m; l++) {
    sum = d[j - 1] + (x - t[j - 1]) * sum;
    j--;
  }
  return sum;
}

bool Boxin3(const std::vector<std::vector<std::vector<double> > >& value,
            const std::vector<double>& xAxis, 
            const std::vector<double>& yAxis,
            const std::vector<double>& zAxis, 
            const int nx, const int ny, const int nz, 
            const double xx, const double yy, const double zz, 
            double& f, const int iOrder) {

  // std::cout << nx << ", " << ny << ", " << nz << "\n";
  //-----------------------------------------------------------------------
  //   BOXIN3 - interpolation of order 1 and 2 in an irregular rectangular
  //            3-dimensional grid.
  //   (Last changed on 13/ 2/00.)
  //-----------------------------------------------------------------------

  int iX0 = 0, iX1 = 0;
  int iY0 = 0, iY1 = 0;
  int iZ0 = 0, iZ1 = 0;
  double fX[4], fY[4], fZ[4];

  // Ensure we are in the grid.
  const double x = std::min(std::max(xx, std::min(xAxis[0], xAxis[nx - 1])),
                            std::max(xAxis[0], xAxis[nx - 1]));
  const double y = std::min(std::max(yy, std::min(yAxis[0], yAxis[ny - 1])),
                            std::max(yAxis[0], yAxis[ny - 1]));
  const double z = std::min(std::max(zz, std::min(zAxis[0], zAxis[nz - 1])),
                            std::max(zAxis[0], zAxis[nz - 1]));

  // Make sure we have enough points.
  if (iOrder < 0 || iOrder > 2 || nx < 1 || ny < 1 || nz < 1) {
    std::cerr << "Boxin3:\n";
    std::cerr << "    Incorrect order or number of points.\n";
    std::cerr << "    No interpolation.\n";
    f = 0.;
    return false;
  }

  if (iOrder == 0 || nx == 1) {
    // Zeroth order interpolation in x.
    // Find the nearest node.
    double dist = fabs(x - xAxis[0]);
    int iNode = 0;
    for (int i = 1; i < nx; i++) {
      if (fabs(x - xAxis[i]) < dist) {
        dist = fabs(x - xAxis[i]);
        iNode = i;
      }
    }
    // Set the summing range.
    iX0 = iNode;
    iX1 = iNode;
    // Establish the shape functions.
    fX[0] = 1.;
    fX[1] = 0.;
    fX[2] = 0.;
    fX[3] = 0.;
  } else if (iOrder == 1 || nx == 2) {
    // First order interpolation in x.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < nx; i++) {
      if ((xAxis[i - 1] - x) * (x - xAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Ensure there won't be divisions by zero.
    if (xAxis[iGrid] == xAxis[iGrid - 1]) {
      std::cerr << "Boxin3:\n";
      std::cerr << "    Incorrect grid; no interpolation.\n";
      f = 0.;
      return false;
    }
    // Compute local coordinates.
    const double xLocal =
        (x - xAxis[iGrid - 1]) / (xAxis[iGrid] - xAxis[iGrid - 1]);
    // Set the summing range.
    iX0 = iGrid - 1;
    iX1 = iGrid;
    // Set the shape functions.
    fX[0] = 1. - xLocal;
    fX[1] = xLocal;
    fX[2] = 0.;
    fX[3] = 0.;
  } else if (iOrder == 2) {
    // Second order interpolation in x.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < nx; i++) {
      if ((xAxis[i - 1] - x) * (x - xAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Compute the local coordinate for this grid segment.
    const double xLocal =
        (x - xAxis[iGrid - 1]) / (xAxis[iGrid] - xAxis[iGrid - 1]);
    // Set the summing range and shape functions.
    if (iGrid == 1) {
      iX0 = iGrid - 1;
      iX1 = iGrid + 1;
      if (xAxis[iX0] == xAxis[iX0 + 1] || xAxis[iX0] == xAxis[iX0 + 2] ||
          xAxis[iX0 + 1] == xAxis[iX0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in x coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fX[0] = (x - xAxis[iX0 + 1]) * (x - xAxis[iX0 + 2]) /
              ((xAxis[iX0] - xAxis[iX0 + 1]) * (xAxis[iX0] - xAxis[iX0 + 2]));
      fX[1] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 2]) /
          ((xAxis[iX0 + 1] - xAxis[iX0]) * (xAxis[iX0 + 1] - xAxis[iX0 + 2]));
      fX[2] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 1]) /
          ((xAxis[iX0 + 2] - xAxis[iX0]) * (xAxis[iX0 + 2] - xAxis[iX0 + 1]));
    } else if (iGrid == nx - 1) {
      iX0 = iGrid - 2;
      iX1 = iGrid;
      if (xAxis[iX0] == xAxis[iX0 + 1] || xAxis[iX0] == xAxis[iX0 + 2] ||
          xAxis[iX0 + 1] == xAxis[iX0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in x coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fX[0] = (x - xAxis[iX0 + 1]) * (x - xAxis[iX0 + 2]) /
              ((xAxis[iX0] - xAxis[iX0 + 1]) * (xAxis[iX0] - xAxis[iX0 + 2]));
      fX[1] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 2]) /
          ((xAxis[iX0 + 1] - xAxis[iX0]) * (xAxis[iX0 + 1] - xAxis[iX0 + 2]));
      fX[2] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 1]) /
          ((xAxis[iX0 + 2] - xAxis[iX0]) * (xAxis[iX0 + 2] - xAxis[iX0 + 1]));
    } else {
      iX0 = iGrid - 2;
      iX1 = iGrid + 1;
      if (xAxis[iX0] == xAxis[iX0 + 1] || xAxis[iX0] == xAxis[iX0 + 2] ||
          xAxis[iX0] == xAxis[iX0 + 3] || xAxis[iX0 + 1] == xAxis[iX0 + 2] ||
          xAxis[iX0 + 1] == xAxis[iX0 + 3] ||
          xAxis[iX0 + 2] == xAxis[iX0 + 3]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in x coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fX[0] = (x - xAxis[iX0 + 1]) * (x - xAxis[iX0 + 2]) /
              ((xAxis[iX0] - xAxis[iX0 + 1]) * (xAxis[iX0] - xAxis[iX0 + 2]));
      fX[1] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 2]) /
          ((xAxis[iX0 + 1] - xAxis[iX0]) * (xAxis[iX0 + 1] - xAxis[iX0 + 2]));
      fX[2] =
          (x - xAxis[iX0]) * (x - xAxis[iX0 + 1]) /
          ((xAxis[iX0 + 2] - xAxis[iX0]) * (xAxis[iX0 + 2] - xAxis[iX0 + 1]));
      fX[0] *= (1. - xLocal);
      fX[1] = fX[1] * (1. - xLocal) + xLocal * (x - xAxis[iX0 + 2]) *
                                          (x - xAxis[iX0 + 3]) /
                                          ((xAxis[iX0 + 1] - xAxis[iX0 + 2]) *
                                           (xAxis[iX0 + 1] - xAxis[iX0 + 3]));
      fX[2] = fX[2] * (1. - xLocal) + xLocal * (x - xAxis[iX0 + 1]) *
                                          (x - xAxis[iX0 + 3]) /
                                          ((xAxis[iX0 + 2] - xAxis[iX0 + 1]) *
                                           (xAxis[iX0 + 2] - xAxis[iX0 + 3]));
      fX[3] = xLocal * (x - xAxis[iX0 + 1]) * (x - xAxis[iX0 + 2]) /
              ((xAxis[iX0 + 3] - xAxis[iX0 + 1]) *
               (xAxis[iX0 + 3] - xAxis[iX0 + 2]));
    }
  }

  if (iOrder == 0 || ny == 1) {
    // Zeroth order interpolation in y.
    // Find the nearest node.
    double dist = fabs(y - yAxis[0]);
    int iNode = 0;
    for (int i = 1; i < ny; i++) {
      if (fabs(y - yAxis[i]) < dist) {
        dist = fabs(y - yAxis[i]);
        iNode = i;
      }
    }
    // Set the summing range.
    iY0 = iNode;
    iY1 = iNode;
    // Establish the shape functions.
    fY[0] = 1.;
    fY[1] = 0.;
    fY[2] = 0.;
  } else if (iOrder == 1 || ny == 2) {
    // First order interpolation in y.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < ny; i++) {
      if ((yAxis[i - 1] - y) * (y - yAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Ensure there won't be divisions by zero.
    if (yAxis[iGrid] == yAxis[iGrid - 1]) {
      std::cerr << "Boxin3:\n";
      std::cerr << "    Incorrect grid; no interpolation.\n";
      f = 0.;
      return false;
    }
    // Compute local coordinates.
    const double yLocal =
        (y - yAxis[iGrid - 1]) / (yAxis[iGrid] - yAxis[iGrid - 1]);
    // Set the summing range.
    iY0 = iGrid - 1;
    iY1 = iGrid;
    // Set the shape functions.
    fY[0] = 1. - yLocal;
    fY[1] = yLocal;
    fY[2] = 0.;
  } else if (iOrder == 2) {
    // Second order interpolation in y.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < ny; i++) {
      if ((yAxis[i - 1] - y) * (y - yAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Compute the local coordinate for this grid segment.
    const double yLocal =
        (y - yAxis[iGrid - 1]) / (yAxis[iGrid] - yAxis[iGrid - 1]);
    // Set the summing range and shape functions.
    // These assignments are shared by all of the following conditions,
    // so it's easier to take them out.
    fY[0] = (y - yAxis[iY0 + 1]) * (y - yAxis[iY0 + 2]) /
            ((yAxis[iY0] - yAxis[iY0 + 1]) * (yAxis[iY0] - yAxis[iY0 + 2]));
    fY[1] = (y - yAxis[iY0]) * (y - yAxis[iY0 + 2]) /
            ((yAxis[iY0 + 1] - yAxis[iY0]) * (yAxis[iY0 + 1] - yAxis[iY0 + 2]));
    fY[2] = (y - yAxis[iY0]) * (y - yAxis[iY0 + 1]) /
            ((yAxis[iY0 + 2] - yAxis[iY0]) * (yAxis[iY0 + 2] - yAxis[iY0 + 1]));

    if (iGrid == 1) {
      iY0 = iGrid - 1;
      iY1 = iGrid + 1;
      if (yAxis[iY0] == yAxis[iY0 + 1] || yAxis[iY0] == yAxis[iY0 + 2] ||
          yAxis[iY0 + 1] == yAxis[iY0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in y coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fY[0] = (y - yAxis[iY0 + 1]) * (y - yAxis[iY0 + 2]) /
              ((yAxis[iY0] - yAxis[iY0 + 1]) * (yAxis[iY0] - yAxis[iY0 + 2]));
      fY[1] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 2]) /
          ((yAxis[iY0 + 1] - yAxis[iY0]) * (yAxis[iY0 + 1] - yAxis[iY0 + 2]));
      fY[2] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 1]) /
          ((yAxis[iY0 + 2] - yAxis[iY0]) * (yAxis[iY0 + 2] - yAxis[iY0 + 1]));
    } else if (iGrid == ny - 1) {
      iY0 = iGrid - 2;
      iY1 = iGrid;
      if (yAxis[iY0] == yAxis[iY0 + 1] || yAxis[iY0] == yAxis[iY0 + 2] ||
          yAxis[iY0 + 1] == yAxis[iY0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in y coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fY[0] = (y - yAxis[iY0 + 1]) * (y - yAxis[iY0 + 2]) /
              ((yAxis[iY0] - yAxis[iY0 + 1]) * (yAxis[iY0] - yAxis[iY0 + 2]));
      fY[1] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 2]) /
          ((yAxis[iY0 + 1] - yAxis[iY0]) * (yAxis[iY0 + 1] - yAxis[iY0 + 2]));
      fY[2] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 1]) /
          ((yAxis[iY0 + 2] - yAxis[iY0]) * (yAxis[iY0 + 2] - yAxis[iY0 + 1]));
    } else {
      iY0 = iGrid - 2;
      iY1 = iGrid + 1;
      if (yAxis[iY0] == yAxis[iY0 + 1] || yAxis[iY0] == yAxis[iY0 + 2] ||
          yAxis[iY0] == yAxis[iY0 + 3] || yAxis[iY0 + 1] == yAxis[iY0 + 2] ||
          yAxis[iY0 + 1] == yAxis[iY0 + 3] ||
          yAxis[iY0 + 2] == yAxis[iY0 + 3]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in y coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fY[0] = (y - yAxis[iY0 + 1]) * (y - yAxis[iY0 + 2]) /
              ((yAxis[iY0] - yAxis[iY0 + 1]) * (yAxis[iY0] - yAxis[iY0 + 2]));
      fY[1] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 2]) /
          ((yAxis[iY0 + 1] - yAxis[iY0]) * (yAxis[iY0 + 1] - yAxis[iY0 + 2]));
      fY[2] =
          (y - yAxis[iY0]) * (y - yAxis[iY0 + 1]) /
          ((yAxis[iY0 + 2] - yAxis[iY0]) * (yAxis[iY0 + 2] - yAxis[iY0 + 1]));

      fY[0] *= (1. - yLocal);
      fY[1] = fY[1] * (1. - yLocal) + yLocal * (y - yAxis[iY0 + 2]) *
                                          (y - yAxis[iY0 + 3]) /
                                          ((yAxis[iY0 + 1] - yAxis[iY0 + 2]) *
                                           (yAxis[iY0 + 1] - yAxis[iY0 + 3]));
      fY[2] = fY[2] * (1. - yLocal) + yLocal * (y - yAxis[iY0 + 1]) *
                                          (y - yAxis[iY0 + 3]) /
                                          ((yAxis[iY0 + 2] - yAxis[iY0 + 1]) *
                                           (yAxis[iY0 + 2] - yAxis[iY0 + 3]));
      fY[3] = yLocal * (y - yAxis[iY0 + 1]) * (y - yAxis[iY0 + 2]) /
              ((yAxis[iY0 + 3] - yAxis[iY0 + 1]) *
               (yAxis[iY0 + 3] - yAxis[iY0 + 2]));
    }
  }

  if (iOrder == 0 || nz == 1) {
    // Zeroth order interpolation in z.
    // Find the nearest node.
    double dist = fabs(z - zAxis[0]);
    int iNode = 0;
    for (int i = 1; i < nz; i++) {
      if (fabs(z - zAxis[i]) < dist) {
        dist = fabs(z - zAxis[i]);
        iNode = i;
      }
    }
    // Set the summing range.
    iZ0 = iNode;
    iZ1 = iNode;
    // Establish the shape functions.
    fZ[0] = 1.;
    fZ[1] = 0.;
    fZ[2] = 0.;
  } else if (iOrder == 1 || nz == 2) {
    // First order interpolation in z.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < nz; i++) {
      if ((zAxis[i - 1] - z) * (z - zAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Ensure there won't be divisions by zero.
    if (zAxis[iGrid] == zAxis[iGrid - 1]) {
      std::cerr << "Boxin3:\n";
      std::cerr << "    Incorrect grid; no interpolation.\n";
      f = 0.;
      return false;
    }
    // Compute local coordinates.
    const double zLocal =
        (z - zAxis[iGrid - 1]) / (zAxis[iGrid] - zAxis[iGrid - 1]);
    // Set the summing range.
    iZ0 = iGrid - 1;
    iZ1 = iGrid;
    // Set the shape functions.
    fZ[0] = 1. - zLocal;
    fZ[1] = zLocal;
    fZ[2] = 0.;
  } else if (iOrder == 2) {
    // Second order interpolation in z.
    // Find the grid segment containing this point.
    int iGrid = 0;
    for (int i = 1; i < nz; i++) {
      if ((zAxis[i - 1] - z) * (z - zAxis[i]) >= 0.) {
        iGrid = i;
      }
    }
    // Compute the local coordinate for this grid segment.
    const double zLocal =
        (z - zAxis[iGrid - 1]) / (zAxis[iGrid] - zAxis[iGrid - 1]);
    // Set the summing range and shape functions.
    // These assignments are shared by all of the following conditions,
    // so it's easier to take them out.
    fZ[0] = (z - zAxis[iZ0 + 1]) * (z - zAxis[iZ0 + 2]) /
            ((zAxis[iZ0] - zAxis[iZ0 + 1]) * (zAxis[iZ0] - zAxis[iZ0 + 2]));
    fZ[1] = (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 2]) /
            ((zAxis[iZ0 + 1] - zAxis[iZ0]) * (zAxis[iZ0 + 1] - zAxis[iZ0 + 2]));
    fZ[2] = (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 1]) /
            ((zAxis[iZ0 + 2] - zAxis[iZ0]) * (zAxis[iZ0 + 2] - zAxis[iZ0 + 1]));

    if (iGrid == 1) {
      iZ0 = iGrid - 1;
      iZ1 = iGrid + 1;
      if (zAxis[iZ0] == zAxis[iZ0 + 1] || zAxis[iZ0] == zAxis[iZ0 + 2] ||
          zAxis[iZ0 + 1] == zAxis[iZ0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in z coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fZ[0] = (z - zAxis[iZ0 + 1]) * (z - zAxis[iZ0 + 2]) /
              ((zAxis[iZ0] - zAxis[iZ0 + 1]) * (zAxis[iZ0] - zAxis[iZ0 + 2]));
      fZ[1] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 2]) /
          ((zAxis[iZ0 + 1] - zAxis[iZ0]) * (zAxis[iZ0 + 1] - zAxis[iZ0 + 2]));
      fZ[2] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 1]) /
          ((zAxis[iZ0 + 2] - zAxis[iZ0]) * (zAxis[iZ0 + 2] - zAxis[iZ0 + 1]));
    } else if (iGrid == nz - 1) {
      iZ0 = iGrid - 2;
      iZ1 = iGrid;
      if (zAxis[iZ0] == zAxis[iZ0 + 1] || zAxis[iZ0] == zAxis[iZ0 + 2] ||
          zAxis[iZ0 + 1] == zAxis[iZ0 + 2]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in z coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }
      fZ[0] = (z - zAxis[iZ0 + 1]) * (z - zAxis[iZ0 + 2]) /
              ((zAxis[iZ0] - zAxis[iZ0 + 1]) * (zAxis[iZ0] - zAxis[iZ0 + 2]));
      fZ[1] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 2]) /
          ((zAxis[iZ0 + 1] - zAxis[iZ0]) * (zAxis[iZ0 + 1] - zAxis[iZ0 + 2]));
      fZ[2] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 1]) /
          ((zAxis[iZ0 + 2] - zAxis[iZ0]) * (zAxis[iZ0 + 2] - zAxis[iZ0 + 1]));
    } else {
      iZ0 = iGrid - 2;
      iZ1 = iGrid + 1;

      if (zAxis[iZ0] == zAxis[iZ0 + 1] || zAxis[iZ0] == zAxis[iZ0 + 2] ||
          zAxis[iZ0] == zAxis[iZ0 + 3] || zAxis[iZ0 + 1] == zAxis[iZ0 + 2] ||
          zAxis[iZ0 + 1] == zAxis[iZ0 + 3] ||
          zAxis[iZ0 + 2] == zAxis[iZ0 + 3]) {
        std::cerr << "Boxin3:\n";
        std::cerr << "    One or more grid points in z coincide.\n";
        std::cerr << "    No interpolation.\n";
        f = 0.;
        return false;
      }

      fZ[0] = (z - zAxis[iZ0 + 1]) * (z - zAxis[iZ0 + 2]) /
              ((zAxis[iZ0] - zAxis[iZ0 + 1]) * (zAxis[iZ0] - zAxis[iZ0 + 2]));
      fZ[1] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 2]) /
          ((zAxis[iZ0 + 1] - zAxis[iZ0]) * (zAxis[iZ0 + 1] - zAxis[iZ0 + 2]));
      fZ[2] =
          (z - zAxis[iZ0]) * (z - zAxis[iZ0 + 1]) /
          ((zAxis[iZ0 + 2] - zAxis[iZ0]) * (zAxis[iZ0 + 2] - zAxis[iZ0 + 1]));

      fZ[0] *= (1. - zLocal);
      fZ[1] = fZ[1] * (1. - zLocal) + zLocal * (z - zAxis[iZ0 + 2]) *
                                          (z - zAxis[iZ0 + 3]) /
                                          ((zAxis[iZ0 + 1] - zAxis[iZ0 + 2]) *
                                           (zAxis[iZ0 + 1] - zAxis[iZ0 + 3]));
      fZ[2] = fZ[2] * (1. - zLocal) + zLocal * (z - zAxis[iZ0 + 1]) *
                                          (z - zAxis[iZ0 + 3]) /
                                          ((zAxis[iZ0 + 2] - zAxis[iZ0 + 1]) *
                                           (zAxis[iZ0 + 2] - zAxis[iZ0 + 3]));
      fZ[3] = zLocal * (z - zAxis[iZ0 + 1]) * (z - zAxis[iZ0 + 2]) /
              ((zAxis[iZ0 + 3] - zAxis[iZ0 + 1]) *
               (zAxis[iZ0 + 3] - zAxis[iZ0 + 2]));
    }
  }

  f = 0.;
  for (int i = iX0; i <= iX1; ++i) {
    for (int j = iY0; j <= iY1; ++j) {
      for (int k = iZ0; k <= iZ1; ++k) {
        // std::cout << "i = " << i << ", j = " << j << ", k = " << k << "\n";
        // std::cout << "value: " << value[i][j][k] << "\n";
        // std::cout << "fX = " << fX[i - iX0] << ", fY = " << fY[j - iY0] << ",
        // fZ = " << fZ[k - iZ0] << "\n";
        f += value[i][j][k] * fX[i - iX0] * fY[j - iY0] * fZ[k - iZ0];
      }
    }
  }
  // std::cout << f << std::endl;
  return true;
}
}
}
