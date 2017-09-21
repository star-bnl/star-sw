#include <iomanip>
#include <cmath>
#include "wcpplib/random/PointsRan.h"
#include "wcpplib/util/FunNameStack.h"

/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

namespace Heed {

PointsRan::PointsRan(const std::vector<double>& fx,
                     const std::vector<double>& fy, double fxmin, double fxmax)
    : xmin(fxmin), xmax(fxmax), x(fx), y(fy) {
  mfunnamep("PointsRan::PointsRan(...)");
  check_econd12(x.size(), !=, y.size(), mcerr);
  check_econd11(x.size(), < 2, mcerr);
  check_econd12(xmin, >=, xmax, mcerr);
  const long q = x.size();
  for (long n = 0; n < q - 1; n++) {
    check_econd12(x[n], >=, x[n + 1], mcerr);
  }
  for (long n = 0; n < q; n++) {
    check_econd11(y[n], < 0.0, mcerr);
  }
  iy.resize(q);
  a.resize(q - 1);
  for (long n = 0; n < q - 1; n++) {
    a[n] = (y[n + 1] - y[n]) / (x[n + 1] - x[n]);
  }
  if (xmin < x[0]) {
    double y0 = y[0] - a[0] * (x[0] - xmin);
    if (y0 < 0.0) {
      x[0] = x[0] - y[0] / a[0];
      check_econd12(xmax, <, x[0], mcerr);
      xmin = x[0];
      y[0] = 0.0;
    } else {
      y[0] = y0;
      x[0] = xmin;
    }
  }
  if (xmax > x[q - 1]) {
    double yq = y[q - 1] + a[q - 2] * (xmax - x[q - 1]);
    if (yq < 0.0) {
      x[q - 1] = x[q - 1] - y[q - 1] / a[0];
      check_econd12(xmin, >, x[q - 1], mcerr);
      xmax = x[q - 1];
      y[q - 1] = 0.0;
    } else {
      x[q - 1] = xmax;
      y[q - 1] = yq;
    }
  }
  // now xmin and xmax should be between x[0] and x[q-1]
  iy[0] = 0.0;
  integ_start = 0.0;
  n_start = 0;
  for (long n = 1; n < q; n++) {
    iy[n] = iy[n - 1] + 0.5 * (x[n] - x[n - 1]) * (y[n - 1] + y[n]);
    if (xmin >= x[n]) {
      integ_start = iy[n];
      n_start = n;
    } else if (xmin > x[n - 1]) {
      integ_start +=
          (xmin - x[n - 1]) * (y[n - 1] + 0.5 * a[n - 1] * (xmin - x[n - 1]));
      n_start = n - 1;
    }
    if (xmax >= x[n]) {
      integ_finish = iy[n];
      n_finish = n;
    } else if (xmax > x[n - 1]) {
      integ_finish +=
          (xmax - x[n - 1]) * (y[n - 1] + 0.5 * a[n - 1] * (xmax - x[n - 1]));
      n_finish = n;
    }
  }
  integ_active = integ_finish - integ_start;
  double s = iy[q - 1];
  integ_total = s;
  check_econd11(s, <= 0.0, mcerr);
}

double PointsRan::ran(double flat_ran) const {
  mfunnamep("double PointsRan::ran(double flat_ran) const");
  flat_ran = integ_start + integ_active * flat_ran;
  // long q = x.get_qel();
  long n1 = n_start;
  long n2 = n_finish;
  long n3;
  while (n2 - n1 > 1) {
    n3 = n1 + (n2 - n1) / 2;
    if (flat_ran < iy[n3]) {
      n2 = n3;
    } else {
      n1 = n3;
    }
  }
  double dran = flat_ran - iy[n1];
  // double dx = sqrt(2.0 * dran);
  double dx;
  if (a[n1] != 0.0) {
    dx = (-y[n1] + sqrt(y[n1] * y[n1] + 2.0 * a[n1] * dran)) / a[n1];
  } else {
    dx = (x[n2] - x[n1]) / (iy[n2] - iy[n1]) * dran;
  }
  // check_econd11(dx , < 0 , mcerr); // for debug
  // check_econd11(dx , > x[n2] - x[n1] , mcerr); // for debug
  double r = x[n1] + dx;
  return r;
}

void PointsRan::print(std::ostream& file) const {
  Ifile << "PointsRan:\n";
  indn.n += 2;
  Ifile << "xmin=" << xmin << " xmax=" << xmax << '\n';
  Ifile << "n_start=" << n_start << " n_finish=" << n_finish << '\n';
  Ifile << "integ_start=" << integ_start << " integ_finish=" << integ_finish
        << '\n';
  Ifile << "integ_total=" << integ_total << " integ_active=" << integ_active
        << '\n';
  // Iprintn(file, integ);
  const long q = x.size();
  Iprintn(file, q);
  for (long n = 0; n < q; n++) {
    file << std::setw(3) << n << ' ' << std::setw(12) << x[n] << ' '
         << std::setw(12) << y[n] << ' ' << std::setw(12) << iy[n];
    if (n < q - 1) file << ' ' << std::setw(12) << a[n];
    file << '\n';
  }
  indn.n -= 2;
}
}
