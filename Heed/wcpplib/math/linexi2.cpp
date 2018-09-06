#include <cfloat>
#include "wcpplib/math/linexi2.h"
#include "wcpplib/util/FunNameStack.h"
/*
Copyright (c) 2000 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file for any purpose
is hereby granted without fee, provided that the above copyright notice,
this permission notice, and notices about any modifications of the original
text appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

namespace Heed {

linexi2_coor::linexi2_coor(const long fqlr, const double* fax)
    : qlr(fqlr), ax(fax) {
  x_mean = 0;
  Dx = 0;
  for (long n = 0; n < qlr; n++) {
    x_mean += ax[n];
    Dx += ax[n] * ax[n];
  }
  x_mean /= qlr;
  Dx /= qlr;
  Dx = Dx - x_mean * x_mean;
}
std::ostream& operator<<(std::ostream& file, const linexi2_coor& l) {
  Ifile << "linexi2_coor: qlr=" << l.qlr << " x_mean=" << l.x_mean
        << " Dx=" << l.Dx << '\n';
  for (int n = 0; n < l.qlr; n++) Ifile << "n=" << n << " x=" << l.ax[n] << '\n';
  return file;
}
linexi2::linexi2(const linexi2_coor& lc, const double* fay)
    : linexi2_coor(lc), ay(fay) {
  y_mean = 0;
  xy_mean = 0;
  for (long n = 0; n < qlr; n++) {
    y_mean += ay[n];
    xy_mean += ax[n] * ay[n];
  }
  y_mean /= qlr;
  xy_mean /= qlr;
  if (Dx > 0)
    a = (xy_mean - x_mean * y_mean) / Dx;
  else
    a = DBL_MAX;
  b = y_mean - a * x_mean;
}
linexi2::linexi2(const long fqlr, const double* fax, const double* fay)
    : linexi2_coor(fqlr, fax), ay(fay) {
  y_mean = 0;
  xy_mean = 0;
  for (long n = 0; n < qlr; n++) {
    y_mean += ay[n];
    xy_mean += ax[n] * ay[n];
  }
  y_mean /= qlr;
  xy_mean /= qlr;
  if (Dx > 0)
    a = (xy_mean - x_mean * y_mean) / Dx;
  else
    a = DBL_MAX;
  b = y_mean - a * x_mean;
}
std::ostream& operator<<(std::ostream& file, const linexi2& l) {
  Ifile << "linexi2_coor: qlr=" << l.qlr << '\n';
  Ifile << "x_mean=" << l.x_mean << " Dx=" << l.Dx << '\n';
  Ifile << "y_mean=" << l.y_mean << " xy_mean=" << l.xy_mean << '\n';
  Ifile << "a=" << l.a << " b=" << l.b << '\n';
  for (int n = 0; n < l.qlr; n++)
    Ifile << "n=" << n << " x=" << l.ax[n] << " y=" << l.ay[n] << '\n';
  return file;
}
linexi2B::linexi2B(linexi2& lx) : linexi2(lx) {
  mfunname("linexi2B::linexi2B(linexi2& lx)");
  B = (double**)new double[lx.qlr * lx.qlr];
  for (long i = 0; i < qlr; i++)
    for (long m = 0; m < qlr; m++)
      B[i][m] = (ax[i] - x_mean) * (ax[m] - x_mean) / Dx + 1;
}
void linexi2B::copy(const linexi2B& lxB) {
  *this = (linexi2&)lxB;  // '=' redefined
  for (long i = 0; i < qlr; i++)
    for (long m = 0; m < qlr; m++) B[i][m] = lxB.B[i][m];
}
linexi2B::linexi2B(const linexi2B& lxB) : linexi2((linexi2&)lxB) {
  mfunname("linexi2B::linexi2B(const linexi2B& lxB)");
  B = (double**)new double[lxB.qlr * lxB.qlr];
  for (long i = 0; i < qlr; i++)
    for (long m = 0; m < qlr; m++) B[i][m] = lxB.B[i][m];
}
}
