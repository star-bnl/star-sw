#ifndef LINEXI2_H
#define LINEXI2_H

#include <iostream>

/*
Drawing straight line by a range of points by method xi2

Copyright (c) 2000 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file for any purpose
is hereby granted without fee, provided that the above copyright notice,
this permission notice, and notices about any modifications of the original
text appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

namespace Heed {

class linexi2_coor {
 public:
  long qlr;
  const double* ax;  // address of coordinates, is not copied
  double x_mean;
  double Dx;
  linexi2_coor(const long fqlr, const double* fax);
  linexi2_coor(const linexi2_coor& lc)
      : qlr(lc.qlr), ax(lc.ax), x_mean(lc.x_mean), Dx(lc.Dx) {
    ;
  }
  linexi2_coor& operator=(const linexi2_coor& lc) {
    qlr = lc.qlr;
    ax = lc.ax;
    x_mean = lc.x_mean;
    Dx = lc.Dx;
    return *this;
  }
};
std::ostream& operator<<(std::ostream& file, const linexi2_coor& l);

class linexi2 : public linexi2_coor {
 public:
  const double* ay;
  double y_mean;
  double xy_mean;
  double a;
  double b;
  linexi2(const linexi2_coor& lc, const double* fay);
  linexi2(const long fqlr, const double* fax, const double* fay);
  linexi2& operator=(linexi2& lx) {
    *((linexi2_coor*)this) = (linexi2_coor&)lx;
    ay = lx.ay;
    y_mean = lx.y_mean;
    xy_mean = lx.xy_mean;
    a = lx.a;
    b = lx.b;
    return *this;
  }
  linexi2(linexi2& lx) : linexi2_coor(lx) {
    ay = lx.ay;
    y_mean = lx.y_mean;
    xy_mean = lx.xy_mean;
    a = lx.a;
    b = lx.b;
  }
  double line(const double x) { return a * x + b; }
};
std::ostream& operator<<(std::ostream& file, const linexi2& l);

class linexi2B : public linexi2 {
 public:
  double** B;
  linexi2B(linexi2& lx);
  ~linexi2B() { delete B; }
  void copy(const linexi2B& lxB);
  linexi2B(const linexi2B& lxB);
  linexi2B& operator=(const linexi2B& lxB) {
    copy(lxB);
    return *this;
  }
};
}
#endif
