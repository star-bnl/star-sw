#include "wcpplib/geometry/plane.h"
/*
Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

namespace Heed {

// **** plane ****

absref absref::*(plane::aref[2]) = {(absref absref::*)&plane::piv,
                                    (absref absref::*)&plane::dir};

absref_transmit plane::get_components() {
  return absref_transmit(2, aref);
}

plane::plane(const straight& sl, const point& pt) : piv(sl.Gpiv()), dir() {
  pvecerror("plane::plane( const straight& sl, const point& pt)");
  int i = sl.check_point_in(pt, 0.0);
  if (i != 0)
    vecerror = 1;
  else
    dir = unit_vec(sl.Gdir() || (pt - sl.Gpiv()));
}
plane::plane(const straight& sl1, const straight& sl2, vfloat prec)
    : piv(sl1.Gpiv()), dir() {
  pvecerror(
      "plane::plane( const straight& sl1, const straight& sl2, vfloat prec)");
  point pt = sl1.cross(sl2, prec);
  if (vecerror == 0) {
    piv = pt;
    dir = unit_vec(sl1.Gdir() || sl2.Gdir());
  } else if (vecerror == 2)  // different parallel lines
  {
    vecerror = 0;
    dir = unit_vec(sl1.Gdir() || (sl2.Gpiv() - sl1.Gpiv()));
  }
  // otherwise vecerror != 0
}

int operator==(const plane& pl1, const plane& pl2) {
  pvecerror("int operator==(const plane &pl1, const plane &pl2)");

  if (!(pl1.dir == pl2.dir || pl1.dir == -pl2.dir)) return 0;
  if (pl1.piv == pl2.piv) return 1;
  if (pl1.check_point_in(pl2.piv, 0) == 1)
    return 1;
  else
    return 0;
}

bool apeq(const plane& pl1, const plane& pl2, vfloat prec) {
  pvecerror("bool apeq(const plane &pl1, const plane &pl2, vfloat prec)");
  if (check_par(pl1.dir, pl2.dir, prec) == 0) return false;
  if (apeq(pl1.piv, pl2.piv, prec)) return true;
  return (pl1.check_point_in(pl2.piv, prec) == 1);
}

int plane::check_point_in(const point& fp, vfloat prec) const {
  pvecerror("int plane::check_point_in(point fp, vfloat prec)");
  vfloat f = distance(fp);
  if (f < prec) return 1;
  return 0;
}

point plane::cross(const straight& sl) const {
  pvecerror("point plane::cross(straight &sl)");
  point slpiv = sl.Gpiv();
  vec sldir = sl.Gdir();
  vfloat r = dir * sldir;
  if (r == 0.0) {
    if (slpiv == piv || check_perp((piv - slpiv), dir, 0.0) == 1) {
      // Line is in plane
      vecerror = 3;
    } else {
      vecerror = 2;
    }
    return point();
  }

  vfloat t = (piv.v - slpiv.v) * dir;
  return point(slpiv.v + t / r * sldir);
}

straight plane::cross(const plane& pl) const {
  pvecerror("point plane::cross(plane &pl)");
  point plpiv = pl.Gpiv();
  vec pldir = pl.Gdir();
  vec a = dir || pldir;  // direction of the overall straight lines
  if (a.length() == 0) {
    if (plpiv == piv || check_par(pldir, dir, 0.0) != 0) {  // planes coinsides
      vecerror = 3;
      return straight();
    } else {
      vecerror = 2;
      return straight();
    }
  }
  a = unit_vec(a);
  vec c = a || dir;  // perpend. for ov. str.
  straight st(piv, c);
  point pt = pl.cross(st);  // one point on ov. str.
  return straight(pt, a);  // overall straight
}

int plane::cross(const polyline& pll, point* crpt, int& qcrpt, polyline* crpll,
                 int& qcrpll, vfloat prec) const {
  pvecerror("int plane::cross(polyline &pll, ...");

  qcrpt = 0;
  qcrpll = 0;
  for (int n = 0; n < pll.qsl; n++) {
    point cpt = cross(pll.sl[n]);
    if (vecerror == 3)  // the line is in the plane
      crpll[qcrpll++] = polyline(&(pll.pt[n]), 2);
    else if (vecerror != 0)
      vecerror = 0;
    else {
      vec v1 = cpt - pll.pt[n];
      if (v1.length() < prec) {
        if (n == 0)  // otherwise it is probably included on the previous step
        {
          crpt[qcrpt++] = cpt;
        }
      } else {
        vec v2 = cpt - pll.pt[n + 1];
        if (v2.length() < prec)
          crpt[qcrpt++] = cpt;
        else if (check_par(v1, v2, prec) == -1)
          // anti-parallel vectors, point inside borders
          crpt[qcrpt++] = cpt;
      }
    }
  }
  if (qcrpt > 0 || qcrpll > 0)
    return 1;
  else
    return 0;
}

vfloat plane::distance(const point& fpt) const {
  pvecerror("vfloat plane::distance(point& fpt)");
  if (fpt == piv) return 0.0;
  vec v = fpt - piv;
  return fabs(v * dir);  // relys that dir is unit length vector
}

std::ostream& operator<<(std::ostream& file, const plane& pl) {
  Ifile << "plane:\n";
  indn.n += 2;
  file << pl.piv << pl.dir;
  indn.n -= 2;
  return file;
}
}
