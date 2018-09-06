#include "wcpplib/geometry/circumf.h"
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

absref absref::*(circumf::aref[2]) = {(absref absref::*)&circumf::piv,
                                      (absref absref::*)&circumf::dir};

circumf::circumf() : piv(), dir(), rad(0) {}
circumf::circumf(const point& fpiv, const vec& fdir, vfloat frad)
    : piv(fpiv), dir(), rad(frad) {
  pvecerror("circumf(...)");
  check_econd11(fdir.length(), == 0, mcerr);
  dir = unit_vec(fdir);
}
circumf::circumf(const circumf& f)
    : absref(f), piv(f.piv), dir(f.dir), rad(f.rad) {}

absref_transmit circumf::get_components() {
  return absref_transmit(2, aref);
}

int operator==(const circumf& f1, const circumf& f2) {
  pvecerror("int operator==(const circumf &f1, const circumf &f2)");

  if (!(f1.dir == f2.dir || f1.dir == -f2.dir)) return 0;
  if (f1.piv == f2.piv && f1.rad == f2.rad)
    return 1;
  else
    return 0;
}

bool apeq(const circumf& f1, const circumf& f2, vfloat prec) {
  pvecerror("bool apeq(const circumf &f1, const circumf &f2, vfloat prec)");
  if (check_par(f1.dir, f2.dir, prec) == 0) return false;
  return apeq(f1.piv, f2.piv, prec) && apeq(f1.rad, f2.rad, prec);
}

int circumf::check_point_in(const point& fp, vfloat prec) const {
  // returns 1 if point on the circumference
  pvecerror("int circumf::check_point_in(const point &fp, vfloat prec) const");
  vec d = fp - piv;
  if (check_perp(d, dir, prec) != 1) return 0;
  if (apeq(d.length(), rad)) return 1;
  return 0;
}
int circumf::cross(const plane& pn, point pt[2], vfloat prec) const {
  pvecerror("int circumf::cross(const plane& pn, point pt[2]) const");
  if (pn.distance(piv) > rad) return 0;  // to avoid cross at very far pn
  plane pnc(piv, dir);
  straight sl(pnc, pn);
  if (vecerror == 3) {
    vecerror = 0;
    return -1;
  }
  if (vecerror == 2) {
    vecerror = 0;
    return 0;
  }
  point closest_pt;
  vfloat d = sl.distance(piv, closest_pt);
  if (apeq(d, rad, prec)) {
    pt[0] = closest_pt;
    return 1;
  }
  if (d > rad) return 0;
  vfloat cat = sqrt(rad * rad - d * d);
  pt[0] = closest_pt + cat * sl.Gdir();
  pt[1] = closest_pt - cat * sl.Gdir();
  return 2;
}

std::ostream& operator<<(std::ostream& file, const circumf& f) {
  Ifile << "circumf(erence):\n";
  indn.n += 2;
  Ifile << "rad=" << f.rad << '\n';
  file << f.piv << f.dir;
  indn.n -= 2;
  return file;
}
}
