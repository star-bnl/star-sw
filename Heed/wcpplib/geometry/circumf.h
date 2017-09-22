#ifndef CIRCUMF_H
#define CIRCUMF_H

#include "wcpplib/geometry/vec.h"

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

class plane;

/// Circumference, determined by point (center), vector (normal), and radius.
class circumf : public absref {
 protected:
  /// Central point, pivot.
  point piv;   
  /// Normal direction, unit vector.
  /// Circles with dir and -dir are considered the same.
  vec dir;     
  /// radius, >0.
  vfloat rad;  

 public:
  point Gpiv(void) const { return piv; }
  vec Gdir(void) const { return dir; }
  vfloat Grad(void) const { return rad; }
 protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  static absref(absref::*aref[2]);

 public:
  /// Default constructor.
  circumf();
  /// Constructor.
  circumf(const point& fpiv, const vec& fdir, vfloat frad);
  circumf(const circumf& f);
  circumf& operator=(const circumf& f) {
    piv = f.piv;
    dir = f.dir;
    rad = f.rad;
    return *this;
  }
  /// Destructor
  virtual ~circumf() {}
  friend int operator==(const circumf& f1, const circumf& f2);
  friend int operator!=(const circumf& f1, const circumf& f2) {
    return f1 == f2 ? 0 : 1;
  }
  friend int apeq(const circumf& f1, const circumf& f2, vfloat prec);
  friend int not_apeq(const circumf& f1, const circumf& f2, vfloat prec) {
    return apeq(f1, f2, prec) == 1 ? 0 : 1;
  }
  /// Return 1 if point on the circumference.
  int check_point_in(const point& fp, vfloat prec) const;
  // return number of crosses and calculates pt.
  // if total circle lies in the plane, it returns -1.
  // prec allow to switch to one point if it is almost one
  int cross(const plane& pn, point pt[2], vfloat prec) const;

  friend std::ostream& operator<<(std::ostream& file, const circumf& f);
};
std::ostream& operator<<(std::ostream& file, const circumf& f);
}

#endif
