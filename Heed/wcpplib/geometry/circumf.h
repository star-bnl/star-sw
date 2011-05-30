#ifndef CIRCUMF_H
#define CIRCUMF_H

#include "wcpplib/geometry/vec.h"
/*
Circumference determined by point (center), vector (normal), and radious.

Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

class plane;

//             **** circumference ****


class circumf: public absref
{protected:
 
  point piv ;  // central point, pivot
  vec dir;     // normal direction, unit vector
               // circles with dir and -dir are considered the same
  vfloat rad;  // radious, >0
               // 
public:
  point Gpiv(void) const {return piv;} 
                          // It should return one any point of line
  vec Gdir(void) const {return dir;}   // return any direction
  vfloat Grad(void) const {return rad;}

protected:  
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff) // free memory
  //  { //mcout<<"straight::Garef is called\n";
  //    fqaref=2; fqareff=0; faref=&aref[0]; fareff=NULL; }
  static absref(absref::*aref[2]);

public:

  circumf();
  circumf(const point& fpiv, const vec& fdir, vfloat frad);
  circumf(const circumf &f);
  circumf& operator=(const circumf& f) {
    piv=f.piv; dir=f.dir; rad=f.rad; return *this;
  }

  friend int operator==(const circumf &f1, const circumf &f2);
  friend int operator!=(const circumf &f1, const circumf &f2)
    { return f1==f2 ? 0 : 1; }
  friend int apeq(const circumf &f1, const circumf &f2, vfloat prec);
  friend int not_apeq(const circumf &f1, const circumf &f2, 
		      vfloat prec)
    { return apeq( f1, f2, prec)==1 ? 0 : 1 ; }
  int check_point_in(const point &fp, vfloat prec) const ; 
                          // returns 1 if point on the circumference
  int cross(const plane& pn, point pt[2], vfloat prec) const ;
  // returns number of crosses and calculates pt.
  // if total circle lyes in the plane, it returns -1.
  // prec allow to switch to one point if it is almost one

  friend ostream& operator<<(ostream& file, const circumf& f);
};

ostream& operator<<(ostream& file, const circumf& f);


#endif
