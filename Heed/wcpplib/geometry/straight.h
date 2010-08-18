#ifndef STRAIGHT_H
#define STRAIGHT_H
/*
Definition of straight line, as combination of vector and point.

Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice, 
and notices about any modifications of the original text 
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

#include "wcpplib/geometry/vec.h"

class plane;

//             **** straight ****


class straight: public absref
{protected:
  // internals are private, because it should be another way to define the
  // straight line. We want the program to be independant on way.
  // The same line can have different piv's along it, and different vec's:
  // dir or -dir. Vector supposed to be of unit length.
 
  point piv ;  // origin point, pivot
  vec dir;     // direction, unit vector
               // 
public:
  point Gpiv(void) const {return piv;} 
                          // It should return one any point of line
  vec Gdir(void) const {return dir;}   // return any direction

protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  
  //virtual void Garef(int& fqaref , absref absref::**&faref, //fixed memory
  //	            int& fqareff, absref **&fareff) // free memory
  //  { //mcout<<"straight::Garef is called\n";
  //    fqaref=2; fqareff=0; faref=&aref[0]; fareff=NULL; }
  static absref(absref::*aref[2]);

public:

  straight():piv(),dir(){;}
  straight(const point& fpiv, const vec& fdir):
    piv(fpiv), dir(unit_vec(fdir)) { ; }
  //straight(const straight &fsl):
  //  piv(fsl.piv), dir(fsl.dir) { ; }
  straight& operator=(const straight& fsl)
    {   //pvecerror(" straight& operator=(const straight& fsl)");
      piv=fsl.piv; dir=fsl.dir; return *this; }
  straight(const point& fp1, const point& fp2):
    piv(fp1), dir() 
    { pvecerror("straight::straight(const point& fp1, const point& fp2)");
    check_econd12(fp1 , == , fp2 , mcerr); 
    dir = unit_vec(fp2-fp1); 
    }
  straight(const plane pl1, const plane pl2);
  // different parallel     vecerror=2
  // the same planes        vecerror=3

  straight(const point* pt, int qpt, int anum);  // interpolates by xi2
  // residuals are calculated in planes normal to axis which is measured.
  // This axis is given by anum. 0 - x, 1 - y, 2 - z.   
  // Unless I've mistaken, the line should necessary be directed
  // toward increasing of this axis.

  straight(const straight sl[4], point pt[2], vfloat prec);
  // Draws line via four lines by interpolation.
  // pt[2] are starting points for two intermidiate layers

  // The same line can have different piv's along it, and different vec's:
  // dir or -dir.  
  friend int operator==(const straight &sl1, const straight &sl2);
  friend int operator!=(const straight &sl1, const straight &sl2)
    { return sl1==sl2 ? 0 : 1; }
  friend int apeq(const straight &sl1, const straight &sl2, vfloat prec);
  friend int not_apeq(const straight &sl1, const straight &sl2, 
		      vfloat prec)
    { return apeq( sl1, sl2, prec)==1 ? 0 : 1 ; }
  int check_point_in(const point &fp, vfloat prec) const ; 
  // returns 1 if point in the straight line. Calculates distance
  // and compares it with prec

  point cross(const straight& sl, vfloat prec) const ;
  // figure out whether there is cross, and calculate point, if there is.
  // good cross in one point (with precision prec)   vecerror=0
  // not crossed lines                               vecerror=1
  // different parallel(exactly) lines               vecerror=2
  // the same(exactly) line (piv and dir may differ) vecerror=3
  // prec set up maximal distance at which lines are considered crossed

  vfloat vecdistance(const straight& sl, int& type_of_cross, point pt[2]) 
    const;
  // shortest distance between lines, may be negative.
  // type_of_cross has same meaning as vecerror from previous function,
  // But the precision is assumed to be 0.
  // pt inited only for type_of_cross == 1 and 0.
  // For type_of_cross == 0 pt[0]==pt[1]
  // pt[0] is point on this line. pt[1] it point on line sl.
  // It draws
  // ez = unit_vec(this->dir) 
  // ey = unit_vec(this->dir || sl.dir) 
  // ex = ey || ez
  // and declares syscoor with this->piv. 
  // vecdistance is just y-coordinate of point of crossing of sl converting 
  // to new syscoor with plane (ey, ez).

  vfloat distance(const straight& sl, int& type_of_cross, point pt[2]) const ;
  // shortest distance between lines, always positive.
  // type_of_cross has same meaning as vecerror from previous function
  // But the precision is assumed to be 0.
  // pt is inited only for type_of_cross == 1 and 0.
  // For type_of_cross == 0 pt[0]==pt[1]
  // pt[0] is point on this line. pt[1] is point on line sl.
  // It is absolute value of vecdistance

  vfloat distance(const point& fpt) const ;
  vfloat distance(const point& fpt, point& fcpt) const ;
  // calculates closest point on the line

  point vecdistance(const vec normal, const straight& slt);
    // space position of cross of plane with normal, may be negative
    // not debugged

  straight(straight* sl,  // array of lines via which it need to draw 
	                  // this line 
	   int qsl,       // number of lines in array
	   const straight& sl_start, // first approximation 
	   int anum,      // prolong axis : 0 - x, 1 - y, 2 - z. 
	   vfloat precision, // wanted precision
	   vfloat* dist,     // array of distances, 
	                     // they may be negative as in vecdistance
	                     // For vecdistance this is this line
	   point (*pt)[2],   // points,  pt[][0] is point on this line
	                     // pt[][1] is point on line sl.
	   vfloat& mean2dist); // mean square distance
    // The constructor draws straight line via qsl lines by xi-2 method
    // residuals are calculated in planes normal to axis which is measured.
    // This axis is given by anum. 0 - x, 1 - y, 2 - z.   
    // The algorithm finds closest points in sl[] to this line and
    // draws new this line by call of 
    // straight(const point* pt, int qpt, int anum);  // interpolates by xi2
    // This is being done in loop while 
    //  while(mean2dist_prev<mean2dist || 
    //	  (mean2dist != 0 && mean2dist_prev-mean2dist>precision) );

  friend ostream& operator<<(ostream& file, const straight& s);
};

ostream& operator<<(ostream& file, const straight& s);


#endif
