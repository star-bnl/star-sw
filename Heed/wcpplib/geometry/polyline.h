#ifndef POLYLINE_H
#define POLYLINE_H
#include "wcpplib/geometry/vec.h"
#include "wcpplib/geometry/straight.h"
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

// **** polyline  ****

#define vec_polyline_index 5

class polyline : public absref {
 protected:
  int qpt;
  point* pt;
  int qsl;
  straight* sl;

 public:
  int Gqpt(void) const { return qpt; }
  point Gpt(int n) const {  // there is no funname line, check directly
                            //check_econd12(n , >= , qpt , mcerr);
    if (n >= qpt) {
      mcerr << "error in polyline:Gpt(int n): n>qpt: n=" << n << " qpt=" << qpt
            << '\n';
      spexit(mcerr);
    }
    return pt[n];
  }
  int Gqsl(void) const { return qsl; }
  straight Gsl(int n) const {
    if (n >= qsl) {
      mcerr << "error in polyline:Gpt(int n): n>qsl: n=" << n << " qsl=" << qsl
            << '\n';
      spexit(mcerr);
    }
    return sl[n];
  }

 protected:
  absref** aref;
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);

 public:

  int check_point_in(const point& fpt, vfloat prec) const;
  //            0 point is not in
  //            1 point coincides with an edge
  //            2 point is inside an interval

  int cross(const straight& fsl, point* pc, int& qpc, polyline* pl, int& qpl,
            vfloat prec) const;
  //If straight line goes exactly by segment of polyline,
  //the fuction gives two end points of adjacent segments and the
  //segment itself.
  //If one of the points is common, it is given several times.
  //For example, if line crosses break point the point is given two times.
  vfloat dist_two_inter(polyline& pl, vfloat prec) const;
  // Distance between two intervals, polylines with one segment of line
  // and two points
  vfloat distance(const point& fpt) const;
  vfloat distance(const point& fpt, point& cpt) const;

 protected:
  void polyline_init(const point* fpt, int fqpt);
  //void polyline_init(straight* fsl, int fqsl);
  void polyline_del(void) {
    if (pt != NULL) {
      delete[] pt;
      pt = NULL;
    }
    if (sl != NULL) {
      delete[] sl;
      sl = NULL;
    }
    if (aref != NULL) {
      delete[] aref;
      aref = NULL;
    }
  }

 public:
  polyline(void) {
    point ptl;
    polyline_init(&ptl, 0);
  }

  polyline(polyline& pl);
  polyline(const polyline& pl);

  polyline(const point* fpt, int fqpt);
  polyline(const point& fpt1, const point& fpt2);  // interval

  polyline& operator=(const polyline& fpl);

  ~polyline(void) { polyline_del(); }
  friend int plane::cross(const polyline& pll, point* crpt, int& qcrpt,
                          polyline* crpll, int& qcrpll, vfloat prec) const;
  friend std::ostream& operator<<(std::ostream& file, const polyline& p);

};

int cross4pllines(const polyline pl[4], vfloat precision, straight& sl,
                  point ptc[4][2]);
// Draws straight line via 4 intervals.
// returns 1 if line is drawn and 0 otherwise

std::ostream& operator<<(std::ostream& file, const polyline& p);

// **** polyline in plane  ****

class polyline_pl : public polyline {
 protected:
  plane pn;

 public:
  plane Gpn(void) const { return pn; }

 protected:
  static absref(absref::* aref_pl);
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
 public:
  polyline_pl(void) : polyline(), pn() { ; }
  polyline_pl(const polyline_pl& pl) : polyline(pl), pn(pl.pn) { ; }
  polyline_pl(const plane& fpn, const point* fpt, int fqpt)
      : polyline(fpt, fqpt), pn(fpn) {
    ;
  }
  polyline_pl(polyline& pl);
  polyline_pl(const polyline& pl);

  polyline_pl& operator=(const polyline_pl& fpl);
  friend std::ostream& operator<<(std::ostream& file, const polyline_pl& p);
};

std::ostream& operator<<(std::ostream& file, const polyline_pl& p);

//             **** polygon (in plane) ****

class polygon : public polyline_pl {
 public:
  //vfloat area(void);
  int s_convex;
  int check_point_in(const point& fpt, vfloat prec) const;
  //            0 point is not in
  //            1 point coincides with an edge
  //            2 point is inside an interval of border
  //            3 point is inside body
  point cross(const straight& fsl, vfloat prec) const;
  // if no cross, returns vecerror=1.

  int range(const point& fpt, const vec& dir, vfloat& rng, point& fptenr,
            vfloat prec) const;
  polygon& operator=(const polygon& fpl);
  polygon(void) : polyline_pl(), s_convex(0) { ; }
  polygon(const polygon& plg) : polyline_pl((polyline_pl) plg) {
    s_convex = plg.s_convex;
  }
  polygon(const polyline_pl& fpl, int fs_convex)
      : polyline_pl(fpl), s_convex(fs_convex) {
    if (fpl.Gqpt() < 4 ||
        fpl.Gpt(0) !=
            fpl.Gpt(qpt -
                    1)) {  // 4 repeats 1, so different points are 3 or more
      mcerr << "ERROR in polygon::polygon(polyline_pl& fpl, int fs_convex)\n";
      mcerr << "fpl.Gqpt() < 4 || fpl.Gpt(0)!=fpl.Gpt(qpt-1)\n";
      spexit(mcerr);
    }
  }
  polygon(const straight* fsl, int fqsl, vfloat prec);
  // Prec is used to find crossing points of straight lines

};
std::ostream& operator<<(std::ostream& file, const polygon& p);

//             ***  rectangle ***

class rectangle : public polygon {
 public:
  point piv;      // central point
  vec dir1;       // directions of sides, unit length
  vec dir2;       // directions of sides, unit length
  vfloat dim[2];  // dimensions
  rectangle(void) : polygon() { ; }
  rectangle(const point& fpiv, vec fdir[2], vfloat fdim[2], vfloat prec);
  // Prec is used to check that sides are perpendicular and
  // at initing of the polygon wia straight lines.
 protected:
  static absref(absref::* aref_rct[4]);
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
};
std::ostream& operator<<(std::ostream& file, const rectangle& f);

//             **** special quadrangle ****  for cathode strip shamber
// 2 lines are going from a point of origin

class spquadr : public polygon {
 protected:
  point piv;
  vec dir1, dir2;
  vfloat awidth;  // width of total plane in units of radians
 public:
  point Gpiv(void) const { return piv; }
  vec Gdir1(void) const { return dir1; }
  vec Gdir2(void) const { return dir2; }
  vfloat Gawidth(void) const { return awidth; }

 protected:
  static absref(absref::* aref_sp[4]);
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
 public:

  vfloat apos(const point& fpt) const  // position in units of radians
      {
    // it is assumed that the point is inside
    //mcout<<"dir1="<<dir1<<"vec(fpt-piv)="<<vec(fpt-piv);
    return acos(cos2vec(dir1, vec(fpt - piv)));
  }
  vfloat apos(const straight& fsl, vfloat prec) const {
    point pth = cross(fsl, prec);
    if (vecerror != 0) return 0.0;
    return apos(pth);
  }
  vfloat perpos(const point& fpt) const  // perpendicular position,
      //distance from basis sl[0]
      {  // it is assumed that the point is inside
    vfloat r = sl[0].distance(fpt);
    return r;
  }
  vfloat perpos(const straight& fsl, vfloat prec) const {
    point pth = cross(fsl, prec);
    if (vecerror != 0) return 0.0;
    return perpos(pth);
  }
  point pt_angle_rad(vfloat rad, vfloat angle);

  spquadr(void) : polygon(), piv(), dir1(), dir2(), awidth(0) { ; }

  spquadr(spquadr& sq);
  spquadr(const spquadr& sq);

  spquadr(const point& fpiv, const straight& sl1, const straight& sl2,
          const vec& fdir1, const vec& fdir2, vfloat prec);

  friend std::ostream& operator<<(std::ostream& file, const spquadr& p);

 private:
  spquadr(const point& fpiv, const straight& /*sl1*/, const straight& /*sl2*/,
          const vec& fdir1, const vec& fdir2, polygon& fplgn)
      : polygon(fplgn),
        piv(fpiv),
        dir1(unit_vec(fdir1)),
        dir2(unit_vec(fdir2)) {
    awidth = acos(cos2vec(dir1, dir2));
  }
};

std::ostream& operator<<(std::ostream& file, const spquadr& p);

}

#endif
