#include "wcpplib/geometry/straight.h"
#include "wcpplib/geometry/plane.h"
#include "wcpplib/math/linexi2.h"
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

// **** straight ****

absref absref::*(straight::aref[2]) = {(absref absref::*)&straight::piv,
                                       (absref absref::*)&straight::dir};

absref_transmit straight::get_components() {
  return absref_transmit(2, aref);
}

straight::straight(const plane pl1, const plane pl2) {
  pvecerror("straight::straight(const plane pl1, const plane pl2)");
  *this = pl1.cross(pl2);
}
int operator==(const straight& sl1, const straight& sl2) {
  pvecerror("int operator==(const straight &sl1, const straight &sl2)");

  if (!(sl1.dir == sl2.dir || sl1.dir == -sl2.dir)) return 0;
  if (sl1.piv == sl2.piv) return 1;
  if (sl1.check_point_in(sl2.piv, 0.0) == 1) return 1;
  return 0;
}

bool apeq(const straight& sl1, const straight& sl2, vfloat prec) {
  pvecerror("int apeq(const straight &sl1, const straight &sl2, vfloat prec)");
  int i = check_par(sl1.dir, sl2.dir, prec);
  if (i == 0) return false;
  if (apeq(sl1.piv, sl2.piv, prec)) return true;
  return (sl1.check_point_in(sl2.piv, prec) == 1);
}

int straight::check_point_in(const point& fp, vfloat prec) const {
  pvecerror("int straight::check_point_in(point fp, vfloat prec)");
  vfloat f = distance(fp);
  if (f <= prec) return 1;
  return 0;
}
point straight::cross(const straight& sl, vfloat prec) const {
  pvecerror("point straight::cross(straight& sl, vfloat prec)");
  point pt[2];
  int type_of_cross;
  vfloat f = vecdistance(sl, type_of_cross, &pt[0]);
  point ptt(dv0);
  if (type_of_cross == 2 || type_of_cross == 3) {
    vecerror = type_of_cross;
    return ptt;
  }
  if (fabs(f) <= prec) {
    vecerror = 0;
    return pt[0];
  } else {
    vecerror = 1;
    return ptt;
  }
}

vfloat straight::vecdistance(const straight& sl, int& type_of_cross,
                             point pt[2]) const {
  pvecerror(
      "vfloat straight::distance(const straight& sl, int type_of_cross,  "
      "point pt[2])");
  pt[0] = point();
  pt[1] = point();
  type_of_cross = 0;
  straight s1, s2;
  s1 = *this;
  s2 = sl;  // s2 may be changed
  if (s1.piv == s2.piv) {                     
    // the same origin point
    if (check_par(s1.dir, s2.dir, 0.0) != 0) {
      // parallel or anti-parallel
      type_of_cross = 3;
      return 0.0;  // coincidence
    } else {       // crossed in piv;
      return 0.0;
    }
  }
  if (check_par(s1.dir, s2.dir, 0.0) != 0) {
    // parallel or anti-parallel
    if (s1.check_point_in(s2.piv, 0.0) == 1) {  
      // point in => the same line
      type_of_cross = 3;
      return 0.0;
    } else {
      // not crossed
      type_of_cross = 2;  // different parallel lines
      return s1.distance(s2.piv);
    }
  }  // now we know that the lines are not parallel

  basis bs(s1.dir, s2.dir, "local");
  // ez is parallel to s1.dir,                        ez=unit_vec(s1.dir)
  // ey is perpendicular to plane which have s1.dir and s2.dir,
  //                                                 ey=unit_vec(ez||s2.dir)
  // ex is vector product of ey and ez,               ex=ey||ez
  // mcout<<bs;
  fixsyscoor scl(&s1.piv, &bs, "local");
  // mcout<<scl;
  plane pn(point(0, 0, 0), vec(1, 0, 0));  // assumed to be in scl
                                           // This plane is defined by
  // mcout<<pn;
  s2.up(&scl);
  // mcout<<s2;
  pt[1] = pn.cross(s2);
  // mcout<<pt;
  if (pt[1].v.y == 0) {
    pt[1].down(&scl);
    pt[0] = pt[1];
    return 0.0;
  } else {
    type_of_cross = 1;
    vfloat d = pt[1].v.y;
    pt[0] = pt[1];
    pt[0].v.y = 0;
    pt[0].down(&scl);
    pt[1].down(&scl);
    return d;
  }
}

vfloat straight::distance(const straight& sl, int& type_of_cross,
                          point pt[2]) const {
  return fabs(vecdistance(sl, type_of_cross, pt));
}

straight::straight(straight* sl, int qsl, const straight& sl_start, int anum,
                   vfloat precision, vfloat* dist,  // may be negative
                   point (*pt)[2], vfloat& mean2dist) {
  pvecerror("void straight::straight(straight* sl, int qsl,...");
  check_econd11(qsl, < 4, mcerr);
  straight sl_finish = sl_start;
  int n;
  mean2dist = max_vfloat;
  vfloat mean2dist_prev = max_vfloat;
  int type_of_cross;
  point* ptf = new point[qsl];
  // mcout<<"straight::straight: starting, qsl="<<qsl
  //     <<"\nsl_start="<<sl_start<<'\n';
  do {
    mean2dist_prev = mean2dist;
    mean2dist = 0;
    *this = sl_finish;
    for (n = 0; n < qsl; n++) {
      dist[n] = vecdistance(sl[n], type_of_cross, pt[n]);
      mean2dist += pow(dist[n], 2);
      check_econd11(type_of_cross, > 1, mcerr);
      ptf[n] = pt[n][1];
    }
    mean2dist /= qsl;
    if (mean2dist > 0) mean2dist = sqrt(mean2dist);
    sl_finish = straight(ptf, qsl, anum);
    // mcout<<"straight::straight: mean2dist_prev="<<mean2dist_prev
    //	 <<" mean2dist="<<mean2dist<<'\n';
    // for( n=0; n<qsl; n++)
    //{
    //  mcout<<"pt[n][0]="<<pt[n][0]<<'\n';
    //  mcout<<"pt[n][1]="<<pt[n][1]<<'\n';
    //}
  } while (mean2dist_prev < mean2dist ||
           (mean2dist != 0 && mean2dist_prev - mean2dist > precision));
  delete[] ptf;
}

vfloat straight::distance(const point& fpt) const {
  pvecerror("vfloat straight::distance(point& fpt)");
  if (fpt == piv) return 0.0;
  vec v = fpt - piv;
  return v.length() * sin2vec(dir, v);  // should be positive
}

vfloat straight::distance(const point& fpt, point& fcpt) const {
  pvecerror("vfloat straight::distance(point& fpt, point& fcpt)");
  if (fpt == piv) {
    fcpt = piv;
    return 0.0;
  }
  vec v = fpt - piv;
  vfloat len = v.length();
  fcpt = piv + len * cos2vec(dir, v) * dir;
  return v.length() * sin2vec(dir, v);
}

point straight::vecdistance(const vec normal, const straight& slt) {
  pvecerror(
      "vfloat straight::vecdistance(const vec normal, const straight& slt)");
  if (check_perp(normal, slt.Gdir(), 0.0) == 1) {
    // if it is perp.
    mcout << "straight::vecdistance: normal=" << normal
          << " slt.Gdir()=" << slt.Gdir();
    vecerror = 1;
    return point(0, 0, 0);
  }
  basis bash(dir, normal, "temprorary");
  fixsyscoor sc(&piv, &bash, "temprorary");
  straight slh = slt;
  slh.up(&sc);
  plane pn = plane(point(0, 0, 0), vec(1, 0, 0));
  return pn.cross(slh);
}

straight::straight(const point* pt, int qpt, int anum) {
  // interpolates by xi2
  pvecerror("straight::straight(const point* pt, int qpt, int anum) ");
  check_econd11(qpt, < 2, mcerr);
  check_econd21(anum, < 0 ||, >= 3, mcerr);

  if (qpt == 2) {
    *this = straight(pt[0], pt[1]);
    return;
  }
  double* x = new double[qpt];
  double* y = new double[qpt];
  double* z = new double[qpt];
  for (int n = 0; n < qpt; n++) {
    x[n] = pt[n].v.x;
    y[n] = pt[n].v.y;
    z[n] = pt[n].v.z;
  }
  point piv1;
  if (anum == 0) {
    linexi2 lcy(qpt, x, y);
    linexi2 lcz(qpt, x, z);
    piv = point(lcy.x_mean, lcy.line(lcy.x_mean), lcz.line(lcy.x_mean));
    piv1 = point(lcy.x_mean + 1, lcy.line(lcy.x_mean + 1),
                 lcz.line(lcy.x_mean + 1));
  } else if (anum == 1) {
    linexi2 lcx(qpt, y, x);
    linexi2 lcz(qpt, y, z);
    piv = point(lcx.line(lcx.x_mean), lcx.x_mean, lcz.line(lcx.x_mean));
    // lcx.x_mean = lcz.x_mean
    piv1 = point(lcx.line(lcx.x_mean + 1), lcx.x_mean + 1,
                 lcz.line(lcx.x_mean + 1));
  } else {
    linexi2 lcx(qpt, z, x);
    linexi2 lcy(qpt, z, y);
    piv = point(lcx.line(lcx.x_mean), lcy.line(lcx.x_mean), lcx.x_mean);
    piv1 = point(lcx.line(lcx.x_mean + 1), lcy.line(lcx.x_mean + 1),
                 lcx.x_mean + 1);
  }
  dir = unit_vec(piv1 - piv);

  delete[] x;
  delete[] y;
  delete[] z;
}

straight::straight(const straight sl[4], point pt[2], vfloat precision) {
  pvecerror(
      "straight::straight(const straight sl[4], point pt[2],  vfloat prec");
  int i;
  vfloat meandist;
  point ptprev[2];
  point ptcurr[2];
  ptprev[0] = pt[0];
  ptprev[1] = pt[1];
  ptcurr[0] = pt[0];
  ptcurr[1] = pt[1];
  do {
    meandist = 0;
    for (i = 0; i < 2; i++) {
      int is;   // index of line to make plane
      int ip;   // index of point to make plane
      int isc;  // index of line to find point
      if (i == 0) {
        is = 0;
        ip = 1;
        isc = 1;
      } else {
        is = 3;
        ip = 0;
        isc = 2;
      }
      plane pn(sl[is], ptcurr[ip]);
      ptcurr[i] = pn.cross(sl[isc]);
      meandist += (ptcurr[i] - ptprev[i]).length2();
      mcout << " i=" << i << " ptprev[i]=" << ptprev[i]
            << " ptcurr[i]=" << ptcurr[i] << '\n';
      ptprev[i] = ptcurr[i];
    }
    meandist /= 2.0;
    meandist = sqrt(meandist);
    mcout << "meandist=" << meandist << '\n';
  } while (meandist >= precision);
  *this = straight(ptcurr[0], ptcurr[1]);
}

std::ostream& operator<<(std::ostream& file, const straight& s) {
  Ifile << "straight (line):\n";
  indn.n += 2;
  file << s.piv << s.dir;
  indn.n -= 2;
  return file;
}
}
