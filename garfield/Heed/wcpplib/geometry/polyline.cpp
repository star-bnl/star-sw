#include "wcpplib/geometry/polyline.h"

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
absref_transmit polyline::get_components() {
  return absref_transmit(qpt + qsl, aref);
}

polyline::polyline(polyline& pl) : absref(pl) {
  mfunname("polyline::polyline(      polyline &pl)");
  polyline_init(pl.pt, pl.qpt);
}
polyline::polyline(const polyline& pl) : absref(pl) {
  mfunname("polyline::polyline(const polyline &pl)");
  polyline_init(pl.pt, pl.qpt);
}
polyline::polyline(const point* fpt, int fqpt) {
  mfunname("polyline::polyline(const point* fpt, int fqpt)");
  polyline_init(fpt, fqpt);
}
polyline::polyline(const point& fpt1, const point& fpt2) {
  // interval
  mfunname("polyline::polyline(const point& fpt1, const point& fpt2)");
  point fpt[2];
  fpt[0] = fpt1;
  fpt[1] = fpt2;
  polyline_init(fpt, 2);
}

polyline& polyline::operator=(const polyline& fpl) {
  mfunname("polyline& polyline::operator=(const polyline& fpl)");
  polyline_del();
  polyline_init(fpl.pt, fpl.qpt);
  return *this;
}

void polyline::polyline_init(const point* fpt, int fqpt) {
  pvecerror("void polyline::polyline_init(const point* fpt, int fqpt)");
  check_econd11(fqpt, < 0, mcerr)
  if (fqpt < 1) {
    qpt = 0;
    qsl = 0;
    pt = NULL;
    sl = NULL;
    aref = NULL;
    return;
  } 
  pt = new point[fqpt];
  for (qpt = 0; qpt < fqpt; ++qpt) pt[qpt] = fpt[qpt];
  if (fqpt >= 2) {
    sl = new straight[qpt - 1];
    for (qsl = 0; qsl < qpt - 1; ++qsl) {
      sl[qsl] = straight(pt[qsl], pt[qsl + 1]);
    }
  } else {
    sl = NULL;
  }
  aref = new absref* [qpt + qsl];
  for (int n = 0; n < qpt; ++n) aref[n] = &pt[n];
  for (int n = 0; n < qsl; ++n) aref[n + qpt] = &sl[n];
}

int polyline::check_point_in(const point& fpt, vfloat prec) const {
  pvecerror("int polyline::check_point_in(point& fpt, vfloat prec)");
  for (int n = 0; n < qpt; ++n) {
    if (apeq(pt[n], fpt, prec)) return 1;
  }
  for (int n = 0; n < qsl; ++n) {
    if (sl[n].check_point_in(fpt, prec) == 1) {
      vec v1 = fpt - pt[n];
      vec v2 = fpt - pt[n + 1];
      if (check_par(v1, v2, prec) == -1) {
        // anti-parallel vectors, point inside borders
        return 2;
      }
    }
  }
  return 0;
}

int polyline::cross(const straight& fsl, point* pc, int& qpc, polyline* pl,
                    int& qpl, vfloat prec) const {
  pvecerror("void polyline::cross(const straight& fsl, ...)");
  qpc = 0;
  qpl = 0;
  for (int n = 0; n < qsl; ++n) {
    pc[qpc] = sl[n].cross(fsl, prec);
    if (vecerror == 1 || vecerror == 2) {
      // lines do not cross
      vecerror = 0;
    } else if (vecerror == 3) {
      // the same straight line
      pl[qpl++] = polyline(&(pt[n]), 2);
    } else {
      vec v1 = pc[qpc] - pt[n];
      if (v1.length() < prec) {
        qpc++;
      } else {
        vec v2 = pc[qpc] - pt[n + 1];
        if (v2.length() < prec) {
          qpc++;
        } else if (check_par(v1, v2, prec) == -1) {
          // anti-parallel vectors, point inside borders
          qpc++;
        }
      }
    }
  }
  if (qpc > 0 || qpl > 0) return 1;
  return 0;
}

vfloat polyline::dist_two_inter(polyline& pl2, vfloat prec) const {
  pvecerror("vfloat polyline::dist_two_inter(polyline& pl)");
  const polyline& pl1 = *this;
  check_econd11(pl1.Gqpt(), != 2, mcerr);
  check_econd11(pl2.Gqpt(), != 2, mcerr);
  point cpt[2];
  int type_of_cross;
  vfloat sldist = pl1.Gsl(0).distance(pl2.Gsl(0), type_of_cross, cpt);
  if (type_of_cross == 2 || type_of_cross == 3) return sldist;
  if (pl1.check_point_in(cpt[0], prec) > 0 &&
      pl2.check_point_in(cpt[1], prec) > 0)
    return sldist;
  vfloat mx = max_vfloat;
  vfloat r;
  if ((r = pl1.distance(pl2.Gpt(0))) < mx) mx = r;
  if ((r = pl1.distance(pl2.Gpt(1))) < mx) mx = r;
  if ((r = pl2.distance(pl1.Gpt(0))) < mx) mx = r;
  if ((r = pl2.distance(pl1.Gpt(1))) < mx) mx = r;
  return mx;
}

vfloat polyline::distance(const point& fpt) const {
  pvecerror("vfloat polyline::distance(const point& fpt) const");
  check_econd11(qsl, <= 0, mcerr);
  vfloat sldist;
  point cpt;
  vfloat mx = max_vfloat;
  int n;
  for (n = 0; n < qsl; n++) {
    sldist = sl[n].distance(fpt, cpt);
    vec v1 = cpt - pt[n];
    vec v2 = cpt - pt[n + 1];
    if (check_par(v1, v2, 0.01) ==
        -1) {  // anti-parallel vectors, point inside borders
      if (sldist < mx) mx = sldist;
    } else {
      if ((sldist = (fpt - pt[n]).length()) < mx) mx = sldist;
      if ((sldist = (fpt - pt[n + 1]).length()) < mx) mx = sldist;
    }
  }
  return mx;
}

vfloat polyline::distance(const point& fpt, point& fcpt) const {
  pvecerror("vfloat polyline::distance(const point& fpt) const");
  check_econd11(qsl, <= 0, mcerr);
  vfloat sldist;
  point cpt;
  vfloat mx = max_vfloat;
  int n;
  for (n = 0; n < qsl; n++) {
    sldist = sl[n].distance(fpt, cpt);
    vec v1 = cpt - pt[n];
    vec v2 = cpt - pt[n + 1];
    if (check_par(v1, v2, 0.01) ==
        -1) {  // anti-parallel vectors, point inside borders
      if (sldist < mx) {
        mx = sldist;
        fcpt = cpt;
      }
    } else {
      if ((sldist = (fpt - pt[n]).length()) < mx) {
        mx = sldist;
        fcpt = pt[n];
      }
      if ((sldist = (fpt - pt[n + 1]).length()) < mx) {
        mx = sldist;
        fcpt = pt[n + 1];
      }
    }
  }
  return mx;
}

int cross4pllines(const polyline pl[4], vfloat precision, straight& sl,
                  point ptc[4][2]) {
  pvecerror(
      "int cross4pllines(const polyline pl[4], straight& sl, point ptc[4][2])");
  int n;
  straight slpl[4];
  for (n = 0; n < 4; n++) slpl[n] = pl[n].Gsl(0);
  point pt[2];
  pt[0] = (pl[1].Gpt(0).v + pl[1].Gpt(1).v) * 0.5;
  pt[1] = (pl[2].Gpt(0).v + pl[2].Gpt(1).v) * 0.5;
  sl = straight(slpl, pt, precision);
  int type_of_cross;
  for (n = 0; n < 4; n++) {
    sl.distance(pl[n].Gsl(0), type_of_cross, ptc[n]);
    // distance should be little, it need to find points
    if (pl[n].check_point_in(ptc[n][1], precision) == 0)  // check sides
      return 0;
  }
  return 1;
}

std::ostream& operator<<(std::ostream& file, const polyline& p) {
  int n;
  Ifile << "polyline:\n";
  indn.n += 2;
  Ifile << "qpt=" << p.qpt << '\n';
  for (n = 0; n < p.qpt; n++) file << p.pt[n];
  Ifile << "qsl=" << p.qsl << '\n';
  for (n = 0; n < p.qsl; n++) file << p.sl[n];
  indn.n -= 2;
  return file;
}
//             **** polyline in plane  ****

absref absref::*(polyline_pl::aref_pl) = (absref absref::*)&polyline_pl::pn;

absref_transmit polyline_pl::get_components() {
  return absref_transmit(1, &aref_pl, qpt + qsl, aref);
}

polyline_pl::polyline_pl(polyline& pl) {
  mfunname("polyline_pl::polyline_pl(     polyline& pl)");
  if (pl.Gqsl() < 2) {
    mcerr << "error in polyline_pl(polyline& pl): qsl=" << Gqsl();
    spexit(mcerr);
  }
  polyline::operator=(pl);
  pn = plane(pl.Gsl(0).Gpiv(), pl.Gsl(0).Gdir() || pl.Gsl(1).Gdir());
}

polyline_pl::polyline_pl(const polyline& pl) {
  mfunname("polyline_pl::polyline_pl(const polyline& pl");
  if (pl.Gqsl() < 2) {
    mcerr << "error in polyline_pl(polyline& pl): qsl=" << Gqsl();
    spexit(mcerr);
  }
  polyline::operator=(pl);
  pn = plane(pl.Gsl(0).Gpiv(), pl.Gsl(0).Gdir() || pl.Gsl(1).Gdir());
}

polyline_pl& polyline_pl::operator=(const polyline_pl& fpl) {
  mfunname("polyline_pl& polyline_pl::operator=(const polyline_pl& fpl)");
  polyline_del();
  polyline_init(fpl.pt, fpl.qpt);
  pn = fpl.pn;
  return *this;
}

std::ostream& operator<<(std::ostream& file, const polyline_pl& p) {
  Ifile << "polyline_pl:\n";
  indn.n += 2;
  file << p.pn;
  // file << statcast(const polyline&, p);
  file << static_cast<const polyline&>(p);
  indn.n -= 2;
  return file;
}

//             **** polygon (in plane) ****

polygon::polygon(const straight* fsl, int fqsl, vfloat prec)
    : polyline_pl(), s_convex(1) {
  pvecerror("polygon::polygon(const straight* fsl, int fqsl)");
  check_econd11a(fqsl, < 3, "fqsl cannot be less 3\n", mcerr);
  int n, m;
  // now check that either the piv's of lines are not equal to each other,
  // or the dir's are not parallel.
  // It does not prove that input data are corrent, but more
  // explicit prove might take too much time.
  for (n = 0; n < fqsl - 1; n++)
    for (m = n + 1; m < fqsl; m++) {
      if (fsl[n].Gpiv() == fsl[m].Gpiv())
        if (check_par(fsl[n].Gdir(), fsl[m].Gdir(), 0) !=
            0)  // 1 par, -1 antipar
        {
          mcerr << "error in polyline_init(straight* fsl, int fqsl):\n"
                << "Parallel lines with the same pivot cannot form polygin\n";
          for (int k = 0; k < fqsl; k++)
            mcout << "n=" << k << " fsl[n]=" << fsl[k];
          spexit(mcerr);
        }
    }
  int qptl = fqsl + 1;
  point* ptl = new point[qptl];
  for (n = 1; n < fqsl; n++) {
    ptl[n] = fsl[n - 1].cross(fsl[n], prec);
    if (vecerror != 0) {
      mcerr << "error in polygon::polygon(straight* fsl, int fqsl):\n"
            << " straight lines are not crossed properly\n"
            << "fsl[n-1]=" << fsl[n - 1] << "fsl[n]=" << fsl[n]
            << "vecerror=" << vecerror << '\n';
      spexit(mcerr);
    }
  }
  ptl[0] = fsl[fqsl - 1].cross(fsl[0], prec);
  if (vecerror != 0) {
    mcerr << "error in polygon::polygon(straight* fsl, int fqsl):\n"
          << " straight lines are not crossed properly\n"
          << "fsl[fqsl-1]=" << fsl[fqsl - 1] << "fsl[0]=" << fsl[0]
          << "vecerror=" << vecerror << '\n';
    spexit(mcerr);
  }
  ptl[fqsl] = ptl[0];
  plane pnl = plane(fsl[0].Gpiv(), fsl[0].Gdir() || fsl[1].Gdir());
  polyline_pl pll(pnl, ptl, qptl);
  *this = polygon(pll, 1);

  delete[] ptl;
}

polygon& polygon::operator=(const polygon& fpl) {
  mfunname("polygon& polygon::operator=(const polygon& fpl)");
  polyline_del();
  polyline_init(fpl.pt, fpl.qpt);
  pn = fpl.pn;
  s_convex = fpl.s_convex;
  return *this;
}

int polygon::check_point_in(const point& fpt, vfloat prec) const {
  pvecerror("int polygon::check_point_in(point& fpt)");
  int i;
  if ((i = polyline::check_point_in(fpt, prec)) > 0) {
    return i;
  }
  if ((i = pn.check_point_in(fpt, prec)) == 0) {
    return i;
  }
  /* The idea of the following algorithm is circulating around the polygon
     and finding of two points, one gives the minimum angle relatively
     some(any) direction, another gives the maximal angle.
     The point resides inside polygon if they are the same at the end of
     circulation.
  */
  point endpt[2];
  endpt[0] = pt[0];  // which is really first or last, depends on pn.Gdir()
  endpt[1] = pt[0];
  double totang = 0;
  double ang, ang2;
  // int s_start[2];
  // s_start[0]=0;
  // s_start[1]=0;
  int n;
  for (n = 0; n < qpt - 1; n++) {
    ang2 = 0.0;
    ang = ang2projvec((pt[n] - fpt), (pt[n + 1] - fpt), pn.Gdir());
    if (ang <= M_PI) {
      // go to opposite direction of clock
      totang += ang;
    } else {
      ang2 = 2 * M_PI - ang;
      totang -= ang2;
    }
  }

  if (fabs(totang) > 6.0) return 3;
  return 0;
}

point polygon::cross(const straight& fsl, vfloat prec) const {
  pvecerror("point polygon::cross(straight& fsl)");
  point cpt = pn.cross(fsl);  // does it cross the plane
  // mcout<<"polygon::cross: cpt="<<cpt;
  // mcout<<"vecerror="<<vecerror<<'\n';
  if (vecerror != 0) return cpt;
  int s = check_point_in(cpt, prec);
  if (s > 0)
    return cpt;
  else {
    vecerror = 1;
    return cpt;
  }
}
int polygon::range(const point& fpt, const vec& dir, vfloat& rng, point& fptenr,
                   vfloat prec) const {
  pvecerror(
      "int polygon::range(const point& fpt, const vec& dir, vfloat& rng, "
      " point &fptenr)");
  straight stl(fpt, dir);
  point pnt = cross(stl, prec);
  if (vecerror != 0) {
    vecerror = 0;
    return 0;
  }
  vec dif = pnt - fpt;
  const int i = check_par(dif, dir, prec);
  if (i == 1) {
    rng = dif.length();
    fptenr = pnt;
    return 1;
  } else
    return 0;
}

std::ostream& operator<<(std::ostream& file, const polygon& p) {
  Ifile << "polygon:\n";
  indn.n += 2;
  Ifile << "s_convex=" << p.s_convex << '\n';
  // file << statcast(const polyline_pl&, p);
  file << static_cast<const polyline_pl&>(p);
  indn.n -= 2;
  return file;
}
//             ***  rectangle ***
absref absref::*(rectangle::aref_rct[4]) = {
    (absref absref::*)&rectangle::pn,   (absref absref::*)&rectangle::piv,
    (absref absref::*)&rectangle::dir1, (absref absref::*)&rectangle::dir2};

absref_transmit rectangle::get_components() {
  return absref_transmit(4, aref_rct, qpt + qsl, aref);
}

rectangle::rectangle(const point& fpiv, vec fdir[2], vfloat fdim[2],
                     vfloat prec) {
  pvecerror(
      "rectangle::rectangle(point fpiv, vec fdir[2], vfloat fdim[2], "
      "vfloat prec)");
  if (check_perp(fdir[0], fdir[1], prec) != 1) {
    mcerr << "rectangle::rectangle(point fpiv, vec fdir[2], vfloat fdim[2]):\n"
          << " error: sides are not perpendicular\n";
    // There is stil no reason found in applications for sides to be
    // necessary perpendicular. The only reason in name of this class
    // choosen occasionly. To my knowledge it denotes a figure with
    // perpendicular sides.
    mcerr << "fdir[2](directions of sides):\n" << fdir[0] << fdir[1];
    spexit(mcerr);
  }
  if (fdim[0] <= 0 || fdim[1] <= 0) {
    mcerr << "rectangle::rectangle(point fpiv, vec fdir[2], vfloat fdim[2]):\n"
          << " error: fdim[0] <=0 || fdim[1] <=0\n";
    mcerr << "fdim (dimensions):" << fdim[0] << ' ' << fdim[1] << '\n';
    mcerr << "fdir[2](directions of sides):\n" << fdir[0] << fdir[1];
    spexit(mcerr);
  }
  piv = fpiv;
  dir1 = unit_vec(fdir[0]);
  dir2 = unit_vec(fdir[1]);
  dim[0] = fdim[0];
  dim[1] = fdim[1];
  // mcout<<"piv:\n"<<piv;
  // mcout<<"dir[2](directions of sides):\n"<<dir[0]<<dir[1];
  // mcout<<"dim (dimensions):"<<dim[0]<<' '<<dim[1]<<'\n';
  straight slh[4];
  slh[0] = straight(piv + dir1 * dim[0] / 2.0, dir2);
  slh[1] = straight(piv + dir2 * dim[1] / 2.0, -dir1);
  slh[2] = straight(piv - dir1 * dim[0] / 2.0, -dir2);
  slh[3] = straight(piv - dir2 * dim[1] / 2.0, dir1);
  polygon::operator=(polygon(slh, 4, prec));
}

std::ostream& operator<<(std::ostream& file, const rectangle& f) {
  Ifile << "rectangle:\n";
  indn.n += 2;
  Ifile << "piv:\n" << f.piv;
  Ifile << "dir1,2(directions of sides):\n" << f.dir1 << f.dir2;
  Ifile << "dim (dimensions):" << f.dim[0] << ' ' << f.dim[1] << '\n';
  file << static_cast<const polygon&>(f);
  indn.n -= 2;
  return file;
}

//             **** special quadrangle ****  for cathode strip shamber

absref absref::*(spquadr::aref_sp[4]) = {
    (absref absref::*)&spquadr::pn,   (absref absref::*)&spquadr::piv,
    (absref absref::*)&spquadr::dir1, (absref absref::*)&spquadr::dir2};

absref_transmit spquadr::get_components() {
  return absref_transmit(4, aref_sp, qpt + qsl, aref);
}

point spquadr::pt_angle_rad(vfloat rad, vfloat angle) {
  vec axis = unit_vec(dir1 || dir2);
  vec rv = dir1;
  rv.turn(axis, angle);
  rv = rv * rad;
  point rpt = piv + rv;
  return rpt;
}

spquadr::spquadr(spquadr& sq)
    : polygon(sq),
      piv(sq.piv),
      dir1(sq.dir1),
      dir2(sq.dir2),
      awidth(sq.awidth) {
  ;
}
spquadr::spquadr(const spquadr& sq)
    : polygon(sq),
      piv(sq.piv),
      dir1(sq.dir1),
      dir2(sq.dir2),
      awidth(sq.awidth) {
  ;
}

spquadr::spquadr(const point& fpiv, const straight& sl1, const straight& sl2,
                 const vec& fdir1, const vec& fdir2, vfloat prec)
    : polygon(), piv(fpiv), dir1(unit_vec(fdir1)), dir2(unit_vec(fdir2)) {
  straight slh[4];
  slh[0] = sl1;
  slh[1] = straight(piv, dir1);
  slh[2] = sl2;
  slh[3] = straight(piv, dir2);
  polygon plgn = polygon(slh, 4, prec);
  *this = spquadr(fpiv, sl1, sl2, fdir1, fdir2, plgn);
}

std::ostream& operator<<(std::ostream& file, const spquadr& p) {
  Ifile << "spquadr:\n";
  indn.n += 2;
  Ifile << "piv:";
  file << p.piv;
  Ifile << "dir1:\n";
  file << p.dir1;
  Ifile << "dir2:\n";
  file << p.dir2;
  Ifile << " awidth=" << p.awidth << '\n';
  file << static_cast<const polygon&>(p);
  indn.n -= 2;
  return file;
}
}
