#include "wcpplib/geometry/trajestep.h"
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

absref absref::*(trajestep::aref[4]) = {(absref absref::*)&trajestep::currpos,
                                        (absref absref::*)&trajestep::dir,
                                        (absref absref::*)&trajestep::relcen,
                                        (absref absref::*)&trajestep::mpoint};

absref_transmit trajestep::get_components() {
  return absref_transmit(4, aref);
}

trajestep::trajestep(const vfloat fmax_range, const vfloat frad_for_straight,
                     const vfloat fmax_straight_arange, 
                     const vfloat fmax_circ_arange, const point& fcurrpos,
                     const vec& fdir, const bool fcurved, const vec& frelcen,
                     vfloat fmrange, vfloat prec)
    : max_range(fmax_range),
      rad_for_straight(frad_for_straight),
      max_straight_arange(fmax_straight_arange),
      max_circ_arange(fmax_circ_arange),
      currpos(fcurrpos),
      dir(),
      curved(fcurved),
      relcen(frelcen),
      s_prec(1),
      mrange(fmrange) {
  pvecerror("trajestep::trajestep(...)");
  if (fdir == dv0) {
    dir = dv0;
    mrange = 0;
  } else {
    dir = unit_vec(fdir);
    if (curved) {
      check_econd11a(check_perp(dir, relcen, prec), != 1,
                     "dir=" << dir << "relcen=" << relcen
                            << "fcurrpos=" << fcurrpos << "fdir=" << fdir,
                     mcerr);
    }
    if (mrange < 0 || mrange > max_range) mrange = max_range;
    s_range_cf = curved;
    if (s_range_cf == 1) {
      const vfloat r = relcen.length();
      if (r >= rad_for_straight) {
        s_range_cf = 0;
        mrange = std::min(mrange, r * max_straight_arange);
      } else {
        mrange = std::min(mrange, r * max_circ_arange);
      }
    }
  }
}

trajestep::trajestep(const trajestep& fts, vfloat fmrange) {
  mfunname("trajestep::trajestep(const trajestep& fts, vfloat fmrange)");
  // Continue propagation from the end point of the old step.
  point fpos;
  vec fdir;
  vec frelcen;
  fts.Gnextpoint1(fts.mrange, fpos, fdir, frelcen);
  vfloat prec = 0.1;  // not important here
  *this =
      trajestep(fts.max_range, fts.rad_for_straight,
                fts.max_straight_arange, fts.max_circ_arange,
                fpos, fdir, fts.curved, frelcen, fmrange, prec);
}

void trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir) const {
  pvecerror("int trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir)");
  check_econd12(frange, >, mrange, mcerr);
  if (s_range_cf == 0) {
    // interpolation by straight line
    fpos = currpos + frange * dir;
    if (!curved) {
      // no curvature
      fdir = dir;
    } else {
      vfloat ang = frange / relcen.length();
      fdir = dir;
      fdir.turn(dir || relcen, ang);
    }
  } else {
    vfloat ang = frange / relcen.length();  // angle to turn
    fdir = dir;
    fdir.turn(dir || relcen, ang);  // direction at the end
    vec frelcen = relcen;
    frelcen.turn(dir || relcen, ang);
    fpos = currpos + relcen - frelcen;
  }
}

void trajestep::Gnextpoint1(vfloat frange, point& fpos, vec& fdir,
                            vec& frelcen) const {
  pvecerror(
      "int trajestep::Gnextpoint(vfloat frange, point& fpos, vec& fdir, "
      "vec& frelcen)");
  check_econd12(frange, >, mrange, mcerr);
  if (s_range_cf == 0) {
    // interpolation by straight line
    fpos = currpos + frange * dir;
    if (!curved) {
      // no curvature
      fdir = dir;
      frelcen = relcen;  // whatever it is
    } else {
      vfloat ang = frange / relcen.length();
      fdir = dir;
      fdir.turn(dir || relcen, ang);
      frelcen = relcen;
      frelcen.turn(dir || relcen, ang);
    }
  } else {
    vfloat ang = frange / relcen.length();  // angle to turn
    fdir = dir;
    fdir.turn(dir || relcen, ang);  // direction at the end
    frelcen = relcen;
    frelcen.turn(dir || relcen, ang);
    fpos = currpos + relcen - frelcen;
  }
}

std::ostream& operator<<(std::ostream& file, const trajestep& f) {
  Ifile << "trajestep: curved=" << f.curved << "\n";
  indn.n += 2;
  Ifile << "currpos:" << f.currpos << indn << "dir=" << f.dir << indn
        << "relcen=" << f.relcen << indn << "s_range_cf=" << f.s_range_cf
        << " s_prec=" << f.s_prec << " mrange=" << f.mrange << '\n' << indn
        << "mpoint=" << f.mpoint;
  indn.n -= 2;
  return file;
}
}
