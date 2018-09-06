#include <array>
#include "wcpplib/geometry/box.h"
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

absref_transmit box::get_components() {
  mfunnamep("box::get_components(...)");
  funnw.ehdr(mcerr);
  mcerr << "one should not call this function, since this object cannot be "
           "modified\n";
  spexit(mcerr);
  return absref_transmit();
}

box::box()
    : m_dx(0), m_dy(0), m_dz(0), m_dxh(0), m_dyh(0), m_dzh(0), m_name("none") {
  mfunname("box::box()");
  init_prec();
  init_planes();
}

box::box(vfloat fdx, vfloat fdy, vfloat fdz, const std::string& fname) {
  pvecerror("box(vfloat fdx, vfloat fdy, vfloat fdz, const string &fname)");
  m_dx = fabs(fdx);
  m_dy = fabs(fdy);
  m_dz = fabs(fdz);
  m_dxh = 0.5 * m_dx;
  m_dyh = 0.5 * m_dy;
  m_dzh = 0.5 * m_dz;
  m_name = fname;
  init_prec();
  init_planes();
}

box::box(vfloat fdx, vfloat fdy, vfloat fdz, vfloat fprec,
         const std::string& fname) {
  pvecerror("box(vfloat fdx, vfloat fdy, vfloat fdz, vfloat fprec, const string &fname)");
  m_dx = fabs(fdx);
  m_dy = fabs(fdy);
  m_dz = fabs(fdz);
  m_dxh = 0.5 * m_dx;
  m_dyh = 0.5 * m_dy;
  m_dzh = 0.5 * m_dz;
  m_name = fname;
  prec = fprec;
  init_planes();
}

box::box(box& fb) : absref(fb), absvol(fb) {
  pvecerror("box(box& fb)");
  m_dx = fb.m_dx;
  m_dy = fb.m_dy;
  m_dz = fb.m_dz;
  m_dxh = 0.5 * m_dx;
  m_dyh = 0.5 * m_dy;
  m_dzh = 0.5 * m_dz;
  prec = fb.prec;
  m_name = fb.m_name;
  init_planes();
}

box::box(const box& fb) : absref(fb), absvol(fb) {
  pvecerror("box(const box& fb)");
  m_dx = fb.m_dx;
  m_dy = fb.m_dy;
  m_dz = fb.m_dz;
  m_dxh = 0.5 * m_dx;
  m_dyh = 0.5 * m_dy;
  m_dzh = 0.5 * m_dz;
  m_name = fb.m_name;
  prec = fb.prec;
  init_planes();
}

void box::init_prec() {
  prec = (m_dxh + m_dyh + m_dzh) / 3.0;
  prec *= vprecision;
}

void box::init_planes() {
  mfunname("void box::init_planes()");
  std::vector<std::shared_ptr<surface> > fsurf(6);
  fsurf[0] = std::make_shared<splane>(
      plane(point(m_dxh, 0, 0), vec(-1, 0, 0)), vec(-1, 0, 0));
  fsurf[1] = std::make_shared<splane>(
      plane(point(-m_dxh, 0, 0), vec(+1, 0, 0)), vec(+1, 0, 0));
  fsurf[2] = std::make_shared<splane>(
      plane(point(0, m_dyh, 0), vec(0, -1, 0)), vec(0, -1, 0));
  fsurf[3] = std::make_shared<splane>(
    plane(point(0, -m_dyh, 0), vec(0, +1, 0)), vec(0, +1, 0));
  fsurf[4] = std::make_shared<splane>(
    plane(point(0, 0, m_dzh), vec(0, 0, -1)), vec(0, 0, -1));
  fsurf[5] = std::make_shared<splane>(
    plane(point(0, 0, -m_dzh), vec(0, 0, +1)), vec(0, 0, +1));
  m_ulsv.ulsvolume_init(fsurf, "ulsv of box", prec);
}

int box::check_point_inside(const point& fpt, const vec& dir) const {
  mfunname("virtual int check_point_inside(const point& fpt, const vec& dir)");
#ifdef TRACE_find_embed_vol
  mcout << "box::check_point_inside: \n";
  print(mcout, 1);
  mcout << "fpt=" << fpt << "dir=" << dir;
#endif
  if (dir == dv0) {
    if (fabs(fpt.v.x) <= m_dxh && fabs(fpt.v.y) <= m_dyh &&
        fabs(fpt.v.z) <= m_dzh) {
      return 1;
    }
    return 0;
  } else {
    if (fabs(fpt.v.x) <= m_dxh - prec && fabs(fpt.v.y) <= m_dyh - prec &&
        fabs(fpt.v.z) <= m_dzh - prec) {
#ifdef TRACE_find_embed_vol
      mcout << "cond 1, returning 1\n";
#endif
      return 1;
    }
    if (fabs(fpt.v.x) > m_dxh + prec || fabs(fpt.v.y) > m_dyh + prec ||
        fabs(fpt.v.z) > m_dzh + prec) {
#ifdef TRACE_find_embed_vol
      if (fabs(fpt.v.x) > m_dxh + prec) mcout << "cond 2.1 satisfied\n";
      if (fabs(fpt.v.y) > m_dyh + prec) mcout << "cond 2.2 satisfied\n";
      if (fabs(fpt.v.z) > m_dzh + prec) mcout << "cond 2.3 satisfied\n";
      mcout << "cond 2, returning 0\n";
#endif
      return 0;
    }
// What remains is point belonging to border
#ifdef IMPROVED_BOUNDARY
    // Below we detect cases when particle is exiting, leaving the
    // case when it is entering
    if (fabs(fpt.v.x) > m_dxh - prec) {
      if (dir.x == 0.0) return 0;
      if ((fpt.v.x > 0 && dir.x > 0) || (fpt.v.x < 0 && dir.x < 0)) {
#ifdef TRACE_find_embed_vol
        mcout << "cond 3, returning 0\n";
#endif
        return 0;
      }
    }
    if (fabs(fpt.v.y) > m_dyh - prec) {
      if (dir.y == 0.0) return 0;
      if ((fpt.v.y > 0 && dir.y > 0) || (fpt.v.y < 0 && dir.y < 0)) {
#ifdef TRACE_find_embed_vol
        mcout << "cond 4, returning 0\n";
#endif
        return 0;
      }
    }
    if (fabs(fpt.v.z) > m_dzh - prec) {
      if (dir.z == 0.0) return 0;
      if ((fpt.v.z > 0 && dir.z > 0) || (fpt.v.z < 0 && dir.z < 0)) {
#ifdef TRACE_find_embed_vol
        mcout << "cond 5, returning 0\n";
#endif
        return 0;
      }
    }
#ifdef TRACE_find_embed_vol
    mcout << "finish, returning 1\n";
#endif
    return 1;

#else
    // for IMPROVED_BOUNDARY
    // In the old version, which is below,
    // if the track is parallel to a boundary, it was interpreted as
    // signature of being inside volume.
    // The general principal, saying that
    // if particle is located at the boundary and headed
    // outside the volume,
    // it is considered in the volume to which it is headed,
    // cannot help to choose between two versions.
    // Of course, the box is convex and the particle flying along the border
    // enters the external space sooner or later and it can be regarded as
    // already outside. But the same can be said about the particle,
    // which is completely indise the volume.
    // So this principle does not work here.
    // The other principle should be applied.
    // If we allow the absolutely thing volumes (control surfaces)
    // we should use the algorithm, which, in particular stops the particle
    // crossing exactly the corner of volume without entering its inside.
    // But this does not allow to choose. So now the old (this) variant
    // is used, untill other arguments appear.

    if (fabs(fpt.v.x) > m_dxh - prec &&
        ((fpt.v.x > 0 && dir.x > 0) || (fpt.v.x < 0 && dir.x < 0))) {
#ifdef TRACE_find_embed_vol
      mcout << "cond 3, returning 0\n";
#endif
      return 0;  // exiting
    }
    if (fabs(fpt.v.y) > m_dyh - prec &&
        ((fpt.v.y > 0 && dir.y > 0) || (fpt.v.y < 0 && dir.y < 0))) {
#ifdef TRACE_find_embed_vol
      mcout << "cond 4, returning 0\n";
#endif
      return 0;
    }
    if (fabs(fpt.v.z) > m_dzh - prec &&
        ((fpt.v.z > 0 && dir.z > 0) || (fpt.v.z < 0 && dir.z < 0))) {
#ifdef TRACE_find_embed_vol
      mcout << "cond 5, returning 0\n";
#endif
      return 0;
    }
#ifdef TRACE_find_embed_vol
    mcout << "finish, returning 1\n";
#endif
    return 1;
#endif
  }
}

void box::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  chname(s);
  Ifile << "box::print(l=" << l << "): " << s << '\n';
  indn.n += 2;
  Ifile << " dx=" << m_dx << " dy=" << m_dy << " dz=" << m_dz
        << " prec=" << prec << '\n';
  Ifile << " dxh=" << m_dxh << " dyh=" << m_dyh << " dzh=" << m_dzh << '\n';
  if (l >= 10) {
    l--;
    indn.n += 2;
    m_ulsv.print(file, l);
    indn.n -= 2;
  }
  absvol::print(file, l);
  indn.n -= 2;
}

int box::range_ext(trajestep& fts, int s_ext) const {
  mfunname("virtual int box::range_ext(trajestep& fts, int s_ext) const");
  if (s_ext == 0) {
    if (fabs(fts.currpos.v.x) > m_dxh + fts.mrange) return 0;
    if (fabs(fts.currpos.v.y) > m_dyh + fts.mrange) return 0;
    if (fabs(fts.currpos.v.z) > m_dzh + fts.mrange) return 0;
  } else {
    if (fabs(fts.currpos.v.x) < m_dxh - fts.mrange &&
        fabs(fts.currpos.v.y) < m_dyh - fts.mrange &&
        fabs(fts.currpos.v.z) < m_dzh - fts.mrange) {
      return 0;
    }
  }
  return m_ulsv.range_ext(fts, s_ext);
}

box* box::copy() const { return new box(*this); }

void box::income(gparticle* /*gp*/) {}
void box::chname(char* nm) const {
  strcpy(nm, "box: ");
  strcat(nm, m_name.c_str());
}

// *****   manip_box  ********

absvol* manip_box::Gavol() const { return (box*)this; }
manip_box* manip_box::copy() const { return new manip_box(*this); }
void manip_box::chname(char* nm) const {
  strcpy(nm, "manip_box: ");
  strcat(nm, m_name.c_str());
}

void manip_box::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  chname(s);
  Ifile << "manip_box::print(l=" << l << "): " << s << '\n';
  l = l - 1;
  if (l > 0) {
    indn.n += 2;
    box::print(file, l);
    indn.n -= 2;
  }
  file.flush();
}

// *****   sh_manip_box  ********

// absvol* sh_manip_box::Gavol() const { return (box*)this; }
absvol* sh_manip_box::Gavol() const { 
  return dynamic_cast<box*>(const_cast<sh_manip_box*>(this)); 
}

absref_transmit sh_manip_box::get_components() {
  return sh_manip_absvol::get_components();
}

sh_manip_box* sh_manip_box::copy() const { return new sh_manip_box(*this); }
void sh_manip_box::chname(char* nm) const {
  strcpy(nm, "sh_manip_box: ");
  strcat(nm, m_name.c_str());
}

void sh_manip_box::print(std::ostream& file, int l) const {
  if (l <= 0) return;
  char s[1000];
  chname(s);
  Ifile << "sh_manip_box::print(l=" << l << "): " << s << '\n';
  l = l - 1;
  if (l > 0) {
    indn.n += 2;
    csys.print(file, l);
    box::print(file, l);
    indn.n -= 2;
  }
  file.flush();
}
}
