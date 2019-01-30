#include "wcpplib/geometry/gparticle.h"
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

void stvpoint::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "stvpoint: sb=" << sb << " s_ent=" << s_ent << " prange=" << prange
        << " time=" << time << '\n';
  indn.n += 2;
  Ifile << "position:\n" << pt << ptloc;
  Ifile << "direction:\n" << dir << dirloc;
  Ifile << "speed=" << speed << '\n';
  if (tid.eid.empty()) {
    Ifile << "point is outside universe\n";
    file.flush();
    indn.n -= 2;
    return;
  }
  tid.print(file, 1);
  char s[100];
  if (sb == 2) {
    next_eid->m_chname(s);
    Ifile << "next volume name " << s << '\n';
  }
  indn.n -= 2;
  file.flush();
}

gparticle::gparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat ftime)
    : m_prevpos(),
      m_nextpos() {
  // As far as I can now understand, primvol will be at m_origin.tid.eid[0]
  mfunname("gparticle::gparticle(...)");
  primvol->m_find_embed_vol(pt, vel, &m_origin.tid);
  m_origin.pt = pt;
  if (vel == dv0) {
    m_origin.dir = dv0;
    m_origin.speed = 0.0;
  } else {
    m_origin.dir = unit_vec(vel);
    m_origin.speed = vel.length();
  }
  m_origin.ptloc = m_origin.pt;
  m_origin.tid.up_absref(&m_origin.ptloc);
  m_origin.dirloc = m_origin.dir;
  m_origin.tid.up_absref(&m_origin.dirloc);
  m_origin.time = ftime;
  m_origin.sb = 0;
  m_origin.s_ent = 1;
  if (m_origin.tid.eid.empty()) return;
  m_alive = true;
  m_currpos = m_origin;
  m_nextpos = m_currpos;
  m_nextpos.s_ent = 0;
}

void gparticle::step(std::vector<gparticle*>& secondaries) {
  // Make step to next point and calculate new step to border.
  mfunname("void gparticle::step()");
  m_prevpos = m_currpos;
  m_currpos = m_nextpos;
  m_total_range_from_origin += m_currpos.prange;
  m_nstep++;
  if (m_currpos.prange == 0) {
    m_nzero_step++;
    check_econd12a(m_nzero_step, >, m_max_qzero_step,
                   "too many zero steps, possible infinite loop\n", mcerr);
  } else {
    m_nzero_step = 0;
  }
  physics_after_new_speed(secondaries);
  if (m_alive) {
    if (m_prevpos.tid != m_currpos.tid) change_vol();
    m_nextpos = calc_step_to_bord();
  }
}

void gparticle::curvature(bool& curved, vec& frelcen, vfloat& fmrange,
                          vfloat /*prec*/) {
  curved = false;
  frelcen.x = 0.;
  frelcen.y = 0.;
  frelcen.z = 0.;
  fmrange = max_vfloat;
  /* The following is for debug
  vec field(0,1,0);
  vfloat rad = 10;
  if (length(m_currpos.dir) > 0 && check_par(m_currpos.dir, field) == 0) {
    curved = true;
    vfloat coef = sin2vec(m_currpos.dir, field);
    rad = rad / coef;
    frelcen = unit_vec(m_currpos.dir || field) * rad;
  }
  */
}

void gparticle::physics_mrange(double& /*fmrange*/) {}

stvpoint gparticle::calc_step_to_bord() {
  // Calculate next point as step to border.
  pvecerror("stvpoint gparticle::calc_step_to_bord()");
  if (m_currpos.sb > 0) {
    // Just switch to new volume.
    return switch_new_vol();
  }
  bool curved = false;
  vec relcen;
  vfloat mrange;
  curvature(curved, relcen, mrange, m_max_straight_arange);
  if (mrange <= 0) {
    // Preserve current point for modification by physics.
    stvpoint temp(m_currpos);
    temp.s_ent = 0;
    return temp;
  }
  // Change to local system.
  m_currpos.tid.up_absref(&relcen);  
  physics_mrange(mrange);
  trajestep ts(m_max_range, m_rad_for_straight, 
               m_max_straight_arange, m_max_circ_arange, 
               m_currpos.ptloc, m_currpos.dirloc, curved, relcen, mrange,
               m_currpos.tid.eid.back()->Gavol()->prec);
  if (ts.mrange <= 0) {
    stvpoint temp(m_currpos);
    temp.s_ent = 0;
    return temp;
  }
  // Here the range is calculated:
  int sb;
  manip_absvol* faeid = nullptr;
  m_currpos.tid.G_lavol()->range(ts, 1, sb, faeid);
  // 1 means inside the volume and makes
  // the program checking embraced volumes
  if (ts.s_prec == 0) {
    // Point is crossed.
    return stvpoint(m_currpos, ts, sb, 0, faeid);
  }
  return stvpoint(m_currpos, ts, ts.mrange, sb, 0, faeid);
}

stvpoint gparticle::switch_new_vol() {
  // Generate next position in new volume.
  mfunname("stvpoint gparticle::switch_new_vol(void)");
  manip_absvol_treeid tidl;
  manip_absvol* eidl = nullptr;
  stvpoint nextp = m_currpos;
  point pth = nextp.pt;
  // Search from primary
  // In this case it does not necessarily switch to encountered volume
  // namely nextp.next_eid
  // Borders of two volumes may coincide. Thus it should look for
  // the deepest volume at this point.
  bool ok = false;
  while (!ok) {
    nextp.tid.eid[0]->m_find_embed_vol(pth, nextp.dir, &tidl);
    if (tidl.eid.empty()) {
      m_alive = false;
      break;
    }
    // By default, assume switching to new volume.
    int s_e = 1;
    if (tidl == nextp.tid) {
      // Remains in the same old volume, may be numerical error
      // Will probably repeat attempt to switch at the same steps until ok.
      s_e = 0;
      double curprec = nextp.tid.G_lavol()->prec;
      if (m_currpos.prange <= curprec) {
        // very bad case, to repeat the trial.
        vec additional_dist = nextp.dir * curprec;
        pth = pth + additional_dist;
        tidl = manip_absvol_treeid();
        continue;
      }
    }
    return stvpoint(pth, nextp.dir, nextp.speed, tidl, 0.0, nextp.time, 0, s_e,
                    eidl);
  }
  return stvpoint();
}

void gparticle::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "gparticle(l=" << l << "): alive=" << m_alive << " nstep=" << m_nstep
        << " total_range_from_origin=" << m_total_range_from_origin
        << " nzero_step=" << m_nzero_step << '\n';
  if (l == 1) {
    file.flush();
    return;
  }
  indn.n += 2;
  if (l - 5 >= 0) {
    Ifile << "origin point:\n";
    indn.n += 2;
    m_origin.print(file, l - 2);
    indn.n -= 2;
  }
  if (l - 4 >= 0) {
    Ifile << "previous point:\n";
    indn.n += 2;
    m_prevpos.print(file, l - 1);
    indn.n -= 2;
  }
  if (l - 2 >= 0) {
    Ifile << "current point:\n";
    indn.n += 2;
    m_currpos.print(file, l);
     indn.n -= 2;
  }
  if (l - 3 >= 0) {
    Ifile << "next point:\n";
    indn.n += 2;
    m_nextpos.print(file, l - 1);
    indn.n -= 2;
  }
  indn.n -= 2;
  file.flush();
}
}
