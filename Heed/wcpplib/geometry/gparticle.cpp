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

void stvpoint::print(std::ostream& file, int l) const {
  if (l < 0) return;
  Ifile << "stvpoint: sb=" << sb << " s_ent=" << s_ent << " prange=" << prange
        << " time=" << time << '\n';
  indn.n += 2;
  Ifile << "position:\n" << pt << ptloc;
  Ifile << "direction of moving:\n" << dir << dirloc;
  Ifile << "speed=" << speed << '\n';
  if (tid.qeid <= 0) {
    Ifile << "point is outside universe, tid.qeid=" << tid.qeid << '\n';
    file.flush();
    indn.n -= 2;
    return;
  }
  tid.print(file, 1);
  char s[100];
  if (sb == 2) {
    next_eid.amvol->m_chname(s);
    Ifile << "next volume name " << s << '\n';
  }
  indn.n -= 2;
  file.flush();
}

long gparticle::max_q_zero_step = 100;

gparticle::gparticle(manip_absvol* primvol, const point& pt, const vec& vel,
                     vfloat time)
    : s_life(0),
      nstep(0),
      total_range_from_origin(0.0),
      n_zero_step(0),
      prevpos(),
      nextpos() {
  mfunname("gparticle::gparticle(...)");
  origin.tid.eid[0].nembed = 0;  // just to clear
  primvol->m_find_embed_vol(pt, vel, &origin.tid);
  origin.pt = pt;
  if (vel == dv0) {
    origin.dir = dv0;
    origin.speed = 0.0;
  } else {
    origin.dir = unit_vec(vel);
    origin.speed = length(vel);
  }
  origin.ptloc = origin.pt;
  origin.tid.up_absref(&origin.ptloc);
  origin.dirloc = origin.dir;
  origin.tid.up_absref(&origin.dirloc);
  origin.time = time;
  origin.sb = 0;
  origin.s_ent = 1;  //origin.next_eid=NULL;
  if (origin.tid.qeid == 0) return;
  s_life = 1;
  currpos = origin;
  nextpos = currpos;
  nextpos.s_ent = 0;
}

void gparticle::step() {
  // make step to nextpos and calculate new step to border
  mfunname("void gparticle::step(void)");
  prevpos = currpos;
  currpos = nextpos;
  curr_relcen = dv0;
  total_range_from_origin += currpos.prange;
  nstep++;
  if (currpos.prange == 0) {
    n_zero_step++;
    check_econd12a(n_zero_step, >, max_q_zero_step,
                   "too much zero steps, possible infinite loop\n", mcerr);
  } else {
    n_zero_step = 0;
  }
  physics_after_new_speed();
  if (s_life == 1) {
    if (prevpos.tid != currpos.tid) change_vol();
    nextpos = calc_step_to_bord();
  }
}

void gparticle::curvature(int& fs_cf, vec& frelcen, vfloat& fmrange,
                          vfloat /*prec*/) {
  fs_cf = 0;
  frelcen = vec(0, 0, 0);
  fmrange = max_vfloat;
  /* The following is for debug
  vec field(0,1,0);
  vfloat rad = 10;
  if (length(currpos.dir) > 0 && check_par(currpos.dir, field) == 0) {
    fs_cf = 1;
    vfloat coef = sin2vec(currpos.dir, field);
    rad = rad / coef;
    frelcen = unit_vec(currpos.dir || field) * rad;
  }
  */
}

void gparticle::physics_mrange(double& /*fmrange*/) {}

// calculate next point as step to border
stvpoint gparticle::calc_step_to_bord() {
  pvecerror("stvpoint gparticle::calc_step_to_bord()");
  curr_relcen = dv0;
  if (currpos.sb > 0) {
    // it does not do step, but switch to new volume
    return switch_new_vol();
  } 
  manip_absvol_eid faeid;
  int s_cf;
  vec relcen;
  vfloat mrange;
  curvature(s_cf, relcen, mrange, gtrajlim.max_straight_arange);
  curr_relcen = relcen;
  if (mrange <= 0) {
    // preserves currpos for modification by physics
    stvpoint temp(currpos);
    temp.s_ent = 0;
    return temp;
  }
  currpos.tid.up_absref(&relcen);  // changing to local system
  physics_mrange(mrange);
  trajestep ts(&gtrajlim, currpos.ptloc, currpos.dirloc, s_cf, relcen, mrange,
               currpos.tid.G_laeid()->amvol->Gavol()->prec);
  if (ts.mrange <= 0) {
    stvpoint temp(currpos);
    temp.s_ent = 0;
    return temp;
  }
  // Here the range is calculated:
  int sb;
  currpos.tid.G_lavol()->range(ts, 1, sb, &faeid);
  // 1 means inside the volume and makes
  // the program checking embraced volumes
  if (ts.s_prec == 0) {
    // point is crossed
    return stvpoint(currpos, ts, sb, 0, faeid);
  }
  return stvpoint(currpos, ts, ts.mrange, sb, 0, faeid);
}

stvpoint gparticle::switch_new_vol(void) {
  // generates next position in new volume
  mfunname("stvpoint gparticle::switch_new_vol(void)");
  manip_absvol_treeid tidl;
  manip_absvol_eid eidl;
  stvpoint nextp = currpos;
  point pth = nextp.pt;
  // search from primary
  // In this case it does not necessary switch to encountered volume
  // namely nextp.next_eid
  // Borders of two volumes may coincide. Thus it should look for
  // the deepest volume at this point.
  bool ok = false;
  while (!ok) {
    nextp.tid.eid[0].amvol->m_find_embed_vol(pth, nextp.dir, &tidl);
    if (tidl.qeid <= 0) {
      s_life = 0;
      break;
    }
    // By default, assume switching to new volume.
    int s_e = 1;
    if (tidl == nextp.tid) {
      // Remains in the same old volume, may be numerical error
      // Will probably repeat attempt to switch at the same steps until ok.
      s_e = 0;  
      double curprec = nextp.tid.G_lavol()->prec;
      if (currpos.prange <= curprec) {
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
  Ifile << "gparticle(l=" << l << "): s_life=" << s_life << " nstep=" << nstep
        << " total_range_from_origin=" << total_range_from_origin
        << " n_zero_step=" << n_zero_step << '\n';
  if (l == 1) {
    file.flush();
    return;
  }
  indn.n += 2;
  if (l - 5 >= 0) {
    Ifile << "origin point:\n";
    indn.n += 2;
    origin.print(file, l - 2);
    indn.n -= 2;
  }
  if (l - 4 >= 0) {
    Ifile << "previous point:\n";
    indn.n += 2;
    prevpos.print(file, l - 1);
    indn.n -= 2;
  }
  if (l - 2 >= 0) {
    Ifile << "current point:\n";
    indn.n += 2;
    currpos.print(file, l);
    Iprint(file, curr_relcen);
    Ifile << " length(curr_relcen)=" << length(curr_relcen) << '\n';
    indn.n -= 2;
  }
  if (l - 3 >= 0) {
    Ifile << "next point:\n";
    indn.n += 2;
    nextpos.print(file, l - 1);
    indn.n -= 2;
  }
  indn.n -= 2;
  file.flush();
}
