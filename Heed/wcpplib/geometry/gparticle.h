#ifndef GPARTICLE_H
#define GPARTICLE_H
#include "wcpplib/geometry/volume.h"

/*
"geometrical particle"
It is particle which does not interact with materials.
It flyes by polyline line or by circumferences from one volume to another.
The flying is represented by changing of class members representing
particle position.
Interacted particle should be derived class from this one.

Copyright (c) 2000 Igor B. Smirnov

The file can be used, copied, modified, and distributed
according to the terms of GNU Lesser General Public License version 2.1
as published by the Free Software Foundation,
and provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
The file is provided "as is" without express or implied warranty.
*/

// point in space, time and velocity
class stvpoint {
 public:
  // coordinates in the first system from tid system
  point pt;
  // unit vector, in the first system from tid system
  vec dir;
  // coordinates in the local system, the last system from tid
  point ptloc;
  // unit vector, in the local system, the last system from tid
  vec dirloc;
  // longitudinal velocity
  vfloat speed;
  //vfloat accel;     // longitudinal acceleration
  manip_absvol_treeid tid;
  //int namvol;  // number of currect volumes
  //manip_absvol* amvol[pqamvol];
  int sb;     // 0 - inside volume, or unknown
              // 1 - on the border of the volume
              // 2 - on the border of an embraced volume
  int s_ent;  // 1 - entering new volume, 0 otherwise
              // embraced volume is also considered new.

  manip_absvol_eid next_eid;  // if nextpos.sb==2
                              // range from previous point
  vfloat prange;
  vfloat time;
  //  vfloat vvel; // value of velocity

  // get least address of manipulator
  const manip_absvol_eid* G_laeid() const { return tid.G_laeid(); }
  // get least address of manipulator
  manip_absvol* G_lamvol() const { return tid.G_lamvol(); }
  // get least address of volume
  absvol* G_lavol() const { return tid.G_lavol(); }

  // constructors
  stvpoint(void)
      : pt(),
        dir(),
        ptloc(),
        dirloc(),
        speed(0.0),
        tid(),
        sb(0),
        s_ent(0),
        next_eid(),
        prange(0.0),
        time(0) {
    ;
  }
  stvpoint(const point& fpt, const vec& fdir, vfloat fspeed,
           manip_absvol_treeid& ftid, vfloat fprange, vfloat ftime, int fsb,
           int fs_ent, manip_absvol_eid& faeid)
      : pt(fpt),
        dir(unit_vec(fdir)),
        speed(fspeed),
        tid(ftid),
        sb(fsb),
        s_ent(fs_ent),
        next_eid(faeid),
        prange(fprange),
        time(ftime) {
    ptloc = pt;
    tid.up_absref(&ptloc);
    dirloc = dir;
    tid.up_absref(&dirloc);
  }
  stvpoint(const stvpoint& pstv, const trajestep& ts,  // in the local system
           vfloat mrange,  // may be less than one in ts
           //manip_absvol_treeid& ftid,
           int fsb, int fs_ent, manip_absvol_eid& faeid)
      : pt(),
        dir(),
        ptloc(),
        dirloc(),
        speed(pstv.speed),
        tid(pstv.tid),
        sb(fsb),
        s_ent(fs_ent),
        next_eid(faeid),
        prange(mrange),
        time(pstv.time + mrange / pstv.speed) {
    if (pstv.speed == 0) {
      time = pstv.time;  // just to put anything
    } else {
      // If speed is changed, this time is to be corrected in derivative class
      time = pstv.time + mrange / pstv.speed;
    }
    ts.Gnextpoint(mrange, ptloc, dirloc);
    pt = ptloc;
    tid.down_absref(&pt);
    dir = dirloc;
    tid.down_absref(&dir);
  }

  stvpoint(const stvpoint& pstv, const trajestep& ts,  // in the local system
           int fsb, int fs_ent, manip_absvol_eid& faeid)
      : pt(),
        dir(),
        ptloc(),
        dirloc(),
        speed(pstv.speed),
        tid(pstv.tid),
        sb(fsb),
        s_ent(fs_ent),
        next_eid(faeid),
        prange(ts.mrange),
        time(pstv.time + ts.mrange / pstv.speed) {
    if (pstv.speed == 0) {
      time = pstv.time;  // just to put anything
    } else {
      // If speed is changed, this time is to be corrected in derivative class
      time = pstv.time + ts.mrange / pstv.speed;
    }
    ptloc = ts.mpoint;
    point temp;
    ts.Gnextpoint(ts.mrange, temp, dirloc);
    pt = ptloc;
    tid.down_absref(&pt);
    dir = dirloc;
    tid.down_absref(&dir);
  }
  stvpoint(const stvpoint& fp)
      : pt(fp.pt),
        dir(fp.dir),
        ptloc(fp.ptloc),
        dirloc(fp.dirloc),
        speed(fp.speed),
        tid(fp.tid),
        sb(fp.sb),
        s_ent(fp.s_ent),
        next_eid(fp.next_eid),
        prange(fp.prange),
        time(fp.time) {
    ;
  }
  // destructor
  virtual ~stvpoint() {}
  virtual void print(std::ostream& file, int l) const;
};

extern trajestep_limit gtrajlim;

class gparticle : public RegPassivePtr {
 public:
  int s_life;
  long nstep;                      // step number
  double total_range_from_origin;  // from origin to currpos
  long n_zero_step;                // number of previous steps with zero range
                                   // including this step
  static long max_q_zero_step;
  stvpoint origin;
  stvpoint prevpos;
  stvpoint currpos;
  stvpoint nextpos;
  vec curr_relcen;  // current relcen computed
  // at the last call of calc_step_to_bord(), only for debug print

  gparticle(void) : s_life(0), nstep(0) { ; }
  gparticle(const stvpoint& sp)
      : s_life(1), nstep(0), origin(sp), prevpos(), currpos(sp), nextpos() {
    nextpos = calc_step_to_bord();
    physics();
  }

  gparticle(manip_absvol* primvol, const point& pt, const vec& vel,
            vfloat time);
  // As far as I can now understand, PassivePtr< primvol > will be at
  // origin.tid.eid[0]

  virtual void step(void);  // Assigns prevpos=currpos; and currpos=nextpos;
  // calls change_vol(); if necessary and makes nextpos=calc_step_to_bord();
  // Derived versions can also recalculate direction of move currpos
  // right after the call of currpos=nextpos;.
  // This is especially important in the case when the move is done
  // by straight steps, but there is a field (magnetic) of any
  // force which deflects the trajectory slightly. In this case
  // at the end point of each interval the velocity is corrected (but the
  // point currpos is not).

  virtual void change_vol(void) { currpos.tid.G_lavol()->income(this); }
  virtual void curvature(int& fs_cf, vec& frelcen, vfloat& fmrange,
                         vfloat prec);
  // Allows to set curvature.
  // For flying particle it is expected to call another function
  // so as to obtain value and direction of force.
  // Can also change currpos.dir.
  // prec is used to find out if the force is parallel or antiparallel to dir
  // In the latter case the range is restricted by the stop point.
  // Thus this prec does not depend on order of geometry sizes.
  // In calc_step_to_bord() it is set to gtrajlim.max_straight_arange.
  // vec& frelcen: position of the center of circumf. relatively currpos

  virtual void physics_after_new_speed(void) { ; }
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.
  virtual void physics(void) { ; }
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.
  virtual void physics_mrange(double& fmrange);
  // Allows to reduce maximal possible range due to continuous processes.
  // Called from calc_step_to_bord() after the call of curvature(...)
  // but before considering the crossing with volumes.
  // Therefore mrange may be reduced after this.

  virtual stvpoint calc_step_to_bord();
  // produces nextpos

  stvpoint switch_new_vol(void);

  virtual void fly(void) {
    mfunname("virtual void gparticle::fly(void)");
    while (s_life == 1) {
      step();
      physics();
    }
  }
  virtual void print(std::ostream& file, int l) const;
  macro_copy_total(gparticle);
  virtual ~gparticle() { ; }
};

#endif
