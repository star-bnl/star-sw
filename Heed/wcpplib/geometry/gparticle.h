#ifndef GPARTICLE_H
#define GPARTICLE_H
#include "wcpplib/geometry/volume.h"

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

/// Point in space, time and velocity
class stvpoint {
 public:
  /// Coordinates in the first system in the tree.
  point pt;
  /// Unit vector, in the first system in the tree.
  vec dir;
  /// Coordinates in the local system (last system in the tree).
  point ptloc;
  /// Unit vector, in the local system (last system in the tree).
  vec dirloc;
  /// Longitudinal velocity
  vfloat speed = 0.;
  manip_absvol_treeid tid;

  /// Position flag
  /// 0 - inside volume, or unknown
  /// 1 - on the border of the volume
  /// 2 - on the border of an embraced volume
  int sb = 0; 
  /// "Entering flag".
  /// 1 - entering new volume, 0 otherwise. i
  /// Embraced volume is also considered new.
  int s_ent = 0;  

  manip_absvol* next_eid = nullptr;  // if nextpos.sb==2
  /// Range from previous point.
  vfloat prange = 0.;
  vfloat time = 0.;

  // constructors
  stvpoint() = default;
  stvpoint(const point& fpt, const vec& fdir, vfloat fspeed,
           manip_absvol_treeid& ftid, vfloat fprange, vfloat ftime, int fsb,
           int fs_ent, manip_absvol* faeid)
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
           int fsb, int fs_ent, manip_absvol* faeid)
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
           int fsb, int fs_ent, manip_absvol* faeid)
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
        time(fp.time) {}
  void print(std::ostream& file, int l) const;
};

extern trajestep_limit gtrajlim;

/// "Geometric particle" (particle which does not interact with materials).
/// It moves along a polyline line or circle from one volume to another.
/// The flying is represented by changing of class members representing
/// particle position.
/// Interacted particle should be derived class from this one.

class gparticle {
 public:
  /// Default constructor.
  gparticle() = default;
  /// Constructor.
  gparticle(manip_absvol* primvol, const point& pt, const vec& vel,
            vfloat time);
  /// Destructor.
  virtual ~gparticle() {}

  bool s_life = false;
  /// Step number.
  long nstep = 0;
  /// Range from origin to currpos.
  double total_range_from_origin = 0.; 
  /// Number of previous steps with zero range (including this step).
  long n_zero_step = 0; 

  static constexpr long max_q_zero_step = 100;
  stvpoint origin;
  stvpoint prevpos;
  stvpoint currpos;
  stvpoint nextpos;
  // current relcen computed
  // at the last call of calc_step_to_bord(), only for debug print
  vec curr_relcen;  

  /// Assign prevpos = currpos and currpos = nextpos,
  /// calls change_vol if necessary and update nextpos =calc_step_to_bord().
  // Derived versions can also recalculate direction of move currpos
  // right after the call of currpos=nextpos;.
  // This is especially important in the case when the move is done
  // by straight steps, but there is a field (magnetic) of any
  // force which deflects the trajectory slightly. In this case
  // at the end point of each interval the velocity is corrected (but the
  // point currpos is not).
  virtual void step(std::vector<gparticle*>& secondaries);

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

  virtual void physics_after_new_speed(std::vector<gparticle*>& /*secondaries*/) {}
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.
  virtual void physics(std::vector<gparticle*>& /*secondaries*/) {}
  // Allows to apply any other processes, to turn the trajectory, kill
  // the particle and so on.
  virtual void physics_mrange(double& fmrange);
  // Allows to reduce maximal possible range due to continuous processes.
  // Called from calc_step_to_bord() after the call of curvature(...)
  // but before considering the crossing with volumes.
  // Therefore mrange may be reduced after this.

  /// Produces nextpos
  virtual stvpoint calc_step_to_bord();

  stvpoint switch_new_vol();

  /// Transport the particle.
  virtual void fly(std::vector<gparticle*>& secondaries) {
    mfunname("virtual void gparticle::fly()");
    while (s_life) {
      step(secondaries);
      physics(secondaries);
    }
  }
  virtual void print(std::ostream& file, int l) const;
  virtual gparticle* copy() const { return new gparticle(*this); }

};
}

#endif
