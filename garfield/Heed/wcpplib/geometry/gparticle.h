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
  /// 1 - entering new volume, 0 otherwise. 
  /// Embraced volume is also considered new.
  int s_ent = 0;  

  manip_absvol* next_eid = nullptr;  // if nextpos.sb==2
  /// Range from previous point.
  vfloat prange = 0.;
  vfloat time = 0.;

  /// Default constructor.
  stvpoint() = default;
  /// Constructor.
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
  /** Constructor.
    * \param pstv previous point
    * \param ts trajectory step (in the local system)
    * \param mrange step length (may be less than the one in ts)
    * \param fsb position flag 
    * \param fs_ent "entering" flag
    * \param faeid next volume
    **/
  stvpoint(const stvpoint& pstv, const trajestep& ts, vfloat mrange,
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
        prange(mrange) {
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
  /** Constructor.
    * \param pstv previous point
    * \param ts trajectory step (in the local system)
    * \param fsb position flag 
    * \param fs_ent "entering" flag
    * \param faeid next volume
    **/
  stvpoint(const stvpoint& pstv, const trajestep& ts,
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
        prange(ts.mrange) {
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
  /// Copy constructor.
  stvpoint(const stvpoint& fp) = default;
  void print(std::ostream& file, int l) const;
};

/// "Geometric particle" (particle that does not interact with materials).
/// It moves along a polyline or circle from one volume to another.
/// Classes for interacting particles should be derived from this base class.

class gparticle {
 public:
  /// Default constructor.
  gparticle() = default;
  /// Constructor.
  gparticle(manip_absvol* primvol, const point& pt, const vec& vel,
            vfloat time);
  /// Destructor.
  virtual ~gparticle() {}

  /// Transport the particle.
  virtual void fly(std::vector<gparticle*>& secondaries) {
    mfunname("virtual void gparticle::fly()");
    while (m_alive) {
      step(secondaries);
      physics(secondaries);
    }
  }

  /// Set limits/parameters for trajectory steps.
  void set_step_limits(const vfloat fmax_range, 
                       const vfloat frad_for_straight,
                       const vfloat fmax_straight_arange,
                       const vfloat fmax_circ_arange) {
    m_max_range = fmax_range;
    m_rad_for_straight = frad_for_straight;
    m_max_straight_arange = fmax_straight_arange;
    m_max_circ_arange = fmax_circ_arange;
  }

  /// Get the current position of the particle.
  const vec& position() const { return m_currpos.pt.v; }
  /// Get the current time of the particle.
  vfloat time() const { return m_currpos.time; }
  /// Get the current direction of the particle.
  const vec& direction() const { return m_currpos.dir; }

  /// Print-out.
  virtual void print(std::ostream& file, int l) const;
  /// Clone the particle.
  virtual gparticle* copy() const { return new gparticle(*this); }

 protected:
  /// Assign prevpos = currpos and currpos = nextpos,
  /// call change_vol if necessary and update nextpos = calc_step_to_bord().
  /// Derived versions can also recalculate the direction at currpos
  /// right after updating currpos = nextpos.
  /// This is especially important in the case when the motion is approximated
  /// by straight-line steps, but there is a (magnetic) field which slightly  
  /// deflects the trajectory. In this case, the velocity is corrected 
  /// at the end point of each interval, but the position is not.
  virtual void step(std::vector<gparticle*>& secondaries);

  /// Move from one volume to another.
  virtual void change_vol() { m_currpos.tid.G_lavol()->income(this); }

  /** Set curvature. Can also change the direction at the current position.
    * \param curved 
    *        flag whether the trajectory is curved
    * \param frelcen
    *        position of the centre of rotation relative to currpos.
    * \param fmrange
    *        step range
    * \param prec 
    *        tolerance for checking if the force is parallel or antiparallel to
    *        dir. In the latter case, the range is restricted by the end point.
    *        In calc_step_to_bord() it is set to m_max_straight_arange.
    */
  virtual void curvature(bool& curved, vec& frelcen, vfloat& fmrange,
                         vfloat prec);

  /// Apply any other processes (turn the trajectory, kill the particle, ...).
  virtual void physics_after_new_speed(std::vector<gparticle*>& /*secondaries*/) {}

  /// Apply any other processes (turn the trajectory, kill the particle, ...).
  virtual void physics(std::vector<gparticle*>& /*secondaries*/) {}

  /// Reduce the maximal possible range due to continuous processes.
  /// Called from calc_step_to_bord after the call of curvature.
  /// but before considering the crossing with volumes.
  /// Therefore mrange may be reduced after this.
  virtual void physics_mrange(double& fmrange);

  /// Determine next position.
  virtual stvpoint calc_step_to_bord();

  /// Generate next position in new volume.
  stvpoint switch_new_vol();

  /// Status flag whether the particle is active.
  bool m_alive = false;

  /// Step number.
  long m_nstep = 0;
  /// Max. number of zero-steps allowed.
  static constexpr long m_max_qzero_step = 100;
  /// Number of previous steps with zero range (including this step).
  long m_nzero_step = 0; 

  /// Original point.
  stvpoint m_origin;
  /// Range from origin to current position.
  double m_total_range_from_origin = 0.; 

  /// Previous point.
  stvpoint m_prevpos;
  /// Current point.
  stvpoint m_currpos;
  /// Next point.
  stvpoint m_nextpos;

 private:
  /// Max. length of trajectory steps.
  vfloat m_max_range = 100. * CLHEP::cm;
  /// Bending radius beyond which to use straight-line steps.
  vfloat m_rad_for_straight = 1000. * CLHEP::cm;
  /// Angular step limit when using straight-line approximation.
  vfloat m_max_straight_arange = 0.1 * CLHEP::rad;
  /// Angular step limit for curved lines.
  vfloat m_max_circ_arange = 0.2 * CLHEP::rad;
};
}

#endif
