#ifndef TRAJESTEP_H
#define TRAJESTEP_H
#include "wcpplib/geometry/vec.h"
#include "wcpplib/safetl/AbsPtr.h"

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

class trajestep_limit : public RegPassivePtr {
 public:
  /// Constructor.
  trajestep_limit(vfloat fmax_range, vfloat frad_for_straight,
                  vfloat fmax_straight_arange, vfloat fmax_circumf_arange)
      : max_range(fmax_range),
        rad_for_straight(frad_for_straight),
        max_straight_arange(fmax_straight_arange),
        max_circumf_arange(fmax_circumf_arange) {}


   vfloat max_range;  

  // The three following parameters regulate the precision for s_cf == 1.
  /// Radius beyond which to prefer straight lines to reduce calculation time.
  vfloat rad_for_straight;
  /// Angle of range if it goes along straight line, but s_cf == 1.
  // But the angle is calculated taking way as circle anyway.
  vfloat max_straight_arange;
  /// Angle of range if it goes along circle.
  vfloat max_circumf_arange;

  // Chooses straight or circle line and calculates maximal range.
  // vfloat& mrange gives first maximal range and filled by finishing
  // maximal range which should be less by value.
  // fs_cf0 : 0 - if the track must be straight.
  //          1 - if the real track is curved. It can or can not be
  //              approximated by straight line.
  // fs_cf1 : 0 - the track is simulated by straight line.
  //          1 - the track is simulated by curved line.
  void range(int fs_cf0, vfloat rad, int& fs_cf1, vfloat& mrange) const;
};

/// Trajectory step of any object (particle, photon, ...).
/// Here we interested in geometrical parameters only.
/// The time, speed, acceleration, mass, and forces are not interesting here.
/// The object can move by straight line or by curved line.
/// The real trajectory is approximated by little steps.
/// Each step can be straight or with constant curvature (circumference).
/// If the path is always straight (for example for light or neutral particles),
/// the step length is limited by extra conditions, typically edge of a volume.
/// If the path is curved, the step length is limited first by length at
/// which the curvature is changed, then by precision of approximation
/// of the real shape of curvature.

class trajestep : public absref {
 public:
  PassivePtr<trajestep_limit> tl;
  point currpos;
  /// Unit vector.
  vec dir;     
  /// Type of trajectory.
  ///  0 - the track is straight,
  ///  1 - curved track (but the range may anyway be calculated 
  /// as straight line, depending on s_range_cf)
  int s_cf; 

  // Position of the center of circumf. relatively currpos
  // Used only if s_cf=1; otherwise ignored.
  // If used, should be perpendicular to dir.
  vec relcen;  

  // 0 - range have been calculated via straight line
  // 1 - via circle
  int s_range_cf;  

  // 1 - range is limited by precision
  int s_prec;      

  /// Maximal possible range
  vfloat mrange;   

  // Finishing point
  // It looks like that at s_prec=1 mpoint is not initiated
  // At s_prec=0 the point is initiated
  point mpoint;    

  void Gnextpoint(vfloat frange, point& fpos, vec& fdir) const;
  void Gnextpoint1(vfloat frange, point& fpos, vec& fdir, vec& frelcen) const;

  /// Constructor.
  /// Here prec is used to check if frelcen is perp. to dir.
  /// If it is not perpendicular with this precision,
  /// the function terminates the program.
  /// To reduce range fmrange may be used.
  trajestep(trajestep_limit* ftl, const point& fcurrpos, const vec& fdir,
            int fs_cf, const vec& frelcen, vfloat fmrange, vfloat prec);
  /** Constructor to continue propagation from the end point of another step.
    * \param fts old step to continue
    *  \param fmrange new range to travel
    */ 
  trajestep(const trajestep& fts, vfloat fmrange);
  /// Default constructor.
  trajestep() : 
        tl(),
        currpos(),
        dir(),
        s_cf(0),
        relcen(),
        s_range_cf(0),
        s_prec(0),
        mrange(0),
        mpoint() {}
  /// Destructor
  virtual ~trajestep() {}

 protected:
  virtual void get_components(ActivePtr<absref_transmit>& aref_tran);
  static absref(absref::*aref[4]);
};
std::ostream& operator<<(std::ostream& file, const trajestep& f);
}

#endif
