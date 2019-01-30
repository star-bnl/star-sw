#ifndef TRAJESTEP_H
#define TRAJESTEP_H
#include "wcpplib/geometry/vec.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

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
  /** Constructor.
    * \param fmax_range maximum step length
    * \param frad_for_straight radius beyond which to use straight-line steps.
    * \param fmax_straight_arange angular step for straight-line approximation.
    * \param fmax_circ_arange angular step for curved steps.
    * \param fcurrpos initial coordinates.
    * \param fdir initial direction.
    * \param fcurved flag whether the trajectory is curved or straight.
    * \param frelcen centre of rotation (only used for curved lines).
    * \param fmrange can be used for reducing/limiting the step length.
    * \param prec tolerance for checking if frelcen is perpendicular to dir.
    */
  trajestep(const vfloat fmax_range, const vfloat frad_for_straight,
            const vfloat fmax_straight_arange, const vfloat fmax_circ_arange,
            const point& fcurrpos, const vec& fdir, const bool fcurved, 
            const vec& frelcen, vfloat fmrange, vfloat prec);
  /** Constructor to continue propagation from the end point of another step.
    * \param fts old step to continue
    * \param fmrange new range to travel
    */ 
  trajestep(const trajestep& fts, vfloat fmrange);
  /// Default constructor.
  trajestep() = default;
  /// Destructor
  virtual ~trajestep() {}

  /// Move to the next point.
  void Gnextpoint(vfloat frange, point& fpos, vec& fdir) const;

  /// Max. step length.
  vfloat max_range = 100. * CLHEP::cm;
  // The three following parameters regulate the precision for curved lines.
  /// Radius beyond which to prefer straight lines to reduce calculation time.
  vfloat rad_for_straight = 1000. * CLHEP::cm;
  /// Angular step for curved lines when using straight-line approximation.
  vfloat max_straight_arange = 0.1 * CLHEP::rad;
  /// Angular step for curved lines.
  vfloat max_circ_arange = 0.2 * CLHEP::rad;

  /// Current position.
  point currpos;
  /// Unit vector.
  vec dir;     
  /// Type of trajectory (curved or straight).
  bool curved = false; 

  /// Centre of rotation relative to currpos.
  /// Used only for curved trajectories. 
  /// If used, should be perpendicular to dir.
  vec relcen;  

  // 0 - range have been calculated via straight line
  // 1 - via circle
  int s_range_cf = 0; 

  // 1 - range is limited by precision
  int s_prec = 0; 

  /// Maximal possible range
  vfloat mrange = 0.;

  // Finishing point
  // It looks like that at s_prec=1 mpoint is not initiated
  // At s_prec=0 the point is initiated
  point mpoint;    

 protected:
  virtual absref_transmit get_components() override;
  static absref(absref::*aref[4]);

 private:
  void Gnextpoint1(vfloat frange, point& fpos, vec& fdir, vec& frelcen) const;
};
std::ostream& operator<<(std::ostream& file, const trajestep& f);
}

#endif
