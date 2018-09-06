#ifndef G_TRACK_SIMPLE_H
#define G_TRACK_SIMPLE_H

#include "Track.hh"

namespace Garfield {

/// Generate tracks based on a cluster density given by the user. 

class TrackSimple : public Track {

 public:
  /// Constructor
  TrackSimple();
  /// Destructor
  virtual ~TrackSimple() {}

  /// Constant distance between clusters.
  void SetEqualSpacing() { m_useEqualSpacing = true; }
  /// Exponentially distributed distance between clusters.
  void SetExponentialSpacing() { m_useEqualSpacing = false; }

  /// Set the cluster density (inverse mean free path).
  void SetClusterDensity(const double d);
  virtual double GetClusterDensity();
  /// Set the stopping power (dE/dx).
  void SetStoppingPower(const double dedx);
  virtual double GetStoppingPower();

  virtual bool NewTrack(const double x0, const double y0, const double z0,
                        const double t0, const double dx0, const double dy0,
                        const double dz0);
  virtual bool GetCluster(double& xcls, double& ycls, double& zcls, 
                          double& tcls, int& n, double& e, double& extra);

 protected:
  bool m_isReady = false;

  // Particle position, time and direction
  double m_x = 0., m_y = 0., m_z = 0., m_t = 0.;
  double m_dx = 0., m_dy = 0., m_dz = 0.;
  // Mean free path (mean spacing between adjacent clusters)
  double m_mfp = 0.04;
  // Average energy per cluster
  double m_eloss = 2530.;

  bool m_useEqualSpacing = false;
};
}

#endif
