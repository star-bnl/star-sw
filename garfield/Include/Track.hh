#ifndef G_TRACK_H
#define G_TRACK_H

#include <string>
#include <cmath>

namespace Garfield {

class Sensor;
class ViewDrift;

/// Abstract base class for track generation.

class Track {

 public:
  /// Constructor
  Track();
  /// Destructor
  virtual ~Track() {}

  /// Set the type of charged particle.
  /// - electron,e-
  /// - positron,e+
  /// - muon,mu-
  /// - mu+
  /// - pion,pi-
  /// - pi+
  /// - kaon,K-
  /// - K+
  /// - proton,p
  /// - anti-proton,p-bar 
  /// - deuteron,d
  /// - alpha
  virtual void SetParticle(const std::string& part);

  /// Set the particle energy.
  void SetEnergy(const double e);
  /// Set the relative momentum of the particle.
  void SetBetaGamma(const double bg);
  /// Set the speed (\f$\beta = v/c\f$) of the particle.
  void SetBeta(const double beta);
  /// Set the Lorentz factor of the particle.
  void SetGamma(const double gamma);
  /// Set the particle momentum.
  void SetMomentum(const double p);
  /// Set the kinetic energy of the particle.
  void SetKineticEnergy(const double ekin);

  /// Return the particle energy.
  double GetEnergy() const { return m_energy; }
  /// Return the \f$\beta\gamma\f$ of the projectile.
  double GetBetaGamma() const { return sqrt(m_beta2 / (1. - m_beta2)); }
  /// Return the speed (\f$\beta = v/c\f$) of the projectile.
  double GetBeta() const { return sqrt(m_beta2); }
  /// Return the Lorentz factor of the projectile.
  double GetGamma() const { return sqrt(1. / (1. - m_beta2)); }
  /// Return the particle momentum.
  double GetMomentum() const { return m_mass * sqrt(m_beta2 / (1. - m_beta2)); }
  /// Return the kinetic energy of the projectile.
  double GetKineticEnergy() const { return m_energy - m_mass; }

  /// Get the charge of the projectile.
  double GetCharge() const { return m_q; }
  /// Get the mass [eV / c2] of the projectile.
  double GetMass() const { return m_mass; }

  /// Set the sensor through which to transport the particle. 
  void SetSensor(Sensor* s);

  /// Calculate a new track starting from (x0, y0, z0) at time t0
  /// in direction (dx0, dy0, dz0).
  virtual bool NewTrack(const double x0, const double y0, const double z0,
                        const double t0, const double dx0, const double dy0,
                        const double dz0) = 0;
  /** Get the next "cluster" (ionising collision of the charged particle).
    * \param xcls,ycls,zcls coordinates of the collision
    * \param tcls time of the collision
    * \param n number of electrons produced
    * \param e deposited energy
    * \param extra additional information (not always implemented)
    */ 
  virtual bool GetCluster(double& xcls, double& ycls, double& zcls,
                          double& tcls, int& n, double& e, double& extra) = 0;

  /// Get the cluster density (number of ionizing collisions per cm or
  /// inverse mean free path for ionization).
  virtual double GetClusterDensity() { return 0.; }
  /// Get the stopping power (mean energy loss [eV] per cm).
  virtual double GetStoppingPower() { return 0.; }

  /// Switch on plotting.
  void EnablePlotting(ViewDrift* viewer);
  /// Switch off plotting. 
  void DisablePlotting();

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  std::string m_className = "Track";

  double m_q = -1.;
  int m_spin = 1;
  double m_mass;
  double m_energy = 0.;
  double m_beta2;
  bool m_isElectron = false;
  std::string m_particleName = "mu-";

  Sensor* m_sensor = nullptr;

  bool m_isChanged = true;

  bool m_usePlotting = false;
  ViewDrift* m_viewer = nullptr;

  bool m_debug = false;

  int m_plotId = -1;
  void PlotNewTrack(const double x0, const double y0, const double z0);
  void PlotCluster(const double x0, const double y0, const double z0);
};
}

#endif
