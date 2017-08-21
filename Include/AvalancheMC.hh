#ifndef G_AVALANCHE_MC_H
#define G_AVALANCHE_MC_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {

/// Calculate drift lines and avalanches based on macroscopic transport 
/// coefficients, using Monte Carlo integration.
 
class AvalancheMC {

 public:
  /// Constructor
  AvalancheMC();
  /// Destructor
  ~AvalancheMC() {}

  /// Set the sensor.
  void SetSensor(Sensor* s);

  /// Switch on drift line plotting.
  void EnablePlotting(ViewDrift* view);
  void DisablePlotting() { m_viewer = NULL; }

  /// Switch on calculation of induced currents (default: disabled).
  void EnableSignalCalculation() { m_useSignal = true; }
  void DisableSignalCalculation() { m_useSignal = false; }

  /// Switch on calculation of induced charge (default: disabled).
  void EnableInducedChargeCalculation() { m_useInducedCharge = true; }
  void DisableInducedChargeCalculation() { m_useInducedCharge = false; }

  /** Switch on equilibration of multiplication and attachment
    * over the drift line (default: enabled) */
  void EnableProjectedPathIntegration() { m_useEquilibration = true; }
  void DisableProjectedPathIntegration() { m_useEquilibration = false; }

  /// Switch on diffusion (default: enabled)
  void EnableDiffusion() { m_useDiffusion = true; }
  void DisableDiffusion() { m_useDiffusion = false; }

  /** Switch on attachment (and multiplication) for drift line calculation
    * (default: enabled). For avalanches the flag is ignored. */
  void EnableAttachment() { m_useAttachment = true; }
  void DisableAttachment() { m_useAttachment = false; }

  /// Switch on calculating trapping with TCAD traps.
  void EnableTcadTraps() { m_useTcadTrapping = true; }
  void DisableTcadTraps() { m_useTcadTrapping = false; }

  /// Switch on TCAD velocity maps
  void EnableTcadVelocity() { m_useTcadVelocity = true; }
  void DisableTcadVelocity() { m_useTcadVelocity = false; }

  /// Enable use of magnetic field in stepping algorithm.
  void EnableMagneticField() { m_useBfield = true; }
  void DisableMagneticField() { m_useBfield = false; }

  /// Use fixed-time steps (default 20 ps).
  void SetTimeSteps(const double d = 0.02);
  /// Use fixed distance steps (default 10 um).
  void SetDistanceSteps(const double d = 0.001);
  /** Use exponentially distributed time steps with mean equal
    * to the specified multiple of the collision time (default model).*/
  void SetCollisionSteps(const int n = 100);

  /// Define a time interval (only carriers inside the interval are drifted).
  void SetTimeWindow(const double t0, const double t1);
  void UnsetTimeWindow() { m_hasTimeWindow = false; }

  /// Treat positive charge carriers as holes (default: ions).
  void SetHoles() { m_useIons = false; }
  void SetIons() { m_useIons = true; }

  /// Set multiplication factor for the signal induced by electrons.
  void SetElectronSignalScalingFactor(const double scale) {
    m_scaleElectronSignal = scale;
  }
  /// Set multiplication factor for the signal induced by holes.
  void SetHoleSignalScalingFactor(const double scale) {
    m_scaleHoleSignal = scale;
  }
  /// Set multiplication factor for the signal induced by ions.
  void SetIonSignalScalingFactor(const double scale) {
    m_scaleIonSignal = scale;
  }

  /// Return the number of electrons and ions/holes in the avalanche.
  void GetAvalancheSize(int& ne, int& ni) const {
    ne = m_nElectrons;
    ni = m_nIons;
  }

  /// Return the number of points along the last simulated drift line.
  unsigned int GetNumberOfDriftLinePoints() const { return m_drift.size(); }
  /// Return the coordinates and time of a point along the last drift line.
  void GetDriftLinePoint(const unsigned int i, double& x, double& y, double& z,
                         double& t) const;

  /** Return the number of electron trajectories in the last 
    * simulated avalanche (including captured electrons). */
  unsigned int GetNumberOfElectronEndpoints() const {
    return m_endpointsElectrons.size();
  }
  unsigned int GetNumberOfHoleEndpoints() const { 
    return m_endpointsHoles.size(); 
  }
  unsigned int GetNumberOfIonEndpoints() const { 
    return m_endpointsIons.size(); 
  }

  /** Return the coordinates and time of start and end point of a given
    * electron drift line.
    * \param i index of the drift line
    * \param x0,y0,z0,t0 coordinates and time of the starting point
    * \param x1,y1,z1,t1 coordinates and time of the end point
    * \param status status code (see GarfieldConstants.hh) 
    */
  void GetElectronEndpoint(const unsigned int i, double& x0, double& y0,
                           double& z0, double& t0, double& x1, double& y1,
                           double& z1, double& t1, int& status) const;
  void GetHoleEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                       double& t0, double& x1, double& y1, double& z1,
                       double& t1, int& status) const;
  void GetIonEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                      double& t0, double& x1, double& y1, double& z1,
                      double& t1, int& status) const;

  /// Simulate the drift line of an electron with a given starting point.
  bool DriftElectron(const double x0, const double y0, const double z0,
                     const double t0);
  bool DriftHole(const double x0, const double y0, const double z0,
                 const double t0);
  bool DriftIon(const double x0, const double y0, const double z0,
                const double t0);
  /// Simulate an avalanche initiated by an electron with given starting point.
  bool AvalancheElectron(const double x0, const double y0, const double z0,
                         const double t0, const bool hole = false);
  bool AvalancheHole(const double x0, const double y0, const double z0,
                     const double t0, const bool electron = false);
  bool AvalancheElectronHole(const double x0, const double y0, const double z0,
                             const double t0);

  /// Switch on debugging messages (default: off).
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  /// Numerical prefactor
  static double c1;

  Sensor* m_sensor;

  struct DriftPoint {
    /// Position
    double x, y, z, t;
    /// Number of secondaries produced at this point
    int ne, nh, ni;
  };
  /// Current drift line
  std::vector<DriftPoint> m_drift;

  /// Step size model
  int m_stepModel;
  /// Fixed time step
  double m_tMc;
  /// Fixed distance step
  double m_dMc;
  /// Sample step size according to collision time
  int m_nMc;

  /// Flag whether a time window should be used.
  bool m_hasTimeWindow;
  /// Time window
  double m_tMin, m_tMax;

  /// Number of electrons produced
  unsigned int m_nElectrons;
  /// Number of holes produced
  unsigned int m_nHoles;
  /// Number of ions produced
  unsigned int m_nIons;

  struct EndPoint {
    double x0, y0, z0, t0;
    double x1, y1, z1, t1;
    int status;
  };
  /// Endpoints of all electrons in the avalanche (including captured ones)
  std::vector<EndPoint> m_endpointsElectrons;
  /// Endpoints of all holes in the avalanche (including captured ones)
  std::vector<EndPoint> m_endpointsHoles;
  /// Endpoints of all ions in the avalanche
  std::vector<EndPoint> m_endpointsIons;

  ViewDrift* m_viewer;

  bool m_useSignal;
  bool m_useInducedCharge;
  bool m_useEquilibration;
  bool m_useDiffusion;
  bool m_useAttachment;
  bool m_useBfield;
  bool m_useIons;
  bool m_withElectrons;
  bool m_withHoles;
  double m_scaleElectronSignal;
  double m_scaleHoleSignal;
  double m_scaleIonSignal;

  /// Use traps from the field component, ComponentTcad2d;
  bool m_useTcadTrapping;
  /// Take the velocity from the field component, ComponentTcad2d;
  bool m_useTcadVelocity;

  bool m_debug;

  /// Compute a drift line with starting point (x0, y0, z0).
  bool DriftLine(const double x0, const double y0, const double z0,
                 const double t0, const int type, const bool aval = false);
  /// Compute an avalanche with starting point (x0, y0, z0).
  bool Avalanche(const double x0, const double y0, const double z0,
                 const double t0, const unsigned int ne, const unsigned int nh,
                 const unsigned int ni);

  /// Compute electric and magnetic field at a given position.
  int GetField(const double x, const double y, const double z,
               double& ex, double& ey, double& ez,
               double& bx, double& by, double& bz, Medium*& medium);
  /// Compute the drift velocity.
  bool GetVelocity(const int type, Medium* medium, 
                   const double x,const double y, const double z,
                   const double ex,const double ey, const double ez,
                   const double bx,const double by, const double bz,
                   double& vx, double& vy, double& vz);
  /// Add a diffusion step.
  bool AddDiffusion(const int type, Medium* medium, const double step,
                    double& x, double& y, double& z, 
                    const double vx, const double vy, const double vz,
                    const double ex, const double ey, const double ez,
                    const double bx, const double by, const double bz);
  /// Terminate a drift line close to the boundary.
  void TerminateLine(double x0, double y0, double z0, double t0,
                     double& x, double& y, double& z, double& t); 
  /// Compute multiplication and losses along the current drift line.
  bool ComputeGainLoss(const int type, int& status);
  /// Compute Townsend and attachment coefficients along the current drift line.
  bool ComputeAlphaEta(const int q, std::vector<double>& alphas,
                       std::vector<double>& etas);
  bool Equilibrate(std::vector<double>& alphas) const; 
  /// Compute the induced signal for the current drift line.
  void ComputeSignal(const double q);
  /// Compute the induced charge for the current drift line.
  void ComputeInducedCharge(const double q);
};
}

#endif
