#ifndef G_AVALANCHE_MC_H
#define G_AVALANCHE_MC_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {

class AvalancheMC {

 public:
  // Constructor
  AvalancheMC();
  // Destructor
  ~AvalancheMC() {}

  void SetSensor(Sensor* s);

  // Switch on/off drift line plotting
  void EnablePlotting(ViewDrift* view);
  void DisablePlotting();

  // Switch on/off calculation of induced currents (default: disabled)
  void EnableSignalCalculation() { m_useSignal = true; }
  void DisableSignalCalculation() { m_useSignal = false; }

  // Switch on/off calculation of induced charge (default: disabled)
  void EnableInducedChargeCalculation() { m_useInducedCharge = true; }
  void DisableInducedChargeCalculation() { m_useInducedCharge = false; }

  // Switch on/off equilibration of multiplication and attachment
  // over the drift line (default: enabled)
  void EnableProjectedPathIntegration() { m_useEquilibration = true; }
  void DisableProjectedPathIntegration() { m_useEquilibration = false; }

  // Switch on/off diffusion (default: enabled)
  void EnableDiffusion() { m_useDiffusion = true; }
  void DisableDiffusion() { m_useDiffusion = false; }

  // Switch on/off attachment (and multiplication) for
  // drift line calculation (default: enabled)
  // For avalanches the flag is ignored
  void EnableAttachment() { m_useAttachment = true; }
  void DisableAttachment() { m_useAttachment = false; }

  // Switch on calculating trapping with TCAD traps
  void EnableTcadTraps() { m_useTcadTrapping = true; }
  void DisableTcadTraps() { m_useTcadTrapping = false; }

  // Switch on/off TCAD velocity maps
  void EnableTcadVelocity() { m_useTcadVelocity = true; }
  void DisableTcadVelocity() { m_useTcadVelocity = false; }

  // Enable/disable magnetic field in stepping algorithm.
  void EnableMagneticField() { m_useBfield = true; }
  void DisableMagneticField() { m_useBfield = false; }

  // Stepping model
  // Fixed time step (default 20 ps)
  void SetTimeSteps(const double d = 0.02);
  // Fixed distance step (default 10 um)
  void SetDistanceSteps(const double d = 0.001);
  // Exponentially distributed time step with mean equal
  // to the specified multiple of the collision time (default model)
  void SetCollisionSteps(const int n = 100);

  void SetTimeWindow(const double t0, const double t1);
  void UnsetTimeWindow();

  // Treat positive charge carriers as holes or ions (default: ions)
  void SetHoles() { m_useIons = false; }
  void SetIons() { m_useIons = true; }

  void SetElectronSignalScalingFactor(const double scale) {
    m_scaleElectronSignal = scale;
  }
  void SetHoleSignalScalingFactor(const double scale) {
    m_scaleHoleSignal = scale;
  }
  void SetIonSignalScalingFactor(const double scale) {
    m_scaleIonSignal = scale;
  }

  void GetAvalancheSize(int& ne, int& ni) const {
    ne = m_nElectrons;
    ni = m_nIons;
  }

  unsigned int GetNumberOfDriftLinePoints() const { return m_nDrift; }
  void GetDriftLinePoint(const unsigned int i, double& x, double& y, double& z,
                         double& t);

  unsigned int GetNumberOfElectronEndpoints() const {
    return m_nEndpointsElectrons;
  }
  unsigned int GetNumberOfHoleEndpoints() const { return m_nEndpointsHoles; }
  unsigned int GetNumberOfIonEndpoints() const { return m_nEndpointsIons; }

  void GetElectronEndpoint(const unsigned int i, double& x0, double& y0,
                           double& z0, double& t0, double& x1, double& y1,
                           double& z1, double& t1, int& status) const;
  void GetHoleEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                       double& t0, double& x1, double& y1, double& z1,
                       double& t1, int& status) const;
  void GetIonEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                      double& t0, double& x1, double& y1, double& z1,
                      double& t1, int& status) const;

  bool DriftElectron(const double x0, const double y0, const double z0,
                     const double t0);
  bool DriftHole(const double x0, const double y0, const double z0,
                 const double t0);
  bool DriftIon(const double x0, const double y0, const double z0,
                const double t0);
  bool AvalancheElectron(const double x0, const double y0, const double z0,
                         const double t0, const bool hole = false);
  bool AvalancheHole(const double x0, const double y0, const double z0,
                     const double t0, const bool electron = false);
  bool AvalancheElectronHole(const double x0, const double y0, const double z0,
                             const double t0);

  // Switch on/off debugging messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  // Numerical prefactors
  static double c1;

  Sensor* m_sensor;

  unsigned int m_nDrift;
  struct driftPoint {
    // Position
    double x, y, z, t;
    // Townsend and attachment coefficient
    double alpha, eta;
    // Number of secondaries produced at this point
    int ne, nh, ni;
  };
  std::vector<driftPoint> m_drift;

  struct avalPoint {
    double x, y, z, t;
    int ne, nh, ni;
  };
  std::vector<avalPoint> m_aval;

  // Step size model
  int m_stepModel;
  // Fixed time step
  double m_tMc;
  // Fixed distance step
  double m_dMc;
  // Sample step size according to collision time
  int m_nMc;

  // Time window
  bool m_hasTimeWindow;
  double m_tMin, m_tMax;

  // Number of electrons, holes and ions produced
  unsigned int m_nElectrons;
  unsigned int m_nHoles;
  unsigned int m_nIons;

  // Number of endpoints (including captured electrons)
  unsigned int m_nEndpointsElectrons;
  unsigned int m_nEndpointsHoles;
  unsigned int m_nEndpointsIons;
  struct endpoint {
    double x0, y0, z0, t0;
    double x1, y1, z1, t1;
    int status;
  };
  std::vector<endpoint> m_endpointsElectrons;
  std::vector<endpoint> m_endpointsHoles;
  std::vector<endpoint> m_endpointsIons;

  bool m_usePlotting;
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

  // Use traps and velocity from the field component, ComponentTcad2d;
  bool m_useTcadTrapping;
  bool m_useTcadVelocity;

  bool m_debug;

  // Compute a drift line with starting point (x0, y0, z0)
  bool DriftLine(const double x0, const double y0, const double z0,
                 const double t0, const int type, const bool aval = false);
  bool Avalanche();
  // Compute effective multiplication and ionisation
  // for the current drift line
  bool ComputeAlphaEta(const int q);
  // Compute the induced signal for the current drift line
  void ComputeSignal(const double q);
  void ComputeInducedCharge(const double q);
};
}

#endif
