#ifndef G_DRIFTLINE_RKF_H
#define G_DRIFTLINE_RKF_H

#include <vector>
#include <string>

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "Medium.hh"
#include "GeometryBase.hh"

namespace Garfield {

class DriftLineRKF {

 public:
  DriftLineRKF();
  ~DriftLineRKF() {}

  void SetSensor(Sensor* s);
  void SetIntegrationAccuracy(const double a);
  void SetMaximumStepSize(const double ms);

  void EnablePlotting(ViewDrift* view);
  void DisablePlotting();

  void SetMaxSteps(const unsigned int& m) { m_maxSteps = m; }

  void SetElectronSignalScalingFactor(const double scale) { m_scaleElectronSignal = scale; }
  void SetHoleSignalScalingFactor(const double scale) { m_scaleHoleSignal = scale; }
  void SetIonSignalScalingFactor(const double scale) { m_scaleIonSignal = scale; }

  bool DriftElectron(const double& x0, const double& y0, const double& z0,
                     const double& t0);
  bool DriftHole(const double& x0, const double& y0, const double& z0,
                 const double& t0);
  bool DriftIon(const double& x0, const double& y0, const double& z0,
                const double& t0);

  void GetEndPoint(double& x, double& y, double& z, double& t, int& st) const;
  unsigned int GetNumberOfDriftLinePoints() const { return m_path.size(); }
  void GetDriftLinePoint(const unsigned int i, double& x, double& y, double& z, double& t) const;

  double GetArrivalTimeSpread();
  double GetGain();
  double GetDriftTime() const;

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

  void EnableVerbose() { m_verbose = true; }
  void DisableVerbose() { m_verbose = false; }

 private:
  static const unsigned int ParticleTypeElectron = 0;
  static const unsigned int ParticleTypeIon = 1;
  static const unsigned int ParticleTypeHole = 2;

  std::string m_className;

  Sensor* m_sensor;
  Medium* m_medium;

  unsigned int m_particleType;
  double m_maxStepSize;
  double m_accuracy;
  unsigned int m_maxSteps;

  bool m_usePlotting;
  ViewDrift* m_view;

  struct step {
    // Position (initial and final)
    double xi, xf;
    double yi, yf;
    double zi, zf;
    // Time (initial and final)
    double ti, tf;
    // Integrated Townsend coefficient
    double alphaint;
    // Status flag
    int status;
  };
  std::vector<step> m_path;
public:
  const std::vector<step> &path() {return *&m_path;}
private:

  double m_scaleElectronSignal;
  double m_scaleHoleSignal;
  double m_scaleIonSignal;

  bool m_debug;
  bool m_verbose;

  // Calculate a drift line starting at a given position.
  bool DriftLine(const double& x0, const double& y0, const double& z0, 
                 const double& t0);
  // Calculate transport parameters for the respective particle type.
  bool GetVelocity(const double& ex, const double& ey, const double& ez,
                   const double& bx, const double& by, const double& bz,
                   double& vx, double& vy, double& vz) const;
  bool GetDiffusion(const double& ex, const double& ey, const double& ez,
                    const double& bx, const double& by, const double& bz,
                    double& dl, double& dt) const;
  bool GetTownsend(const double& ex, const double& ey, const double& ez,
                   const double& bx, const double& by, const double& bz,
                   double& alpha) const;
  // Terminate a drift line at the edge of a boundary.
  bool EndDriftLine();
  // Drift a particle to a wire
  bool DriftToWire(double x0, double y0, double z0, const double& xw,
                   const double& yw, const double& rw);
  // Determine the longitudinal diffusion over the drift line.
  double IntegrateDiffusion(const double& x, const double& y, const double& z,
                            const double& xe, const double& ye,
                            const double& ze);
  // Determine the effective gain over the drift line.
  double IntegrateTownsend(const double& x, const double& y, const double& z,
                           const double& xe, const double& ye, const double& ze,
                           const double& tol);
  // Calculate the signal for the current drift line.
  void ComputeSignal() const;
};
}

#endif
