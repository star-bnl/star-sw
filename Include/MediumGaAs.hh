#ifndef G_MEDIUM_GAAS_H
#define G_MEDIUM_GAAS_H

#include "Medium.hh"

namespace Garfield {

/// Gallium-Arsenide.

class MediumGaAs : public Medium {

 public:
  /// Constructor
  MediumGaAs();
  /// Destructor
  virtual ~MediumGaAs() {}

  bool IsSemiconductor() const { return true; }

  void GetComponent(const unsigned int i, std::string& label, double& f);

  /// Set electron and hole trapping cross-sections [cm-2].
  void SetTrapCrossSection(const double ecs, const double hcs);
  /// Set the density of traps [cm-3].
  void SetTrapDensity(const double n);
  /// Set the trapping time [ns] for electrons and holes.
  void SetTrappingTime(const double etau, const double htau);

  // Electron transport parameters
  bool ElectronVelocity(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& vx, double& vy, double& vz);
  bool ElectronTownsend(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& alpha);
  bool ElectronAttachment(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& eta);
  // Hole transport parameters
  bool HoleVelocity(const double ex, const double ey, const double ez,
                    const double bx, const double by, const double bz,
                    double& vx, double& vy, double& vz);
  bool HoleTownsend(const double ex, const double ey, const double ez,
                    const double bx, const double by, const double bz,
                    double& alpha);
  bool HoleAttachment(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& eta);

  void SetLowFieldMobility(const double mue, const double muh);

 private:
  // Band-gap energy [eV]
  // double m_bandGap = 1.42;
  // Low-field mobility
  double eMobility = 8.8e-6;
  double hMobility = 3.2e-6;
  // Hall factor
  double eHallFactor = 1.05;
  double hHallFactor = 1.25;

  // Trapping parameters
  double eTrapCs = 1.e-15;
  double hTrapCs = 1.e-15;
  double eTrapDensity = 1.e13;
  double hTrapDensity = 1.e13;
  double eTrapTime = 0.;
  double hTrapTime = 0.;
  int trappingModel = 0;

  // Models
  bool m_hasUserMobility = false;

};
}

#endif
