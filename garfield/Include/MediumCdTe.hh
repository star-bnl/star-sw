#ifndef G_MEDIUM_CDTE_H
#define G_MEDIUM_CDTE_H

#include "Medium.hh"

namespace Garfield {

/// Cadmium-Telluride.

class MediumCdTe : public Medium {

 public:
  /// Constructor
  MediumCdTe();
  /// Destructor
  virtual ~MediumCdTe() {}

  bool IsSemiconductor() const { return true; }

  void GetComponent(const unsigned int i, 
                    std::string& label, double& f);

  // Trapping cross-section
  void SetTrapCrossSection(const double ecs, const double hcs);
  void SetTrapDensity(const double n);
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
  void SetSaturationVelocity(const double vsate, const double vsath);

 private:
  // Band gap energy [eV]
  // m_bandGap = 1.44;
  // Low-field mobility
  double m_eMobility = 1.1e-6;
  double m_hMobility = 0.1e-6;
  // Saturation velocity
  double m_eSatVel = 1.02e-2;
  double m_hSatVel = 0.72e-2;
  // Hall factor
  double m_eHallFactor = 1.15;
  double m_hHallFactor = 0.7;

  // Trapping parameters
  double m_eTrapCs = 1.e-15;
  double m_hTrapCs = 1.e-15;
  double m_eTrapDensity = 1.e13;
  double m_hTrapDensity = 1.e13;
  double m_eTrapTime = 0.;
  double m_hTrapTime = 0.;
  unsigned int m_trappingModel = 0;

  // Models
  bool m_hasUserMobility = false;
  bool m_hasUserSaturationVelocity = false;

};
}

#endif
