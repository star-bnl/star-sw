// Cadmium-Telluride
#ifndef G_MEDIUM_CDTE_H
#define G_MEDIUM_CDTE_H

#include "Medium.hh"

namespace Garfield {

class MediumCdTe : public Medium {

 public:
  // Constructor
  MediumCdTe();
  // Destructor
  ~MediumCdTe() {}

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

  bool GetOpticalDataRange(double& emin, double& emax, 
                           const unsigned int i = 0);
  bool GetDielectricFunction(const double e, double& eps1, double& eps2,
                             const unsigned int i = 0);

 private:
  // double m_bandGap;

  // Low-field mobility
  double m_eMobility, m_hMobility;
  // Saturation velocity
  double m_eSatVel, m_hSatVel;
  // Hall factor
  double m_eHallFactor, m_hHallFactor;

  // Trapping parameters
  double m_eTrapCs, m_hTrapCs;
  double m_eTrapDensity, m_hTrapDensity;
  double m_eTrapTime, m_hTrapTime;
  unsigned int m_trappingModel;

  // Models
  bool m_hasUserMobility;
  bool m_hasUserSaturationVelocity;

  // Optical data
  std::string m_opticalDataFile;
  struct opticalData {
    // Energy [eV]
    double energy;
    // Dielectric function
    double eps1, eps2;
  };
  std::vector<opticalData> m_opticalDataTable;

  bool LoadOpticalData(const std::string& filename);
};
}

#endif
