// Gallium-Arsenide
#ifndef G_MEDIUM_GAAS_H
#define G_MEDIUM_GAAS_H

#include "Medium.hh"

namespace Garfield {

class MediumGaAs : public Medium {

 public:
  // Constructor
  MediumGaAs();
  // Destructor
  ~MediumGaAs() {}

  bool IsSemiconductor() const { return true; }

  void GetComponent(const unsigned int i, std::string& label, double& f);

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

  bool GetOpticalDataRange(double& emin, double& emax, 
                           const unsigned int& i = 0);
  bool GetDielectricFunction(const double& e, double& eps1, double& eps2,
                             const unsigned int& i = 0);

 private:
  // Low-field mobility
  double eMobility, hMobility;
  // Hall factor
  double eHallFactor, hHallFactor;

  // Trapping parameters
  double eTrapCs, hTrapCs;
  double eTrapDensity, hTrapDensity;
  double eTrapTime, hTrapTime;
  int trappingModel;

  // Models
  bool m_hasUserMobility;

  // Optical data
  bool m_hasOpticalData;
  std::string opticalDataFile;
  struct opticalData {
    // Energy [eV]
    double energy;
    // Dielectric function
    double eps1, eps2;
  };
  std::vector<opticalData> opticalDataTable;
  bool LoadOpticalData(const std::string filename);
};
}

#endif
