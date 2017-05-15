// Solid crystalline silicon

#ifndef G_MEDIUM_SILICON_H
#define G_MEDIUM_SILICON_H

#include "Medium.hh"

namespace Garfield {

class MediumSilicon : public Medium {

 public:
  // Constructor
  MediumSilicon();
  // Destructor
  ~MediumSilicon() {}

  bool IsSemiconductor() const { return true; }

  // Doping concentration [cm-3] and type ('i', 'n', 'p')
  void SetDoping(const char type, const double c);
  void GetDoping(char& type, double& c) const;

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
  void SetLatticeMobilityModelMinimos();
  void SetLatticeMobilityModelSentaurus();
  void SetLatticeMobilityModelReggiani();

  void SetDopingMobilityModelMinimos();
  void SetDopingMobilityModelMasetti();

  void SetSaturationVelocity(const double vsate, const double vsath);
  void SetSaturationVelocityModelMinimos();
  void SetSaturationVelocityModelCanali();
  void SetSaturationVelocityModelReggiani();

  void SetHighFieldMobilityModelMinimos();
  void SetHighFieldMobilityModelCanali();
  void SetHighFieldMobilityModelReggiani();
  void SetHighFieldMobilityModelConstant();

  void SetImpactIonisationModelVanOverstraetenDeMan();
  void SetImpactIonisationModelGrant();


  // Scaling 
  void SetDiffusionScaling(const double d){
    diffScale = d;
  }

  // Microscopic transport properties
  bool SetMaxElectronEnergy(const double e);
  double GetMaxElectronEnergy() const { return m_eFinalG; }

  bool Initialise();

  // When enabled, the scattering rates table is written to file
  // when loaded into memory.
  void EnableScatteringRateOutput() { m_useCfOutput = true; }
  void DisableScatteringRateOutput() { m_useCfOutput = false; }

  void EnableNonParabolicity() { m_useNonParabolicity = true; }
  void DisableNonParabolicity() { m_useNonParabolicity = false; }
  void EnableFullBandDensityOfStates() { m_useFullBandDos = true; }
  void DisableFullBandDensityOfStates() { m_useFullBandDos = false; }
  void EnableAnisotropy() { m_useAnisotropy = true; }
  void DisableAnisotropy() { m_useAnisotropy = false; }

  // Get the electron energy (and its gradient)
  // for a given (crystal) momentum
  double GetElectronEnergy(const double px, const double py, const double pz,
                           double& vx, double& vy, double& vz,
                           const int band = 0);
  // Get the electron (crystal) momentum for a given kinetic energy
  void GetElectronMomentum(const double e, double& px, double& py, double& pz,
                           int& band);

  // Get the null-collision rate [ns-1]
  double GetElectronNullCollisionRate(const int band);
  // Get the (real) collision rate [ns-1] at a given electron energy
  double GetElectronCollisionRate(const double e, const int band);
  // Sample the collision type
  bool GetElectronCollision(const double e, int& type, int& level, double& e1,
                            double& dx, double& dy, double& dz, int& nion,
                            int& ndxc, int& band);
  unsigned int GetNumberOfIonisationProducts() const { 
    return m_ionProducts.size(); 
  }
  bool GetIonisationProduct(const unsigned int i, int& type, 
                            double& energy) const;

  // Density of states
  double GetConductionBandDensityOfStates(const double e, const int band = 0);
  double GetValenceBandDensityOfStates(const double e, const int band = -1);

  // Reset the collision counters
  void ResetCollisionCounters();
  // Get the total number of electron collisions
  unsigned int GetNumberOfElectronCollisions() const;
  // Get number of scattering rate terms
  unsigned int GetNumberOfLevels() const;
  // Get number of collisions for a specific level
  unsigned int GetNumberOfElectronCollisions(const unsigned int level) const;

  unsigned int GetNumberOfElectronBands() const;
  int GetElectronBandPopulation(const int band);

  bool GetOpticalDataRange(double& emin, double& emax, 
                           const unsigned int i = 0);
  bool GetDielectricFunction(const double e, double& eps1, double& eps2,
                             const unsigned int i = 0);

  void ComputeSecondaries(const double e0, double& ee, double& eh);

 private:
  static const int LatticeMobilityModelSentaurus = 0;
  static const int LatticeMobilityModelMinimos = 1;
  static const int LatticeMobilityModelReggiani = 2;
  static const int DopingMobilityModelMinimos = 0;
  static const int DopingMobilityModelMasetti = 1;
  static const int SaturationVelocityModelMinimos = 0;
  static const int SaturationVelocityModelCanali = 1;
  static const int SaturationVelocityModelReggiani = 2;
  static const int HighFieldMobilityModelMinimos = 0;
  static const int HighFieldMobilityModelCanali = 1;
  static const int HighFieldMobilityModelReggiani = 2;
  static const int HighFieldMobilityModelConstant = 3;
  static const int ImpactIonisationModelVanOverstraeten = 0;
  static const int ImpactIonisationModelGrant = 1;

  // DiffusionScale
  double diffScale;

  double m_bandGap;
  // Doping
  char m_dopingType;
  double m_dopingConcentration;

  // Effective masses
  // X valleys
  double m_mLongX, m_mTransX;
  // L valleys
  double m_mLongL, m_mTransL;
  // Non-parabolicity parameters [1/eV]
  double m_alphaX, m_alphaL;
  // Lattice mobility
  double m_eLatticeMobility, m_hLatticeMobility;
  // Low-field mobility
  double m_eMobility, m_hMobility;
  // High-field mobility parameters
  double m_eBetaCanali, m_hBetaCanali;
  double m_eBetaCanaliInv, m_hBetaCanaliInv;
  // Saturation velocity
  double m_eSatVel, m_hSatVel;
  // Hall factor
  double m_eHallFactor, m_hHallFactor;

  // Trapping parameters
  double m_eTrapCs, m_hTrapCs;
  double m_eTrapDensity, m_hTrapDensity;
  double m_eTrapTime, m_hTrapTime;
  int m_trappingModel;

  // Impact ionisation parameters
  double m_eImpactA0, m_eImpactA1, m_eImpactA2;
  double m_eImpactB0, m_eImpactB1, m_eImpactB2;
  double m_hImpactA0, m_hImpactA1;
  double m_hImpactB0, m_hImpactB1;

  // Models
  bool m_hasUserMobility;
  bool m_hasUserSaturationVelocity;
  int m_latticeMobilityModel;
  int m_dopingMobilityModel;
  int m_saturationVelocityModel;
  int m_highFieldMobilityModel;
  int m_impactIonisationModel;

  // Options
  bool m_useCfOutput;
  bool m_useNonParabolicity;
  bool m_useFullBandDos;
  bool m_useAnisotropy;

  // Energy range of scattering rates
  double m_eFinalXL, m_eStepXL;
  double m_eFinalG, m_eStepG;
  double m_eFinalV, m_eStepV;
  static const int nEnergyStepsXL = 2000;
  static const int nEnergyStepsG = 2000;
  static const int nEnergyStepsV = 2000;

  // Number of scattering terms
  int m_nLevelsX, m_nLevelsL, m_nLevelsG;
  int m_nLevelsV;
  // Number of valleys
  int m_nValleysX, m_nValleysL;
  // Energy offset
  double m_eMinL, m_eMinG;
  int m_ieMinL, m_ieMinG;

  // Electron scattering rates
  double m_cfNullElectronsX;
  double m_cfNullElectronsL;
  double m_cfNullElectronsG;
  std::vector<double> m_cfTotElectronsX;
  std::vector<double> m_cfTotElectronsL;
  std::vector<double> m_cfTotElectronsG;
  std::vector<std::vector<double> > m_cfElectronsX;
  std::vector<std::vector<double> > m_cfElectronsL;
  std::vector<std::vector<double> > m_cfElectronsG;
  std::vector<double> m_energyLossElectronsX;
  std::vector<double> m_energyLossElectronsL;
  std::vector<double> m_energyLossElectronsG;
  // Cross-section type
  std::vector<int> m_scatTypeElectronsX;
  std::vector<int> m_scatTypeElectronsL;
  std::vector<int> m_scatTypeElectronsG;

  // Hole scattering rates
  double m_cfNullHoles;
  std::vector<double> m_cfTotHoles;
  std::vector<std::vector<double> > m_cfHoles;
  std::vector<double> m_energyLossHoles;
  // Cross-section type
  std::vector<int> m_scatTypeHoles;

  // Collision counters
  unsigned int m_nCollElectronAcoustic;
  unsigned int m_nCollElectronOptical;
  unsigned int m_nCollElectronIntervalley;
  unsigned int m_nCollElectronImpurity;
  unsigned int m_nCollElectronIonisation;
  std::vector<unsigned int> m_nCollElectronDetailed;
  std::vector<unsigned int> m_nCollElectronBand;

  struct ionProd {
    int type;
    double energy;
  };
  std::vector<ionProd> m_ionProducts;

  // Density of states tables
  double m_eStepDos;
  std::vector<double> m_fbDosValence;
  std::vector<double> m_fbDosConduction;
  double m_fbDosMaxV, m_fbDosMaxC;

  // Optical data
  std::string m_opticalDataFile;
  struct opticalData {
    // Energy [eV]
    double energy;
    // Dielectric function
    double eps1, eps2;
  };
  std::vector<opticalData> m_opticalDataTable;

  bool UpdateTransportParameters();
  void UpdateLatticeMobilityMinimos();
  void UpdateLatticeMobilitySentaurus();
  void UpdateLatticeMobilityReggiani();

  void UpdateDopingMobilityMinimos();
  void UpdateDopingMobilityMasetti();

  void UpdateSaturationVelocityMinimos();
  void UpdateSaturationVelocityCanali();
  void UpdateSaturationVelocityReggiani();

  void UpdateHighFieldMobilityCanali();

  void UpdateImpactIonisationVanOverstraetenDeMan();
  void UpdateImpactIonisationGrant();

  bool ElectronMobilityMinimos(const double e, double& mu) const;
  bool ElectronMobilityCanali(const double e, double& mu) const;
  bool ElectronMobilityReggiani(const double e, double& mu) const;
  bool ElectronImpactIonisationVanOverstraetenDeMan(const double e,
                                                    double& alpha) const;
  bool ElectronImpactIonisationGrant(const double e, double& alpha) const;
  bool HoleMobilityMinimos(const double e, double& mu) const;
  bool HoleMobilityCanali(const double e, double& mu) const;
  bool HoleMobilityReggiani(const double e, double& mu) const;
  bool HoleImpactIonisationVanOverstraetenDeMan(const double e,
                                                double& alpha) const;
  bool HoleImpactIonisationGrant(const double e, double& alpha) const;

  bool LoadOpticalData(const std::string& filename);

  bool ElectronScatteringRates();
  bool ElectronAcousticScatteringRates();
  bool ElectronOpticalScatteringRates();
  bool ElectronIntervalleyScatteringRatesXX();
  bool ElectronIntervalleyScatteringRatesXL();
  bool ElectronIntervalleyScatteringRatesLL();
  bool ElectronIntervalleyScatteringRatesXGLG();
  bool ElectronIonisationRatesXL();
  bool ElectronIonisationRatesG();
  bool ElectronImpurityScatteringRates();

  bool HoleScatteringRates();
  bool HoleAcousticScatteringRates();
  bool HoleOpticalScatteringRates();
  bool HoleIonisationRates();

  // void ComputeSecondaries(const double e0, double& ee, double& eh);
  void InitialiseDensityOfStates();
};
}

#endif
