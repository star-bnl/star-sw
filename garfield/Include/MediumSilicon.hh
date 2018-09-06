#ifndef G_MEDIUM_SILICON_H
#define G_MEDIUM_SILICON_H

#include "Medium.hh"

namespace Garfield {
/// %Solid crystalline silicon

class MediumSilicon : public Medium {

 public:
  /// Constructor
  MediumSilicon();
  /// Destructor
  virtual ~MediumSilicon() {}

  bool IsSemiconductor() const override { return true; }

  /// Doping concentration [cm-3] and type ('i', 'n', 'p')
  void SetDoping(const char type, const double c);
  void GetDoping(char& type, double& c) const;

  /// Trapping cross-sections for electrons and holes.
  void SetTrapCrossSection(const double ecs, const double hcs);
  void SetTrapDensity(const double n);
  void SetTrappingTime(const double etau, const double htau);

  // Electron transport parameters
  bool ElectronVelocity(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& vx, double& vy, double& vz) override;
  bool ElectronTownsend(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& alpha) override;
  bool ElectronAttachment(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& eta) override;
  // Hole transport parameters
  bool HoleVelocity(const double ex, const double ey, const double ez,
                    const double bx, const double by, const double bz,
                    double& vx, double& vy, double& vz) override;
  bool HoleTownsend(const double ex, const double ey, const double ez,
                    const double bx, const double by, const double bz,
                    double& alpha) override;
  bool HoleAttachment(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& eta) override;

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
  void SetDiffusionScaling(const double d) { m_diffScale = d; }

  // Microscopic transport properties
  bool SetMaxElectronEnergy(const double e);
  double GetMaxElectronEnergy() const { return m_eFinalG; }

  bool Initialise();

  // When enabled, the scattering rates table is written to file
  // when loaded into memory.
  void EnableScatteringRateOutput(const bool on = true) { m_cfOutput = on; }
  void EnableNonParabolicity(const bool on = true) { m_nonParabolic = on; }
  void EnableFullBandDensityOfStates(const bool on = true) { 
    m_fullBandDos = on; 
  }
  void EnableAnisotropy(const bool on = true) { m_anisotropic = on; }

  // Get the electron energy (and its gradient)
  // for a given (crystal) momentum
  double GetElectronEnergy(const double px, const double py, const double pz,
                           double& vx, double& vy, double& vz,
                           const int band = 0) override;
  // Get the electron (crystal) momentum for a given kinetic energy
  void GetElectronMomentum(const double e, double& px, double& py, double& pz,
                           int& band) override;

  // Get the null-collision rate [ns-1]
  double GetElectronNullCollisionRate(const int band) override;
  // Get the (real) collision rate [ns-1] at a given electron energy
  double GetElectronCollisionRate(const double e, const int band) override;
  // Sample the collision type
  bool GetElectronCollision(const double e, int& type, int& level, double& e1,
                            double& dx, double& dy, double& dz, 
                            std::vector<std::pair<int, double> >& secondaries,
                            int& ndxc, int& band) override;

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
                           const unsigned int i = 0) override;
  bool GetDielectricFunction(const double e, double& eps1, double& eps2,
                             const unsigned int i = 0) override;

  void ComputeSecondaries(const double e0, double& ee, double& eh);

 private:
  enum class LatticeMobility {
    Sentaurus = 0,
    Minimos,
    Reggiani 
  };
  enum class DopingMobility {
    Minimos = 0,
    Masetti
  };
  enum class SaturationVelocity {
    Minimos = 0,
    Canali,
    Reggiani
  };
  enum class HighFieldMobility {
    Minimos = 0,
    Canali,
    Reggiani,
    Constant
  };
  enum class ImpactIonisation {
    VanOverstraeten = 0,
    Grant
  };

  // Diffusion scaling factor
  double m_diffScale = 1.;

  double m_bandGap = 1.12;
  // Doping
  char m_dopingType = 'i';
  double m_dopingConcentration = 0.;

  // Effective masses
  // X valleys
  double m_mLongX = 0.916;
  double m_mTransX = 0.191;
  // L valleys
  double m_mLongL = 1.59;
  double m_mTransL = 0.12;
  // Non-parabolicity parameters [1/eV]
  double m_alphaX = 0.5;
  double m_alphaL = 0.5;
  // Lattice mobility
  double m_eLatticeMobility = 1.35e-6;
  double m_hLatticeMobility = 0.45e-6;
  // Low-field mobility
  double m_eMobility = 1.35e-6;
  double m_hMobility = 0.45e-6;
  // High-field mobility parameters
  double m_eBetaCanali = 1.109;
  double m_hBetaCanali = 1.213;
  double m_eBetaCanaliInv = 1. / 1.109;
  double m_hBetaCanaliInv = 1. / 1.213;
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
  int m_trappingModel = 0;

  // Impact ionisation parameters
  double m_eImpactA0 = 3.318e5;
  double m_eImpactA1 = 0.703e6;
  double m_eImpactA2 = 0.;
  double m_eImpactB0 = 1.135e6;
  double m_eImpactB1 = 1.231e6;
  double m_eImpactB2 = 0.;
  double m_hImpactA0 = 1.582e6;
  double m_hImpactA1 = 0.671e6;
  double m_hImpactB0 = 2.036e6;
  double m_hImpactB1 = 1.693e6;

  // Models
  bool m_hasUserMobility = false;
  bool m_hasUserSaturationVelocity = false;
  LatticeMobility m_latticeMobilityModel = LatticeMobility::Sentaurus;
  DopingMobility m_dopingMobilityModel = DopingMobility::Masetti;
  SaturationVelocity m_saturationVelocityModel = SaturationVelocity::Canali;
  HighFieldMobility m_highFieldMobilityModel = HighFieldMobility::Canali;
  ImpactIonisation m_impactIonisationModel = ImpactIonisation::VanOverstraeten;

  // Options
  bool m_cfOutput = false;
  bool m_nonParabolic = true;
  bool m_fullBandDos = true;
  bool m_anisotropic = true;

  // Energy range of scattering rates
  double m_eFinalXL = 4.;
  double m_eStepXL;
  double m_eFinalG = 10.;
  double m_eStepG;
  double m_eFinalV = 8.5;
  double m_eStepV;
  static const int nEnergyStepsXL = 2000;
  static const int nEnergyStepsG = 2000;
  static const int nEnergyStepsV = 2000;

  // Number of scattering terms
  int m_nLevelsX = 0;
  int m_nLevelsL = 0;
  int m_nLevelsG = 0;
  int m_nLevelsV = 0;
  // Number of valleys
  int m_nValleysX = 6;
  int m_nValleysL = 8;
  // Energy offset
  double m_eMinL = 1.05;
  double m_eMinG = 2.24;
  int m_ieMinL = 0;
  int m_ieMinG = 0;

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
  unsigned int m_nCollElectronAcoustic = 0;
  unsigned int m_nCollElectronOptical = 0;
  unsigned int m_nCollElectronIntervalley = 0;
  unsigned int m_nCollElectronImpurity = 0;
  unsigned int m_nCollElectronIonisation = 0;
  std::vector<unsigned int> m_nCollElectronDetailed;
  std::vector<unsigned int> m_nCollElectronBand;

  // Density of states tables
  double m_eStepDos;
  std::vector<double> m_fbDosValence;
  std::vector<double> m_fbDosConduction;
  double m_fbDosMaxV, m_fbDosMaxC;

  // Optical data
  std::string m_opticalDataFile = "OpticalData_Si.txt";
  std::vector<double> m_opticalDataEnergies;
  std::vector<std::pair<double, double> > m_opticalDataEpsilon;

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
