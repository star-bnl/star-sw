#ifndef G_MEDIUM_MAGBOLTZ_9
#define G_MEDIUM_MAGBOLTZ_9

#include "MediumGas.hh"

namespace Garfield {

/// Interface to %Magboltz (version 9).
///  - http://magboltz.web.cern.ch/magboltz/

class MediumMagboltz : public MediumGas {

 public:
  /// Constructor
  MediumMagboltz();
  /// Destructor
  virtual ~MediumMagboltz() {}

  /// Set the highest electron energy to be included
  //// in the scattering rates table.
  bool SetMaxElectronEnergy(const double e);
  /// Get the highest electron energy in the scattering rates table.
  double GetMaxElectronEnergy() const { return m_eFinal; }

  // Set/get the highest photon energy to be included
  // in the scattering rates table
  bool SetMaxPhotonEnergy(const double e);
  double GetMaxPhotonEnergy() const { return m_eFinalGamma; }

  // Switch on/off automatic adjustment of max. energy when an
  // energy exceeding the present range is requested
  void EnableEnergyRangeAdjustment() { m_useAutoAdjust = true; }
  void DisableEnergyRangeAdjustment() { m_useAutoAdjust = false; }

  // Switch on/off anisotropic scattering (enabled by default)
  void EnableAnisotropicScattering() {
    m_useAnisotropic = true;
    m_isChanged = true;
  }
  void DisableAnisotropicScattering() {
    m_useAnisotropic = false;
    m_isChanged = true;
  }

  // Select secondary electron energy distribution parameterization
  void SetSplittingFunctionOpalBeaty();
  void SetSplittingFunctionGreenSawada();
  void SetSplittingFunctionFlat();

  // Switch on/off de-excitation handling
  void EnableDeexcitation();
  void DisableDeexcitation() { m_useDeexcitation = false; }
  // Switch on/off discrete photoabsorption levels
  void EnableRadiationTrapping();
  void DisableRadiationTrapping() { m_useRadTrap = false; }

  // Switch on/off simplified simulation of Penning transfers by means of
  // transfer probabilities (not compatible with de-excitation handling)
  void EnablePenningTransfer(const double r, const double lambda);
  void EnablePenningTransfer(const double r, const double lambda,
                             std::string gasname);
  void DisablePenningTransfer();
  void DisablePenningTransfer(std::string gasname);

  // When enabled, the gas cross-section table is written to file
  // when loaded into memory.
  void EnableCrossSectionOutput() { m_useCsOutput = true; }
  void DisableCrossSectionOutput() { m_useCsOutput = false; }

  // Multiply excitation cross-sections by a uniform scaling factor
  void SetExcitationScalingFactor(const double r, std::string gasname);

  bool Initialise(const bool verbose = false);
  void PrintGas();

  // Get the overall null-collision rate [ns-1]
  double GetElectronNullCollisionRate(const int band) override;
  // Get the (real) collision rate [ns-1] at a given electron energy e [eV]
  double GetElectronCollisionRate(const double e, const int band) override;
  // Get the collision rate [ns-1] for a specific level
  double GetElectronCollisionRate(const double e, const unsigned int level,
                                  const int band);
  // Sample the collision type
  bool GetElectronCollision(const double e, int& type, int& level, double& e1,
                            double& dx, double& dy, double& dz, 
                            std::vector<std::pair<int, double> >& secondaries,
                            int& ndxc, int& band) override;
  void ComputeDeexcitation(int iLevel, int& fLevel);
  unsigned int GetNumberOfDeexcitationProducts() const override { 
    return m_dxcProducts.size();
  }
  bool GetDeexcitationProduct(const unsigned int i, double& t, double& s, 
                              int& type, double& energy) const override;

  double GetPhotonCollisionRate(const double e) override;
  bool GetPhotonCollision(const double e, int& type, int& level, double& e1,
                          double& ctheta, int& nsec, double& esec) override;

  // Reset the collision counters
  void ResetCollisionCounters();
  // Get total number of electron collisions
  unsigned int GetNumberOfElectronCollisions() const;
  // Get number of collisions broken down by cross-section type
  unsigned int GetNumberOfElectronCollisions(int& nElastic, int& nIonising,
                                             int& nAttachment, int& nInelastic,
                                             int& nExcitation, 
                                             int& nSuperelastic) const;
  // Get number of cross-section terms
  int GetNumberOfLevels();
  // Get detailed information about a given cross-section term i
  bool GetLevel(const unsigned int i, int& ngas, int& type, std::string& descr,
                double& e);
  // Get number of collisions for a specific cross-section term
  unsigned int GetNumberOfElectronCollisions(const unsigned int level) const;

  int GetNumberOfPenningTransfers() const { return m_nPenning; }

  // Get total number of photon collisions
  int GetNumberOfPhotonCollisions() const;
  // Get number of photon collisions by collision type
  int GetNumberOfPhotonCollisions(int& nElastic, int& nIonising,
                                  int& nInelastic) const;

  void RunMagboltz(const double e, const double b, const double btheta,
                   const int ncoll, bool verbose, double& vx, double& vy,
                   double& vz, double& dl, double& dt, 
                   double& alpha, double& eta, double& lor,
                   double& vxerr, double& vyerr, double& vzerr,
                   double& dlerr, double& dterr, 
                   double& alphaerr, double& etaerr, double& lorerr,
                   double& alphatof);

  // Generate a new gas table (can later be saved to file)
  void GenerateGasTable(const int numCollisions = 10,
                        const bool verbose = true);

  double fit3d4p, fitHigh4p;
  double fit3dQCO2, fit3dEtaCO2;
  double fit3dQCH4, fit3dEtaCH4;
  double fit3dQC2H6, fit3dEtaC2H6;
  double fit4pEtaCH4;
  double fit4pEtaC2H6;
  double fit4sEtaC2H6;
  double fitLineCut;

 private:
  static const int nEnergySteps = 20000;
  static const int nEnergyStepsLog = 200;
  static const int nEnergyStepsGamma = 5000;
  static const int nMaxInelasticTerms = 250;
  static const int nMaxLevels = 512;
  static const int nCsTypes = 6;
  static const int nCsTypesGamma = 4;

  static const int DxcTypeRad;
  static const int DxcTypeCollIon;
  static const int DxcTypeCollNonIon;

  // Energy spacing of collision rate tables
  double m_eFinal, m_eStep;
  double m_eHigh, m_eHighLog;
  double m_lnStep;
  bool m_useAutoAdjust = true;

  // Flag enabling/disabling output of cross-section table to file
  bool m_useCsOutput = false;
  // Number of different cross-section types in the current gas mixture
  unsigned int m_nTerms = 0;
  // Recoil energy parameter
  double m_rgas[m_nMaxGases];
  // Opal-Beaty-Peterson splitting parameter [eV]
  double m_wOpalBeaty[nMaxLevels];
  // Green-Sawada splitting parameters [eV]
  double m_gsGreenSawada[m_nMaxGases];
  double m_gbGreenSawada[m_nMaxGases];
  double m_tsGreenSawada[m_nMaxGases];
  double m_taGreenSawada[m_nMaxGases];
  double m_tbGreenSawada[m_nMaxGases];
  bool m_hasGreenSawada[m_nMaxGases];
  // Energy loss
  std::vector<double> m_energyLoss;
  // Cross-section type
  std::vector<int> m_csType;
  // Parameters for calculation of scattering angles
  bool m_useAnisotropic = true;
  double m_scatParameter[nEnergySteps][nMaxLevels];
  double m_scatParameterLog[nEnergyStepsLog][nMaxLevels];
  std::vector<int> m_scatModel;
  double m_scatCut[nEnergySteps][nMaxLevels];
  double m_scatCutLog[nEnergyStepsLog][nMaxLevels];

  // Level description
  std::vector<std::string> m_description;

  // Total collision frequency
  std::vector<double> m_cfTot;
  std::vector<double> m_cfTotLog;
  // Null-collision frequency
  double m_cfNull;
  // Collision frequencies
  std::vector<std::vector<double> > m_cf;
  std::vector<std::vector<double> > m_cfLog;

  // Collision counters
  // 0: elastic
  // 1: ionisation
  // 2: attachment
  // 3: inelastic
  // 4: excitation
  // 5: super-elastic
  unsigned int m_nCollisions[nCsTypes];
  // Number of collisions for each cross-section term
  std::vector<unsigned int> m_nCollisionsDetailed;

  // Penning transfer
  // Penning transfer probability (by level)
  std::vector<double> m_rPenning;
  // Mean distance of Penning ionisation (by level)
  std::vector<double> m_lambdaPenning;
  // Number of Penning ionisations
  unsigned int m_nPenning = 0;

  // Deexcitation
  // Flag enabling/disabling detailed simulation of de-excitation process
  bool m_useDeexcitation = false;
  // Flag enabling/disable radiation trapping
  // (absorption of photons discrete excitation lines)
  bool m_useRadTrap = true;

  struct deexcitation {
    // Gas component
    int gas;
    // Associated cross-section term
    int level;
    // Level description
    std::string label;
    // Energy
    double energy;
    // Number of de-excitation channels
    int nChannels;
    // Branching ratios
    std::vector<double> p;
    // Final levels
    std::vector<int> final;
    // Type of transition
    std::vector<int> type;
    // Oscillator strength
    double osc;
    // Total decay rate
    double rate;
    // Doppler broadening
    double sDoppler;
    // Pressure broadening
    double gPressure;
    // Effective width
    double width;
    // Integrated absorption collision rate
    double cf;
  };
  std::vector<deexcitation> m_deexcitations;
  // Mapping between deexcitations and cross-section terms.
  int m_iDeexcitation[nMaxLevels];

  // List of de-excitation products
  int nDeexcitationProducts;
  struct dxcProd {
    // Radial spread
    double s;
    // Time delay
    double t;
    // Type of deexcitation product
    int type;
    // Energy of the electron or photon
    double energy;
  };
  std::vector<dxcProd> m_dxcProducts;

  // Ionisation potentials
  double m_ionPot[m_nMaxGases];
  // Minimum ionisation potential
  double m_minIonPot;

  // Scaling factor for excitation cross-sections
  double m_scaleExc[m_nMaxGases];
  // Flag selecting secondary electron energy distribution model
  bool m_useOpalBeaty = true;
  bool m_useGreenSawada = false;

  // Energy spacing of photon collision rates table
  double m_eFinalGamma, m_eStepGamma;
  // Number of photon collision cross-section terms
  int m_nPhotonTerms;
  // Total photon collision frequencies
  std::vector<double> m_cfTotGamma;
  // Photon collision frequencies
  std::vector<std::vector<double> > m_cfGamma;
  std::vector<int> csTypeGamma;
  // Photon collision counters
  // 0: elastic
  // 1: ionisation
  // 2: inelastic
  // 3: excitation
  int m_nPhotonCollisions[nCsTypesGamma];

  bool GetGasNumberMagboltz(const std::string& input, int& number) const;
  bool Mixer(const bool verbose = false);
  void SetupGreenSawada();
  void ComputeAngularCut(const double parIn, double& cut, double& parOut) const;
  void ComputeDeexcitationTable(const bool verbose);
  void ComputeDeexcitationInternal(int iLevel, int& fLevel);
  bool ComputePhotonCollisionTable(const bool verbose);
};
}
#endif
