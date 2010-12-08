// Solid crystalline silicon

#ifndef G_MEDIUM_SILICON_H
#define G_MEDIUM_SILICON_H

#include <string>
#include <vector>

#include "Medium.hh"

namespace Garfield {

class MediumSilicon : public Medium {

  public:
    // Constructor
    MediumSilicon();
    // Destructor
    ~MediumSilicon() {}

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

    // Microscopic transport properties
    bool   SetMaxElectronEnergy(const double e);
    double GetMaxElectronEnergy() const {return eFinalG;}

    // When enabled, the scattering rates table is written to file
    // when loaded into memory.
    void EnableScatteringRateOutput()  {useCfOutput = true;}
    void DisableScatteringRateOutput() {useCfOutput = false;}

    void EnableNonParabolicity()  {useNonParabolicity = true;}
    void DisableNonParabolicity() {useNonParabolicity = false;}
    void EnableFullBandDensityOfStates()  {useFullBandDos = true;}
    void DisableFullBandDensityOfStates() {useFullBandDos = false;}
    void EnableAnisotropy()  {useAnisotropy = true;}
    void DisableAnisotropy() {useAnisotropy = false;} 

    // Get the electron energy (and its gradient) 
    // for a given (crystal) momentum
    double GetElectronEnergy(const double px, const double py, const double pz,
                        double& vx, double& vy, double& vz, const int band = 0);
    // Get the electron (crystal) momentum for a given kinetic energy
    void GetElectronMomentum(const double e, 
                             double& px, double& py, double& pz, 
                             int& band);
    
    // Get the null-collision rate [ns-1]
    double GetElectronNullCollisionRate(const int band);
    // Get the (real) collision rate [ns-1] at a given electron energy
    double GetElectronCollisionRate(const double e, const int band);
    // Sample the collision type
    bool   GetElectronCollision(const double e, int& type, int& level,
                                double& e1,
                                double& dx, double& dy, double& dz,
                                int& nsec, double& esec, int& band);
    double GetConductionBandDensityOfStates(const double e, 
                                            const int band = 0);

    // Reset the collision counters
    void ResetCollisionCounters();
    // Get the total number of electron collisions
    int GetNumberOfElectronCollisions() const;
    // Get number of scattering rate terms
    int GetNumberOfLevels();
    // Get number of collisions for a specific level
    int GetNumberOfElectronCollisions(const int level) const;

    int GetNumberOfElectronBands();
    int GetElectronBandPopulation(const int band);

    bool GetOpticalDataRange(double& emin, double& emax, const int i = 0);
    bool GetDielectricFunction(const double e, double& eps1, double& eps2, const int i = 0);

  private:

    static const int LatticeMobilityModelSentaurus   = 0;
    static const int LatticeMobilityModelMinimos     = 1;
    static const int LatticeMobilityModelReggiani    = 2;
    static const int DopingMobilityModelMinimos      = 0;
    static const int DopingMobilityModelMasetti      = 1;
    static const int SaturationVelocityModelMinimos  = 0;
    static const int SaturationVelocityModelCanali   = 1;
    static const int SaturationVelocityModelReggiani = 2;
    static const int HighFieldMobilityModelMinimos   = 0;
    static const int HighFieldMobilityModelCanali    = 1;
    static const int HighFieldMobilityModelReggiani  = 2;
    static const int HighFieldMobilityModelConstant  = 3;
    static const int ImpactIonisationModelVanOverstraeten = 0;
    static const int ImpactIonisationModelGrant = 1;

    double bandGap;
    // Doping
    char   dopingType;
    double dopingConcentration;

    // Effective masses
    // X valleys
    double mLongX, mTransX;
    // L valleys
    double mLongL, mTransL;
    // Lattice mobility
    double eLatticeMobility, hLatticeMobility;
    // Low-field mobility
    double eMobility, hMobility;
    // High-field mobility parameters
    double eBetaCanali, hBetaCanali;
    // Saturation velocity
    double eSatVel, hSatVel;
    // Hall factor
    double eHallFactor, hHallFactor;
    
    // Trapping parameters
    double eTrapCs, hTrapCs;
    double eTrapDensity, hTrapDensity;
    double eTrapTime, hTrapTime;
    int trappingModel;
    
    // Impact ionisation parameters
    double eImpactA0, eImpactA1, eImpactA2;
    double eImpactB0, eImpactB1, eImpactB2;
    double hImpactA0, hImpactA1, hImpactA2;
    double hImpactB0, hImpactB1, hImpactB2;    
    
    // Models
    bool hasUserMobility;
    bool hasUserSaturationVelocity;
    int latticeMobilityModel;
    int dopingMobilityModel;
    int saturationVelocityModel;
    int highFieldMobilityModel;
    int impactIonisationModel;

    // Options 
    bool useCfOutput;
    bool useNonParabolicity;
    bool useFullBandDos;
    bool useAnisotropy;
 
    // Scattering rates
    double eFinalXL, eStepXL;
    double eFinalG, eStepG;
    static const int nEnergyStepsXL = 2000;
    static const int nEnergyStepsG = 2000;

    // Number of scattering terms
    int nLevelsX, nLevelsL, nLevelsG;
    // Number of valleys
    int nValleysX, nValleysL;
    // Energy offset
    double eMinL, eMinG;
    int ieMinL, ieMinG;

    double cfNullElectronsX, cfNullElectronsL, cfNullElectronsG;
    std::vector<double> cfTotElectronsX;
    std::vector<double> cfTotElectronsL;
    std::vector<double> cfTotElectronsG;
    std::vector<std::vector<double> > cfElectronsX;
    std::vector<std::vector<double> > cfElectronsL;
    std::vector<std::vector<double> > cfElectronsG;
    std::vector<double> energyLossElectronsX;
    std::vector<double> energyLossElectronsL;
    std::vector<double> energyLossElectronsG;
    // Cross-section type
    std::vector<int> scatTypeElectronsX;
    std::vector<int> scatTypeElectronsL;
    std::vector<int> scatTypeElectronsG;

    int nCollElectronAcoustic, nCollElectronOptical;
    int nCollElectronIntervalley;
    int nCollElectronImpurity;
    int nCollElectronIonisation;
    std::vector<int> nCollElectronDetailed;    
    std::vector<int> nCollElectronBand;

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
    bool ElectronImpactIonisationVanOverstraetenDeMan(
                                       const double e, double& alpha) const;
    bool ElectronImpactIonisationGrant(const double e, double& alpha) const;
    bool HoleMobilityMinimos(const double e, double& mu) const;
    bool HoleMobilityCanali(const double e, double& mu) const;
    bool HoleMobilityReggiani(const double e, double& mu) const;
    bool HoleImpactIonisationVanOverstraetenDeMan(
                                   const double e, double& alpha) const;
    bool HoleImpactIonisationGrant(const double e, double& alpha) const;
        
    // Optical data
    bool hasOpticalData;
    std::string opticalDataFile;
    struct opticalData {
      // Energy [eV]
      double energy;
      // Dielectric function
      double eps1, eps2;
    };
    std::vector<opticalData> opticalDataTable;    
    bool LoadOpticalData(const std::string filename);

    bool ElectronScatteringRates();
    bool ElectronAcousticScatteringRates();
    bool ElectronIntervalleyScatteringRatesXX();
    bool ElectronIntervalleyScatteringRatesXL();
    bool ElectronIntervalleyScatteringRatesLL();
    bool ElectronIonisationRates();
    bool ElectronImpurityScatteringRates();

};

}

#endif
