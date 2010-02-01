// Interface to Magboltz program (version 8.6)

#ifndef G_MEDIUM_MAGBOLTZ_86
#define G_MEDIUM_MAGBOLTZ_86

#include <vector>

#include "Medium.hh"

namespace Garfield {

extern "C" {
  
  // COMMON blocks used in gas cross-section subroutines
  extern struct {
    long long nGas;
    long long nStep;
    long long nAniso;
    double efinal;
    double estep;
    double akt;
    double ary;
    double tempc;
    double torr;
    long long ipen;
  } inpt_; 
  
  extern struct {
    double echarg;
    double emass;
    double amu;
    double pir2;
  } cnsts_;  
    
  extern struct {
    double an1, an2, an3, an4, an5, an6, an;
    double frac[6];
  } ratio_;    

  void gasmix_(long long* ngs, double* q, 
        double* qin, long long* nin, double* e, double* ei, char* name, 
        double* virl, double* eb, double* peqel, double* peqin, 
        double* penfra, long long* kel, long long* kin, char scrpt[226][30]);

}

class MediumMagboltz86 : public Medium {

  public:
    // Constructor
    MediumMagboltz86();
    // Destructor
    ~MediumMagboltz86() {}    
  
    // Set/get the gas mixture
    bool SetComposition(const std::string gas1, const double f1, 
                        const std::string gas2 = "", const double f2 = 0.,
                        const std::string gas3 = "", const double f3 = 0.,
                        const std::string gas4 = "", const double f4 = 0.,
                        const std::string gas5 = "", const double f5 = 0., 
                        const std::string gas6 = "", const double f6 = 0.);
    void GetComposition(std::string& gas1, double& f1,
                        std::string& gas2, double& f2,
                        std::string& gas3, double& f3,
                        std::string& gas4, double& f4,
                        std::string& gas5, double& f5,
                        std::string& gas6, double& f6);
    void   GetComponent(const int i, std::string& label, double& f);
    
    // Set/get the highest electron energy to be included in the cross-section table
    bool   SetMaxEnergy(const double e);
    double GetMaxEnergy() const;

    // Switch on/off anisotropic scattering (enabled by default)
    void   EnableAnisotropicScattering() {anisotropic = true;}
    void   DisableAnisotropicScattering() {anisotropic = false;}
    
    // Get the (real) collision rate [ns-1] at a given electron energy e [eV]
    double GetCollisionRate(const double e);
    // Sample the collision type
    bool   GetCollision(const double e, int& type, int& level, double& s, 
                        double& ctheta, double& eloss, double& esec);
    // Get the number of null-collision intervals
    bool   GetNullCollisionIntervals(int& nIntervals, double& emax) const;
    // Get the null-collision rate [ns-1] in the given interval
    double GetNullCollisionRate(const int interval);
    // Get the overall null-collision rate [ns-1]
    double GetNullCollisionRate();

    // Reset the collision counters
    void ResetCollisionCounters();
    // Get total number of collisions
    int GetNumberOfCollisions() const;
    // Get number of collisions broken down by cross-section type
    int GetNumberOfCollisions(int& nElastic, int& nIonising, int& nAttachment,
                              int& nInelastic, int& nSuperelastic) const;
    // Get number of cross-section terms                              
    int GetNumberOfLevels();
    // Get detailed information about a given cross-section term i
    bool GetLevel(const int i, int& gas, int& type, 
                  std::string& descr, double& e);    
    // Get number of collisions for a specific cross-section term    
    int GetNumberOfCollisions(const int level) const;
                                                                   
  private:

    static const int nEnergySteps = 4000;
    static const int nMaxGases = 6;
    static const int nMaxInelasticTerms = 220;
    static const int nMaxLevels = 512;
    
    // Gas mixture
    int gas[nMaxGases];
    double fraction[nMaxGases];
   
    // Energy spacing of collision rate tables
    double eFinal, eStep; 
  
    // Number of different cross-section types in the current gas mixture
    int nTerms;
    // Recoil energy parameter
    double rgas[nMaxLevels];
    // Parameter for Opal-Beaty-Peterson splitting function [eV]
    double wSplit[nMaxLevels];
    // Energy loss
    double energyLoss[nMaxLevels];
    // Cross-section type
    int csType[nMaxLevels];
    // Parameters for calculation of scattering angles
    bool anisotropic;
    double scatParameter[nEnergySteps][nMaxLevels];
    int    scatModel[nMaxLevels];
    float  scatCut[nEnergySteps][nMaxLevels];
    
    // Level description
    char description[nMaxLevels][30];    
    
    // Total collision frequency
    double cfTot[nEnergySteps];
    // Null-collision frequencies
    double cfNull[8];  
    // Collision frequencies
    double cf[nEnergySteps][nMaxLevels];
   
    // Ionisation and attachment rates
    double cfIon[nEnergySteps];
    double cfAtt[nEnergySteps];
    
    // Collision counters
    // 0: elastic
    // 1: ionisation
    // 2: attachment
    // 3: inelastic
    // 4: super-elastic
    int nCollisions[5];
    // Number of collisions for each cross-section term
    std::vector<int> nCollisionsDetailed;
  
    bool GetGasNumber(std::string gas, int& number) const;
    bool GetGasName(const int number, std::string& gas) const;
    bool Mixer();
    void ComputeAngularCut(double parIn, float& cut, double &parOut);
  
};

}
#endif
