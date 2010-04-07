// Interface to Magboltz program (version 8.6)

#ifndef G_MEDIUM_MAGBOLTZ_86
#define G_MEDIUM_MAGBOLTZ_86

#include <vector>

#include "Medium.hh"

namespace Garfield {

extern "C" {
  
  // COMMON blocks

  extern struct {
    double eovb;
    double wb;
    double btheta, bmag;
  } bfld_;

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
    double tmax;
    double small;
    double api;
    double estart;
    double theta, phi;
    double tcfmax[8];
    double rstart;
    double efield;
    long long nmax;
  } setp_;
  
  extern struct {
    double echarg;
    double emass;
    double amu;
    double pir2;
  } cnsts_;  

  // Gas mixture   
  extern struct {
    long long ngasn[6];
  } gasn_; 
  extern struct {
    double an1, an2, an3, an4, an5, an6, an;
    double frac[6];
  } ratio_;
   
  // Output
  extern struct {
    double wx;
    double wy;
    double wz;
  } vel_;  
  extern struct {
    double difxx, difyy, difzz;
    double difyz, difxy, difxz;
  } diflab_;
  extern struct {
    double difln;
    double diftr;
  } difvel_;
  extern struct {
    double alpha;
    double att;
  } ctowns_; 

  void gasmix_(long long* ngs, double* q, 
        double* qin, long long* nin, double* e, double* ei, char* name, 
        double* virl, double* eb, double* peqel, double* peqin, 
        double* penfra, long long* kel, long long* kin, char scrpt[226][30]);

  void setup1_();

  void mixer_();

  void elimit_(long long* ielow);
  void elimitb_(long long* ielow);
  void elimitc_(long long* ielow);
  
  void monte_();
  void montea_();
  void monteb_();
  void montec_();

  void alpcalc_();
  void alpclca_();
  void alpclcb_();
  void alpclcc_();

  void prnter_();
  void output_();
  void output2_();

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
    
    // Set/get the highest electron energy to be included 
    // in the scattering rates table
    bool   SetMaxElectronEnergy(const double e);
    double GetMaxElectronEnergy() const {return eFinal;}

    // Switch on/off automatic adjustment of max. energy when an
    // energy exceeding the present range is requested
    void EnableEnergyRangeAdjustment() {adjust = true;}
    void DisableEnergyRangeAdjustment() {adjust = false;}

    // Switch on/off anisotropic scattering (enabled by default)
    void EnableAnisotropicScattering() {anisotropic = true;}
    void DisableAnisotropicScattering() {anisotropic = false;}
    
    // Switch on/off de-excitation treatment
    void EnableDeexcitation()  {deexcitation = true;}
    void DisableDeexcitation() {deexcitation = false;}

    // When enabled, the gas cross-section table is written to file
    // when loaded into memory
    void EnableCrossSectionOutput()  {csoutput = true;}
    void DisableCrossSectionOutput() {csoutput = false;}
    
    // Get the overall null-collision rate [ns-1]
    double GetElectronNullCollisionRate();
    // Get the (real) collision rate [ns-1] at a given electron energy e [eV]
    double GetElectronCollisionRate(const double e);
    // Sample the collision type
    bool   GetElectronCollision(const double e, int& type, int& level, 
                        double& e1, double& ctheta, 
                        double& s, double& esec);

    double GetPhotonCollisionRate(const double e);
    bool   GetPhotonCollision(const double e, int& type, int& level,
                              double& e1, double& ctheta, 
                              double& s, double& esec);

    // Reset the collision counters
    void ResetCollisionCounters();
    // Get total number of collisions
    int GetNumberOfCollisions() const;
    // Get number of collisions broken down by cross-section type
    int GetNumberOfCollisions(int& nElastic, int& nIonising, int& nAttachment,
                              int& nInelastic, int& nExcitation, 
                              int& nSuperelastic) const;
    // Get number of cross-section terms                              
    int GetNumberOfLevels();
    // Get detailed information about a given cross-section term i
    bool GetLevel(const int i, int& gas, int& type, 
                  std::string& descr, double& e);    
    // Get number of collisions for a specific cross-section term    
    int GetNumberOfCollisions(const int level) const;

    void RunMagboltz(const double e, const double b, const double btheta,
                     const int ncoll = 5, bool verbose = true);
                                                                   
  private:

    static const int nEnergySteps = 4000;
    static const int nMaxGases = 6;
    static const int nMaxInelasticTerms = 220;
    static const int nMaxLevels = 512;
    static const int nMaxPhotonLevels = 12;
    
    // Gas mixture
    int gas[nMaxGases];
    double fraction[nMaxGases];
   
    // Energy spacing of collision rate tables
    double eFinal, eStep;
    bool adjust; 
  
    // Flag enabling/disabling output of cross-section table to file
    bool csoutput;
    // Number of different cross-section types in the current gas mixture
    int nTerms;
    // Recoil energy parameter
    double rgas[nMaxLevels];
    // For ionisation: Opal-Beaty-Peterson splitting parameter [eV]
    // For excitation: Photon energy
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
   
    // Collision counters
    // 0: elastic
    // 1: ionisation
    // 2: attachment
    // 3: inelastic
    // 4: excitation
    // 5: super-elastic
    int nCollisions[6];
    // Number of collisions for each cross-section term
    std::vector<int> nCollisionsDetailed;

    // Deexcitation rates
    bool deexcitation;
    double fDeexcitation[nMaxLevels];
    double fRadiative[nMaxLevels];
    double fCollIon[nMaxLevels];
    double fCollLoss[nMaxLevels];
    // Minimum ionisation potential
    double minIonPot;

    // Number of photon collision cross-section terms
    int nPhotonTerms;    
    // Total photon collision frequencies
    std::vector<double> cfTotGamma;
    // Photon collision frequencies
    double cfGamma[nEnergySteps][nMaxPhotonLevels];
    int csTypeGamma[nMaxPhotonLevels];
  
    bool GetGasNumber(std::string gasname, int& number) const;
    bool GetGasName(const int number, std::string& gasname) const;
    bool Mixer();
    void ComputeAngularCut(double parIn, float& cut, double &parOut);
    void ComputeDeexcitationTable();
    bool ComputePhotonCollisionTable();

};

}
#endif
