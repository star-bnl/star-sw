// Interface to Magboltz program (version 8)

#ifndef G_MEDIUM_MAGBOLTZ_86
#define G_MEDIUM_MAGBOLTZ_86

#include <vector>

#include "Medium.hh"

namespace Garfield {

extern "C" {
  
  // Magboltz COMMON blocks

  // Magnetic field
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
  
  // Physical constants
  extern struct {
    double echarg;
    double emass;
    double amu;
    double pir2;
  } cnsts_;  

  // Definition of the gas mixture   
  extern struct {
    long long ngasn[6];
  } gasn_; 
  extern struct {
    double an1, an2, an3, an4, an5, an6, an;
    double frac[6];
  } ratio_;
   
  // Calculation results
  // Drift velocity
  extern struct {
    double wx, wy, wz;
  } vel_;  
  extern struct {
    double dwx, dwy, dwz;
  } velerr_;

  // Diffusion
  extern struct {
    double difxx, difyy, difzz;
    double difyz, difxy, difxz;
  } diflab_;
  extern struct {
    double dxxer, dyyer, dzzer;
    double dyzer, dxyer, dxzer;
  } diferb_;
  extern struct {
    double difln, diftr;
  } difvel_;
  extern struct {
    double dfler, dfter;
  } diferl_;

  // Townsend and attachment coefficient
  extern struct {
    double alpha, att;
  } ctowns_; 
  extern struct {
    double alper, atter;
  } ctwner_;
  extern struct {
    double ralpha, ralper;
    double tofene, tofener, tofwv, tofwver;
    double tofdl, tofdler, tofdt, tofdter;
    double tofwr, tofwrer;
    double rattof, ratofer;
  } tofout_;

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

    bool IsGas() const {return true;}
  
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
    void GetComponent(const int i, std::string& label, double& f);
    
    void   SetAtomicNumber(const double z);
    double GetAtomicNumber() const;
    void   SetAtomicWeight(const double a);
    double GetAtomicWeight() const;
    void   SetNumberDensity(const double n);
    double GetNumberDensity() const;
    void   SetMassDensity(const double rho);
    double GetMassDensity() const;
 
    // Set/get the highest electron energy to be included 
    // in the scattering rates table
    bool   SetMaxElectronEnergy(const double e);
    double GetMaxElectronEnergy() const {return eFinal;}

    // Set/get the highest photon energy to be included
    // in the scattering rates table
    bool   SetMaxPhotonEnergy(const double e);
    double GetMaxPhotonEnergy() const {return eFinalGamma;}

    // Switch on/off automatic adjustment of max. energy when an
    // energy exceeding the present range is requested
    void EnableEnergyRangeAdjustment()  {useAutoAdjust = true;}
    void DisableEnergyRangeAdjustment() {useAutoAdjust = false;}

    // Switch on/off anisotropic scattering (enabled by default)
    void EnableAnisotropicScattering()  {
      useAnisotropic = true;  isChanged = true;
    }
    void DisableAnisotropicScattering() {
      useAnisotropic = false; isChanged = true;
    }

    // Switch on/off secondary electron energy distribution 
    // according to Opal et al. (enabled by default) 
    // If switched off, a flat distribution is used (for test purposes)
    void EnableSplittingFunction()  {useSplittingFunction = true;}
    void DisableSplittingFunction() {useSplittingFunction = false;}
    
    // Switch on/off de-excitation handling
    void EnableDeexcitation();
    void DisableDeexcitation() {useDeexcitation = false;}
    // Switch on/off discrete photoabsorption levels
    void EnableRadiationTrapping();
    void DisableRadiationTrapping() {useRadTrap = false;}

    // Switch on/off simplified simulation of Penning transfers by means of 
    // transfer probabilities (not compatible with de-excitation handling)
    void EnablePenningTransfer(const double r, const double lambda);
    void EnablePenningTransfer(const double r, const double lambda,
                               const std::string gasname);
    void DisablePenningTransfer();
    void DisablePenningTransfer(const std::string gasname);

    // When enabled, the gas cross-section table is written to file
    // when loaded into memory
    void EnableCrossSectionOutput()  {useCsOutput = true;}
    void DisableCrossSectionOutput() {useCsOutput = false;}

    // Multiply excitation cross-sections by a uniform scaling factor
    void SetExcitationScalingFactor(const double r);
 
    // Get the overall null-collision rate [ns-1]
    double GetElectronNullCollisionRate();
    // Get the (real) collision rate [ns-1] at a given electron energy e [eV]
    double GetElectronCollisionRate(const double e, const int band);
    // Sample the collision type
    bool   GetElectronCollision(const double e, int& type, int& level, 
                                double& e1, double& ctheta, 
                                int& nsec, double& esec, int& band);
    int  GetNumberOfDeexcitationProducts() {return nDeexcitationProducts;}
    bool GetDeexcitationProduct(const int i, double& t, double& s, 
                                int& type, double& energy); 

    double GetPhotonCollisionRate(const double e);
    bool   GetPhotonCollision(const double e, int& type, int& level,
                              double& e1, double& ctheta, 
                              int& nsec, double& esec);

    // Reset the collision counters
    void ResetCollisionCounters();
    // Get total number of electron collisions
    int GetNumberOfElectronCollisions() const;
    // Get number of collisions broken down by cross-section type
    int GetNumberOfElectronCollisions(
                  int& nElastic, int& nIonising, int& nAttachment,
                  int& nInelastic, int& nExcitation, int& nSuperelastic) const;
    // Get number of cross-section terms                              
    int GetNumberOfLevels();
    // Get detailed information about a given cross-section term i
    bool GetLevel(const int i, int& ngas, int& type, 
                  std::string& descr, double& e);    
    // Get number of collisions for a specific cross-section term    
    int GetNumberOfElectronCollisions(const int level) const;

    int GetNumberOfPenningTransfers() const {return nPenning;}

    // Get total number of photon collisions
    int GetNumberOfPhotonCollisions() const;
    // Get number of photon collisions by collision type
    int GetNumberOfPhotonCollisions(
                  int& nElastic, int& nIonising, int& nInelastic) const;

    void RunMagboltz(const double e, const double b, const double btheta,
                     const int ncoll, bool verbose,
                     double& vx, double& vy, double& vz, 
                     double& dl, double& dt,
                     double& alpha, double& eta,
                     double& vxerr, double& vyerr, double& vzerr,
                     double& dlerr, double& dterr,
                     double& alphaerr, double& etaerr,
                     double& alphapt, double& etapt,
                     double& alphapterr, double& etapterr,
                     double& alphatof);
 
    void SetIonMobility(const double e, const double mu);
    bool IonVelocity(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& vx, double& vy, double& vz);

  private:

    static const int nEnergySteps = 20000;
    static const int nEnergyStepsGamma = 1000;
    static const int nMaxGases = 6;
    static const int nMaxInelasticTerms = 220;
    static const int nMaxLevels = 512;
    static const int nCsTypes = 6;
   
    // Gas mixture
    int gas[nMaxGases];
    double fraction[nMaxGases];
    double atWeight[nMaxGases];
    double atNum[nMaxGases];
   
    // Energy spacing of collision rate tables
    double eFinal, eStep;
    bool useAutoAdjust;
  
    // Flag enabling/disabling output of cross-section table to file
    bool useCsOutput;
    // Number of different cross-section types in the current gas mixture
    int nTerms;
    // Recoil energy parameter
    double rgas[nMaxLevels];
    // For ionisation: Opal-Beaty-Peterson splitting parameter [eV]
    double wSplit[nMaxLevels];
    // Energy loss
    double energyLoss[nMaxLevels];
    // Cross-section type
    int csType[nMaxLevels];
    // Parameters for calculation of scattering angles
    bool useAnisotropic;
    double scatParameter[nEnergySteps][nMaxLevels];
    int    scatModel[nMaxLevels];
    double scatCut[nEnergySteps][nMaxLevels];
    
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

    // Penning transfer
    // Flag enabling/disabling Penning transfer
    bool usePenning;
    // Penning transfer probability
    double rPenningGlobal;
    double rPenningGas[nMaxGases];
    double rPenning[nMaxLevels];
    // Mean distance of Penning ionization
    double lambdaPenningGlobal;
    double lambdaPenningGas[nMaxGases];
    double lambdaPenning[nMaxLevels];
    // Number of Penning ionisations
    int nPenning;

    // Deexcitation 
    // Flag enabling/disabling detailed simulation of de-excitation process
    bool useDeexcitation;
    // Flag enabling/disable radiation trapping (re-absorption of 
    // photons produced in the de-excitation cascade)
    bool useRadTrap;

    int nDeexcitations;
    struct deexcitation {
      // Gas component
      int gas;
      // Level description
      std::string label;
      // Energy
      double energy;
      // Number of de-excitation channels
      int nChannels;
      // Branching ratio
      std::vector<double> p;
      // Final level
      std::vector<int> final;
      // Type of transition
      std::vector<int> type;
      // Oscillator strength
      double osc;
      // Total rate
      double rate; 
    };
    std::vector<deexcitation> deexcitations;
    int iDeexcitation[nMaxLevels];
    int nDeexcitationProducts;
    // List of de-excitation products
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
    std::vector<dxcProd> dxcProducts;

    // Ionisation potentials
    double ionPot[nMaxGases];
    // Minimum ionisation potential
    double minIonPot;

    // Scaling factor for excitation cross-sections
    double scaleExc;
    // Flag switching on/off secondary electron energy distribution
    bool useSplittingFunction;

    // Energy spacing of photon collision rates table
    double eFinalGamma, eStepGamma;
    // Number of photon collision cross-section terms
    int nPhotonTerms; 
    // Total photon collision frequencies
    std::vector<double> cfTotGamma;
    // Photon collision frequencies
    std::vector<std::vector<double> > cfGamma;
    std::vector<int> csTypeGamma;
    // Photon collision counters
    // 0: elastic
    // 1: ionisation
    // 2: inelastic
    // 3: excitation
    int nPhotonCollisions[4];

    // Ion transport properties
    int nIonMobilities;
    std::vector<double> ionMobilityGrid;
    std::vector<double> ionMobilityValues;

    bool GetGasNumber(std::string gasname, int& number) const;
    bool GetGasInfo(const int number, std::string& gasname, 
                    double& a, double& z) const;
    bool Mixer();
    void ComputeAngularCut(double parIn, double& cut, double &parOut);
    void ComputeDeexcitationTable();
    void ComputeDeexcitation(int iLevel);
    bool ComputePhotonCollisionTable();

};

}
#endif
