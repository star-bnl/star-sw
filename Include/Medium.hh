// Abstract base class for media

#ifndef G_MEDIUM_H
#define G_MEDIUM_H

#include <string>
#include <vector>

namespace Garfield {

class Medium {

  public:
    // Constructor    
    Medium();
    // Destructor
    virtual ~Medium() {}

    // Return the id number of the class instance
    int GetId() const {return id;}
    // Medium name/identifier
    std::string GetName() const {return name;}
    virtual
    bool IsGas() const {return false;}

    // Temperature [K]
    void   SetTemperature(const double t);
    double GetTemperature() const {return temperature;}
    // Pressure [Torr]
    void   SetPressure(const double p);
    double GetPressure() const {return pressure;}
    // Relative static dielectric constant
    void   SetDielectricConstant(const double eps);
    double GetDielectricConstant() const {return epsilon;}
    
    // Get number of components
    int    GetNumberOfComponents() const {return nComponents;}
    virtual
    void   GetComponent(const int i, std::string& label, double& f);
    // Effective atomic number and weight
    virtual
    void   SetAtomicNumber(const double z);
    virtual
    double GetAtomicNumber() const       {return atomicNumber;}
    virtual
    void   SetAtomicWeight(const double a);
    virtual
    double GetAtomicWeight() const       {return atomicWeight;}
    // Number density [cm-3] and mass density [g/cm3]
    virtual
    void   SetNumberDensity(const double n);
    virtual
    double GetNumberDensity() const      {return density;}
    virtual
    void   SetMassDensity(const double rho);
    virtual
    double GetMassDensity() const;

    // Transport properties
    virtual 
    void EnableDrift()  {driftable = true;}
    void DisableDrift() {driftable = false;}
    virtual 
    void EnablePrimaryIonisation()  {ionisable = true;}
    void DisablePrimaryIonisation() {ionisable = false;}

    bool IsDriftable() const   {return driftable;}
    bool IsMicroscopic() const {return microscopic;}
    bool IsIonisable() const   {return ionisable;}

    // W value and Fano factor
    void   SetW(const double w) {wValue = w;};
    double GetW() {return wValue;}
    void   SetFanoFactor(const double f) {fanoFactor = f;}
    double GetFanoFactor() {return fanoFactor;}

    // Transport parameters for electrons    
    // Drift velocity [cm / ns]
    virtual 
    bool ElectronVelocity(const double ex, const double ey, const double ez, 
                          const double bx, const double by, const double bz, 
                          double& vx, double& vy, double& vz);
    // Longitudinal and transverse diffusion coefficients [cm1/2]
    virtual 
    bool ElectronDiffusion(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& dl, double& dt);
    // Diffusion tensor: diagonal elements are the diffusion 
    // coefficients [cm] along e, btrans, e x b, 
    // off-diagonal elements are covariances are the covariances
    virtual 
    bool ElectronDiffusion(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double cov[3][3]);
    // Ionisation coefficient [cm-1]
    virtual 
    bool ElectronTownsend(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& alpha);
    // Attachment coefficient [cm-1]
    virtual 
    bool ElectronAttachment(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& eta);    

    // Microscopic electron transport properties
    
    // Dispersion relation (Energy vs. wave vector)
    virtual
    double GetElectronEnergy(const double px, const double py, const double pz,
                        double& vx, double& vy, double& vz, const int band = 0);
    virtual
    void GetElectronMomentum(const double e, 
                             double& px, double& py, double& pz, 
                             const int band = 0);

    // Null-collision rate [ns-1]
    virtual 
    double GetElectronNullCollisionRate();
    // Collision rate [ns-1] for given electron energy
    virtual 
    double GetElectronCollisionRate(const double e, const int band = 0);
    virtual 
    bool GetElectronCollision(const double e, int& type, int& level, double& e1,
                      double& ctheta, int& nsec, double& esec, int& band);

    virtual
    int GetNumberOfDeexcitationProducts() {return 0;}
    virtual
    bool GetDeexcitationProduct(const int i, double& t, double& s, 
                                int& type, double& energy);

    // Transport parameters for holes
    virtual 
    bool HoleVelocity(const double ex, const double ey, const double ez, 
                      const double bx, const double by, const double bz, 
                      double& vx, double& vy, double& vz);
    virtual 
    bool HoleDiffusion(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double& dl, double& dt);
    virtual 
    bool HoleDiffusion(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double cov[3][3]);                       
    virtual 
    bool HoleTownsend(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& alpha);
    virtual 
    bool HoleAttachment(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& eta);                

    // Transport parameters for ions
    virtual 
    bool IonVelocity(const double ex, const double ey, const double ez, 
                     const double bx, const double by, const double bz, 
                     double& vx, double& vy, double& vz);
    virtual 
    bool IonDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& dl, double& dt);
    // Dissociation coefficient
    virtual 
    bool IonDissociation(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& diss);    

    // Select extrapolation method for fields below/above the table range.
    // Options are "constant"/"linear"/"exponential".
    void SetExtrapolationMethodVelocity(const std::string extrLow,
                                        const std::string extrHigh);
    void SetExtrapolationMethodDiffusion(const std::string extrLow,
                                         const std::string extrHigh);
    void SetExtrapolationMethodTownsend(const std::string extrLow,
                                        const std::string extrHigh);
    void SetExtrapolationMethodAttachment(const std::string extrLow,
                                          const std::string extrHigh);
    void SetExtrapolationMethodIonMobility(const std::string extrLow,
                                           const std::string extrHigh);
    void SetExtrapolationMethodIonDissociation(const std::string extrLow,
                                               const std::string extrHigh);

    // Set the degree of polynomial interpolation (usually 2).
    void SetInterpolationMethodVelocity(const int intrp);
    void SetInterpolationMethodDiffusion(const int intrp);
    void SetInterpolationMethodTownsend(const int intrp);
    void SetInterpolationMethodAttachment(const int intrp);
    void SetInterpolationMethodIonMobility(const int intrp);
    void SetInterpolationMethodIonDissociation(const int intrp);
    
    // Optical properties
    // Energy range [eV] of available optical data
    virtual 
    bool GetOpticalDataRange(double& emin, double& emax, const int i = 0);
    // Complex dielectric function
    virtual 
    bool GetDielectricFunction(const double e, 
                               double& eps1, double& eps2, const int i = 0);
    // Photoabsorption cross-section [cm2]
    virtual 
    bool GetPhotoAbsorptionCrossSection(const double e, 
                                        double& sigma, const int i = 0);
    virtual
    double GetPhotonCollisionRate(const double e);
    virtual 
    bool GetPhotonCollision(const double e, int& type, int& level, double& e1,
                            double& ctheta, int& nsec, double& esec);

    // Switch on/off debugging  messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    std::string className;

    static int idCounter;
    
    static double inverseElectronMass;

    // Id number
    int id;
    // Name
    std::string name;
    // Temperature [K]
    double temperature;
    // Pressure [Torr]
    double pressure;
    // Static dielectric constant
    double epsilon;
    // Number of components
    int nComponents;
    // (Effective) atomic number Z
    double atomicNumber;
    // Atomic weight A
    double atomicWeight;
    // Number density [cm-3]
    double density;

    // Transport flags
    bool driftable, microscopic, ionisable;

    // W value and Fano factor
    double wValue, fanoFactor;

    // Update flag
    bool isChanged;
    
    // Switch on/off debugging messages
    bool debug;

    // Field grids
    int nEfields;
    int nBfields;
    int nAngles;

    std::vector<double> eFields;
    std::vector<double> bFields;
    std::vector<double> bAngles;

    // Tables of transport parameters
    bool map2d;
    // Electrons
    bool hasElectronVelocityE, hasElectronVelocityB, hasElectronVelocityExB;
    bool hasElectronDiffLong, hasElectronDiffTrans, hasElectronDiffTens;
    bool hasElectronTownsend, hasElectronAttachment;
    std::vector<std::vector<std::vector<double> > > tabElectronVelocityE;
    std::vector<std::vector<std::vector<double> > > tabElectronVelocityExB;
    std::vector<std::vector<std::vector<double> > > tabElectronVelocityB;
    std::vector<std::vector<std::vector<double> > > tabElectronDiffLong;
    std::vector<std::vector<std::vector<double> > > tabElectronDiffTrans;
    std::vector<std::vector<std::vector<double> > > tabElectronTownsend;
    std::vector<std::vector<std::vector<double> > > tabElectronAttachment;

    std::vector<std::vector<std::vector<std::vector<double> > > > tabElectronDiffTens;

    // Holes
    bool hasHoleVelocityE, hasHoleVelocityB, hasHoleVelocityExB;
    bool hasHoleDiffLong, hasHoleDiffTrans, hasHoleDiffTens;
    bool hasHoleTownsend, hasHoleAttachment;
    std::vector<std::vector<std::vector<double> > > tabHoleVelocityE;
    std::vector<std::vector<std::vector<double> > > tabHoleVelocityExB;
    std::vector<std::vector<std::vector<double> > > tabHoleVelocityB;
    std::vector<std::vector<std::vector<double> > > tabHoleDiffLong;
    std::vector<std::vector<std::vector<double> > > tabHoleDiffTrans;
    std::vector<std::vector<std::vector<double> > > tabHoleTownsend;
    std::vector<std::vector<std::vector<double> > > tabHoleAttachment;

    std::vector<std::vector<std::vector<std::vector<double> > > > tabHoleDiffTens;

    // Ions
    bool hasIonMobility;
    bool hasIonDiffLong, hasIonDiffTrans;
    bool hasIonDissociation;
    std::vector<std::vector<std::vector<double> > > tabIonMobility;
    std::vector<std::vector<std::vector<double> > > tabIonDiffLong;
    std::vector<std::vector<std::vector<double> > > tabIonDiffTrans;
    std::vector<std::vector<std::vector<double> > > tabIonDissociation;

    // Thresholds for Townsend, attachment and dissociation coefficients.
    int thrElectronTownsend;
    int thrElectronAttachment;
 
    int thrHoleTownsend;
    int thrHoleAttachment;
    int thrIonDissociation;

    // Extrapolation methods
    int extrLowVelocity, extrHighVelocity;
    int extrLowDiffusion, extrHighDiffusion;
    int extrLowTownsend, extrHighTownsend;
    int extrLowAttachment, extrHighAttachment;
    int extrLowMobility, extrHighMobility;
    int extrLowDissociation, extrHighDissociation;

    // Interpolation methods
    int intpVelocity;
    int intpDiffusion;
    int intpTownsend;
    int intpAttachment;
    int intpMobility;
    int intpDissociation;
 
    double Interpolate1D(const double e,
                         const std::vector<double>& table, 
                         const std::vector<double>& fields,
                         const int intpMeth, 
                         const int jExtr, const int iExtr);
    bool GetExtrapolationIndex(std::string extrStr, int& extrNb);
    void InitParamArrays(const int eRes, const int bRes, const int aRes,
         std::vector<std::vector<std::vector<double> > >& tab, 
         const double val);
    void InitParamTensor(const int eRes, const int bRes, 
                         const int aRes, const int tRes,
         std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
         const double val);

    // Scaling of fields and transport parameters.
    virtual
    double ScaleElectricField(const double e) {return e;}
    virtual
    double ScaleVelocity(const double v) {return v;}
    virtual
    double ScaleDiffusion(const double d) {return d;}
    virtual
    double ScaleTownsend(const double alpha) {return alpha;}
    virtual
    double ScaleAttachment(const double eta) {return eta;}

};

}

#endif
