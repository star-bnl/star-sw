// Abstract base class for media

#ifndef G_MEDIUM_H
#define G_MEDIUM_H

#include <string>

namespace Garfield {

class Medium {

  public:
    // Constructor    
    Medium();
    // Destructor
    virtual ~Medium() {}

    // Return the id number of the class instance
    int    GetId() const {return id;}
    // Medium name/identifier
    void   SetName(const std::string s) {name = s;}
    std::string GetName() const {return name;}

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
    void   SetAtomicNumber(const double z);
    double GetAtomicNumber() const       {return atomicNumber;}
    void   SetAtomicWeight(const double a);
    double GetAtomicWeight() const       {return atomicWeight;}
    // Number density [cm-3] and mass density [g/cm3]
    void   SetNumberDensity(const double n);
    double GetNumberDensity() const      {return density;}
    void   SetMassDensity(const double rho);
    double GetMassDensity() const;

    // Transport properties
    virtual 
    void EnableDrift()  {driftable = true;}
    void DisableDrift() {driftable = false;}
    virtual 
    void EnablePrimaryIonisation()  {ionisable = true;}
    void DisablePrimaryIonisation() {ionisable = false;}

    bool IsDriftable()   {return driftable;}
    bool IsMicroscopic() {return microscopic;}
    bool IsIonisable()   {return ionisable;}

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
    // Null-collision rate [ns-1]
    virtual 
    double GetElectronNullCollisionRate();
    // Collision rate [ns-1] for given electron energy
    virtual 
    double GetElectronCollisionRate(const double e);
    virtual 
    bool GetElectronCollision(const double e, int& type, int& level, double& e1,
                      double& ctheta, double& s, double& esec);
    virtual 
    int GetNumberOfLevels();

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

    // Transport parameters for (negative) ions
    // Drift velocity [cm / ns]
    virtual 
    bool IonVelocity(const double ex, const double ey, const double ez, 
                     const double bx, const double by, const double bz, 
                     double& vx, double& vy, double& vz);
    // Longitudinal and transverse diffusion coefficients  [cm1/2]
    virtual 
    bool IonDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& dl, double& dt);
    // Dissociation coefficient
    virtual 
    bool IonDissociation(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& diss);    

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
                            double& ctheta, double& s, double& esec);

    // Plotting
    void PlotElectronVelocity(const double emin, const double emax);
    void PlotHoleVelocity(const double emin, const double emax);
    void PlotIonVelocity(const double emin, const double emax);
    void PlotElectronHoleVelocity(const double emin, const double emax);
    void PlotElectronIonVelocity(const double emin, const double emax);

    void PlotElectronTownsend(const double emin, const double emax);
    void PlotHoleTownsend(const double emin, const double emax);
    void PlotElectronHoleTownsend(const double emin, const double emax);

    void PlotElectronAttachment(const double emin, const double emax);
    void PlotHoleAttachment(const double emin, const double emax);
    void PlotElectronHoleAttachment(const double emin, const double emax);

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}
    void EnableWarnings()   {warning = true;}
    void DisableWarnings()  {warning = false;}

  protected:

    static int idCounter;

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
    // Update flag
    bool isChanged;

    // Switch on/off debugging and warning messages
    bool debug, warning;

    void PlotVelocityCommon(const double emin, const double emax);
    void PlotTownsendCommon(const double emin, const double emax);
    void PlotAttachmentCommon(const double emin, const double emax);

};

}

#endif
