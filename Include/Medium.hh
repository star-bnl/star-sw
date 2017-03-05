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
  virtual ~Medium();

  // Return the id number of the class instance
  int GetId() const { return m_id; }
  // Medium name/identifier
  const std::string& GetName() const { return m_name; }
  virtual bool IsGas() const { return false; }
  virtual bool IsSemiconductor() const { return false; }

  // Temperature [K]
  void SetTemperature(const double t);
  double GetTemperature() const { return m_temperature; }
  // Pressure [Torr]
  void SetPressure(const double p);
  double GetPressure() const { return m_pressure; }
  // Relative static dielectric constant
  void SetDielectricConstant(const double eps);
  double GetDielectricConstant() const { return m_epsilon; }

  // Get number of components
  unsigned int GetNumberOfComponents() const { return m_nComponents; }
  virtual void GetComponent(const unsigned int i, 
                            std::string& label, double& f);
  // Effective atomic number and weight
  virtual void SetAtomicNumber(const double z);
  virtual double GetAtomicNumber() const { return m_z; }
  virtual void SetAtomicWeight(const double a);
  virtual double GetAtomicWeight() const { return m_a; }
  // Number density [cm-3] and mass density [g/cm3]
  virtual void SetNumberDensity(const double n);
  virtual double GetNumberDensity() const { return m_density; }
  virtual void SetMassDensity(const double rho);
  virtual double GetMassDensity() const;

  // Transport properties
  virtual void EnableDrift() { m_driftable = true; }
  void DisableDrift() { m_driftable = false; }
  virtual void EnablePrimaryIonisation() { m_ionisable = true; }
  void DisablePrimaryIonisation() { m_ionisable = false; }

  bool IsDriftable() const { return m_driftable; }
  bool IsMicroscopic() const { return m_microscopic; }
  bool IsIonisable() const { return m_ionisable; }

  // W value and Fano factor
  void SetW(const double w) { m_w = w; }
  double GetW() { return m_w; }
  void SetFanoFactor(const double f) { m_fano = f; }
  double GetFanoFactor() { return m_fano; }

  // Transport parameters for electrons
  // Drift velocity [cm / ns]
  virtual bool ElectronVelocity(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& vx,
                                double& vy, double& vz);
  // Longitudinal and transverse diffusion coefficients [cm1/2]
  virtual bool ElectronDiffusion(const double ex, const double ey,
                                 const double ez, const double bx,
                                 const double by, const double bz, double& dl,
                                 double& dt);
  // Diffusion tensor: diagonal elements are the diffusion
  // coefficients [cm] along e, btrans, e x b,
  // off-diagonal elements are the covariances
  virtual bool ElectronDiffusion(const double ex, const double ey,
                                 const double ez, const double bx,
                                 const double by, const double bz,
                                 double cov[3][3]);
  // Ionisation coefficient [cm-1]
  virtual bool ElectronTownsend(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz,
                                double& alpha);
  // Attachment coefficient [cm-1]
  virtual bool ElectronAttachment(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz,
                                  double& eta);

  // Microscopic electron transport properties

  // Dispersion relation (Energy vs. wave vector)
  virtual double GetElectronEnergy(const double px, const double py,
                                   const double pz, double& vx, double& vy,
                                   double& vz, const int band = 0);
  virtual void GetElectronMomentum(const double e, double& px, double& py,
                                   double& pz, int& band);

  // Null-collision rate [ns-1]
  virtual double GetElectronNullCollisionRate(const int band = 0);
  // Collision rate [ns-1] for given electron energy
  virtual double GetElectronCollisionRate(const double e, const int band = 0);
  virtual bool GetElectronCollision(const double e, int& type, int& level,
                                    double& e1, double& dx, double& dy,
                                    double& dz, int& nion, int& ndxc,
                                    int& band);

  virtual unsigned int GetNumberOfIonisationProducts() { return 0; }
  virtual bool GetIonisationProduct(const unsigned int i, 
                                    int& type, double& energy);

  virtual unsigned int GetNumberOfDeexcitationProducts() { return 0; }
  virtual bool GetDeexcitationProduct(const int i, double& t, double& s,
                                      int& type, double& energy);

  // Transport parameters for holes
  virtual bool HoleVelocity(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& vx, double& vy, double& vz);
  virtual bool HoleDiffusion(const double ex, const double ey, const double ez,
                             const double bx, const double by, const double bz,
                             double& dl, double& dt);
  virtual bool HoleDiffusion(const double ex, const double ey, const double ez,
                             const double bx, const double by, const double bz,
                             double cov[3][3]);
  virtual bool HoleTownsend(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& alpha);
  virtual bool HoleAttachment(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& eta);

  // Transport parameters for ions
  virtual bool IonVelocity(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& vx, double& vy, double& vz);
  virtual bool IonDiffusion(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& dl, double& dt);
  // Dissociation coefficient
  virtual bool IonDissociation(const double ex, const double ey,
                               const double ez, const double bx,
                               const double by, const double bz, double& diss);

  // Set the range of fields to be covered by the transport tables.
  void SetFieldGrid(double emin, double emax, int ne, bool logE,
                    double bmin = 0., double bmax = 0., int nb = 1,
                    double amin = 0., double amax = 0., int na = 1);
  void SetFieldGrid(const std::vector<double>& efields,
                    const std::vector<double>& bfields,
                    const std::vector<double>& angles);
  void GetFieldGrid(std::vector<double>& efields, std::vector<double>& bfields,
                    std::vector<double>& angles);

  bool GetElectronVelocityE(const unsigned int ie, 
                            const unsigned int ib, 
                            const unsigned int ia, double& v);
  bool GetElectronVelocityExB(const unsigned int ie, 
                              const unsigned int ib, 
                              const unsigned int ia, double& v);
  bool GetElectronVelocityB(const unsigned int ie, 
                            const unsigned int ib, 
                            const unsigned int ia, double& v);

  bool GetElectronLongitudinalDiffusion(const unsigned int ie, 
                                        const unsigned int ib, 
                                        const unsigned int ia, double& dl);
  bool GetElectronTransverseDiffusion(const unsigned int ie, 
                                      const unsigned int ib, 
                                      const unsigned int ia, double& dt);
  bool GetElectronTownsend(const unsigned int ie, 
                           const unsigned int ib, 
                           const unsigned int ia, double& alpha);
  bool GetElectronAttachment(const unsigned int ie, 
                             const unsigned int ib, 
                             const unsigned int ia, double& eta);

  bool GetHoleVelocityE(const unsigned int ie, const unsigned int ib, 
                        const unsigned int ia, double& v);
  bool GetHoleVelocityExB(const unsigned int ie, const unsigned int ib, 
                          const unsigned int ia, double& v);
  bool GetHoleVelocityB(const unsigned int ie, const unsigned int ib, 
                        const unsigned int ia, double& v);
  bool GetHoleLongitudinalDiffusion(const unsigned int ie, 
                                    const unsigned int ib, 
                                    const unsigned int ia, double& dl);
  bool GetHoleTransverseDiffusion(const unsigned int ie, 
                                  const unsigned int ib, 
                                  const unsigned int ia, double& dt);
  bool GetHoleTownsend(const unsigned int ie, const unsigned int ib, 
                       const unsigned int ia, double& alpha);
  bool GetHoleAttachment(const unsigned int ie, const unsigned int ib, 
                         const unsigned int ia, double& eta);

  bool GetIonMobility(const unsigned int ie, const unsigned int ib, 
                      const unsigned int ia, double& mu);
  bool GetIonLongitudinalDiffusion(const unsigned int ie, 
                                   const unsigned int ib, 
                                   const unsigned int ia, double& dl);
  bool GetIonTransverseDiffusion(const unsigned int ie, 
                                 const unsigned int ib, 
                                 const unsigned int ia, double& dt);
  bool GetIonDissociation(const unsigned int ie, 
                          const unsigned int ib, 
                          const unsigned int ia, double& diss);

  void ResetElectronVelocity();
  void ResetElectronDiffusion();
  void ResetElectronTownsend();
  void ResetElectronAttachment();
  void ResetHoleVelocity();
  void ResetHoleDiffusion();
  void ResetHoleTownsend();
  void ResetHoleAttachment();
  void ResetIonMobility();
  void ResetIonDiffusion();
  void ResetIonDissociation();

  bool SetIonMobility(const unsigned int ie, const unsigned int ib, 
                      const unsigned int ia, const double mu);
  bool SetIonMobility(const std::vector<double>& fields,
                      const std::vector<double>& mobilities);

  // Select extrapolation method for fields below/above the table range.
  // Options are "constant"/"linear"/"exponential".
  void SetExtrapolationMethodVelocity(const std::string& extrLow,
                                      const std::string& extrHigh);
  void SetExtrapolationMethodDiffusion(const std::string& extrLow,
                                       const std::string& extrHigh);
  void SetExtrapolationMethodTownsend(const std::string& extrLow,
                                      const std::string& extrHigh);
  void SetExtrapolationMethodAttachment(const std::string& extrLow,
                                        const std::string& extrHigh);
  void SetExtrapolationMethodIonMobility(const std::string& extrLow,
                                         const std::string& extrHigh);
  void SetExtrapolationMethodIonDissociation(const std::string& extrLow,
                                             const std::string& extrHigh);

  // Set the degree of polynomial interpolation (usually 2).
  void SetInterpolationMethodVelocity(const unsigned int intrp);
  void SetInterpolationMethodDiffusion(const unsigned int intrp);
  void SetInterpolationMethodTownsend(const unsigned int intrp);
  void SetInterpolationMethodAttachment(const unsigned int intrp);
  void SetInterpolationMethodIonMobility(const unsigned int intrp);
  void SetInterpolationMethodIonDissociation(const unsigned int intrp);

  // Scaling of fields and transport parameters.
  virtual double ScaleElectricField(const double e) const { return e; }
  virtual double UnScaleElectricField(const double e) const { return e; }
  virtual double ScaleVelocity(const double v) const { return v; }
  virtual double ScaleDiffusion(const double d) const { return d; }
  virtual double ScaleDiffusionTensor(const double d) const { return d; }
  virtual double ScaleTownsend(const double alpha) const { return alpha; }
  virtual double ScaleAttachment(const double eta) const { return eta; }
  virtual double ScaleDissociation(const double diss) const { return diss; }

  // Optical properties
  // Energy range [eV] of available optical data
  virtual bool GetOpticalDataRange(double& emin, double& emax, 
                                   const unsigned int i = 0);
  // Complex dielectric function
  virtual bool GetDielectricFunction(const double e, double& eps1, double& eps2,
                                     const unsigned int i = 0);
  // Photoabsorption cross-section [cm2]
  virtual bool GetPhotoAbsorptionCrossSection(const double e, double& sigma,
                                              const unsigned int i = 0);
  virtual double GetPhotonCollisionRate(const double e);
  virtual bool GetPhotonCollision(const double e, int& type, int& level,
                                  double& e1, double& ctheta, int& nsec,
                                  double& esec);

  // Switch on/off debugging  messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  std::string m_className;

  static int m_idCounter;

  // Id number
  int m_id;
  // Name
  std::string m_name;
  // Temperature [K]
  double m_temperature;
  // Pressure [Torr]
  double m_pressure;
  // Static dielectric constant
  double m_epsilon;
  // Number of components
  unsigned int m_nComponents;
  // (Effective) atomic number Z
  double m_z;
  // Atomic weight A
  double m_a;
  // Number density [cm-3]
  double m_density;

  // Transport flags
  bool m_driftable;
  bool m_microscopic;
  bool m_ionisable;

  // W value and Fano factor
  double m_w, m_fano;

  // Update flag
  bool m_isChanged;

  // Switch on/off debugging messages
  bool m_debug;

  // Field grids
  std::vector<double> m_eFields;
  std::vector<double> m_bFields;
  std::vector<double> m_bAngles;

  // Tables of transport parameters
  bool m_map2d;
  // Electrons
  bool m_hasElectronVelocityE, m_hasElectronVelocityB, m_hasElectronVelocityExB;
  bool m_hasElectronDiffLong, m_hasElectronDiffTrans, m_hasElectronDiffTens;
  bool m_hasElectronAttachment;
  std::vector<std::vector<std::vector<double> > > tabElectronVelocityE;
  std::vector<std::vector<std::vector<double> > > tabElectronVelocityExB;
  std::vector<std::vector<std::vector<double> > > tabElectronVelocityB;
  std::vector<std::vector<std::vector<double> > > tabElectronDiffLong;
  std::vector<std::vector<std::vector<double> > > tabElectronDiffTrans;
  std::vector<std::vector<std::vector<double> > > tabElectronTownsend;
  std::vector<std::vector<std::vector<double> > > tabElectronAttachment;

  std::vector<std::vector<std::vector<std::vector<double> > > >
      tabElectronDiffTens;

  // Holes
  bool m_hasHoleVelocityE, m_hasHoleVelocityB, m_hasHoleVelocityExB;
  bool m_hasHoleDiffLong, m_hasHoleDiffTrans, m_hasHoleDiffTens;
  bool m_hasHoleTownsend, m_hasHoleAttachment;
  std::vector<std::vector<std::vector<double> > > tabHoleVelocityE;
  std::vector<std::vector<std::vector<double> > > tabHoleVelocityExB;
  std::vector<std::vector<std::vector<double> > > tabHoleVelocityB;
  std::vector<std::vector<std::vector<double> > > tabHoleDiffLong;
  std::vector<std::vector<std::vector<double> > > tabHoleDiffTrans;
  std::vector<std::vector<std::vector<double> > > tabHoleTownsend;
  std::vector<std::vector<std::vector<double> > > tabHoleAttachment;

  std::vector<std::vector<std::vector<std::vector<double> > > > tabHoleDiffTens;

  // Ions
  bool m_hasIonMobility;
  bool m_hasIonDiffLong, m_hasIonDiffTrans;
  bool m_hasIonDissociation;
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
  unsigned int m_extrLowVelocity, m_extrHighVelocity;
  unsigned int m_extrLowDiffusion, m_extrHighDiffusion;
  unsigned int m_extrLowTownsend, m_extrHighTownsend;
  unsigned int m_extrLowAttachment, m_extrHighAttachment;
  unsigned int m_extrLowMobility, m_extrHighMobility;
  unsigned int m_extrLowDissociation, m_extrHighDissociation;

  // Interpolation methods
  unsigned int m_intpVelocity;
  unsigned int m_intpDiffusion;
  unsigned int m_intpTownsend;
  unsigned int m_intpAttachment;
  unsigned int m_intpMobility;
  unsigned int m_intpDissociation;

  double Interpolate1D(const double e, const std::vector<double>& table,
                       const std::vector<double>& fields, 
                       const unsigned int intpMeth,
                       const int jExtr, const int iExtr);
  bool GetExtrapolationIndex(std::string extrStr, unsigned int& extrNb);
  void CloneTable(std::vector<std::vector<std::vector<double> > >& tab,
                  const std::vector<double>& efields,
                  const std::vector<double>& bfields,
                  const std::vector<double>& angles, 
                  const unsigned int intp,
                  const unsigned int extrLow, const unsigned int extrHigh, 
                  const double init,
                  const std::string& label);
  void CloneTensor(
      std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
      const unsigned int n, 
      const std::vector<double>& efields,
      const std::vector<double>& bfields, 
      const std::vector<double>& angles,
      const unsigned int intp, 
      const unsigned int extrLow, const unsigned int extrHigh, 
      const double init,
      const std::string& label);

  void InitParamArrays(const unsigned int eRes, const unsigned int bRes, 
                       const unsigned int aRes,
                       std::vector<std::vector<std::vector<double> > >& tab,
                       const double val);
  void InitParamTensor(
      const unsigned int eRes, const unsigned int bRes, 
      const unsigned int aRes, const unsigned int tRes,
      std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
      const double val);
};
}

#endif
