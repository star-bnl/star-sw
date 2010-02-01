#include <iostream>
#include <cmath>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "Plotting.hh"

namespace Garfield {

int Medium::idCounter = -1;

Medium::Medium() : 
  id(++idCounter), name(""), 
  temperature(293.15), pressure(760.), 
  epsilon(1.), 
  nComponents(1), atomicNumber(1.), atomicWeight(0.), density(0.),
  driftable(false), microscopic(false), ionisable(false),
  isChanged(true),
  debug(false), warning(false) {
  
}

void 
Medium::SetTemperature(const double t) {

  if (t <= 0.) {
    std::cerr << "Medium::SetTemperature:" << std::endl;
    std::cerr << "    Temperature [K] must be greater than zero." << std::endl;
    return;
  }
  temperature = t;
  isChanged = true;

}

void 
Medium::SetPressure(const double p) {

  if (p <= 0.) {
    std::cerr << "Medium::SetPressure:" << std::endl;
    std::cerr << "    Pressure [Torr] must be greater than zero." << std::endl;
    return;
  }
  pressure = p;
  isChanged = true;  

}

void 
Medium::SetDielectricConstant(const double eps) {

  if (eps < 1.) {
    std::cerr << "Medium::SetDielectricConstant: " << std::endl;
    std::cerr << "    Dielectric constant must be >= 1." << std::endl;
    return;
  }
  epsilon = eps;
  isChanged = true;  

}

double
Medium::GetMassDensity() const {

  return density * AtomicMassUnit * atomicWeight;
  
}

void
Medium::GetComponent(const int i, std::string& label, double& f) {

  if (i < 0 || i >= nComponents) {
    std::cerr << "Medium::GetComponent: Index out of range" << std::endl;
  }
  
  label = name; f = 1.;
  
}

void 
Medium::SetAtomicNumber(const double z) {

  if (z < 1.) {
    std::cerr << "Medium::SetAtomicNumber:" << std::endl;
    std::cerr << "    Atomic number must be >= 1." << std::endl;
    return;
  }
  atomicNumber = z;
  isChanged = true;  

}

void 
Medium::SetAtomicWeight(const double a) {

  if (a <= 0.) {
    std::cerr << "Medium::SetAtomicWeight:" << std::endl;
    std::cerr << "    Atomic weight must be greater than zero." << std::endl;
    return;
  }
  atomicWeight = a;
  isChanged = true;  

}

void 
Medium::SetNumberDensity(const double n) {

  if (n <= 0.) {
    std::cerr << "Medium::SetNumberDensity:" << std::endl;
    std::cerr << "    Density [cm-3] must be greater than zero." << std::endl;
    return;
  }
  density = n;
  isChanged = true;  

}

void 
Medium::SetMassDensity(const double rho) {

  if (rho <= 0.) {
    std::cerr << "Medium::SetMassDensity:" << std::endl;
    std::cerr << "    Density [g/cm3] must be greater than zero." << std::endl;
    return;
  }

  if (atomicWeight <= 0.) {
    std::cerr << "Medium::SetMassDensity:" << std::endl;
    std::cerr << "    Atomic weight is not defined." << std::endl;
    return;
  }
  density = rho / (AtomicMassUnit * atomicWeight);
  isChanged = true;  

}

bool 
Medium::ElectronVelocity(const double ex, const double ey, const double ez, 
                         const double bx, const double by, const double bz, 
                         double& vx, double& vy, double& vz) {

  if (warning) {
    std::cerr << "Medium::ElectronVelocity:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  vx = vy = vz = 0.;
  return false;
            
}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& dl, double& dt) {

  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  // Calculate diffusion coefficients using the Einstein relation
  if (e < Small) {
    dl = dt = 0.;
  } else {
    dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  return true;

}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double cov[3][3]) {

  if (warning) {
    std::cerr << "Medium::ElectronDiffusionTensor:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  return false;

}

bool 
Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& alpha) {

  if (warning) {
    std::cerr << "Medium::ElectronTownsend:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  alpha = 0.;
  return false;

}

bool 
Medium::ElectronAttachment(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& eta) {

  if (warning) {
    std::cerr << "Medium::ElectronAttachment:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  eta = 0.;
  return false;

}

double 
Medium::GetNullCollisionRate() {

  if (warning) {
    std::cerr << "Medium::GetNullCollisionRate:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  return 0.;
  
}

double 
Medium::GetCollisionRate(const double e) {

  if (warning) {
    std::cerr << "Medium::GetCollisionRate:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  return 0.;

}

bool 
Medium::GetCollision(const double e, int& type, int& level,
                     double& s, double& ctheta, double& eloss, double& esec) {
  
  if (warning) {
    std::cerr << "Medium::GetCollision:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  return 0.;
                            
}

int 
Medium::GetNumberOfLevels() {

  if (warning) {
    std::cerr << "Medium::GetNumberOfLevels:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  return 0;

}
                
bool 
Medium::HoleVelocity(const double ex, const double ey, const double ez, 
                     const double bx, const double by, const double bz, 
                     double& vx, double& vy, double& vz) {
            
  if (warning) {
    std::cerr << "Medium::HoleVelocity:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  vx = vy = vz = 0.;
  return false;            

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& dl, double& dt) {

  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  // Calculate diffusion coefficients using the Einstein relation
  if (e < Small) {
    dl = dt = 0.;
  } else {
    dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  return true;

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double cov[3][3]) {

  if (warning) {
    std::cerr << "Medium::HoleDiffusionTensor:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  return false;

}

bool 
Medium::HoleTownsend(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& alpha) {

  if (warning) {
    std::cerr << "Medium::HoleTownsend:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  alpha = 0.;
  return false;

}

bool 
Medium::HoleAttachment(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double& eta) {
            
  if (warning) {
    std::cerr << "Medium::HoleAttachment:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  eta = 0.;
  return false;

}

bool 
Medium::IonVelocity(const double ex, const double ey, const double ez, 
                    const double bx, const double by, const double bz, 
                    double& vx, double& vy, double& vz) {

  if (warning) {
    std::cerr << "Medium::IonVelocity:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  vx = vy = vz = 0.;
  return false;

}

bool 
Medium::IonDiffusion(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& dl, double& dt) {

  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  // Calculate diffusion coefficients using the Einstein relation
  if (e < Small) {
    dl = dt = 0.;
  } else {
    dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  return true;

}

bool 
Medium::IonDissociation(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& diss) {

  if (warning) {
    std::cerr << "Medium::IonDissociation:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  diss = 0.;
  return false;

}

bool 
Medium::GetOpticalDataRange(double& emin, double& emax, const int i) {

  if (warning) {
    std::cerr << "Medium::GetOpticalDataRange:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  return false;

}

bool 
Medium::GetDielectricFunction(const double e, 
                              double& eps1, double& eps2, const int i) {

  if (warning) {
    std::cerr << "Medium::GetDielectricFunction:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  eps1 = 1.; eps2 = 0.;
  return false;

}

bool 
Medium::GetPhotoAbsorptionCrossSection(const double e, 
                                       double& sigma, const int i) {

  if (warning) {
    std::cerr << "Medium::GetPhotoAbsorptionCrossSection:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." << std::endl;
  }
  sigma = 0.;
  return false;

}

void 
Medium::PlotElectronVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine->PlotVelocity(this, true, false, false);
  
}

void 
Medium::PlotHoleVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine->PlotVelocity(this, false, true, false);

}

void 
Medium::PlotIonVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine->PlotVelocity(this, false, false, true);

}

void 
Medium::PlotElectronHoleVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine->PlotVelocity(this, true, true, false);

}

void 
Medium::PlotElectronIonVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine->PlotVelocity(this, false, false, true);

}

void 
Medium::PlotElectronTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine->PlotTownsend(this, true, false);

}

void 
Medium::PlotHoleTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine->PlotTownsend(this, false, true);

}

void 
Medium::PlotElectronHoleTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine->PlotTownsend(this, true, true);

}

void 
Medium::PlotElectronAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine->PlotAttachment(this, true, false);

}

void 
Medium::PlotHoleAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine->PlotAttachment(this, false, true);
  
}

void 
Medium::PlotElectronHoleAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine->PlotAttachment(this, true, true);
  
}

void
Medium::PlotVelocityCommon(const double emin, const double emax) {

  plottingEngine->SetRangeX(emin, emax);
  plottingEngine->SetLabelX("electric field [V/cm]");
  plottingEngine->SetLabelY("drift velocity [V/cm]");    
  plottingEngine->SetTitle(name);

}

void
Medium::PlotTownsendCommon(const double emin, const double emax) {

  plottingEngine->SetRangeX(emin, emax);
  plottingEngine->SetLabelX("electric field [V/cm]");
  plottingEngine->SetLabelY("Townsend coefficient [1/cm]");    
  plottingEngine->SetTitle(name);  

}

void
Medium::PlotAttachmentCommon(const double emin, const double emax) {

  plottingEngine->SetRangeX(emin, emax);
  plottingEngine->SetLabelX("electric field [V/cm]");
  plottingEngine->SetLabelY("attachment coefficient [1/cm]");
  plottingEngine->SetTitle(name);

}

}