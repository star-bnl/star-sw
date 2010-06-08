#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

#include <map>

#include "MediumMagboltz86.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"
#include "OpticalData.hh"

namespace Garfield {

MediumMagboltz86::MediumMagboltz86() :
  Medium(), 
  eFinal(40.), eStep(eFinal / nEnergySteps), useAutoAdjust(true), 
  useCsOutput(false), 
  nTerms(0), useAnisotropic(true), 
  usePenning(false), rPenning(0.), nPenning(0), 
  useDeexcitation(false), nDeexcitations(0), nDeexcitationProducts(0),
  eFinalGamma(20.), eStepGamma(eFinalGamma / nEnergyStepsGamma),
  hasIonMobility(false), muIon(1.e-9) {
  
  // Set physical constants in Magboltz common blocks
  cnsts_.echarg = ElementaryCharge;
  cnsts_.emass = ElectronMassGramme;
  cnsts_.amu = AtomicMassUnit;
  cnsts_.pir2 = BohrRadius * BohrRadius * Pi;  
  inpt_.ary = RydbergEnergy;

  // Default gas mixture: pure argon
  for (int i = nMaxGases; i--;) {
    fraction[i] = 0.;
    gas[i] = 0;
  }
  gas[0] = 2;
  fraction[0] = 1.;
  GetGasName(gas[0], name);
  
  // Set parameters in Magboltz common blocks
  inpt_.nGas = nComponents;  
  inpt_.nStep = nEnergySteps;
  // Scattering model
  inpt_.nAniso = 2;
  // Max. energy [eV]
  inpt_.efinal = eFinal;
  // Energy step size [eV]
  inpt_.estep = eStep;
  // Thermal energy
  inpt_.akt = BoltzmannConstant * temperature;
  inpt_.tempc = temperature - 273.15;
  inpt_.torr = pressure;
  // Disable Penning transfer
  inpt_.ipen = 0;
  
  isChanged = true;

  EnableDrift();
  DisablePrimaryIonisation();
  microscopic = true;
  
  // Collision counters
  nCollisionsDetailed.clear();
  nCollisions[0] = 0; nCollisions[1] = 0; nCollisions[2] = 0;
  nCollisions[3] = 0; nCollisions[4] = 0;
  nPhotonCollisions[0] = 0; nPhotonCollisions[1] = 0; nPhotonCollisions[2] = 0;
  
}

bool 
MediumMagboltz86::SetComposition(const std::string gas1, const double f1, 
                                 const std::string gas2, const double f2,
                                 const std::string gas3, const double f3,
                                 const std::string gas4, const double f4,
                                 const std::string gas5, const double f5, 
                                 const std::string gas6, const double f6) {

  int i = 0;
  
  // Find the gas number corresponding to the input string.
  int ng;
  if (f1 > 0. && GetGasNumber(gas1, ng)) {
    gas[i] = ng; fraction[i] = f1; ++i;
  }
  if (f2 > 0. && GetGasNumber(gas2, ng)) {
    gas[i] = ng; fraction[i] = f2; ++i;
  }
  if (f3 > 0. && GetGasNumber(gas3, ng)) {
    gas[i] = ng; fraction[i] = f3; ++i;
  }
  if (f4 > 0. && GetGasNumber(gas4, ng)) {
    gas[i] = ng; fraction[i] = f4; ++i;
  }
  if (f5 > 0. && GetGasNumber(gas5, ng)) {
    gas[i] = ng; fraction[i] = f5; ++i;
  }
  if (f6 > 0. && GetGasNumber(gas6, ng)) {
    gas[i] = ng; fraction[i] = f6; ++i;
  }    
  
  // Check if at least one valid ingredient was specified. 
  if (i <= 0) {
    std::cerr << "MediumMagboltz86: Error setting the composition. "
              << "    No valid ingredients were specified." << std::endl;
    return false;
  }
  
  nComponents = i;
  std::string gasname = "";
  name = "";  
  double sum = 0.;  
  for (i = 0; i < nComponents; ++i) {
    if (i > 0) name += "/";  
    GetGasName(gas[i], gasname);
    name += gasname;
    sum += fraction[i];
  }
  // Normalise the fractions to one.
  for (i = 0; i < nMaxGases; ++i) { 
    if (i < nComponents) {
      fraction[i] /= sum;
    } else {
      fraction[i] = 0.;
    }
  }
  
  std::cout << "MediumMagboltz86::SetComposition:" << std::endl;
  std::cout << "    " << name;
  if (nComponents > 1) {
    std::cout << " (" << fraction[0] * 100;
    for (i = 1; i < nComponents; ++i) {
      std::cout << "/" << fraction[i] * 100;
    }
    std::cout << ")";
  }
  std::cout << std::endl;
  
  // Set parameters in Magboltz common blocks
  inpt_.nGas = nComponents;  
  
  // Require a recalculation of the collision rates.
  isChanged = true;
    
  return true;
  
}

void 
MediumMagboltz86::GetComposition(std::string& gas1, double& f1,
                                 std::string& gas2, double& f2,
                                 std::string& gas3, double& f3,
                                 std::string& gas4, double& f4,
                                 std::string& gas5, double& f5,
                                 std::string& gas6, double& f6) {

  GetGasName(gas[0], gas1); f1 = fraction[0];
  GetGasName(gas[1], gas2); f2 = fraction[1];
  GetGasName(gas[2], gas3); f3 = fraction[2];
  GetGasName(gas[3], gas4); f4 = fraction[3];
  GetGasName(gas[4], gas5); f5 = fraction[4];
  GetGasName(gas[5], gas6); f6 = fraction[5];

}

void
MediumMagboltz86::GetComponent(const int i, std::string& label, double& f) {

  if (i < 0 || i >= nComponents) {
    std::cerr << "MediumMagboltz86::GetComponent:" << std::endl;
    std::cerr << "    Index out of range." << std::endl;
    label = "";
    f = 0.;
    return;
  }
  
  GetGasName(gas[i], label);
  f = fraction[i];

}

bool 
MediumMagboltz86::SetMaxElectronEnergy(const double e) {

  if (e <= 1.e-20) {
    std::cerr << "MediumMagboltz86::SetMaxElectronEnergy:" << std::endl;
    std::cerr << "    Provided upper electron energy limit (" << e
              <<  " eV) is too small." << std::endl;
    return false;
  }
  eFinal = e;
  
  // Determine the energy interval size
  eStep = eFinal / nEnergySteps;
  
  // Set max. energy and step size also in Magboltz common block
  inpt_.efinal = eFinal;
  inpt_.estep = eStep;
  
  isChanged = true;

  return true;
  
}

bool 
MediumMagboltz86::SetMaxPhotonEnergy(const double e) {

  if (e <= 1.e-20) {
    std::cerr << "MediumMagboltz86::SetMaxPhotonEnergy:" << std::endl;
    std::cerr << "    Provided upper photon energy limit (" << e
              <<  " eV) is too small." << std::endl;
    return false;
  }
  eFinalGamma = e;
  
  // Determine the energy interval size
  eStepGamma = eFinalGamma / nEnergyStepsGamma;
  
  isChanged = true;

  return true;
  
}

void
MediumMagboltz86::EnableDeexcitation() {

  if (usePenning) {
    std::cerr << "MediumMagboltz86::EnableDeexcitation:" << std::endl;
    std::cerr << "    Penning transfer will be switched off." << std::endl;
  }
  usePenning = false;
  useDeexcitation = true;
  isChanged = true;
  nDeexcitationProducts = 0;

}

void
MediumMagboltz86::EnablePenningTransfer(const double r) {

  if (r < 0. || r > 1.) {
    std::cerr << "MediumMagboltz86::EnablePenningTransfer:" << std::endl;
    std::cerr << "    Penning transfer probability must be " 
              << " in the range [0, 1]." << std::endl;
    return;
  }
  
  rPenning = r;
  if (useDeexcitation) {
    std::cout << "MediumMagboltz86::EnablePenningTransfer:" << std::endl;
    std::cout << "    Deexcitation handling will be switched off." 
              << std::endl;
  }
  usePenning = true;
  
}

double 
MediumMagboltz86::GetElectronNullCollisionRate() {

  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the "
                << " collision rates table." << std::endl;
      return 0.;
    }
    isChanged = false;
  }
      
  return cfNull[0];
  
}

double 
MediumMagboltz86::GetElectronCollisionRate(const double e) {

  if (e <= 0.) {
    std::cerr << "MediumMagboltz86: Electron energy must be greater than zero."
              << std::endl;
    return cfTot[0];
  }
  if (e > eFinal && useAutoAdjust) {    
    std::cerr << "MediumMagboltz86::GetElectronCollisionRate:" << std::endl;
    std::cerr << "    Collision rate at " << e 
              << " eV is not included in the current table." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxElectronEnergy(1.05 * e);    
  }
    
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86:" << std::endl;
      std::cerr << "    Error calculating the collision rates table."
                << std::endl;
      return 0.;
    }
    isChanged = false;
  }

  if (e > eFinal) return cfTot[nEnergySteps - 1];  
  return cfTot[int(e / eStep)];

}

bool 
MediumMagboltz86::GetElectronCollision(const double e, int& type, int& level, 
                double& e1, double& ctheta, double& s, double& esec) {

  if (e > eFinal && useAutoAdjust) {
    std::cerr << "MediumMagboltz86::GetElectronCollision:" << std::endl;
    std::cerr << "    Provided electron energy  (" << e 
              << " eV) exceeds current energy range  (" << eFinal 
              << " eV)." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxElectronEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << "MediumMagboltz86::GetElectronCollision:" << std::endl;
    std::cerr << "    Electron energy must be greater than zero." << std::endl;
    return false;
  }
  
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: " << std::endl;
      std::cerr << "    Error calculating the collision rates table." 
                << std::endl;
      return false;
    }
    isChanged = false;
  }

  // Energy interval
  const int iE = e < eFinal ? int(e / eStep) : nEnergySteps - 1;
  
  double r = RndmUniform();
  int iLow = 0;
  int iUp  = nTerms - 1;  
  if (r <= cf[iE][iLow]) {
    level = iLow;
  } else if (r >= cf[iE][iUp]) {
    level = iUp;
  } else {
    int iMid;
    while (iUp - iLow > 1) {
      iMid = (iLow + iUp) >> 1;
      if (r < cf[iE][iMid]) {
        iUp = iMid;
      } else {
        iLow = iMid;
      }
    }
    level = iUp;
  }
  
  // Collision type
  type = csType[level] % 6;

  // Increase the collision counters
  ++nCollisions[type];
  ++nCollisionsDetailed[level];

  // Energy loss
  double loss = energyLoss[level];
  // Secondary electron energy (none by default)
  esec = 0.; s = 0.;
  // Ionising collision
  if (type == 1) {
    // Splitting parameter
    const double w = wSplit[level];
    // Sample the secondary electron energy according to 
    // the Opal-Beaty-Peterson parametrisation  
    esec = w * tan(RndmUniform() * atan(0.5 * (e - loss) / w));
    if (esec <= 0) esec = 1.e-20;
    loss += esec;
  // Excitation
  } else if (type == 4 && useDeexcitation && iDeexcitation[level] >= 0) {
    ComputeDeexcitation(iDeexcitation[level]);
    esec = -1.;
    if (nDeexcitationProducts > 0) s = -1.;
  } else if (type == 4 && usePenning) {
    if (energyLoss[level] * rgas[level] > minIonPot && 
        RndmUniform() < rPenning) {
      esec = RndmUniform() * (energyLoss[level] * rgas[level] - minIonPot);
      if (esec <= 0) esec = 1.e-20;
      ++nPenning;
    }
  }

  // Make sure the energy loss is smaller than the energy
  if (e < loss) loss = e - 0.0001;
  
  // Determine the scattering angle
  double ctheta0;
  if (useAnisotropic) {
    switch (scatModel[level]) {
      case 0:
        ctheta0 = 1. - 2. * RndmUniform();
        break;
      case 1:
        ctheta0 = 1. - RndmUniform() * scatCut[iE][level];
        if (RndmUniform() > scatParameter[iE][level]) ctheta = -ctheta;
        break;
      case 2:
        ctheta0 = 1. - 2. * RndmUniform();
        ctheta0 = (ctheta0 + scatParameter[iE][level]) / 
                  (1. + scatParameter[iE][level] * ctheta0);
        break;
      default:
        std::cerr << "MediumMagboltz86::GetElectronCollision:" << std::endl;
        std::cerr << "    Unknown scattering model. " << std::endl;
        std::cerr << "    Using isotropic distribution instead." << std::endl;
        ctheta0 = 1. - 2. * RndmUniform();
        break;
    }
  } else {
    ctheta0 = 1. - 2. * RndmUniform();
  }

  const double s1 = rgas[level];
  const double s2 = (s1 * s1) / (s1 - 1.);
  const double stheta0 = sin(acos(ctheta0));
  double arg = 1. - s1 * loss / e,;
  if (arg < Small) arg = Small;
  const double d = 1. - ctheta0 * sqrt(arg);

  // Update the energy  
  e1 = e * (1. - loss / (s1 * e) - 2. * d / s2);
  if (e1 < Small) e1 = Small;
  double q = sqrt((e / e1) * arg) / s1;
  if (q > 1.) q = 1.;
  const double theta = asin(q * stheta0);
  
  ctheta = cos(theta);
  if (ctheta0 < 0.) {
    double u = (s1 - 1.) * (s1 - 1.) / arg;
    if (ctheta0 * ctheta0 > u) ctheta *= -1.;
  }
  return true;

}

bool
MediumMagboltz86::GetDeexcitationProduct(const int i, double& t, 
                                         int& type, double& energy) {

  if (i < 0 || i >= nDeexcitationProducts || !useDeexcitation) return false;
  t = dxcProducts[i].t;
  type = dxcProducts[i].type;
  energy = dxcProducts[i].energy;
  return true;

}

double 
MediumMagboltz86::GetPhotonCollisionRate(const double e) {

  if (e <= 0.) {
    std::cerr << "MediumMagboltz86: Photon energy must be greater than zero."
              << std::endl;
    return cfTotGamma[0];
  }
  if (e > eFinalGamma && useAutoAdjust) {
    std::cerr << "MediumMagboltz86::GetPhotonCollisionRate:" << std::endl;
    std::cerr << "    Collision rate at " << e 
              << " eV is not included in the current table." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxPhotonEnergy(1.05 * e);
  }
    
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86:" << std::endl;
      std::cerr << "     Error calculating the collision rates table."
                << std::endl;
      return 0.;
    }
    isChanged = false;
  }

  if (e > eFinalGamma) return cfTotGamma[nEnergyStepsGamma - 1];
  return cfTotGamma[int(e / eStepGamma)];

}

bool
MediumMagboltz86::GetPhotonCollision(const double e, int& type, int& level,
                                     double& e1, double& ctheta, double& s,
                                     double& esec) {

  if (e > eFinalGamma && useAutoAdjust) {
    std::cerr << "MediumMagboltz86::GetPhotonCollision:" << std::endl;
    std::cerr << "    Provided electron energy  (" << e 
              << " eV) exceeds current energy range  (" << eFinalGamma
              << " eV)." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxPhotonEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << "MediumMagboltz86::GetPhotonCollision:" << std::endl;
    std::cerr << "    Photon energy must be greater than zero." << std::endl;
    return false;
  }
  
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating" 
                << " the collision rates table." << std::endl;
      return false;
    }
    isChanged = false;
  }

  // Energy interval
  const int iE = e < eFinalGamma ? int(e / eStepGamma) : nEnergyStepsGamma - 1;
  
  double r = RndmUniform();
  int iLow = 0;
  int iUp  = nPhotonTerms - 1;  
  if (r <= cfGamma[iE][iLow]) {
    level = iLow;
  } else if (r >= cfGamma[iE][iUp]) {
    level = iUp;
  } else {
    int iMid;
    while (iUp - iLow > 1) {
      iMid = (iLow + iUp) >> 1;
      if (r < cfGamma[iE][iMid]) {
        iUp = iMid;
      } else {
        iLow = iMid;
      }
    }
    level = iUp;
  }
  
  // Collision type
  type = csTypeGamma[level] % 3;
  int ngas = int(csTypeGamma[level] / 3);
  ++nPhotonCollisions[type];

  // Secondary electron energy
  esec = 0.;
  // Ionising collision
  if (type == 1) {
    esec = e - ionPot[ngas];
    if (esec < 1.e-20) esec = 1.e-20;
    e1 = 0.;
  }

  // Determine the scattering angle
  ctheta = 1. - 2. * RndmUniform();
  
  return true;

}

void 
MediumMagboltz86::ResetCollisionCounters() {

  nCollisions[0] = 0; nCollisions[1] = 0; nCollisions[2] = 0;
  nCollisions[3] = 0; nCollisions[4] = 0; nCollisions[5] = 0;
  for (int j = nTerms; j--;) nCollisionsDetailed[j] = 0;
  nPenning = 0;
  nPhotonCollisions[0] = 0; nPhotonCollisions[1] = 0; nPhotonCollisions[2] = 0;
  
}

int 
MediumMagboltz86::GetNumberOfElectronCollisions() const {

  return nCollisions[0] + nCollisions[1] + nCollisions[2] + 
         nCollisions[3] + nCollisions[4] + nCollisions[5];
  
}

int 
MediumMagboltz86::GetNumberOfElectronCollisions(
        int& nElastic,   int& nIonisation, int& nAttachment, 
        int& nInelastic, int& nExcitation, int& nSuperelastic) const {

  nElastic = nCollisions[0];    nIonisation = nCollisions[1];
  nAttachment = nCollisions[2]; nInelastic = nCollisions[3];
  nExcitation = nCollisions[4]; nSuperelastic = nCollisions[5];  
  return nCollisions[0] + nCollisions[1] + nCollisions[2] + 
         nCollisions[3] + nCollisions[4] + nCollisions[5];

}

int 
MediumMagboltz86::GetNumberOfLevels() {

  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the"
                << " collision rates table." << std::endl;
      return 0;
    }
    isChanged = false;
  }

  return nTerms;

}

bool 
MediumMagboltz86::GetLevel(const int i, int& gas, int& type,
                           std::string& descr, double& e) {

  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the " 
                << " collision rates table." << std::endl;
      return false;
    }
    isChanged = false;
  }

  if (i < 0 || i >= nTerms) {
    std::cerr << "MediumMagboltz86::GetLevel:" << std::endl;
    std::cerr << "    Requested level (" << i
              << " does not exist." << std::endl;
    return false;
  }  
  
  // Collision type
  type = csType[i] % 6;
  gas = int(csType[i] / 6);
  // Description
  descr = "                              ";
  for (int j = 30; j--;) descr[j] = description[i][j];
  // Threshold energy
  e = rgas[i] * energyLoss[i];  
  return true;

}

int 
MediumMagboltz86::GetNumberOfElectronCollisions(const int level) const {

  if (level < 0 || level >= nTerms) {
    std::cerr << "MediumMagboltz86::GetNumberOfElectronCollisions:" 
              << std::endl;
    std::cerr << "    Requested cross-section term (" 
              << level << ") does not exist." << std::endl;
    return 0;
  }
  return nCollisionsDetailed[level];

}  

int
MediumMagboltz86::GetNumberOfPhotonCollisions() const {

  return nPhotonCollisions[0] + nPhotonCollisions[1] + nPhotonCollisions[2];

}

int
MediumMagboltz86::GetNumberOfPhotonCollisions(
    int& nElastic, int& nIonising, int& nInelastic) const {

  nElastic   = nPhotonCollisions[0];
  nIonising  = nPhotonCollisions[1];
  nInelastic = nPhotonCollisions[2];
  return nPhotonCollisions[0] + nPhotonCollisions[1] + nPhotonCollisions[2];

}

void
MediumMagboltz86::SetIonMobility(const double mu) {

  if (fabs(mu) < Small) {
    std::cerr << "MediumMagboltz86::SetIonMobility:" << std::endl;
    std::cerr << "    Ion mobility must be greater than zero." << std::endl;
    return;
  }

  muIon = mu;
  hasIonMobility = true;
  std::cout << "MediumMagboltz86::SetIonMobility:" << std::endl;
  std::cout << "    Ion mobility set to " << mu << " cm2 / (V ns)." 
            << std::endl;
  if (mu < 0.)  std::cout << "    Warning: Mobility is negative!" << std::endl;

}

bool
MediumMagboltz86::IonVelocity(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& vx, double& vy, double& vz) {

  if (!hasIonMobility) {
    vx = vy = vz = 0.;
    return false;
  }

  vx = muIon * ex; vy = muIon * ey; vz = muIon * ez;
  return true;

}

bool 
MediumMagboltz86::GetGasNumber(std::string name, int& number) const {

  // Convert to upper-case
  for (unsigned int i = 0; i < name.length(); ++i) {
    name[i] = toupper(name[i]);
  }
  
  if (name == "") {
    number = 0; return false;
  }
  
  // CF4
  if (name == "CF4" || name == "FREON" || 
      name == "FREON-14" || name == "TETRAFLUOROMETHANE") {
    number = 1; return true;
  }
  // Argon
  if (name == "AR" || name == "ARGON") {
    number = 2; return true;
  }
  // Helium 4
  if (name == "HE" || name == "HELIUM" || name == "HE-4" || 
      name == "HELIUM-4" || name == "HE4" || name == "HELIUM4") {
    number = 3; return true;
  }
  // Helium 3
  if (name == "HE-3" || name == "HELIUM-3" || name == "HE3" || 
      name == "HELIUM3") {
    number = 4; return true;
  }
  // Neon
  if (name == "NE" || name == "NEON") {
    number = 5; return true;
  }
  // Krypton
  if (name == "KR" || name == "KRYPTON") {
    number = 6; return true;
  }
  // Xenon
  if (name == "XE" || name == "XENON") {
    number = 7; return true;
  }
  // Methane
  if (name == "CH4" || name == "METHANE" ) {
    number = 8; return true;
  }
  // Ethane
  if (name == "C2H6" || name == "ETHANE") {
    number = 9; return true;
  }
  // Propane
  if (name == "C3H8" || name == "PROPANE") {
    number = 10; return true;
  }
  // Isobutane
  if (name == "C4H10" || name == "ISOBUTANE" || name == "ISO" || 
      name == "IC4H10" || name == "ISO-C4H10" || name == "ISOC4H10") {
    number = 11; return true;
  }
  // CO2 (isotropic)
  if (name == "CO2" || name == "CARBON-DIOXIDE") {
    number = 12; return true;
  }
  // Neopentane
  if (name == "NEOPENTANE" || name == "NEO-PENTANE" || 
      name == "NEO-C5H12" || name == "NEOC5H12" || name == "C5H12") {
    number = 13; return true;
  }
  // Water
  if (name == "H2O" || name == "WATER" || name == "WATER-VAPOUR") {
    number = 14; return true;
  }
  // Oxygen
  if (name == "O2" || name == "OXYGEN") {
    number = 15; return true;
  }
  // Nitrogen
  if (name == "N2" || name == "NITROGEN" || 
      name == "NITROGEN-ISOTROPIC" || name == "N2-ISOTROPIC") {
    number = 16; return true;
  }
  // Nitric oxide (NO)
  if (name == "NO" || name == "NITRIC-OXIDE" || name == "NITROGEN-MONOXIDE") {
    number = 17; return true;
  }
  // Nitrous oxide (N2O)
  if (name == "N2O" || name == "NITROUS-OXIDE" || 
      name == "DINITROGEN-MONOXIDE" || name == "LAUGHING-GAS") {
    number = 18; return true;
  }
  // Ethene (C2H4)
  if (name == "C2H4" || name == "ETHENE" || name == "ETHYLENE") {
    number = 19; return true;
  }
  // Acetylene (C2H2)
  if (name == "C2H2" || name == "ACETYL" || 
      name == "ACETYLENE" || name == "ETHYNE") {
    number = 20; return true;
  }
  // Hydrogen
  if (name == "H2" || name == "HYDROGEN") {
    number = 21; return true;
  }
  // Deuterium
  if (name == "D2" || name == "DEUTERIUM") {
    number = 22; return true;
  }
  // Carbon monoxide (CO)
  if (name == "CO" || name == "CARBON-MONOXIDE") {
    number = 23; return true;
  }
  // Methylal (dimethoxymethane, CH3-O-CH2-O-CH3, "hot" version)
  if (name == "METHYLAL" || name == "METHYLAL-HOT" || name == "DMM" ||
      name == "DIMETHOXYMETHANE" || name == "C3H8O2") {
    number = 24; return true;
  }
  // DME
  if (name == "DME" || name == "DIMETHYL-ETHER" || name == "WOOD-ETHER" || 
      name == "METHOXYMETHANE" || name == "METHYL-ETHER" || name == "C2H60") {
    number = 25; return true;
  }
  // Reid step
  if (name == "REID-STEP") {
    number = 26; return true;
  }
  // Maxwell model
  if (name == "MAXWELL-MODEL") {
    number = 27; return true;
  }
  // Reid ramp
  if (name == "REID-RAMP") {
    number = 28; return true;
  }
  // C2F6
  if (name == "C2F6" || name == "FREON-116" || name == "ZYRON-116" || 
      name == "ZYRON-116-N5" || name == "HEXAFLUOROETHANE") {
    number = 29; return true;
  }
  // SF6
  if (name == "SF6" || name == "SULPHUR-HEXAFLUORIDE" || 
      name == "SULFUR-HEXAFLUORIDE") {
    number = 30; return true;
  }
  // NH3
  if (name == "NH3" || name == "AMMONIA") {
    number = 31; return true;
  }
  // Propene
  if (name == "C3H6" || name == "PROPENE" || name == "PROPYLENE") {
    number = 32; return true;
  }
  // Cyclopropane
  if (name == "C-PROPANE" || name == "CYCLO-PROPANE" || 
      name == "CYCLOPROPANE" || name == "C-C3H6" || name == "CYCLO-C3H6") {
    number = 33; return true;
  }
  // Methanol
  if (name == "METHANOL" || name == "METHYL-ALCOHOL" || 
      name == "WOOD-ALCOHOL" || name == "CH3OH") {
    number = 34; return true;
  }
  // Ethanol
  if (name == "ETHANOL" || name == "ETHYL-ALCOHOL" || 
      name == "GRAIN-ALCOHOL" || name == "C2H5OH") {
    number = 35; return true;
  }
  // Propanol
  if (name == "PROPANOL" || name == "2-PROPANOL" || name == "ISOPROPYL" || 
      name == "ISO-PROPANOL" || name == "ISOPROPANOL" || 
      name == "ISOPROPYL-ALCOHOL" || name == "C3H7OH") {
    number = 36; return true;
  }
  // Cesium / Caesium.
  if (name == "CS" || name == "CESIUM" || name == "CAESIUM") {
    number = 37; return true;
  }
  // Fluorine
  if (name == "F2" || name == "FLUOR" || name == "FLUORINE") {
    number = 38; return true;
  }
  if (name == "CS2" || name == "CARBON-DISULPHIDE" || 
      name == "CARBON-DISULFIDE") {
    number = 39; return true;
  }
  // COS
  if (name == "COS" || name == "CARBONYL-SULPHIDE" || 
      name == "CARBONYL-SULFIDE") {
    number = 40; return true;
  }
  // Deuterated methane
  if (name == "DEUT-METHANE" || name == "DEUTERIUM-METHANE" || 
      name == "DEUTERATED-METHANE" || name == "CD4") {
    number = 41; return true;
  }
  // BF3
  if (name == "BF3" || name == "BORON-TRIFLUORIDE") {
    number = 42; return true;
  }
  // C2HF5 and C2H2F4.
  if (name == "C2HF5" || name == "C2H2F4" || name == "C2F5H" || 
      name == "C2F4H2" || name == "FREON-134" || name == "FREON-134-A" || 
      name == "FREON-125" || name == "ZYRON-125" || 
      name == "TETRAFLUOROETHANE" || name == "PENTAFLUOROETHANE") {
    number = 43; return true;
  }
  // CHF3
  if (name == "CHF3" || name == "FREON-23" || name == "TRIFLUOROMETHANE") {
    number = 50; return true;
  }
  // CF3Br
  if (name == "CF3BR" || name == "TRIFLUOROBROMOMETHANE" || 
      name == "HALON-1301" || name == "FREON-13B1") {
    number = 51; return true;
  }
  // C3F8
  if (name == "C3F8" || name == "OCTAFLUOROPROPANE" || name == "R218" || 
      name == "FREON-218" || name == "PERFLUOROPROPANE" || 
      name == "RC-218" || name == "PFC-218") {
    number = 52; return true;
  }
  // Ozone
  if (name == "OZONE" || name == "O3") {
    number = 53; return true;
  }
  // Mercury
  if (name == "MERCURY" || name == "HG" || name == "HG2") {
    number = 54; return true;
  }
  // H2S
  if (name == "H2S" || name == "HYDROGEN-SULPHIDE" || name == "SEWER-GAS" ||
      name == "HYDROGEN-SULFIDE" || name == "HEPATIC-ACID" ||
      name == "SULFUR-HYDRIDE" || name == "DIHYDROGEN-MONOSULFIDE" || 
      name == "DIHYDROGEN-MONOSULPHIDE" || name == "SULPHUR-HYDRIDE" || 
      name == "STINK-DAMP" || name == "SULFURATED-HYDROGEN") {
    number = 55; return true;
  }
  // n-butane
  if (name == "N-BUTANE" || name == "N-C4H10") {
    number = 56; return true;
  }
  // n-pentane
  if (name == "N-PENTANE" || name == "N-C5H12") {
    number = 57; return true;
  }
  // Nitrogen
  if (name == "NI" || name == "NITROGEN" || name == "NI-ANISOTROPIC" || 
      name == "NITROGEN-ANISOTROPIC" || name == "N2" || 
      name == "N2-ANISOTROPIC") {
    number = 58; return true;
  }
  // Germane, GeH4
  if (name == "GERMANE" || name == "GERM" || name == "GERMANIUM-HYDRIDE" || 
      name == "GERMANIUM-TETRAHYDRIDE" || name == "GERMANOMETHANE" || 
      name == "MONOGERMANE" || name == "GEH4") {
    number = 59; return true;
  }
  // Silane, SiH4
  if (name == "SILANE" || name == "SIL" || name == "SILICON-HYDRIDE" ||
      name == "SILICON-TETRAHYDRIDE" || name == "SILICANE" || 
      name == "MONOSILANE" || name == "SIH4") {
    number = 60; return true;
  }
  
  std::cerr << "MediumMagboltz86::GetGasNumber():" << std::endl;
  std::cerr << "    Gas " << name << " is not defined." << std::endl;
  return false;
  
}

bool 
MediumMagboltz86::GetGasName(const int number, std::string& gasname) const {
  
  switch (number) {
    case 0:  gasname = ""; return false; break;
    case 1:  gasname = "CF4";     break;
    case 2:  gasname = "Ar";      break;
    case 3:  gasname = "He";      break;
    case 4:  gasname = "He-3";    break;
    case 5:  gasname = "Ne";      break;
    case 6:  gasname = "Kr";      break;
    case 7:  gasname = "Xe";      break;
    case 8:  gasname = "CH4";     break;
    case 9:  gasname = "C2H6";    break;
    case 10: gasname = "C3H8";    break;
    case 11: gasname = "iC4H10";  break;
    case 12: gasname = "CO2";     break;
    case 13: gasname = "Neopentane"; break;
    case 14: gasname = "H2O";     break;
    case 15: gasname = "O2";      break;
    case 16: gasname = "N2";      break;
    case 17: gasname = "NO";      break;
    case 18: gasname = "N2O";     break;
    case 19: gasname = "C2H4";    break;
    case 20: gasname = "C2H2";    break;
    case 21: gasname = "H2";      break;
    case 22: gasname = "D2";      break;
    case 23: gasname = "CO";      break;
    case 24: gasname = "Methylal"; break;
    case 25: gasname = "DME";     break;
    case 26: gasname = "Reid-Step";     break;
    case 27: gasname = "Maxwell-Model"; break;
    case 28: gasname = "Reid-Ramp";     break;
    case 29: gasname = "C2F6";    break;
    case 30: gasname = "SF6";     break;
    case 31: gasname = "NH3";     break;
    case 32: gasname = "C3H6";    break;
    case 33: gasname = "Cyclopropane"; break;
    case 34: gasname = "Methanol";     break;
    case 35: gasname = "Ethanol";      break;
    case 36: gasname = "Propanol";     break;
    case 37: gasname = "Cs";      break;
    case 38: gasname = "F2";      break;
    case 39: gasname = "CS2";     break;
    case 40: gasname = "COS";     break;
    case 41: gasname = "CD4";     break;
    case 42: gasname = "BF3";     break;
    case 43: gasname = "C2HF5";   break;
    case 50: gasname = "CHF3";    break;
    case 51: gasname = "CF3Br";   break;
    case 52: gasname = "C3F8";    break;
    case 53: gasname = "O3";      break;
    case 54: gasname = "Hg";      break;
    case 55: gasname = "H2S";     break;
    case 56: gasname = "n-C4H10"; break;
    case 57: gasname = "n-C5H12"; break;
    case 58: gasname = "N2";      break;
    case 59: gasname = "GeH4";    break;
    case 60: gasname = "SiH4";    break;
    default: gasname = ""; return false; break;
  }
  return true;

}

bool 
MediumMagboltz86::Mixer() {

  // Set constants and parameters in Magboltz common blocks
  cnsts_.echarg = ElementaryCharge;
  cnsts_.emass = ElectronMassGramme;
  cnsts_.amu = AtomicMassUnit;
  cnsts_.pir2 = BohrRadius * BohrRadius * Pi;  
  inpt_.ary = RydbergEnergy;

  inpt_.akt = BoltzmannConstant * temperature;
  inpt_.tempc = temperature - 273.15;
  inpt_.torr = pressure;

  inpt_.nGas = nComponents;
  inpt_.nStep = nEnergySteps;
  inpt_.nAniso = 2;
  inpt_.efinal = eFinal;
  inpt_.estep = eStep;
  
  // Correct for density
  const double density = LoschmidtNumber * (pressure / AtmosphericPressure) * 
                         (273.15 / temperature);
  const double prefactor = density * SpeedOfLight * sqrt(2. / ElectronMass);

  // Fill electron energy array, reset collision rates
  for (int i = nEnergySteps; i--;) {
    cfTot[i] = 0.; 
    scatModel[i] = 0; 
    for (int j = nMaxLevels; j--;) {
      cf[i][j] = 0.;
      scatParameter[i][j] = 0.5;
      scatCut[i][j] = 1.;
    }
  }
  for (int i = nMaxLevels; i--;) iDeexcitation[i] = -1;
  nDeexcitations = 0;
  deexcitations.clear();

  for (int i = nMaxGases; i--;) ionPot[i] = -1.;
  minIonPot = -1.;
  
  // Gas cross-section
  static double q[nEnergySteps][6];
  // Parameters for scattering angular distribution
  static double pEqEl[nEnergySteps][6];
  // Inelastic cross-sections
  static double qIn[nEnergySteps][nMaxInelasticTerms];
  // Parameters for angular distribution in inelastic collisions
  static double pEqIn[nEnergySteps][nMaxInelasticTerms]; 
  // Penning transfer parameters
  static double penFra[nMaxInelasticTerms][3];
  // Description of cross-section terms
  static char scrpt[226][30];
  
  // Number of inelastic cross-section terms
  long long nIn;
  // Threshold energies
  double e[6], eIn[nMaxInelasticTerms];
  
  // Virial coefficient (not used)
  double virial;
  // Splitting function parameter
  double w;
  // (An)isotropic scattering models
  long long kIn[nMaxInelasticTerms] = {0};
  long long kEl[6] = {0};    
  
  char name[15];  
          
  if (debug) {
    std::cout << "MediumMagboltz86::Mixer:" << std::endl;
    std::cout << "    Creating table of collision rates with " 
              << nEnergySteps << " energy steps " << std::endl;
    std::cout << "    between 0 and " << eFinal << " eV." << std::endl;
  }
  nTerms = 0;
  
  std::ofstream outfile;
  if (useCsOutput) outfile.open("cs.txt", std::ios::out);
  outfile << "# " << std::endl;

  // Loop over the gases in the mixture.  
  for (int iGas = 0; iGas < nComponents; ++iGas) {
    // Retrieve the gas cross-section data from Magboltz.
    long long ngs = gas[iGas];
    gasmix_(&ngs, q[0], qIn[0], &nIn, e, eIn, name, &virial, &w, 
            pEqEl[0], pEqIn[0], penFra[0], kEl, kIn, scrpt);
    if (debug) {
      std::cout << "    " << name << std::endl;
      std::cout << "      m / M:                " << 0.5 * e[1] << std::endl;
      std::cout << "      Ionisation threshold: " << e[2] << " eV" << std::endl;
      std::cout << "      Attachment threshold: " << e[3] << " eV" << std::endl;
      std::cout << "      Splitting parameter:  " << w << " eV" << std::endl;
    }
    int np0 = nTerms;
    
    // Check if there is still sufficient space.
    if (np0 + nIn + 2 >= nMaxLevels) {
      std::cerr << "MediumMagboltz86::Mixer:" << std::endl;
      std::cerr << "    Max. number of levels (" << nMaxLevels 
                << ") exceeded." << std::endl;
      return false;
    }
    
    double van = fraction[iGas] * prefactor;
        
    int np = np0;
    // Elastic scattering
    ++nTerms;
    scatModel[np] = kEl[1];
    double r = 1. + e[1] / 2.;
    rgas[np] = r;
    energyLoss[np] = 0.; 
    for (int j = 0; j < 30; ++j) {
      description[np][j] = scrpt[1][j];
    }
    csType[np] = 6 * iGas;
    bool withIon = false, withAtt = false;
    // Ionisation
    if (eFinal >= e[2]) {
      withIon = true;
      ++nTerms; ++np;
      scatModel[np] = kEl[2];
      rgas[np] = r;
      energyLoss[np] = e[2] / r;
      wSplit[np] = w;
      ionPot[iGas] = e[2];
      for (int j = 0; j < 30; ++j) {
        description[np][j] = scrpt[2][j];
      }
      csType[np] = 6 * iGas + 1;
    }
    // Attachment
    if (eFinal >= e[3]) {
      withAtt = true;
      ++nTerms; ++np;
      scatModel[np] = kEl[3];
      rgas[np] = r;
      energyLoss[np] = 0.;
      for (int j = 0; j < 30; ++j) {
        description[np][j] = scrpt[3][j];
      }
      csType[np] = 6 * iGas + 2;
    }
    // Inelastic terms
    for (int j = 0; j < nIn; ++j) {
      ++np;
      scatModel[np] = kIn[j];
      rgas[np] = r;
      energyLoss[np] = eIn[j] / r;
      for (int k = 0; k < 30; ++k) {
        description[np][k] = scrpt[6 + j][k];
      }
      if ((description[np][1] == 'E' && description[np][2] == 'X') ||
          (description[np][0] == 'E' && description[np][1] == 'X')) {
        // Excitation
        csType[np] = 6 * iGas + 4;     
      } else if (eIn[j] < 0.) {
        // Super-elastic collision
        csType[np] = 6 * iGas + 5;
      } else {
        // Inelastic collision
        csType[np] = 6 * iGas + 3;
      }
    }
    nTerms += nIn;
    // Loop over the energy table
    for (int iE = 0; iE < nEnergySteps; ++iE) {
      np = np0;
      if (useCsOutput) {
        outfile << iE * eStep << "  " << q[iE][1] << "  " << q[iE][2] 
                << "  " << q[iE][3] << "  ";
      }
      // Elastic scattering
      cf[iE][np] = q[iE][1] * van;
      if (scatModel[np] == 1) {
        ComputeAngularCut(pEqEl[iE][1], scatCut[iE][np], scatParameter[iE][np]);
      } else if (scatModel[np] == 2) {
        scatParameter[iE][np] = pEqEl[iE][1];
      }
      // Ionisation
      if (withIon) {
        ++np;
        cf[iE][np] = q[iE][2] * van;
        if (scatModel[np] == 1) {
          ComputeAngularCut(pEqEl[iE][2], scatCut[iE][np], 
                            scatParameter[iE][np]);
        } else if (scatModel[np] == 2) {
          scatParameter[iE][np] = pEqEl[iE][2];
        }
      }
      // Attachment
      if (withAtt) {
        ++np;
        cf[iE][np] = q[iE][3] * van;
      }
      // Inelastic terms
      for (int j = 0; j < nIn; ++j) {
        ++np;
        if (useCsOutput) outfile << qIn[iE][j] << "  ";
        cf[iE][np] = qIn[iE][j] * van;
        if (cf[iE][np] < 0.) {
          std::cerr << "MediumMagboltz86::Mixer:" << std::endl;
          std::cerr << "    Negative inelastic cross-section at " 
                    << iE * eStep << " eV." << std::endl; 
          std::cerr << "    Set to zero." << std::endl;
          cf[iE][np] = 0.;
        }
        if (scatModel[np] == 1) {
          ComputeAngularCut(pEqIn[iE][j], scatCut[iE][np], 
                            scatParameter[iE][np]);
        } else if (scatModel[np] == 2) {
          scatParameter[iE][np] = pEqIn[iE][j];
        }
      }
      if (useCsOutput) outfile << std::endl;
    }
  }
  if (useCsOutput) outfile.close();

  // Find the min. ionisation threshold
  for (int i = nMaxGases; i--;) {
    if (ionPot[i] < 0.) continue;
    if (minIonPot < 0.) {
      minIonPot = ionPot[i];
    } else if (ionPot[i] < minIonPot) {
      minIonPot = ionPot[i];
    }
  }
  if (debug) {
    std::cout << "Lowest ionisation threshold in the mixture: " << minIonPot
              << " eV" << std::endl;
  }

  for (int iE = nEnergySteps; iE--;) {
    // Calculate the total collision frequency
    for (int k = nTerms; k--;) {
      if (cf[iE][k] < 0.) {
          std::cerr << "MediumMagboltz86::Mixer:" << std::endl;
          std::cerr << "    Negative collision rate at " 
                    << iE * eStep << " eV. " << std::endl;
          std::cerr << "    Set to zero." << std::endl;
          cf[iE][k] = 0.;
      }
      cfTot[iE] += cf[iE][k];
    }
    // Normalise the collision frequencies
    if (cfTot[iE] != 0.) {
      for (int k = nTerms; k--;) cf[iE][k] /= cfTot[iE];
    }
    for (int k = 1; k < nTerms; ++k) {
      cf[iE][k] += cf[iE][k - 1];
    }
    const double eroot = sqrt(eStep * (iE + 0.5));
    cfTot[iE] *= eroot;
  }
  
  int nInterval = int(nEnergySteps / 8.);  
  // Calculate the null collision frequencies
  for (int i = 0; i < 8; ++i) {
    cfNull[i] = 0.;
    for (int j = nInterval * i; j < nInterval * (i + 1); ++j) {
      if (cfTot[j] >= cfNull[i]) cfNull[i] = cfTot[j];
    }
  } 

  double nullmax = cfNull[0];
  for (int i = 1; i < 8; ++i) {
    if (cfNull[i] > nullmax) nullmax = cfNull[i];
  }
  for (int i = 0; i < 8; ++i) {
    cfNull[i] = nullmax;
  }
  
  // Reset the collision counters
  nCollisionsDetailed.resize(nTerms);
  nCollisions[0] = 0; nCollisions[1] = 0; nCollisions[2] = 0;
  nCollisions[3] = 0; nCollisions[4] = 0; nCollisions[5] = 0;
  for (int j = nTerms; j--;) nCollisionsDetailed[j] = 0;
  
  if (debug) {
    std::cout << "Magboltz86::Mixer:" << std::endl;
    std::cout << "    Energy [eV]    Collision Rate [ns-1]" << std::endl;
    for (int i = 0; i < 8; ++i) { 
      std::cout << "    " << std::setw(10) 
                << (2 * i + 1) * eFinal / 16
                << "    " << std::setw(18)
                << cfTot[(i + 1) * nEnergySteps / 16] << std::endl;
    }
  }

  if (useDeexcitation) ComputeDeexcitationTable();
  if (!ComputePhotonCollisionTable()) {
     std::cerr << "MediumMagboltz86: " << std::endl;
     std::cerr << "    Photon collision rates could not be calculated." 
               << std::endl;
     if (useDeexcitation)return false;
  }

  return true;

}

void 
MediumMagboltz86::ComputeAngularCut(double parIn, double& cut, double &parOut) {

  // Set cuts on angular distribution and
  // renormalise forward scattering probability

  if (parIn <= 1.) {
    cut = 1.;
    parOut = parIn;
    return;
  }

  const double rads = 2. / Pi;
  const double cns = parIn - 0.5;
  const double thetac = asin(2. * sqrt(cns - cns * cns));
  const double fac = (1. - cos(thetac)) / pow(sin(thetac), 2.); 
  parOut = cns * fac + 0.5;
  cut = thetac * rads;
  
}

void
MediumMagboltz86::ComputeDeexcitationTable() {

  for (int i = nMaxLevels; i--;) iDeexcitation[i] = -1;
  deexcitations.clear();

  std::map<std::string, int> mapLevels;
  // Make a mapping of all excitation levels 
  for (int i = 0; i < nTerms; ++i) {
    if (csType[i] % 6 != 4) continue;
    switch (gas[int(csType[i] / 6)]) {
      case 2:
        // Argon
        std::string level = "       ";
        for (int j = 0; j < 7; ++j) level[j] = description[i][5 + j];
        if      (level == "1S5    ") mapLevels["Ar_1S5"] = i;
        else if (level == "1S4    ") mapLevels["Ar_1S4"] = i;
        else if (level == "1S3    ") mapLevels["Ar_1S3"] = i;
        else if (level == "1S2    ") mapLevels["Ar_1S2"] = i;
        else if (level == "2P10   ") mapLevels["Ar_2P10"] = i;
        else if (level == "2P9    ") mapLevels["Ar_2P9"] = i;
        else if (level == "2P8    ") mapLevels["Ar_2P8"] = i;
        else if (level == "2P7    ") mapLevels["Ar_2P7"] = i;
        else if (level == "2P6    ") mapLevels["Ar_2P6"] = i;
        else if (level == "2P5    ") mapLevels["Ar_2P5"] = i;
        else if (level == "2P4    ") mapLevels["Ar_2P4"] = i;
        else if (level == "2P3    ") mapLevels["Ar_2P3"] = i;
        else if (level == "2P2    ") mapLevels["Ar_2P2"] = i;
        else if (level == "2P1    ") mapLevels["Ar_2P1"] = i;
        else if (level == "3D6    ") mapLevels["Ar_3D6"] = i;
        else if (level == "3D5    ") mapLevels["Ar_3D5"] = i;
        else if (level == "3D3    ") mapLevels["Ar_3D3"] = i;
        else if (level == "3D4!   ") mapLevels["Ar_3D4!"] = i;
        else if (level == "3D4    ") mapLevels["Ar_3D4"] = i;
        else if (level == "3D1!!  ") mapLevels["Ar_3D1!!"] = i;
        else if (level == "2S5    ") mapLevels["Ar_2S5"] = i;
        else if (level == "2S4    ") mapLevels["Ar_2S4"] = i;
        else if (level == "3D1!   ") mapLevels["Ar_3D1!"] = i;
        else if (level == "3D2    ") mapLevels["Ar_3D2"] = i;
        else if (level == "3S1!!!!") mapLevels["Ar_3S1!!!!"] = i;
        else if (level == "3S1!!  ") mapLevels["Ar_3S1!!"] = i;
        else if (level == "3S1!!! ") mapLevels["Ar_3S1!!!"] = i;
        else if (level == "2S3    ") mapLevels["Ar_2S3"] = i;
        else if (level == "2S2    ") mapLevels["Ar_2S2"] = i;
        else if (level == "3S1!   ") mapLevels["Ar_3S1!"] = i;
        else if (level == "4D5    ") mapLevels["Ar_4D5"] = i;
        else if (level == "3S4    ") mapLevels["Ar_3S4"] = i;
        else if (level == "4D2    ") mapLevels["Ar_4D2"] = i;
        else if (level == "4S1!   ") mapLevels["Ar_4S1!"] = i;
        else if (level == "3S2    ") mapLevels["Ar_3S2"] = i;
        else if (level == "5D5    ") mapLevels["Ar_5D5"] = i;
        else if (level == "4S4    ") mapLevels["Ar_4S4"] = i;
        else if (level == "5D2    ") mapLevels["Ar_5D2"] = i;
        else if (level == "6D5    ") mapLevels["Ar_6D5"] = i;
        else if (level == "5S1!   ") mapLevels["Ar_5S1!"] = i;
        else if (level == "4S2    ") mapLevels["Ar_4S2"] = i;
        else if (level == "5S4    ") mapLevels["Ar_5S4"] = i;
        else if (level == "6D2    ") mapLevels["Ar_6D2"] = i;
        else if (level == "HIGH   ") mapLevels["Ar_High"] = i;
        break;
    }
  }

  std::map<std::string, int> mapDxc;
  std::map<std::string, int>::iterator itMap;
  nDeexcitations = 0;
  for (itMap = mapLevels.begin(); itMap != mapLevels.end(); itMap++) {
    std::string level = (*itMap).first;
    mapDxc[level] = nDeexcitations;
    iDeexcitation[(*itMap).second] = nDeexcitations;
    ++nDeexcitations;
  }
 
  deexcitation newDxc;
  for (itMap = mapLevels.begin(); itMap != mapLevels.end(); itMap++) {
    std::string level = (*itMap).first;
    newDxc.label = level;
    newDxc.energy = energyLoss[(*itMap).second] * rgas[(*itMap).second];
    if (level == "Ar_1S5" || level == "Ar_1S3") {
      // Metastable 4s levels
      newDxc.p.clear(); newDxc.final.clear(); newDxc.nChannels = 0;
    } else if (level == "Ar_1S4") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.119; newDxc.final[0] = -1;
    } else if (level == "Ar_1S2") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.51; newDxc.final[0] = -1;
    } else if (level == "Ar_2P10") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.0189;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 5.43e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 9.8e-4;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 1.9e-4;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P9") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.0331; newDxc.final[0] = mapDxc["Ar_1S5"];
    } else if (level == "Ar_2P8") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 9.28e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 0.0215;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 1.47e-3; newDxc.final[2] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P7") {
      int nc = 4; 
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 5.18e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 0.025;   newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 2.43e-3; newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 1.06e-3; newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P6") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.0245;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 4.9e-3;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 5.03e-3; newDxc.final[2] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P5") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels =nc;
      newDxc.p[0] = 0.0402; newDxc.final[0] = mapDxc["Ar_1S4"];
    } else if (level == "Ar_2P4") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 6.25e-4; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 2.2e-5;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0186;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 0.0139;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P3") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 3.8e-3;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 8.47e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0223;  newDxc.final[2] = mapDxc["Ar_1S3"];
    } else if (level == "Ar_2P2") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 6.39e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 1.83e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0117;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 0.0153;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P1") {
      int nc = 2;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 2.36e-4; newDxc.final[0] = mapDxc["Ar_1S4"];
      newDxc.p[1] = 0.0445;  newDxc.final[1] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_3D6") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 8.1e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P4"];
      newDxc.p[2] = 3.6e-4; newDxc.final[2] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3D5") {
      int nc = 5;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 7.4e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 3.9e-5; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 3.2e-5; newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 1.4e-4; newDxc.final[3] = mapDxc["Ar_2P3"];
      newDxc.p[4] = 1.7e-4; newDxc.final[4] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3D3") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 4.9e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.6e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 2.5e-3; newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 3.9e-4; newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.1e-4; newDxc.final[5] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3D4!") {
      newDxc.p.clear(); newDxc.final.clear(); newDxc.nChannels = 0;
    } else if (level == "Ar_3D4") {
      int nc = 2;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.011;  newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 8.8e-5; newDxc.final[1] = mapDxc["Ar_2P6"];
    } else if (level == "Ar_3D1!!") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.2e-4; newDxc.final[0] = mapDxc["Ar_2P9"];
      newDxc.p[1] = 5.7e-3; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 7.3e-3; newDxc.final[2] = mapDxc["Ar_2P7"];
    } else if (level == "Ar_2S5") {
      int nc = 8;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 4.9e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 0.011;  newDxc.final[1] = mapDxc["Ar_2P9"];
      newDxc.p[2] = 1.1e-3; newDxc.final[2] = mapDxc["Ar_2P8"];
      newDxc.p[3] = 4.6e-4; newDxc.final[3] = mapDxc["Ar_2P7"];
      newDxc.p[4] = 3.3e-3; newDxc.final[4] = mapDxc["Ar_2P6"];
      newDxc.p[5] = 5.9e-5; newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 1.2e-4; newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 3.1e-4; newDxc.final[7] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_2S4") {
      int nc = 10;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.077;   newDxc.final[0] = -1;
      newDxc.p[1] = 2.44e-3; newDxc.final[1] = mapDxc["Ar_2P10"];
      newDxc.p[2] = 8.9e-3;  newDxc.final[2] = mapDxc["Ar_2P8"];
      newDxc.p[3] = 4.6e-3;  newDxc.final[3] = mapDxc["Ar_2P7"];
      newDxc.p[4] = 2.7e-3;  newDxc.final[4] = mapDxc["Ar_2P6"];
      newDxc.p[5] = 1.3e-3;  newDxc.final[5] = mapDxc["Ar_2P5"];
      newDxc.p[6] = 4.5e-4;  newDxc.final[6] = mapDxc["Ar_2P4"];
      newDxc.p[7] = 2.9e-5;  newDxc.final[7] = mapDxc["Ar_2P3"];
      newDxc.p[8] = 3.e-5;   newDxc.final[8] = mapDxc["Ar_2P2"];
      newDxc.p[9] = 1.6e-4;  newDxc.final[9] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_3D1!") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 3.1e-3; newDxc.final[0] = mapDxc["Ar_2P9"];
      newDxc.p[1] = 2.e-3;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 9.8e-6; newDxc.final[2] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_3D2") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.27;    newDxc.final[0] = -1;
      newDxc.p[1] = 9.52e-4; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 0.011;   newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.3e-3;  newDxc.final[3] = mapDxc["Ar_2P6"];
    } else if (level == "Ar_3S1!!!!") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 8.3e-4; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 0.013;  newDxc.final[1] = mapDxc["Ar_2P4"];
      newDxc.p[2] = 2.2e-3; newDxc.final[2] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_3S1!!") {
      int nc = 3;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 3.69e-4; newDxc.final[0] = mapDxc["Ar_2P7"];
      newDxc.p[1] = 3.76e-3; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 6.2e-3;  newDxc.final[2] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3S1!!!") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.015; newDxc.final[0] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_2S3") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 3.26e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.22e-3; newDxc.final[1] = mapDxc["Ar_2P7"];
      newDxc.p[2] = 0.01;    newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 5.1e-3;  newDxc.final[3] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_2S2") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.035;  newDxc.final[0] = -1;
      newDxc.p[1] = 8.9e-3; newDxc.final[1] = mapDxc["Ar_2P3"];
      newDxc.p[2] = 3.4e-3; newDxc.final[2] = mapDxc["Ar_2P2"];
      newDxc.p[3] = 1.9e-3; newDxc.final[3] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_3S1!") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 0.318;   newDxc.final[0] = -1;
      newDxc.p[1] = 3.96e-4; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 4.2e-4;  newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 4.5e-3;  newDxc.final[3] = mapDxc["Ar_2P4"];
      newDxc.p[4] = 7.1e-3;  newDxc.final[4] = mapDxc["Ar_2P2"];
      newDxc.p[5] = 5.2e-3;  newDxc.final[5] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_4D5") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 2.78e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.8e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 8.6e-4;  newDxc.final[2] = mapDxc["Ar_2P6"];
      newDxc.p[3] = 9.2e-4;  newDxc.final[3] = mapDxc["Ar_2P5"];
      newDxc.p[4] = 4.6e-4;  newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.6e-4;  newDxc.final[5] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3S4") {
      int nc = 9;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 4.21e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.e-3;   newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 1.7e-3;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 7.2e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 3.5e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 1.2e-4;  newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 4.2e-6;  newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 3.3e-5;  newDxc.final[7] = mapDxc["Ar_2P2"];
      newDxc.p[8] = 9.7e-5;  newDxc.final[8] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_4D2") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.7e-4; newDxc.final[0] = mapDxc["Ar_2P7"];
    } else if (level == "Ar_4S1!") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.05e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 3.1e-5;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.5e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.e-4;   newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 5.8e-5;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 1.2e-4;  newDxc.final[5] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_3S2") {
      int nc = 9;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 2.85e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 5.1e-5;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 5.3e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 1.6e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 1.5e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 6.e-4;   newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 2.48e-3; newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 9.6e-4;  newDxc.final[7] = mapDxc["Ar_2P2"];
      newDxc.p[8] = 3.59e-4; newDxc.final[8] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_5D5") {
      int nc = 8;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 2.2e-3;  newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.1e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 7.6e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.2e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 2.4e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 2.1e-4;  newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 2.4e-4;  newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 1.2e-4;  newDxc.final[7] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_4S4") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.9e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.1e-3; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 5.2e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 5.1e-4; newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 9.4e-5; newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 5.4e-5; newDxc.final[5] = mapDxc["Ar_2P4"];
    } else if (level == "Ar_5D2") {
      int nc = 4;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 5.9e-5; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 9.e-6;  newDxc.final[1] = mapDxc["Ar_2P7"];
      newDxc.p[2] = 1.5e-4; newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 3.1e-5; newDxc.final[3] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_6D5") {
      int nc = 6;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.9e-3;  newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 4.2e-4;  newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 3.e-4;   newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 5.1e-5;  newDxc.final[3] = mapDxc["Ar_2P4"];
      newDxc.p[4] = 6.6e-5;  newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.21e-4; newDxc.final[5] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_5S1!") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 7.7e-5; newDxc.final[0] = mapDxc["Ar_2P5"];
    } else if (level == "Ar_4S2") {
      int nc = 7;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 4.5e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.1e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 1.2e-4; newDxc.final[3] = mapDxc["Ar_2P5"];
      newDxc.p[4] = 1.8e-4; newDxc.final[4] = mapDxc["Ar_2P4"];
      newDxc.p[5] = 9.e-4;  newDxc.final[5] = mapDxc["Ar_2P3"];
      newDxc.p[6] = 3.3e-4; newDxc.final[6] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_5S4") {
      int nc = 5;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 3.6e-4; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 1.5e-4; newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 1.4e-4; newDxc.final[3] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_6D2") {
      int nc = 0;
      newDxc.p.clear(); newDxc.final.clear(); newDxc.nChannels = nc;
    } else if (level == "Ar_High") {
      int nc = 1;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.nChannels = nc;
      newDxc.p[0] = 1.e-2; newDxc.final[0] = -1;
    } else {
      std::cerr << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl;
      std::cerr << "    Missing de-excitation data for level " 
                << level << "." << std::endl;
      std::cerr << "    Program bug!" << std::endl;
      return;
    }
    deexcitations.push_back(newDxc);
  }
  
  if (debug) {
    std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl; 
    std::cout << "    Found " << nDeexcitations
              << " levels with available de-excitation data." << std::endl;
  }

  if (nComponents != 2) {
    std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl;
    std::cout << "    Gas mixture has " << nComponents 
              << " components." << std::endl;
    std::cout << "    Collisional deexcitation is implemented only for" 
              << " binary mixtures." << std::endl;
  } else if ((gas[0] == 2 && gas [1] == 8) || (gas[0] == 8 && gas[1] == 2)) {
    // Ar-CH4
    int ar = 0;
    const double b1 = 0.2;
    const double b3 = 22.121274;
    const double b4 = 3.842488;
    double fB = b4 / b3;
    double fA = b1 / b3;
    double gA = (1. - b1) / b3;
    double p = pressure / 760.;
    fB *= p;
    fA *= p;
    gA *= p;
    if (gas[0] == 2) {
      fB *= fraction[1];
      fA *= fraction[0];
      gA *= fraction[0];
      ar = 0;
    } else {
      fB *= fraction[0];
      fA *= fraction[1];
      gA *= fraction[1];
      ar = 1;
    }
    for (int j = nDeexcitations; j--;) {
      std::string level = deexcitations[j].label;
      if (level == "Ar_1S5" || level == "Ar_1S4" || 
          level == "Ar_1S3" || level == "Ar_1S2") {
        deexcitations[j].p.push_back(gA / 500.);
        deexcitations[j].final.push_back(-3);
        deexcitations[j].nChannels += 1;
      } else if (level == "Ar_2P10" || level == "Ar_2P9" || level == "Ar_2P8" ||
          level == "Ar_2P7"  || level == "Ar_2P6" || level == "Ar_2P5" ||
          level == "Ar_2P4"  || level == "Ar_2P3" || level == "Ar_2P2" ||
          level == "Ar_2P1") {
        deexcitations[j].p.push_back(fB / 30. + fA / 30.);
        deexcitations[j].final.push_back(-2);
        deexcitations[j].p.push_back(gA / 30.);
        deexcitations[j].final.push_back(-3);
        deexcitations[j].nChannels += 2;
      } else if (level == "Ar_3D6"     || level == "Ar_3D5"   ||
                 level == "Ar_3D3"     || level == "Ar_3D4!"  ||
                 level == "Ar_3D4"     || level == "Ar_3D1!!" ||
                 level == "Ar_2S5"     || level == "Ar_2S4"   ||
                 level == "Ar_3D1!"    || level == "Ar_3D2"   ||
                 level == "Ar_3S1!!!!" || level == "Ar_3S1!!" ||
                 level == "Ar_3S1!!!"  || level == "Ar_2S3"   ||
                 level == "Ar_2S2"     || level == "Ar_3S1!") {
        deexcitations[j].p.push_back(fB / 40. + fA / 40.);
        deexcitations[j].final.push_back(-2);
        deexcitations[j].p.push_back(gA / 40.);
        deexcitations[j].final.push_back(-3);
        deexcitations[j].nChannels += 2;
      } else if (level == "Ar_4D5" || level == "Ar_3S4"  || 
                 level == "Ar_4D2" || level == "Ar_4S1!" ||
                 level == "Ar_3S2" || level == "Ar_5D5"  || 
                 level == "Ar_4S4" || level == "Ar_5D2"  ||
                 level == "Ar_6D5" || level == "Ar_5S1!" ||
                 level == "Ar_4S2" || level == "Ar_5S4"  ||
                 level == "Ar_6D2" || level == "Ar_High") {
        deexcitations[j].p.push_back(fB / 100. + fA / 100.);
        deexcitations[j].final.push_back(-2);
        deexcitations[j].p.push_back(gA / 40.);
        deexcitations[j].final.push_back(-3);
        deexcitations[j].nChannels += 2;
      }
    }
  }

  if (debug) {
    std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl;
    std::cout << "    Level             Lifetimes [ns]" << std::endl;
    std::cout << "                Total      Radiative      Collisional"
              << std::endl;
  }
  for (int i = 0; i < nDeexcitations; ++i) {
    deexcitations[i].rate = 0.;
    double fRad = 0.;
    double fPenn = 0.; 
    double fLoss = 0.;
    for (int j = deexcitations[i].nChannels; j--;) {
      deexcitations[i].rate += deexcitations[i].p[j];
      if (deexcitations[i].final[j] >= -1) fRad += deexcitations[i].p[j];
      else if (deexcitations[i].final[j] == -2) fPenn += deexcitations[i].p[j];
      else if (deexcitations[i].final[j] == -3) fLoss += deexcitations[i].p[j];
    }
    if (deexcitations[i].rate > 0.) {
      if (debug) {
        std::cout << std::setw(15) << deexcitations[i].label << "  " 
                  << std::setw(10) << 1. / deexcitations[i].rate << "  ";
        if (fRad > 0.) {
          std::cout << std::setw(10) <<  1. / fRad << "  ";
        } else {
          std::cout << "----------  ";
        }
        if (fPenn + fLoss > 0.) {
          std::cout << std::setw(10) << 1. / (fPenn + fLoss) << std::endl;
        } else {
          std::cout << "----------   " << std::endl;
        }
      }
      for (int j = 0; j < deexcitations[i].nChannels; ++j) {
        deexcitations[i].p[j] /= deexcitations[i].rate;
        if (j > 0) deexcitations[i].p[j] += deexcitations[i].p[j - 1];
      }
    }
  }
  
}

void
MediumMagboltz86::ComputeDeexcitation(int iLevel) {
  
  nDeexcitationProducts = 0;
  dxcProducts.clear();

  dxcProd newDxcProd;
  newDxcProd.t = 0.;

  while (iLevel >= 0 && iLevel < nDeexcitations) {
    if (deexcitations[iLevel].rate <= 0.) return;
    // Determine the de-excitation time
    newDxcProd.t += -log(RndmUniformPos()) / deexcitations[iLevel].rate;
    // Select the transition
    int fLevel = -3;
    const double r = RndmUniform();
    for (int j = 0; j < deexcitations[iLevel].nChannels; ++j) {
      if (r <= deexcitations[iLevel].p[j]) {
        fLevel = deexcitations[iLevel].final[j];
        break;
      }
    }
    if (fLevel < -2) {
      // Loss, end of cascade
      return;
    } else if (fLevel == -2) {
      // Penning ionisation
      newDxcProd.energy = deexcitations[iLevel].energy - minIonPot;
      newDxcProd.type = -1;
      ++nPenning;
    } else if (fLevel == -1) {
      // Radiative decay to ground state
      newDxcProd.energy = deexcitations[iLevel].energy;
      newDxcProd.type = 1;
    } else {
      // Transition to energetically higher level (e. g. excimer)
      if (deexcitations[iLevel].energy < deexcitations[fLevel].energy) {
        iLevel = fLevel;
        continue;
      }
      // Radiative transition 
      newDxcProd.energy = deexcitations[iLevel].energy - 
                          deexcitations[fLevel].energy;
      newDxcProd.type = 1;
    }
    dxcProducts.push_back(newDxcProd);
    ++nDeexcitationProducts;
    iLevel = fLevel;
  }

}

bool
MediumMagboltz86::ComputePhotonCollisionTable() {

  OpticalData data;
  double cs;
  double eta;
  std::string gasname;

  const double density = LoschmidtNumber * (pressure / AtmosphericPressure) * 
                         (273.15 / temperature);

  // Reset the collision rate arrays
  cfTotGamma.resize(nEnergyStepsGamma);
  for (int j = nEnergyStepsGamma; j--;) {
    cfTotGamma[j] = 0.;
    for (int i = nMaxPhotonLevels; i--;) cfGamma[j][i] = 0.;
  }

  nPhotonTerms = 0;
  for (int i = 0; i < nComponents; ++i) {
    const double prefactor = density * SpeedOfLight * fraction[i];
    GetGasName(gas[i], gasname);
    if (!data.SetMaterial(gasname)) return false;
    for (int j = 0; j < nEnergyStepsGamma; ++j) {
      // Retrieve total photoabsorption cross-section and ionisation yield
      data.GetPhotoabsorptionCrossSection(j * eStepGamma, cs);
      data.GetPhotoionisationYield(j * eStepGamma, eta);
      cfTotGamma[j] += cs * prefactor;
      // Ionisation
      cfGamma[j][nPhotonTerms] = cs * prefactor * eta;
      csTypeGamma[nPhotonTerms] = i * 3 + 1;
      // "Neutral" absorption
      cfGamma[j][nPhotonTerms + 1] = cs * prefactor * (1. - eta);
      csTypeGamma[nPhotonTerms + 1] = i * 3 + 2;
    }
    nPhotonTerms += 2;
  }
  
  if (useCsOutput) {
    std::ofstream csfile;
    csfile.open("csgamma.txt", std::ios::out);
    for (int j = 0; j < nEnergyStepsGamma; ++j) {
      csfile << j * eStepGamma << "  ";
      for (int i = 0; i < nPhotonTerms; ++i) csfile << cfGamma[j][i] << "  ";
      csfile << std::endl;
    }
    csfile.close();
  }

  // Normalise the collision rates
  for (int j = 0; j < nEnergyStepsGamma; ++j) {
    if (cfTotGamma[j] <= 0.) continue;
    for (int i = 0; i < nPhotonTerms; ++i) {
      cfGamma[j][i] /= cfTotGamma[j];
      if (i > 0) cfGamma[j][i] += cfGamma[j][i - 1];
    }
  }

  return true;

}

void
MediumMagboltz86::RunMagboltz(const double e, 
                              const double bmag, const double btheta,
                              const int ncoll, bool verbose,
                              double& vx, double& vy, double& vz,
                              double& dl, double& dt,
                              double& alpha, double& eta) {

  // Set input parameters in Magboltz common blocks
  inpt_.nGas = nComponents;
  inpt_.nStep = nEnergySteps;

  inpt_.tempc = temperature - 273.15;
  inpt_.torr = pressure;
  inpt_.nAniso = 2;
  inpt_.ipen = 0;
  setp_.nmax = ncoll;

  setp_.efield = e;
  bfld_.bmag = bmag;
  bfld_.btheta = btheta;

  gasn_.ngasn[0] = gas[0]; gasn_.ngasn[1] = gas[1]; gasn_.ngasn[2] = gas[2];
  gasn_.ngasn[3] = gas[3]; gasn_.ngasn[4] = gas[4]; gasn_.ngasn[5] = gas[5];
  ratio_.frac[0] = 100 * fraction[0]; ratio_.frac[1] = 100 * fraction[1];
  ratio_.frac[2] = 100 * fraction[2]; ratio_.frac[3] = 100 * fraction[3];
  ratio_.frac[4] = 100 * fraction[4]; ratio_.frac[5] = 100 * fraction[5];

  // Call Magboltz internal setup routine
  setup1_();

  // Calculate final energy
  inpt_.efinal = 0.5;
  // If E/p > 15 start at 8 eV 
  if (e * temperature / (293.15 * pressure) > 15) {
    inpt_.efinal = 8.;
  }
  setp_.estart = inpt_.efinal / 50.;

  long long ielow = 1;
  while (ielow == 1) {
    mixer_();
    // Loop to calculate final energy
    if (bmag == 0. || btheta == 0. || fabs(btheta) == 180.) {
      elimit_(&ielow);
    } else if (btheta == 90.) {
      elimitb_(&ielow);
    } else {
      elimitc_(&ielow);
    }
    if (ielow == 1) {
      inpt_.efinal *= sqrt(2.);
      setp_.estart = inpt_.efinal / 50.;
    }
  }

  if (verbose) prnter_();
  
  if (bmag == 0.) {
    monte_();
  } else if (btheta == 0. || btheta == 180.) {
    montea_();
  } else if (btheta == 90.) {
    monteb_();
  } else {
    montec_();
  }
  if (verbose) output_();

  // If attachment or ionisation rate is greater than sstmin,
  // then include spatial gradients in the solution.
  const double sstmin = 30.;
  double alpp = ctowns_.alpha * 760. * temperature / (pressure * 293.15);
  double attp = ctowns_.att   * 760. * temperature / (pressure * 293.15);
  if (fabs(alpp - attp) > sstmin || alpp > sstmin || attp > sstmin) {
    if (bmag == 0.) {
      alpcalc_();
    } else if (btheta == 0. || btheta == 180.) {
      alpclca_();
    } else if (btheta == 90.) {
      alpclcb_();
    } else {
      alpclcc_();
    }
  }
  if (verbose) output2_();

  vx = vel_.wx * 1.e-9;
  vy = vel_.wy * 1.e-9;
  vz = vel_.wz * 1.e-9;

  dt = sqrt(0.2 * difvel_.diftr / vz) * 1.e-4;
  dl = sqrt(0.2 * difvel_.difln / vz) * 1.e-4;
 
  alpha = ctowns_.alpha;
  eta   = ctowns_.att;

}

}
