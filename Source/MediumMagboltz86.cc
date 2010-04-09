#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

#include "MediumMagboltz86.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"
#include "OpticalData.hh"

namespace Garfield {

MediumMagboltz86::MediumMagboltz86() :
  Medium(), 
  eFinal(40.), eStep(eFinal / nEnergySteps), adjust(true), csoutput(false), 
  nTerms(0), anisotropic(true), 
  penning(false), rPenning(0.), nPenning(0), deexcitation(false),
  eFinalGamma(20.), eStepGamma(eFinalGamma / nEnergyStepsGamma) {
  
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

  if (penning) {
    std::cerr << "MediumMagboltz86::EnableDeexcitation:" << std::endl;
    std::cerr << "    Penning transfer will be switched off." << std::endl;
  }
  penning = false;
  deexcitation = true;
  isChanged = true;

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
  if (deexcitation) {
    std::cerr << "MediumMagboltz86::EnablePenningTransfer:" << std::endl;
    std::cerr << "    Deexcitation handling will be switched off." 
              << std::endl;
  }
  penning = true;
  
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
  if (e > eFinal && adjust) {    
    std::cerr << "MediumMagboltz86::GetElectronCollisionRate:" << std::endl;
    std::cerr << "    Collision rate at " << e 
              << " eV is not included in the current table." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxElectronEnergy(1.05 * e);    
  }
    
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the collision rates table."
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

  if (e > eFinal && adjust) {
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
      std::cerr << "MediumMagboltz86: Error calculating" 
                << " the collision rates table." << std::endl;
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
  esec = 0.;
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
  } else if (type == 4 && deexcitation && fDeexcitation[level] > 0.) {
    // Determine the de-excitation time
    s = -log(RndmUniformPos()) / fDeexcitation[level];
    // Sample the de-excitation process
    r = RndmUniform();
    if (r < fRadiative[level]) {
      // Photon emission
      esec = - wSplit[level];
    } else if (r < fCollIon[level]) {
      // Penning ionisation
      esec = RndmUniform() * (energyLoss[level] * rgas[level] - minIonPot);
      if (esec <= 0) esec = 1.e-20;
      ++nPenning;
    }
  } else if (type == 4 && penning) {
    if (energyLoss[level] * rgas[level] > minIonPot && 
        RndmUniform() < rPenning) {
      esec = RndmUniform() * (energyLoss[level] * rgas[level] - minIonPot);
      if (esec <= 0) esec = 1.e-20;
      type = 1;
    }
  }

  // Make sure the energy loss is smaller than the energy
  if (e < loss) loss = e - 0.0001;
  
  // Determine the scattering angle
  double ctheta0;
  if (anisotropic) {
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

  if (debug && type == 4 && level < 47) {
    std::cout << "MediumMagboltz86::GetElectronCollision:" << std::endl;
    std::cout << "    Energy:      " << e << std::endl;
    std::cout << "    Level:       " << level << std::endl;
    std::cout << "    Type:        " << type << std::endl;
  }
  return true;

}

double 
MediumMagboltz86::GetPhotonCollisionRate(const double e) {

  if (e <= 0.) {
    std::cerr << "MediumMagboltz86: Photon energy must be greater than zero."
              << std::endl;
    return cfTotGamma[0];
  }
  if (e > eFinalGamma && adjust) {
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

  if (e > eFinalGamma && adjust) {
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
  
  if (debug) {
    std::cout << "MediumMagboltz86::GetPhotonCollision:" << std::endl;
    std::cout << "    Energy:      " << e << std::endl;
    std::cout << "    Gas:         " << ngas << std::endl;
    std::cout << "    Level:       " << level << std::endl;
    std::cout << "    Type:        " << type << std::endl;
  }
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

bool 
MediumMagboltz86::GetGasNumber(std::string gasname, int& number) const {

  // Convert to upper-case
  for (unsigned int i = 0; i < gasname.length(); ++i) {
    gasname[i] = toupper(gasname[i]);
  }
  
  if (gasname == "") {
    number = 0; return false;
  }
  
  // CF4
  if (gasname == "CF4" || gasname == "FREON" || 
      gasname == "FREON-14" || gasname == "TETRAFLUOROMETHANE") {
    number = 1; return true;
  }
  // Argon
  if (gasname == "AR" || gasname == "ARGON") {
    number = 2; return true;
  }
  // Helium 4
  if (gasname == "HE" || gasname == "HELIUM" || gasname == "HE-4" || 
      gasname == "HELIUM-4") {
    number = 3; return true;
  }
  // Helium 3
  if (gasname == "HE-3" || gasname == "HELIUM-3") {
    number = 4; return true;
  }
  // Neon
  if (gasname == "NE" || gasname == "NEON") {
    number = 5; return true;
  }
  // Krypton
  if (gasname == "KR" || gasname == "KRYPTON") {
    number = 6; return true;
  }
  // Xenon
  if (gasname == "XE" || gasname == "XENON") {
    number = 7; return true;
  }
  // Methane
  if (gasname == "CH4" || gasname == "METHANE" ) {
    number = 8; return true;
  }
  // Ethane
  if (gasname == "C2H6" || gasname == "ETHANE") {
    number = 9; return true;
  }
  // Propane
  if (gasname == "C3H8" || gasname == "PROPANE") {
    number = 10; return true;
  }
  // Isobutane
  if (gasname == "C4H10" || gasname == "ISOBUTANE" || gasname == "ISO" || 
      gasname == "IC4H10" || gasname == "ISO-C4H10" || gasname == "ISOC4H10") {
    number = 11; return true;
  }
  // CO2 (isotropic)
  if (gasname == "CO2" || gasname == "CARBON-DIOXIDE") {
    number = 12; return true;
  }
  // Neopentane
  if (gasname == "NEOPENTANE" || gasname == "NEO-PENTANE" || 
      gasname == "NEO-C5H12" || gasname == "NEOC5H12" || gasname == "C5H12") {
    number = 13; return true;
  }
  // Water
  if (gasname == "H2O" || gasname == "WATER" || gasname == "WATER-VAPOUR") {
    number = 14; return true;
  }
  // Oxygen
  if (gasname == "O2" || gasname == "OXYGEN") {
    number = 15; return true;
  }
  // Nitrogen
  if (gasname == "N2" || gasname == "NITROGEN" || 
      gasname == "NITROGEN-ISOTROPIC" || gasname == "N2-ISOTROPIC") {
    number = 16; return true;
  }
  // Nitric oxide (NO)
  if (gasname == "NO" || gasname == "NITRIC-OXIDE" || 
      gasname == "NITROGEN-MONOXIDE") {
    number = 17; return true;
  }
  // Nitrous oxide (N2O)
  if (gasname == "N2O" || gasname == "NITROUS-OXIDE" || 
      gasname == "DINITROGEN-MONOXIDE" || gasname == "LAUGHING-GAS") {
    number = 18; return true;
  }
  // Ethene (C2H4)
  if (gasname == "C2H4" || gasname == "ETHENE" || gasname == "ETHYLENE") {
    number = 19; return true;
  }
  // Acetylene (C2H2)
  if (gasname == "C2H2" || gasname == "ACETYL" || 
      gasname == "ACETYLENE" || gasname == "ETHYNE") {
    number = 20; return true;
  }
  // Hydrogen
  if (gasname == "H2" || gasname == "HYDROGEN") {
    number = 21; return true;
  }
  // Deuterium
  if (gasname == "D2" || gasname == "DEUTERIUM") {
    number = 22; return true;
  }
  // Carbon monoxide (CO)
  if (gasname == "CO" || gasname == "CARBON-MONOXIDE") {
    number = 23; return true;
  }
  // Methylal (dimethoxymethane, CH3-O-CH2-O-CH3, "hot" version)
  if (gasname == "METHYLAL" || gasname == "METHYLAL-HOT" || 
      gasname == "DIMETHOXYMETHANE" || gasname == "DMM" || 
      gasname == "C3H8O2") {
    number = 24; return true;
  }
  // DME
  if (gasname == "DME" || gasname == "DIMETHYL-ETHER" || 
      gasname == "METHOXYMETHANE" || 
      gasname == "METHYL-ETHER" || gasname == "WOOD-ETHER" || 
      gasname == "C2H6O") {
    number = 25; return true;
  }
  // Reid step
  if (gasname == "REID-STEP") {
    number = 26; return true;
  }
  // Maxwell model
  if (gasname == "MAXWELL-MODEL") {
    number = 27; return true;
  }
  // Reid ramp
  if (gasname == "REID-RAMP") {
    number = 28; return true;
  }
  // C2F6
  if (gasname == "C2F6" || gasname == "FREON-116" || gasname == "ZYRON-116" || 
      gasname == "ZYRON-116-N5" || gasname == "HEXAFLUOROETHANE") {
    number = 29; return true;
  }
  // SF6
  if (gasname == "SF6" || gasname == "SULPHUR-HEXAFLUORIDE" || 
      gasname == "SULFUR-HEXAFLUORIDE") {
    number = 30; return true;
  }
  // NH3
  if (gasname == "NH3" || gasname == "AMMONIA") {
    number = 31; return true;
  }
  // Propene
  if (gasname == "C3H6" || gasname == "PROPENE" || gasname == "PROPYLENE") {
    number = 32; return true;
  }
  // Cyclopropane
  if (gasname == "C-PROPANE" || gasname == "CYCLO-PROPANE" || 
      gasname == "CYCLOPROPANE" || gasname == "C-C3H6" || 
      gasname == "CYCLO-C3H6") {
    number = 33; return true;
  }
  // Methanol
  if (gasname == "METHANOL" || gasname == "METHYL-ALCOHOL" || 
      gasname == "WOOD-ALCOHOL" || gasname == "CH3OH") {
    number = 34; return true;
  }
  // Ethanol
  if (gasname == "ETHANOL" || gasname == "ETHYL-ALCOHOL" || 
      gasname == "GRAIN-ALCOHOL" || gasname == "C2H5OH") {
    number = 35; return true;
  }
  // Propanol
  if (gasname == "PROPANOL" || gasname == "2-PROPANOL" || 
      gasname == "ISO-PROPANOL" || gasname == "ISOPROPANOL" || 
      gasname == "ISOPROPYL" || 
      gasname == "ISOPROPYL-ALCOHOL" || gasname == "C3H7OH") {
    number = 36; return true;
  }
  // Cesium / Caesium.
  if (gasname == "CS" || gasname == "CESIUM" || gasname == "CAESIUM") {
    number = 37; return true;
  }
  // Fluorine
  if (gasname == "F2" || gasname == "FLUOR" || gasname == "FLUORINE") {
    number = 38; return true;
  }
  if (gasname == "CS2" || gasname == "CARBON-DISULPHIDE" || 
      gasname == "CARBON-DISULFIDE") {
    number = 39; return true;
  }
  // COS
  if (gasname == "COS" || gasname == "CARBONYL-SULPHIDE" || 
      gasname == "CARBONYL-SULFIDE") {
    number = 40; return true;
  }
  // Deuterated methane
  if (gasname == "DEUT-METHANE" || gasname == "DEUTERIUM-METHANE" || 
      gasname == "DEUTERATED-METHANE" || gasname == "CD4") {
    number = 41; return true;
  }
  // BF3
  if (gasname == "BF3" || gasname == "BORON-TRIFLUORIDE") {
    number = 42; return true;
  }
  // C2HF5 and C2H2F4.
  if (gasname == "C2HF5" || gasname == "C2H2F4" || gasname == "C2F5H" || 
      gasname == "C2F4H2" || gasname == "FREON-134" || 
      gasname == "FREON-134-A" || gasname == "FREON-125" ||
      gasname == "ZYRON-125" || gasname == "TETRAFLUOROETHANE" || 
      gasname == "PENTAFLUOROETHANE") {
    number = 43; return true;
  }
  // CHF3
  if (gasname == "CHF3" || gasname == "FREON-23" || 
      gasname == "TRIFLUOROMETHANE") {
    number = 50; return true;
  }
  // CF3Br
  if (gasname == "CF3BR" || gasname == "TRIFLUOROBROMOMETHANE" || 
      gasname == "HALON-1301" || gasname == "FREON-13B1") {
    number = 51; return true;
  }
  // C3F8
  if (gasname == "C3F8" || gasname == "OCTAFLUOROPROPANE" || 
      gasname == "R218" || gasname == "FREON-218" || 
      gasname == "PERFLUOROPROPANE" || 
      gasname == "RC-218" || gasname == "PFC-218") {
    number = 52; return true;
  }
  // Ozone
  if (gasname == "OZONE" || gasname == "O3") {
    number = 53; return true;
  }
  // Mercury
  if (gasname == "MERCURY" || gasname == "HG" || gasname == "HG2") {
    number = 54; return true;
  }
  // H2S
  if (gasname == "H2S" || gasname == "HYDROGEN-SULPHIDE" || 
      gasname == "HYDROGEN-SULFIDE" ||
      gasname == "HEPATIC-ACID" || gasname == "SEWER-GAS" || 
      gasname == "SULFUR-HYDRIDE" ||
      gasname == "DIHYDROGEN-MONOSULFIDE" || 
      gasname == "DIHYDROGEN-MONOSULPHIDE" ||
      gasname == "SULPHUR-HYDRIDE" || gasname == "STINK-DAMP" || 
      gasname == "SULFURETED-HYDROGEN") {
    number = 55; return true;
  }
  // n-butane
  if (gasname == "N-BUTANE" || gasname == "N-C4H10") {
    number = 56; return true;
  }
  // n-pentane
  if (gasname == "N-PENTANE" || gasname == "N-C5H12") {
    number = 57; return true;
  }
  // Nitrogen
  if (gasname == "NI" || gasname == "NITROGEN" || 
      gasname == "NI-ANISOTROPIC" || 
      gasname == "NITROGEN-ANISOTROPIC" ||
      gasname == "N2" || gasname == "N2-ANISOTROPIC") {
    number = 58; return true;
  }
  // Germane, GeH4
  if (gasname == "GERMANE" || gasname == "GERM" || 
      gasname == "GERMANIUM-HYDRIDE" || 
      gasname == "GERMANIUM-TETRAHYDRIDE" ||
      gasname == "GERMANOMETHANE" || gasname == "MONOGERMANE" || 
      gasname == "GEH4") {
    number = 59; return true;
  }
  // Silane, SiH4
  if (gasname == "SILANE" || gasname == "SIL" || 
      gasname == "SILICON-HYDRIDE" || 
      gasname == "SILICON-TETRAHYDRIDE" ||
      gasname == "SILICANE" || gasname == "MONOSILANE" || gasname == "SIH4") {
    number = 60; return true;
  }
  
  std::cerr << "MediumMagboltz86::GetGasNumber():" << std::endl;
  std::cerr << "    Gas " << gasname << " is not defined." << std::endl;
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
  const double density = LoschmidtNumber * (pressure / 760.) * 
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
  for (int i = nMaxLevels; i--;) {
    fDeexcitation[i] = 0.;
    fRadiative[i] = 0.;
    fCollIon[i] = 0.;
    fCollLoss[i] = 0.;
  }

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
  if (csoutput) outfile.open("cs.txt", std::ios::out);
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
      if (description[np][1] == 'E' &&
          description[np][2] == 'X') {
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
      if (csoutput) {
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
        if (csoutput) outfile << qIn[iE][j] << "  ";
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
      if (csoutput) outfile << std::endl;
    }
  }
  if (csoutput) outfile.close();

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

  if (deexcitation) ComputeDeexcitationTable();
  if (!ComputePhotonCollisionTable()) {
     std::cerr << "MediumMagboltz86: " << std::endl;
     std::cerr << "    Photon collision rates could not be calculated." 
               << std::endl;
     if (deexcitation)return false;
  }

  return true;

}

void 
MediumMagboltz86::ComputeAngularCut(double parIn, float& cut, double &parOut) {

  // Set cuts on angular distribution and
  // renormalise forward scattering probability

  cut = 1.;
  parOut = parIn;
  if (parIn <= 1.) return;
  const double rads = 2. / Pi;
  const double cns = parIn - 0.5;
  const double thetac = asin(2. * sqrt(cns - cns * cns));
  const double fac = (1 - cos(thetac)) / (sin(thetac) * sin(thetac));
  parOut = cns * fac + 0.5;
  cut = thetac * rads;
  
}

void
MediumMagboltz86::ComputeDeexcitationTable() {

  // Fill radiative deexcitation rates
  for (int i = 0; i < nTerms; ++i) {
    fRadiative[i] = fCollIon[i] = fCollLoss[i] = 0.;
    if (csType[i] % 6 != 4) continue;
    wSplit[i] = energyLoss[i] * rgas[i];
    switch (gas[int(csType[i] / 6)]) {
      case 2:
        // Argon
        std::string level = "       ";
        for (int j = 0; j < 7; ++j) level[j] = description[i][5 + j];
        // Metastable 3p54s levels
        if (level == "1S5    ") {
          // Katori, Shimizu (1993)
          fRadiative[i] = 1. / 38.e9; continue;
        } else if (level == "1S3    ") {
          // Small-Warren (1975)
          fRadiative[i] = 1. / 45.e9;
        // Radiative 3p54s levels
        } else if (level == "1S4    ") {
          fRadiative[i] = 1. / 8.6; continue;
        } else if (level == "1S2    ") {
          fRadiative[i] = 1. / 2.2; continue;
        // 3p54p levels
        // Lifetimes from Wiese et al. (1989)
        // These levels decay to the 4s state
        // Photon energies from NIST database
        } else if (level == "2P10   ") {
          fRadiative[i] = 1. / 40.5; wSplit[i] = 1.359; continue;
        } else if (level == "2P9    ") {
          fRadiative[i] = 1. / 30.7; wSplit[i] = 1.527; continue;
        } else if (level == "2P8    ") {
          fRadiative[i] = 1. / 30.6; wSplit[i] = 1.547; continue;
        } else if (level == "2P7    ") {
          fRadiative[i] = 1. / 30.2; wSplit[i] = 1.530; continue;
        } else if (level == "2P6    ") {
          fRadiative[i] = 1. / 29.4; wSplit[i] = 1.623; continue;
        } else if (level == "2P5    ") {
          fRadiative[i] = 1. / 24.4; wSplit[i] = 1.649; continue;
        } else if (level == "2P4    ") {
          fRadiative[i] = 1. / 29.3; wSplit[i] = 1.559; continue;
        } else if (level == "2P3    ") {
          fRadiative[i] = 1. / 29.0; wSplit[i] = 1.754; continue;
        } else if (level == "2P2    ") {
          fRadiative[i] = 1. / 28.3; wSplit[i] = 1.780; continue;
        } else if (level == "2P1    ") {
          fRadiative[i] = 1. / 21.7; wSplit[i] = 1.652; continue;
        // 3p53d levels
        // Lifetimes from Gruzdev, Loginov (1975)
        // Most levels decay to 4p
        // Photon energies from NIST database
        } else if (level == "3D6    ") {
          fRadiative[i] = 1. / 54.2; wSplit[i] = 0.938; continue;
        } else if (level == "3D5    ") {
          fRadiative[i] = 1. / 40.8; continue;
        } else if (level == "3D4!   ") {
          // No line found, assume equal to 3d4
          fRadiative[i] = 1. / 52.0; wSplit[i] = 0.918; continue;
        } else if (level == "3D4    ") {
          fRadiative[i] = 1. / 50.8; wSplit[i] = 0.918; continue;
        } else if (level == "3D3    ") {
          fRadiative[i] = 1. / 3.5; wSplit[i] = 0.732; continue;
        } else if (level == "3D2    ") {
          fRadiative[i] = 1. / 9.0; continue;
        } else if (level == "3D1!!  ") {
          fRadiative[i] = 1. / 49.9; wSplit[i] = 0.910;  continue;
        } else if (level == "3D1!   ") {
          fRadiative[i] = 1. / 49.0; wSplit[i] = 1.023; continue;
        } else if (level == "3S1!!!!") {
          fRadiative[i] = 1. / 49.9; wSplit[i] = 0.931; continue;
        } else if (level == "3S1!!! ") {
          fRadiative[i] = 1. / 49.7; wSplit[i] = 0.934; continue;
        } else if (level == "3S1!!  ") {
          fRadiative[i] = 1. / 48.3; wSplit[i] = 1.062; continue;
        } else if (level == "3S1!   ") {
          fRadiative[i] = 1. / 3.36; continue;
        // 3p55s levels
        // Lifetimes from Gruzdev, Loginov (1975)
        // Photon energies from NIST database
        } else if (level == "2S5    ") {
          fRadiative[i] = 1. / 42.1; wSplit[i] = 1.161; continue;
        } else if (level == "2S4    ") {
          fRadiative[i] = 1. / 4.74; continue;
        } else if (level == "2S3    ") {
          fRadiative[i] = 1. / 43.9; wSplit[i] = 1.334; continue;
        } else if (level == "2S2    ") {
          fRadiative[i] = 1. / 3.2; continue;       
        // 3p54d levels
        } else if (level == "4D5    ") {
          fRadiative[i] = 1. / 113.; continue;
        } else if (level == "4D2    ") {
          fRadiative[i] = 1. / 10.; continue;
        } else if (level == "4S1!   ") {
          fRadiative[i] = 1. / 3.78; continue;
        // 3p56s levels
        } else if (level == "3S4    ") {
          fRadiative[i] = 1. / 5.73; continue;
        } else if (level == "3S2    ") {
          fRadiative[i] = 1. / 32.6; continue;
        // 3p55d levels
        } else if (level == "5D5    ") {
          fRadiative[i] = 1. / 111.; continue;
        } else if (level == "5D2    ") {
          fRadiative[i] = 1. / 5.42; continue;
        } else if (level == "5S1!   ") {
          fRadiative[i] = 1. / 3.69; continue;
        // 3p57s levels
        } else if (level == "4S4    ") {
          fRadiative[i] = 1. / 15.1; continue;
        } else if (level == "4S2    ") {
          fRadiative[i] = 1. / 15.7; continue;
        // Other levels
        } else if (level == "6D5    ") {
          // Zurro et al. (1973)
          fRadiative[i] = 1. / 104.0; continue;
        } else if (level == "5S4    ") {
          // Afanaseva, Gruzdev (1975)
          fRadiative[i] = 1. / 221.6; continue;
        } else if (level == "6D2    ") {
          // Zurro et al. (1973)
          fRadiative[i] = 1. / 300; continue;
        } else if (level == "HIGH   ") {
          // Fantasy value
          fRadiative[i] = 1. / 500.; continue;
        } 
        break;
    }
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
    double fB = (3.842488 / 22.121274);
    fB *= pressure / 760.;
    if (gas[0] == 2) {
      fB *= fraction[1];
      ar = 0;
    } else {
      fB *= fraction[0];
      ar = 1;
    }
    for (int j = nTerms; j--;) {
      if (int(csType[j] / 6) == ar && csType[j] % 6 == 4) {
        if (energyLoss[j] * rgas[j] < minIonPot) continue;
        std::string level = "       ";
        for (int k = 0; k < 7; ++k) level[k] = description[j][5 + k];
        // 3p54p levels
        // Average lifetime assumed to be 30 ns
        if (level == "2P10   " || level == "2P9    " ||
                   level == "2P8    " || level == "2P7    " ||
                   level == "2P6    " || level == "2P5    " ||
                   level == "2P4    " || level == "2P3    " ||
                   level == "2P2    " || level == "2P1    ") {
          fCollIon[j] = fB / 30.;
        // 3p53d levels
        // Average lifetime assumed to be 40 ns
        } else if (level == "3D6    " || level == "3D5    " ||
                   level == "3D4!   " || level == "3D4    " ||
                   level == "3D3    " || level == "3D2    " ||
                   level == "3D1!!  " || level == "3D1!   " ||
                   level == "3S1!!!!" || level == "3S1!!! " ||
                   level == "3S1!!  " || level == "3S1!   ") { 
          fCollIon[j] = fB / 40.;
        // Higher levels
        } else {
          fCollIon[j] = fB * fRadiative[j];
        }
      }
    }    
  } else {
    std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl;
    std::cout << "    No data on collisional deexcitation present."
              << std::endl;
  }

  if (debug) std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" 
                       << std::endl; 
  for (int i = 0; i < nTerms; ++i) {
    fDeexcitation[i] = fRadiative[i] + fCollIon[i] + fCollLoss[i];
    if (fDeexcitation[i] > 0.) {
      if (debug) {
        std::string descr = "                              ";
        for (int j = 30; j--;) descr[j] = description[i][j];
        std::cout << descr << std::endl;
        std::cout << "    Deexcitation rate:     " 
                  << fDeexcitation[i] << " ns-1" << std::endl;
        if (fDeexcitation[i] > 0.) {
          std::cout << "    Radiative decay probability:  "
                    << fRadiative[i] / fDeexcitation[i] << std::endl;
          std::cout << "    Penning transfer probability: "
                    << fCollIon[i] / fDeexcitation[i] << std::endl;
          std::cout << "    Photon energy: " << wSplit[i] << " eV" 
                    << std::endl;
        }
      } 
      fRadiative[i] /= fDeexcitation[i];
      fCollIon[i] /= fDeexcitation[i];
      fCollLoss[i] /= fDeexcitation[i];
      fCollIon[i] += fRadiative[i];
      fCollLoss[i] += fCollIon[i];
    }
  }
  
}

bool
MediumMagboltz86::ComputePhotonCollisionTable() {

  OpticalData data;
  double cs;
  double eta;
  std::string gasname;

  const double density = LoschmidtNumber * (pressure / 760.) * 
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
  
  if (csoutput) {
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
                              const int ncoll, bool verbose) {

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

  double vx = vel_.wx * 1.e-9;
  double vy = vel_.wy * 1.e-9;
  double vz = vel_.wz * 1.e-9;

  double dt = sqrt(0.2 * difvel_.diftr / vz) * 1.e-4;
  double dl = sqrt(0.2 * difvel_.difln / vz) * 1.e-4;
 
  double alpha = ctowns_.alpha;
  double eta   = ctowns_.att;

  std::cout << "Drift velocity: " << vx << " " << vy << " " << vz << std::endl;
  std::cout << "Longitudinal diffusion: " << dl << std::endl;
  std::cout << "Transverse diffusion:   " << dt << std::endl;
  std::cout << "Townsend coefficient:   " << alpha << std::endl;
  std::cout << "Attachment coefficient: " << eta << std::endl;

}

}
