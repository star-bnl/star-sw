#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

#include "MediumMagboltz86.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumMagboltz86::MediumMagboltz86() :
  Medium(), 
  eFinal(40.), eStep(eFinal / nEnergySteps), adjust(true), 
  nTerms(0), anisotropic(true), deexcitation(false) {
  
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
  } else if (r >= cf[iE][nTerms - 1]) {
    level = nTerms - 1;
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
      esec = - energyLoss[level];
    } else if (r < fCollIon[level]) {
      // Penning ionisation
      esec = RndmUniform() * (energyLoss[level] - minIonPot);
      if (esec <= 0) esec = 1.e-20;
    }
  }

  bool penning = false;
  // TEMPORARY HANDLING OF PENNING TRANSFERS FOR AR/ISOBUTANE
  if (penning && level < 47 && type == 4) {
    // Excited level of argon
    if (RndmUniform() < 0.4) {
      // Penning ionisation
      type = 1;
      esec = 1.;
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

  // Increase the collision counters
  ++nCollisions[type];
  ++nCollisionsDetailed[level];
  
  if (debug) {
    std::cout << "MediumMagboltz86::GetElectronCollision:" << std::endl;
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
  
}

int 
MediumMagboltz86::GetNumberOfCollisions() const {

  return nCollisions[0] + nCollisions[1] + nCollisions[2] + 
         nCollisions[3] + nCollisions[4] + nCollisions[5];
  
}

int 
MediumMagboltz86::GetNumberOfCollisions(int& nElastic, 
                                        int& nIonisation, 
                                        int& nAttachment, 
                                        int& nInelastic,
                                        int& nExcitation,
                                        int& nSuperelastic) const {

  nElastic = nCollisions[0]; 
  nIonisation = nCollisions[1];
  nAttachment = nCollisions[2];  
  nInelastic = nCollisions[3];
  nExcitation = nCollisions[4]; 
  nSuperelastic = nCollisions[5];  
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
MediumMagboltz86::GetNumberOfCollisions(const int level) const {

  if (level < 0 || level >= nTerms) {
    std::cerr << "MediumMagboltz86::GetNumberOfCollisions:" << std::endl;
    std::cerr << "    Requested cross-section term (" 
              << level << ") does not exist." << std::endl;
    return 0;
  }
  return nCollisionsDetailed[level];

}  


bool 
MediumMagboltz86::GetGasNumber(std::string gas, int& number) const {

  // Convert to upper-case
  for (unsigned int i = 0; i < gas.length(); ++i) {
    gas[i] = toupper(gas[i]);
  }
  
  if (gas == "") {
    number = 0; return false;
  }
  
  // CF4
  if (gas == "CF4" || gas == "FREON" || 
      gas == "FREON-14" || gas == "TETRAFLUOROMETHANE") {
    number = 1; return true;
  }
  // Argon
  if (gas == "AR" || gas == "ARGON") {
    number = 2; return true;
  }
  // Helium 4
  if (gas == "HE" || gas == "HELIUM" || gas == "HE-4" || gas == "HELIUM-4") {
    number = 3; return true;
  }
  // Helium 3
  if (gas == "HE-3" || gas == "HELIUM-3") {
    number = 4; return true;
  }
  // Neon
  if (gas == "NE" || gas == "NEON") {
    number = 5; return true;
  }
  // Krypton
  if (gas == "KR" || gas == "KRYPTON") {
    number = 6; return true;
  }
  // Xenon
  if (gas == "XE" || gas == "XENON") {
    number = 7; return true;
  }
  // Methane
  if (gas == "CH4" || gas == "METHANE" ) {
    number = 8; return true;
  }
  // Ethane
  if (gas == "C2H6" || gas == "ETHANE") {
    number = 9; return true;
  }
  // Propane
  if (gas == "C3H8" || gas == "PROPANE") {
    number = 10; return true;
  }
  // Isobutane
  if (gas == "C4H10" || gas == "ISOBUTANE" || gas == "ISO" || 
      gas == "IC4H10" || gas == "ISO-C4H10" || gas == "ISOC4H10") {
    number = 11; return true;
  }
  // CO2 (isotropic)
  if (gas == "CO2" || gas == "CARBON-DIOXIDE") {
    number = 12; return true;
  }
  // Neopentane
  if (gas == "NEOPENTANE" || gas == "NEO-PENTANE" || 
      gas == "NEO-C5H12" || gas == "NEOC5H12" || gas == "C5H12") {
    number = 13; return true;
  }
  // Water
  if (gas == "H2O" || gas == "WATER" || gas == "WATER-VAPOUR") {
    number = 14; return true;
  }
  // Oxygen
  if (gas == "O2" || gas == "OXYGEN") {
    number = 15; return true;
  }
  // Nitrogen
  if (gas == "N2" || gas == "NITROGEN" || 
      gas == "NITROGEN-ISOTROPIC" || gas == "N2-ISOTROPIC") {
    number = 16; return true;
  }
  // Nitric oxide (NO)
  if (gas == "NO" || gas == "NITRIC-OXIDE" || gas == "NITROGEN-MONOXIDE") {
    number = 17; return true;
  }
  // Nitrous oxide (N2O)
  if (gas == "N2O" || gas == "NITROUS-OXIDE" || 
      gas == "DINITROGEN-MONOXIDE" || gas == "LAUGHING-GAS") {
    number = 18; return true;
  }
  // Ethene (C2H4)
  if (gas == "C2H4" || gas == "ETHENE" || gas == "ETHYLENE") {
    number = 19; return true;
  }
  // Acetylene (C2H2)
  if (gas == "C2H2" || gas == "ACETYL" || 
      gas == "ACETYLENE" || gas == "ETHYNE") {
    number = 20; return true;
  }
  // Hydrogen
  if (gas == "H2" || gas == "HYDROGEN") {
    number = 21; return true;
  }
  // Deuterium
  if (gas == "D2" || gas == "DEUTERIUM") {
    number = 22; return true;
  }
  // Carbon monoxide (CO)
  if (gas == "CO" || gas == "CARBON-MONOXIDE") {
    number = 23; return true;
  }
  // Methylal (dimethoxymethane, CH3-O-CH2-O-CH3, "hot" version)
  if (gas == "METHYLAL" || gas == "METHYLAL-HOT" || 
      gas == "DIMETHOXYMETHANE" || gas == "DMM" || gas == "C3H8O2") {
    number = 24; return true;
  }
  // DME
  if (gas == "DME" || gas == "DIMETHYL-ETHER" || gas == "METHOXYMETHANE" || 
      gas == "METHYL-ETHER" || gas == "WOOD-ETHER" || gas == "C2H6O") {
    number = 25; return true;
  }
  // Reid step
  if (gas == "REID-STEP") {
    number = 26; return true;
  }
  // Maxwell model
  if (gas == "MAXWELL-MODEL") {
    number = 27; return true;
  }
  // Reid ramp
  if (gas == "REID-RAMP") {
    number = 28; return true;
  }
  // C2F6
  if (gas == "C2F6" || gas == "FREON-116" || gas == "ZYRON-116" || 
      gas == "ZYRON-116-N5" || gas == "HEXAFLUOROETHANE") {
    number = 29; return true;
  }
  // SF6
  if (gas == "SF6" || gas == "SULPHUR-HEXAFLUORIDE" || 
      gas == "SULFUR-HEXAFLUORIDE") {
    number = 30; return true;
  }
  // NH3
  if (gas == "NH3" || gas == "AMMONIA") {
    number = 31; return true;
  }
  // Propene
  if (gas == "C3H6" || gas == "PROPENE" || gas == "PROPYLENE") {
    number = 32; return true;
  }
  // Cyclopropane
  if (gas == "C-PROPANE" || gas == "CYCLO-PROPANE" || 
      gas == "CYCLOPROPANE" || gas == "C-C3H6" || gas == "CYCLO-C3H6") {
    number = 33; return true;
  }
  // Methanol
  if (gas == "METHANOL" || gas == "METHYL-ALCOHOL" || 
      gas == "WOOD-ALCOHOL" || gas == "CH3OH") {
    number = 34; return true;
  }
  // Ethanol
  if (gas == "ETHANOL" || gas == "ETHYL-ALCOHOL" || gas == "GRAIN-ALCOHOL" || 
      gas == "C2H5OH") {
    number = 35; return true;
  }
  // Propanol
  if (gas == "PROPANOL" || gas == "2-PROPANOL" || gas == "ISO-PROPANOL" || 
      gas == "ISOPROPANOL" || gas == "ISOPROPYL" || 
      gas == "ISOPROPYL-ALCOHOL" || gas == "C3H7OH") {
    number = 36; return true;
  }
  // Cesium / Caesium.
  if (gas == "CS" || gas == "CESIUM" || gas == "CAESIUM") {
    number = 37; return true;
  }
  // Fluorine
  if (gas == "F2" || gas == "FLUOR" || gas == "FLUORINE") {
    number = 38; return true;
  }
  if (gas == "CS2" || gas == "CARBON-DISULPHIDE" || gas == "CARBON-DISULFIDE") {
    number = 39; return true;
  }
  // COS
  if (gas == "COS" || gas == "CARBONYL-SULPHIDE" || gas == "CARBONYL-SULFIDE") {
    number = 40; return true;
  }
  // Deuterated methane
  if (gas == "DEUT-METHANE" || gas == "DEUTERIUM-METHANE" || 
      gas == "DEUTERATED-METHANE" || gas == "CD4") {
    number = 41; return true;
  }
  // BF3
  if (gas == "BF3" || gas == "BORON-TRIFLUORIDE") {
    number = 42; return true;
  }
  // C2HF5 and C2H2F4.
  if (gas == "C2HF5" || gas == "C2H2F4" || gas == "C2F5H" || gas == "C2F4H2" || 
      gas == "FREON-134" || gas == "FREON-134-A" || gas == "FREON-125" ||
      gas == "ZYRON-125" || gas == "TETRAFLUOROETHANE" || 
      gas == "PENTAFLUOROETHANE") {
    number = 43; return true;
  }
  // CHF3
  if (gas == "CHF3" || gas == "FREON-23" || gas == "TRIFLUOROMETHANE") {
    number = 50; return true;
  }
  // CF3Br
  if (gas == "CF3BR" || gas == "TRIFLUOROBROMOMETHANE" || 
      gas == "HALON-1301" || gas == "FREON-13B1") {
    number = 51; return true;
  }
  // C3F8
  if (gas == "C3F8" || gas == "OCTAFLUOROPROPANE" || gas == "R218" || 
      gas == "FREON-218" || gas == "PERFLUOROPROPANE" || 
      gas == "RC-218" || gas == "PFC-218") {
    number = 52; return true;
  }
  // Ozone
  if (gas == "OZONE" || gas == "O3") {
    number = 53; return true;
  }
  // Mercury
  if (gas == "MERCURY" || gas == "HG" || gas == "HG2") {
    number = 54; return true;
  }
  // H2S
  if (gas == "H2S" || gas == "HYDROGEN-SULPHIDE" || gas == "HYDROGEN-SULFIDE" ||
      gas == "HEPATIC-ACID" || gas == "SEWER-GAS" || gas == "SULFUR-HYDRIDE" ||
      gas == "DIHYDROGEN-MONOSULFIDE" || gas == "DIHYDROGEN-MONOSULPHIDE" ||
      gas == "SULPHUR-HYDRIDE" || gas == "STINK-DAMP" || 
      gas == "SULFURETED-HYDROGEN") {
    number = 55; return true;
  }
  // n-butane
  if (gas == "N-BUTANE" || gas == "N-C4H10") {
    number = 56; return true;
  }
  // n-pentane
  if (gas == "N-PENTANE" || gas == "N-C5H12") {
    number = 57; return true;
  }
  // Nitrogen
  if (gas == "NI" || gas == "NITROGEN" || gas == "NI-ANISOTROPIC" || 
      gas == "NITROGEN-ANISOTROPIC" ||
      gas == "N2" || gas == "N2-ANISOTROPIC") {
    number = 58; return true;
  }
  // Germane, GeH4
  if (gas == "GERMANE" || gas == "GERM" || gas == "GERMANIUM-HYDRIDE" || 
      gas == "GERMANIUM-TETRAHYDRIDE" ||
      gas == "GERMANOMETHANE" || gas == "MONOGERMANE" || gas == "GEH4") {
    number = 59; return true;
  }
  // Silane, SiH4
  if (gas == "SILANE" || gas == "SIL" || gas == "SILICON-HYDRIDE" || 
      gas == "SILICON-TETRAHYDRIDE" ||
      gas == "SILICANE" || gas == "MONOSILANE" || gas == "SIH4") {
    number = 60; return true;
  }
  
  std::cerr << "MediumMagboltz86::GetGasNumber():" << std::endl;
  std::cerr << "    Gas " << gas << " is not defined." << std::endl;
  return false;
  
}

bool 
MediumMagboltz86::GetGasName(const int number, std::string& gas) const {
  
  switch (number) {
    case 0:
      gas = ""; return false; break;
    case 1:
      gas = "CF4"; break;
    case 2:
      gas = "Ar"; break;
    case 3:
      gas = "He"; break;
    case 4:
      gas = "He-3"; break;
    case 5:
      gas = "Ne"; break;
    case 6:
      gas = "Kr"; break;
    case 7:
      gas = "Xe"; break;
    case 8:
      gas = "CH4"; break;
    case 9:
      gas = "C2H6"; break;
    case 10:
      gas = "C3H8"; break;
    case 11:
      gas = "iC4H10"; break;
    case 12:
      gas = "CO2"; break;
    case 13:
      gas = "Neopentane"; break;
    case 14:
      gas = "H2O"; break;
    case 15:
      gas = "O2"; break;
    case 16:
      gas = "N2"; break;
    case 17:
      gas = "NO"; break;
    case 18:
      gas = "N2O"; break;
    case 19:
      gas = "C2H4"; break;
    case 20:
      gas = "C2H2"; break;
    case 21:
      gas = "H2"; break;
    case 22:
      gas = "D2"; break;
    case 23:
      gas = "CO"; break;
    case 24:
      gas = "Methylal"; break;
    case 25:
      gas = "DME"; break;
    case 26:
      gas = "Reid-Step"; break;
    case 27:
      gas = "Maxwell-Model"; break;
    case 28:
      gas = "Reid-Ramp"; break;
    case 29:
      gas = "C2F6"; break;
    case 30:
      gas = "SF6"; break;
    case 31:
      gas = "NH3"; break;
    case 32:
      gas = "C3H6"; break;
    case 33:
      gas = "Cyclopropane"; break;
    case 34:
      gas = "Methanol"; break;
    case 35:
      gas = "Ethanol"; break;
    case 36:
      gas = "Propanol"; break;
    case 37:
      gas = "Cs"; break;
    case 38:
      gas = "F2"; break;
    case 39:
      gas = "CS2"; break;
    case 40:
      gas = "COS"; break;
    case 41:
      gas = "CD4"; break;
    case 42:
      gas = "BF3"; break;
    case 43:
      gas = "C2HF5"; break;
    case 50:
      gas = "CHF3"; break;
    case 51:
      gas = "CF3Br"; break;
    case 52:
      gas = "C3F8"; break;
    case 53:
      gas = "O3"; break;
    case 54:
      gas = "Hg"; break;
    case 55:
      gas = "H2S"; break;
    case 56:
      gas = "n-C4H10"; break;
    case 57:
      gas = "n-C5H12"; break;
    case 58:
      gas = "N2"; break;
    case 59:
      gas = "GeH4"; break;
    case 60:
      gas = "SiH4"; break;
    default:
      gas = ""; return false; break;
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
  if (debug) outfile.open("cs.txt", std::ios::out);
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
      if (debug) {
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
        if (debug) outfile << qIn[iE][j] << "  ";
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
      if (debug) outfile << std::endl;
    }
  }
  if (debug) outfile.close();

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

  minIonPot = -1.;
  // Fill radiative deexcitation rates
  for (int i = 0; i < nTerms; ++i) {
    fRadiative[i] = 0.;
    fCollIon[i] = 0.;
    fCollLoss[i] = 0.;
    // Find the min. ionisation threshold in the mixture
    if (csType[i] % 6 == 1) {
      if (minIonPot <= 0.) {
        minIonPot = energyLoss[i];
      } else if (energyLoss[i] < minIonPot) {
        minIonPot = energyLoss[i];
      }
    }
    if (csType[i] % 6 != 4) continue;
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
        // Wiese et al. (1989)
        } else if (level == "2P10   ") {
          fRadiative[i] = 1. / 40.5; continue;
        } else if (level == "2P9    ") {
          fRadiative[i] = 1. / 30.7; continue;
        } else if (level == "2P8    ") {
          fRadiative[i] = 1. / 30.6; continue;
        } else if (level == "2P7    ") {
          fRadiative[i] = 1. / 30.2; continue;
        } else if (level == "2P6    ") {
          fRadiative[i] = 1. / 29.4; continue;
        } else if (level == "2P5    ") {
          fRadiative[i] = 1. / 24.4; continue;
        } else if (level == "2P4    ") {
          fRadiative[i] = 1. / 29.3; continue;
        } else if (level == "2P3    ") {
          fRadiative[i] = 1. / 29.0; continue;
        } else if (level == "2P2    ") {
          fRadiative[i] = 1. / 28.3; continue;
        } else if (level == "2P1    ") {
          fRadiative[i] = 1. / 21.7; continue;
        // 3p53d levels
        // Gruzdev, Loginov (1975)
        } else if (level == "3D6    ") {
          fRadiative[i] = 1. / 54.2; continue;
        } else if (level == "3D5    ") {
          fRadiative[i] = 1. / 40.8; continue;
        } else if (level == "3D4!   ") {
          fRadiative[i] = 1. / 52.0; continue;
        } else if (level == "3D4    ") {
          fRadiative[i] = 1. / 50.8; continue;
        } else if (level == "3D3    ") {
          fRadiative[i] = 1. / 3.5; continue;
        } else if (level == "3D2    ") {
          fRadiative[i] = 1. / 9.0; continue;
        } else if (level == "3D1!!  ") {
          fRadiative[i] = 1. / 49.9; continue;
        } else if (level == "3D1!   ") {
          fRadiative[i] = 1. / 49.0; continue;
        } else if (level == "3S1!!!!") {
          fRadiative[i] = 1. / 49.9; continue;
        } else if (level == "3S1!!! ") {
          fRadiative[i] = 1. / 49.7; continue;
        } else if (level == "3S1!!  ") {
          fRadiative[i] = 1. / 48.3; continue;
        } else if (level == "3S1!   ") {
          fRadiative[i] = 1. / 3.36; continue;
        // 3p55s levels
        // Gruzdev, Loginov (1975)
        } else if (level == "2S5    ") {
          fRadiative[i] = 1. / 42.1; continue;
        } else if (level == "2S4    ") {
          fRadiative[i] = 1. / 4.74; continue;
        } else if (level == "2S3    ") {
          fRadiative[i] = 1. / 43.9; continue;
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
    double fB = 3.842488;
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
        fCollIon[j] = fB;
      }
    }    
  } else {
    std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl;
    std::cout << "    No data on collisional deexcitation present."
              << std::endl;
  }

  if (debug) std::cout << "MediumMagboltz86::ComputeDeexcitationTable:" << std::endl; 
  for (int i = 0; i < nTerms; ++i) {
    fDeexcitation[i] = fRadiative[i] + fCollIon[i] + fCollLoss[i];
    if (fDeexcitation[i] > 0.) {
      if (debug) {
        std::string descr = "                              ";
        for (int j = 30; j--;) descr[j] = description[i][j];
        std::cout << "    " << descr << std::endl;
        std::cout << "    Deexcitation rate:     " 
                  << fDeexcitation[i] << " ns-1" << std::endl;
        std::cout << "    Radiative decay rate:  "
                  << fRadiative[i] << " ns-1" << std::endl;
        std::cout << "    Penning transfer rate: "
                  << fCollIon[i] << " ns-1" << std::endl;
      } 
      fRadiative[i] /= fDeexcitation[i];
      fCollIon[i] /= fDeexcitation[i];
      fCollLoss[i] /= fDeexcitation[i];
      fCollIon[i] += fRadiative[i];
      fCollLoss[i] += fCollIon[i];
    }
  }
  
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

  std::cout << "Drift velocity: " << vz << std::endl;
  std::cout << "Longitudinal diffusion: " << dl << std::endl;
  std::cout << "Transverse diffusion:   " << dt << std::endl;
  std::cout << "Townsend coefficient:   " << alpha << std::endl;
  std::cout << "Attachment coefficient: " << eta << std::endl;

}

}
