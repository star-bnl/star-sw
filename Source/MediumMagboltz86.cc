#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

#include <map>

#include "MediumMagboltz86.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "OpticalData.hh"

namespace Garfield {

MediumMagboltz86::MediumMagboltz86() :
  Medium(),
  eFinal(40.), eStep(eFinal / nEnergySteps), useAutoAdjust(true), 
  useCsOutput(false), 
  nTerms(0), useAnisotropic(true), 
  usePenning(false), rPenningGlobal(0.), lambdaPenningGlobal(0.), 
  nPenning(0), 
  useDeexcitation(false), useRadTrap(true),
  nDeexcitations(0), nDeexcitationProducts(0),
  scaleExc(1.), useSplittingFunction(true),
  eFinalGamma(20.), eStepGamma(eFinalGamma / nEnergyStepsGamma),
  nIonMobilities(false) {
 
  className = "MediumMagboltz86";
 
  // Set physical constants in Magboltz common blocks.
  cnsts_.echarg = ElementaryCharge;
  cnsts_.emass = ElectronMassGramme;
  cnsts_.amu = AtomicMassUnit;
  cnsts_.pir2 = BohrRadius * BohrRadius * Pi;  
  inpt_.ary = RydbergEnergy;

  // Default gas mixture: pure argon
  for (int i = nMaxGases; i--;) {
    fraction[i] = 0.;
    gas[i] = 0;
    atWeight[i] = 0.;
    atNum[i] = 0.;
  }
  gas[0] = 2;
  fraction[0] = 1.;
  GetGasInfo(gas[0], name, atWeight[0], atNum[0]);
  
  // Set parameters in Magboltz common blocks.
  inpt_.nGas = nComponents;  
  inpt_.nStep = nEnergySteps;
  // Select the scattering model.
  inpt_.nAniso = 2;
  // Max. energy [eV]
  inpt_.efinal = eFinal;
  // Energy step size [eV]
  inpt_.estep = eStep;
  // Temperature and pressure
  inpt_.akt = BoltzmannConstant * temperature;
  inpt_.tempc = temperature - 273.15;
  inpt_.torr = pressure;
  // Disable Penning transfer.
  inpt_.ipen = 0;
 
  // Initialise Penning parameters
  for (int i = nMaxGases; i--;) {
    rPenningGas[i] = 0.;
    lambdaPenningGas[i] = 0.;
  }
  for (int i = nMaxLevels; i--;) {
    rPenning[i] = 0.;
    lambdaPenning[i] = 0.;
  }  
 
  isChanged = true;

  EnableDrift();
  EnablePrimaryIonisation();
  microscopic = true;
  
  // Initialize the collision counters.
  nCollisionsDetailed.clear();
  nCollisions[0] = 0; nCollisions[1] = 0; nCollisions[2] = 0;
  nCollisions[3] = 0; nCollisions[4] = 0;
  nPhotonCollisions[0] = 0; nPhotonCollisions[1] = 0; nPhotonCollisions[2] = 0;
  
  ionMobilityGrid.clear();
  ionMobilityValues.clear();

}

bool 
MediumMagboltz86::SetComposition(const std::string gas1, const double f1, 
                                 const std::string gas2, const double f2,
                                 const std::string gas3, const double f3,
                                 const std::string gas4, const double f4,
                                 const std::string gas5, const double f5, 
                                 const std::string gas6, const double f6) {

  // Make a backup copy of the gas composition.
  int gasOld[nMaxGases];
  for (int i = nMaxGases; i--;) {
    gasOld[i] = gas[i];
  }
  int nComponentsOld = nComponents;

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
    std::cerr << className << "::SetComposition:\n";
    std::cerr << "    Error setting the composition.\n";
    std::cerr << "    No valid ingredients were specified.\n";
    return false;
  }
  
  nComponents = i;
  std::vector<std::string> gasname;
  gasname.resize(nComponents);
  name = "";  
  double sum = 0.;
  for (i = 0; i < nComponents; ++i) {
    gasname[i] = "";
    if (i > 0) name += "/";  
    GetGasInfo(gas[i], gasname[i], atWeight[i], atNum[i]);
    name += gasname[i];
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
  
  std::cout << className << "::SetComposition:\n";
  std::cout << "    " << name;
  if (nComponents > 1) {
    std::cout << " (" << fraction[0] * 100;
    for (i = 1; i < nComponents; ++i) {
      std::cout << "/" << fraction[i] * 100;
    }
    std::cout << ")";
  }
  std::cout << "\n";
  
  // Set the number of gases in the Magboltz common block.
  inpt_.nGas = nComponents;  
  
  // Force a recalculation of the collision rates.
  isChanged = true;
  
  // Copy the previous Penning transfer parameters.
  double rPenningGasOld[nMaxGases];
  double lambdaPenningGasOld[nMaxGases];
  for (int i = nMaxGases; i--;) {
    rPenningGasOld[i] = rPenningGas[i];
    lambdaPenningGasOld[i] = lambdaPenningGas[i];
    rPenningGas[i] = 0.;
    lambdaPenningGas[i] = 0.;
  }
  for (int i = nComponents; i--;) {
    for (int j = nComponentsOld; j--;) {
      if (gas[i] == gasOld[j]) {
        if (rPenningGasOld[j] > 0.) {
          rPenningGas[i] = rPenningGasOld[j];
          std::cout << className << "::SetComposition:\n";
          std::cout << "    Adopting Penning transfer parameters for " 
                    << gasname[i] << " from previous mixture.\n";
          std::cout << "      r      = " << rPenningGas[i] << "\n";
          std::cout << "      lambda = " << lambdaPenningGas[i] << " cm\n";
        }
      }
    }
  }
  return true;
  
}

void 
MediumMagboltz86::GetComposition(std::string& gas1, double& f1,
                                 std::string& gas2, double& f2,
                                 std::string& gas3, double& f3,
                                 std::string& gas4, double& f4,
                                 std::string& gas5, double& f5,
                                 std::string& gas6, double& f6) {

  double a, z;
  GetGasInfo(gas[0], gas1, a, z); f1 = fraction[0];
  GetGasInfo(gas[1], gas2, a, z); f2 = fraction[1];
  GetGasInfo(gas[2], gas3, a, z); f3 = fraction[2];
  GetGasInfo(gas[3], gas4, a, z); f4 = fraction[3];
  GetGasInfo(gas[4], gas5, a, z); f5 = fraction[4];
  GetGasInfo(gas[5], gas6, a, z); f6 = fraction[5];

}

void
MediumMagboltz86::GetComponent(const int i, std::string& label, double& f) {

  if (i < 0 || i >= nComponents) {
    std::cerr << className << "::GetComponent:\n";
    std::cerr << "    Index out of range.\n";
    label = "";
    f = 0.;
    return;
  }
  
  double a, z;
  GetGasInfo(gas[i], label, a, z);
  f = fraction[i];

}

void
MediumMagboltz86::SetAtomicNumber(const double z) {

  std::cerr << className << "::SetAtomicNumber:\n";
  std::cerr << "    Effective Z cannot be changed"
            << " directly to " << z << ".\n"; 
  std::cerr << "    Use SetComposition to define the gas mixture.\n";

}

void
MediumMagboltz86::SetAtomicWeight(const double a) {

  std::cerr << className << "::SetAtomicWeight:\n";
  std::cerr << "    Effective A cannot be changed"
            << " directly to " << a << ".\n";
  std::cerr << "    Use SetComposition to define the gas mixture.\n";

}

void
MediumMagboltz86::SetNumberDensity(const double n) {

  std::cerr << className << "::SetNumberDensity:\n";
  std::cerr << "    Density cannot directly be changed to " << n << ".\n";
  std::cerr << "    Use SetTemperature and SetPressure.\n";
  
}

void
MediumMagboltz86::SetMassDensity(const double rho) {

  std::cerr << className << "::SetMassDensity:\n";
  std::cerr << "    Density cannot directly be changed to "
            << rho << ".\n";
  std::cerr << "    Use SetTemperature, SetPressure"
            << " and SetComposition.\n";
            
}

double
MediumMagboltz86::GetAtomicWeight() const {

  // Effective A, weighted by the fractions of the components.
  double a = 0.;
  for (int i = 0; i < nComponents; ++i) {
    a += atWeight[i] * fraction[i];
  }
  return a;
  
}

double
MediumMagboltz86::GetNumberDensity() const {

  // Ideal gas law.
  return LoschmidtNumber * (pressure / AtmosphericPressure) * 
                           (273.15 / temperature);
                                                         
}

double
MediumMagboltz86::GetMassDensity() const {

  return GetNumberDensity() * GetAtomicWeight() * AtomicMassUnit;
  
}

double
MediumMagboltz86::GetAtomicNumber() const {

  // Effective Z, weighted by the fractions of the components.
  double z = 0.;
  for (int i = 0; i < nComponents; ++i) {
    z += atNum[i] * fraction[i];
  }
  return z;
  
}

bool 
MediumMagboltz86::SetMaxElectronEnergy(const double e) {

  if (e <= Small) {
    std::cerr << className << "::SetMaxElectronEnergy:\n";
    std::cerr << "    Provided upper electron energy limit (" << e
              <<  " eV) is too small.\n";
    return false;
  }
  eFinal = e;
  
  // Determine the energy interval size.
  eStep = eFinal / nEnergySteps;
  
  // Set max. energy and step size also in Magboltz common block.
  inpt_.efinal = eFinal;
  inpt_.estep = eStep;
  
  // Force recalculation of the scattering rates table.
  isChanged = true;

  return true;
  
}

bool 
MediumMagboltz86::SetMaxPhotonEnergy(const double e) {

  if (e <= Small) {
    std::cerr << className << "::SetMaxPhotonEnergy:\n";
    std::cerr << "    Provided upper photon energy limit (" << e
              <<  " eV) is too small.\n";
    return false;
  }
  eFinalGamma = e;
  
  // Determine the energy interval size.
  eStepGamma = eFinalGamma / nEnergyStepsGamma;

  // Force recalculation of the scattering rates table.
  isChanged = true;
 
  return true;
  
}

void
MediumMagboltz86::EnableDeexcitation() {

  std::cout << className << "::EnableDeexcitation:\n";
  if (usePenning) {
    std::cout << "    Penning transfer will be switched off.\n";
  }
  if (useRadTrap) {
    std::cout << "    Radiation trapping is switched on.\n";
  } else {
    std::cout << "    Radiation trapping is switched off.\n";
  }
  usePenning = false;
  useDeexcitation = true;
  isChanged = true;
  nDeexcitationProducts = 0;

}

void
MediumMagboltz86::EnableRadiationTrapping() {

  useRadTrap = true;
  if (!useDeexcitation) {
    std::cout << className << "::EnableRadiationTrapping:\n";
    std::cout << "    Radiation trapping is enabled"
              << " but de-excitation is not.\n";
  } else {
    isChanged = true;
  }

}

void
MediumMagboltz86::EnablePenningTransfer(const double r, 
                                        const double lambda) {

  if (r < 0. || r > 1.) {
    std::cerr << className << "::EnablePenningTransfer:\n";
    std::cerr << "    Penning transfer probability must be " 
              << " in the range [0, 1].\n";
    return;
  }

  rPenningGlobal = r;
  if (lambda < Small) {
    lambdaPenningGlobal = 0.;
  } else {
    lambdaPenningGlobal = lambda;
  }

  std::cout << className << "::EnablePenningTransfer:\n";
  std::cout << "    Global Penning transfer parameters set to: \n";
  std::cout << "    r      = " << rPenningGlobal << "\n";
  std::cout << "    lambda = " << lambdaPenningGlobal << "\n";

  for (int i = nTerms; i--;) { 
    rPenning[i] = rPenningGlobal;
    lambdaPenning[i] = lambdaPenningGlobal;
  }
  
  if (useDeexcitation) {
    std::cout << className << "::EnablePenningTransfer:\n";
    std::cout << "    Deexcitation handling will be switched off.\n"; 
  }
  usePenning = true;
  
}

void
MediumMagboltz86::EnablePenningTransfer(const double r, 
                                        const double lambda, 
                                        const std::string gasname) {

  if (r < 0. || r > 1.) {
    std::cerr << className << "::EnablePenningTransfer:\n";
    std::cerr << "    Penning transfer probability must be " 
              << " in the range [0, 1].\n";
    return;
  }

  // Get the number of this gas.
  int ng;
  if (!GetGasNumber(gasname, ng)) {
    std::cerr << className << "::EnablePenningTransfer:\n";
    std::cerr << "    Gas " << gasname << " is not defined.\n";
    return;
  }

  // Look for this gas in the present gas mixture.
  bool found = false;
  int iGas = -1;
  for (int i = nComponents; i--;) {
    if (gas[i] == ng) {
      rPenningGas[i] = r;
      if (lambda < Small) {
        lambdaPenningGas[i] = 0.;
      } else {
        lambdaPenningGas[i] = lambda;
      }
      found = true;
      iGas = i;
      break;
    }
  }
  
  if (!found) {
    std::cerr << className << "::EnablePenningTransfer:\n";
    std::cerr << "    Specified gas (" << gasname 
              << ") is not part of the present gas mixture.\n";
    return;
  }
  
  int nLevelsFound = 0;
  for (int i = nTerms; i--;) {
    if (int(csType[i] / nCsTypes) == iGas) {
      if (csType[i] % nCsTypes == 4) ++nLevelsFound; 
      rPenning[i] = rPenningGas[iGas];
      lambdaPenning[i] = lambdaPenningGas[iGas];
    }
  }

  if (nLevelsFound > 0) {
    std::cout << className << "::EnablePenningTransfer:\n";
    std::cout << "    Penning transfer parameters " << nLevelsFound
              << " excitation levels set to:\n";
    std::cout << "      r      = " << rPenningGas[iGas];
    std::cout << "      lambda = " << lambdaPenningGas[iGas] << " cm\n"; 
  } else {
    std::cerr << className << "::EnablePenningTransfer:\n";
    std::cerr << "    Specified gas (" << gasname 
              << ") has no excitation levels in the present energy range.\n";
  }

  usePenning = true; 

}
 
void
MediumMagboltz86::DisablePenningTransfer() {

  for (int i = nTerms; i--;) {
    rPenning[i] = 0.;
    lambdaPenning[i] = 0.;
  }
  rPenningGlobal = 0.;
  lambdaPenningGlobal = 0.;

  for (int i = nMaxGases; i--;) {
    rPenningGas[i] = 0.;
    lambdaPenningGas[i] = 0.;
  }

  usePenning = false;    

}

void
MediumMagboltz86::DisablePenningTransfer(const std::string gasname) {

  // Get the number of this gas.
  int ng;
  if (!GetGasNumber(gasname, ng)) {
    std::cerr << className << "::DisablePenningTransfer:\n";
    std::cerr << "    Gas " << gasname << " is not defined.\n";
    return;
  }

  // Look for this gas in the present gas mixture.
  bool found = false;
  int iGas = -1;
  for (int i = nComponents; i--;) {
    if (gas[i] == ng) {
      rPenningGas[i] = 0.;
      lambdaPenningGas[i] = 0.;
      found = true;
      iGas = i;
      break;
    }
  }
  
  if (!found) {
    std::cerr << className << "::DisablePenningTransfer:\n";
    std::cerr << "    Specified gas (" << gasname 
              << ") is not part of the present gas mixture.\n";
    return;
  }
  
  int nLevelsFound = 0;
  for (int i = nTerms; i--;) {
    if (int(csType[i] / nCsTypes) == iGas) {
      rPenning[i] = 0.;
      lambdaPenning[i] = 0.;
    } else {
      if (csType[i] % nCsTypes == 4 && rPenning[i] > Small) {
        ++nLevelsFound;
      }
    }
  }

  if (nLevelsFound <= 0) {
    // There are no more excitation levels with r > 0.
    std::cout << className << "::DisablePenningTransfer:\n";
    std::cout << "    Penning transfer globally switched off.\n";
    usePenning = false;
  }

}

void
MediumMagboltz86::SetExcitationScalingFactor(const double r) {

  if (r <= 0.) {
    std::cerr << className << "::SetScalingFactor:\n";
    std::cerr << "    Incorrect value for scaling factor: " << r << "\n";
    return;
  }

  scaleExc = r;
  isChanged = true;

}

double 
MediumMagboltz86::GetElectronNullCollisionRate() {

  // If necessary, update the collision rates table.
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the "
                << " collision rates table.\n";
      return 0.;
    }
    isChanged = false;
  }
      
  return cfNull[0];
  
}

double 
MediumMagboltz86::GetElectronCollisionRate(const double e, const int band) {

  // Check if the electron energy is within the currently set range.
  if (e <= 0.) {
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Electron energy must be greater than zero.\n";
    return cfTot[0];
  }
  if (e > eFinal && useAutoAdjust) {    
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Collision rate at " << e 
              << " eV is not included in the current table.\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);    
  }

  // If necessary, update the collision rates table.
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << className << "::GetElectronCollisionRate:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return 0.;
    }
    isChanged = false;
  }

  if (debug && band != 0) {
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    This medium does not have a band structure.\n";
  }

  const int iStep = int(e / eStep);
  if (iStep >= nEnergySteps) return cfTot[nEnergySteps - 1];
  if (iStep < 0) return cfTot[0]; 
  return cfTot[iStep];

}

bool 
MediumMagboltz86::GetElectronCollision(const double e, int& type, int& level, 
                                       double& e1, double& ctheta, 
                                       int& nsec, double& esec, 
                                       int& band) {

  // Check if the electron energy is within the currently set range.
  if (e > eFinal && useAutoAdjust) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Provided electron energy  (" << e 
              << " eV) exceeds current energy range  (" << eFinal 
              << " eV).\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Electron energy must be greater than zero.\n";
    return false;
  }
  
    // If necessary, update the collision rates table.
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << className << "::GetElectronCollision:\n";
      std::cerr << "    Error calculating the collision rates table.\n"; 
      return false;
    }
    isChanged = false;
  }

  if (debug && band != 0) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    This medium does not have a band structure.\n";
  }

  // Get the energy interval.
  const int iE = e < eFinal ? int(e / eStep) : nEnergySteps - 1;
  
  // Sample the scattering process.
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
  
  // Extract the collision type.
  type = csType[level] % nCsTypes;

  // Increase the collision counters.
  ++nCollisions[type];
  ++nCollisionsDetailed[level];

  // Get the energy loss for this process.
  double loss = energyLoss[level];
  nsec = 0;
  // Secondary electron energy (none by default)
  esec = 0.;
  if (type == 1) {
    // Ionisation
    // Get the splitting parameter.
    const double w = wSplit[level];
    // Sample the secondary electron energy according to 
    // the Opal-Beaty-Peterson parameterisation.
    if (useSplittingFunction) { 
      esec = w * tan(RndmUniform() * atan(0.5 * (e - loss) / w));
      // Rescaling (SST)
      // esec = w * pow(esec / w, 0.9524);
    } else {
      esec = RndmUniform() * (e - loss);
    }
    if (esec <= 0) esec = Small;
    loss += esec;
    nsec = 1;
  } else if (type == 4) {
    // Excitation
    // Dissociative excitation: continuous loss distribution?
    if (description[level][5] == 'D' &&
        description[level][6] == 'I' &&
        description[level][7] == 'S') {
      if (fabs(loss * rgas[level] - 12.) < Small &&
          e > 2 * loss * rgas[level]) {
        loss += 2. * RndmUniform();
      }
    }
    // Follow the de-excitation cascade (if switched on).
    if (useDeexcitation && iDeexcitation[level] >= 0) {
      ComputeDeexcitation(iDeexcitation[level]);
      esec = -1.;
      nsec = nDeexcitationProducts;
    } else if (usePenning) {
      nDeexcitationProducts = 0;
      dxcProducts.clear();
      // Simplified treatment of Penning ionisation.
      // If the energy threshold of this level exceeds the 
      // ionisation potential of one of the gases,
      // create a new electron (with probability rPenning).
      if (energyLoss[level] * rgas[level] > minIonPot && 
          RndmUniform() < rPenning[level]) {
        // The energy of the secondary electron is assumed to be given by
        // the difference of excitation and ionisation threshold.
        esec = energyLoss[level] * rgas[level] - minIonPot;
        if (esec <= 0) esec = Small;
        // Add the secondary electron to the list.
        dxcProd newDxcProd;
        newDxcProd.t = 0.;
        newDxcProd.s = -lambdaPenning[level] * log(RndmUniformPos());
        newDxcProd.energy = esec;
        newDxcProd.type = DxcTypeElectron;
        dxcProducts.push_back(newDxcProd);
        nsec = 1;
        nDeexcitationProducts = 1;
        ++nPenning;
      }
    }
  }

  // Make sure the energy loss is smaller than the energy.
  if (e < loss) loss = e - 0.0001;
  
  // Determine the scattering angle.
  double ctheta0 = 1. - 2. * RndmUniform();
  if (useAnisotropic) {
    switch (scatModel[level]) {
      case 0:
        break;
      case 1:
        ctheta0 = 1. - RndmUniform() * scatCut[iE][level];
        if (RndmUniform() > scatParameter[iE][level]) ctheta = -ctheta;
        break;
      case 2:
        ctheta0 = (ctheta0 + scatParameter[iE][level]) / 
                  (1. + scatParameter[iE][level] * ctheta0);
        break;
      default:
        std::cerr << className << "::GetElectronCollision:\n";
        std::cerr << "    Unknown scattering model. \n";
        std::cerr << "    Using isotropic distribution.\n";
        break;
    }
  }

  const double s1 = rgas[level];
  const double s2 = (s1 * s1) / (s1 - 1.);
  const double stheta0 = sqrt(1. - ctheta0 * ctheta0);
  const double arg = std::max(1. - s1 * loss / e, Small);
  const double d = 1. - ctheta0 * sqrt(arg);

  // Update the energy. 
  e1 = std::max(e * (1. - loss / (s1 * e) - 2. * d / s2), Small);
  double q = std::min(sqrt((e / e1) * arg) / s1, 1.);
  const double stheta = q * stheta0;
  
  ctheta = sqrt(1. - stheta * stheta);
  if (ctheta0 < 0.) {
    const double u = (s1 - 1.) * (s1 - 1.) / arg;
    if (ctheta0 * ctheta0 > u) ctheta *= -1.;
  }
  return true;

}

bool
MediumMagboltz86::GetDeexcitationProduct(const int i, double& t, double& s,
                                         int& type, double& energy) {

  if (i < 0 || i >= nDeexcitationProducts || 
      !(useDeexcitation || usePenning)) return false;
  t = dxcProducts[i].t;
  s = dxcProducts[i].s;
  type = dxcProducts[i].type;
  energy = dxcProducts[i].energy;
  return true;

}

double 
MediumMagboltz86::GetPhotonCollisionRate(const double e) {

  if (e <= 0.) {
    std::cerr << className << "::GetPhotonCollisionRate:\n";
    std::cerr << "    Photon energy must be greater than zero.\n";
    return cfTotGamma[0];
  }
  if (e > eFinalGamma && useAutoAdjust) {
    std::cerr << className << "::GetPhotonCollisionRate:\n";
    std::cerr << "    Collision rate at " << e 
              << " eV is not included in the current table.\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxPhotonEnergy(1.05 * e);
  }
    
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << className << "::GetPhotonCollisionRate:\n";
      std::cerr << "     Error calculating the collision rates table.\n";
      return 0.;
    }
    isChanged = false;
  }

  if (e > eFinalGamma) return cfTotGamma[nEnergyStepsGamma - 1];
  return cfTotGamma[int(e / eStepGamma)];

}

bool
MediumMagboltz86::GetPhotonCollision(const double e, int& type, int& level,
                                     double& e1, double& ctheta, 
                                     int& nsec, double& esec) {

  if (e > eFinalGamma && useAutoAdjust) {
    std::cerr << className << "::GetPhotonCollision:\n";
    std::cerr << "    Provided electron energy  (" << e 
              << " eV) exceeds current energy range  (" << eFinalGamma
              << " eV).\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxPhotonEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << className << "::GetPhotonCollision:\n";
    std::cerr << "    Photon energy must be greater than zero.\n";
    return false;
  }
  
  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating" 
                << " the collision rates table.\n";
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
 
  nsec = 0;  
  esec = 0.;
  type = csTypeGamma[level];
  // Collision type
  if (type < 0) {
    // Excitation
    if (useRadTrap && useDeexcitation) {
      ++nPhotonCollisions[3];
      ComputeDeexcitation(-type - 1);
      type = 3;
      nsec = nDeexcitationProducts;
    } else {
      type = 2;
    }
  } else {
    type = type % 3;
    int ngas = int(csTypeGamma[level] / 3);
    ++nPhotonCollisions[type];
    // Ionising collision
    if (type == 1) {
      esec = e - ionPot[ngas];
      if (esec < Small) esec = Small;
      e1 = 0.;
      nsec = 1;
    }
  } 

  // Determine the scattering angle
  ctheta = 1. - 2 * RndmUniform();
  
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
                << " collision rates table.\n";
      return 0;
    }
    isChanged = false;
  }

  return nTerms;

}

bool 
MediumMagboltz86::GetLevel(const int i, int& ngas, int& type,
                           std::string& descr, double& e) {

  if (isChanged) {
    if (!Mixer()) {
      std::cerr << "MediumMagboltz86: Error calculating the " 
                << " collision rates table.\n";
      return false;
    }
    isChanged = false;
  }

  if (i < 0 || i >= nTerms) {
    std::cerr << className << "::GetLevel:\n";
    std::cerr << "    Requested level (" << i
              << " does not exist.\n";
    return false;
  }  
  
  // Collision type
  type = csType[i] % nCsTypes;
  ngas = int(csType[i] / nCsTypes);
  // Description (from Magboltz)
  descr = "                              ";
  for (int j = 30; j--;) descr[j] = description[i][j];
  // Threshold energy
  e = rgas[i] * energyLoss[i];
  if (debug) {
    std::cout << className << "::GetLevel:\n";
    std::cout << "    Level " << i << ": " << descr << "\n";
    std::cout << "    Type " << type << "\n",
    std::cout << "    Threshold energy: " << e << " eV\n";   
    if (type == 4 && usePenning && 
        e > minIonPot) {
      std::cout << "    Penning transfer coefficient: " 
                << rPenning[i] << "\n";
    }
  }
  return true;

}

int 
MediumMagboltz86::GetNumberOfElectronCollisions(const int level) const {

  if (level < 0 || level >= nTerms) {
    std::cerr << className << "::GetNumberOfElectronCollisions:\n"; 
    std::cerr << "    Requested cross-section term (" 
              << level << ") does not exist.\n";
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
MediumMagboltz86::SetIonMobility(const double e, const double mu) {

  if (mu < Small) {
    std::cerr << className << "::SetIonMobility:\n";
    std::cerr << "    Ion mobility must be greater than zero.\n";
    return;
  }

  if (e < 0.) {
    std::cerr << className << "::SetIonMobility:\n";
    std::cerr << "    Energy must be greater than zero.\n";
    return;
  }

  if (nIonMobilities == 0) {
    ionMobilityGrid.push_back(e);
    ionMobilityValues.push_back(mu);
    ++nIonMobilities;
    return;
  }

  if (e > ionMobilityGrid.back()) {
    ionMobilityGrid.push_back(e);
    ionMobilityValues.push_back(mu);
    ++nIonMobilities;
    return;
  }

  ionMobilityGrid.resize(nIonMobilities + 1);
  for (int i = nIonMobilities; i--;) {
    if (ionMobilityGrid[i] > e) {
      ionMobilityGrid[i + 1] = ionMobilityGrid[i];
      ionMobilityValues[i + 1] = ionMobilityValues[i];
      if (i == 0) {
        ionMobilityGrid[i] = e;
        ionMobilityValues[i] = mu;
      }
    } else if (ionMobilityGrid[i] < e) {
      ionMobilityGrid[i + 1] = e;
      ionMobilityValues[i + 1] = mu;
      break;
    } else {
      // Replace the previous value
      std::cout << className << "::SetIonMobility:\n";
      std::cout << "    Ion mobility at E = " << e << " was " 
                << ionMobilityValues[i] << ",\n";
      std::cout << "    Replaced by " << mu << ".\n";
      ionMobilityValues[i] = mu;
      break;
    }
  }
  ++nIonMobilities;

}

bool
MediumMagboltz86::IonVelocity(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  if (nIonMobilities <= 0) {
    vx = vy = vz = 0.;
    return false;
  }

  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (nIonMobilities == 1 || e <= ionMobilityGrid[0]) {
    vx = ionMobilityValues[0] * ex;
    vy = ionMobilityValues[0] * ey;
    vz = ionMobilityValues[0] * ez;
    return true;
  }

  if (e >= ionMobilityGrid[nIonMobilities - 1]) {
    const double a2 = log(ionMobilityValues[nIonMobilities - 1] / 
                          ionMobilityValues[nIonMobilities - 2]) /
                      (ionMobilityGrid[nIonMobilities - 1] - 
                       ionMobilityGrid[nIonMobilities - 2]);
    const double a1 = log(ionMobilityValues[nIonMobilities - 1]) - 
                      a2 * ionMobilityGrid[nIonMobilities - 1];
    return exp(a1 + a2 * e);
  }

  int iLow = 0;
  int iUp  = nIonMobilities - 1;
  int iMid;
  while (iUp - iLow > 1) {
    iMid = (iLow + iUp) >> 1;
    if (e < ionMobilityGrid[iMid]) {
      iUp = iMid;
    } else {
      iLow = iMid;
    }
  }

  double mu = ionMobilityValues[iLow] + (e - ionMobilityGrid[iLow]) * 
              (ionMobilityValues[iUp] - ionMobilityValues[iLow]) / 
              (ionMobilityGrid[iUp] - ionMobilityGrid[iLow]);
  const double b = sqrt(bx * bx + by * by + bz * bz);
  if (b > Small) {
    // TODO: Laue-Langevin?
  }
  vx = mu * ex; vy = mu * ey; vz = mu * ez;
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
  if (name == "CO2" || name == "CARBON-DIOXIDE" ||
      name == "CARBONDIOXIDE") {
    number = 12; return true;
  }
  // Neopentane
  if (name == "NEOPENTANE" || name == "NEO-PENTANE" || 
      name == "NEO-C5H12" || name == "NEOC5H12" || 
      name == "DIMETHYLPROPANE" || name == "C5H12") {
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
  if (name == "NO" || name == "NITRIC-OXIDE" || 
      name == "NITROGEN-MONOXIDE") {
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
      name == "DIMETHOXYMETHANE" || name == "FORMAL" || 
      name == "C3H8O2") {
    number = 24; return true;
  }
  // DME
  if (name == "DME" || 
      name == "DIMETHYL-ETHER" || name == "DIMETHYLETHER" || 
      name == "METHYL-ETHER" || name == "METHYLETHER" ||
      name == "WOOD-ETHER" || name == "WOODETHER" ||
      name == "DIMETHYL-OXIDE" || name == "DEMEON" || 
      name == "METHOXYMETHANE" || name == "C2H6O") {
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
      name == "CYCLOPROPANE" || name == "C-C3H6" || 
      name == "CC3H6" || name == "CYCLO-C3H6") {
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
  
  std::cerr << className << "::GetGasNumber():\n";
  std::cerr << "    Gas " << name << " is not defined.\n";
  return false;
  
}

bool 
MediumMagboltz86::GetGasInfo(const int number, 
                             std::string& gasname, 
                             double& a, double& z) const {
  
  switch (number) {
    case 1:  
      gasname = "CF4";  
      a = 12.0107 + 4 * 18.9984032; z = 6 + 4 * 9;
      break;
    case 2:  
      gasname = "Ar";   
      a =  39.948;         z = 18;
      break;
    case 3:  
      gasname = "He";   
      a =   4.002602;      z =  2;
      break;
    case 4:  
      gasname = "He-3"; 
      a =   3.01602931914; z =  2;
      break;
    case 5:  
      gasname = "Ne";   
      a =  20.1797;        z = 10;  
      break;
    case 6:  
      gasname = "Kr";   
      a =  37.798;         z = 36;
      break;
    case 7:  
      gasname = "Xe";   
      a = 131.293;         z = 54;
      break;
    case 8:  
      gasname = "CH4";
      a =     12.0107 +  4 * 1.00794; z =     6 +  4;
      break;
    case 9:  
      gasname = "C2H6";  
      a = 2 * 12.0107 +  6 * 1.00794; z = 2 * 6 +  6;
      break;
    case 10: 
      gasname = "C3H8";     
      a = 3 * 12.0107 +  8 * 1.00794; z = 3 * 6 +  8;
      break;
    case 11: 
      gasname = "iC4H10";   
      a = 4 * 12.0107 + 10 * 1.00794; z = 4 * 6 + 10;
      break;
    case 12: 
      gasname = "CO2";  
      a = 12.0107 + 2 * 15.9994; z = 6 + 2 * 8;
      break;
    case 13: 
      gasname = "neoC5H12"; 
      a = 5 * 12.0107 + 12 * 1.00794; z = 5 * 6 + 12;
      break;
    case 14: 
      gasname = "H2O";  
      a = 2 * 1.00794 + 15.9994; z = 2 + 8;
      break;
    case 15: 
      gasname = "O2";   
      a = 2 * 15.9994; z = 2 * 8;
      break;
    case 16: 
      gasname = "N2";   
      a = 2 * 14.0067; z = 2 * 7;
      break;
    case 17: 
      gasname = "NO";   
      a = 14.0067 + 15.9994; z = 7 + 8;
      break;
    case 18: 
      gasname = "N2O";  
      a = 2 * 14.0067 + 15.9994; z = 2 * 7 + 8;
      break;
    case 19: 
      gasname = "C2H4"; 
      a = 2 * 12.0107 + 4 * 1.00794; z = 2 * 6 + 4;
      break;
    case 20: 
      gasname = "C2H2"; 
      a = 2 * 12.0107 + 2 * 1.00794; z = 2 * 6 + 2;
      break;
    case 21: 
      gasname = "H2";   
      a = 2 * 1.00794; z = 2;
      break;
    case 22: 
      gasname = "D2";   
      a = 2 * 2.01410177785; z = 2;
      break;
    case 23: 
      gasname = "CO";   
      a = 12.0107 + 15.9994; z = 6 + 8;
      break;
    case 24: 
      gasname = "Methylal"; 
      a = 3 * 12.0107 + 8 * 1.00794 + 2 * 15.9994; 
      z = 3 * 6 + 8 + 2 * 8;
      break;
    case 25: gasname = "DME";      
      a = 2 * 12.0107 + 6 * 1.00794 +     15.9994; 
      z = 2 * 6 + 6 + 8;
      break;
    case 26: gasname = "Reid-Step";     a = 1.; z = 1.; break;
    case 27: gasname = "Maxwell-Model"; a = 1.; z = 1.; break;
    case 28: gasname = "Reid-Ramp";     a = 1.; z = 1.; break;
    case 29: 
      gasname = "C2F6";    
      a = 2 * 12.0107 + 6 * 18.9984032; z = 2 * 6 + 6 * 9;
      break;
    case 30: 
      gasname = "SF6";     
      a =     32.065  + 6 * 18.9984032; z = 16 + 6 * 9;
      break;
    case 31: 
      gasname = "NH3";     
      a = 14.0067 + 3 * 1.00794; z = 7 + 3;
      break;
    case 32: 
      gasname = "C3H6";   
      a = 3 * 12.0107 + 6 * 1.00794; z = 3 * 6 + 6;
      break;
    case 33: 
      gasname = "cC3H6";  
      a = 3 * 12.0107 + 6 * 1.00794; z = 3 * 6 + 6;
      break;
    case 34: 
      gasname = "CH3OH";  
      a =     12.0107 + 4 * 1.00794 + 15.9994; z =     6 + 4 + 8;
      break;
    case 35: 
      gasname = "C2H5OH"; 
      a = 2 * 12.0107 + 6 * 1.00794 + 15.9994; z = 2 * 6 + 6 + 8;
      break;
    case 36: 
      gasname = "C3H7OH"; 
      a = 3 * 12.0107 + 8 * 1.00794 + 15.9994; z = 3 * 6 + 8 * 8;
      break;
    case 37: 
      gasname = "Cs";      
      a = 132.9054519; z = 55;
      break;
    case 38: 
      gasname = "F2";      
      a = 2 * 18.9984032; z = 2 * 9;
      break;
    case 39: 
      gasname = "CS2";     
      a = 12.0107 + 2 * 32.065; z = 6 + 2 * 16;
      break;
    case 40: 
      gasname = "COS";     
      a = 12.0107 + 15.9994 + 32.065; z = 6 + 8 + 16;
      break;
    case 41: 
      gasname = "CD4";     
      a = 12.0107 + 4 * 2.01410177785; z = 6 + 4;
      break;
    case 42: 
      gasname = "BF3";     
      a = 10.811 + 3 * 18.9984032; z = 5 + 3 * 9;
      break;
    case 43: 
      gasname = "C2HF5";   
      a = 2 * 12.0107 + 1.00794 + 5 * 18.9984032; z = 2 * 6 + 1 + 5 * 9;
      break;
    case 50: 
      gasname = "CHF3";    
      a =     12.0107 + 1.00794 + 3 * 18.9984032; z =     6 + 1 + 3 * 9;
      break;
    case 51: 
      gasname = "CF3Br";   
      a = 12.0107 + 3 * 18.9984032 + 79.904; z = 6 + 3 * 9 + 35;
      break;
    case 52: 
      gasname = "C3F8";    
      a = 3 * 12.0107 + 8 * 18.9984032; z = 3 * 6 + 8 * 9;
      break;
    case 53: 
      gasname = "O3";      
      a = 3 * 15.9994; z = 3 * 8;
      break;
    case 54: 
      gasname = "Hg";      
      a = 2 * 200.59; z = 80;
      break;
    case 55: 
      gasname = "H2S";     
      a = 2 * 1.00794 + 32.065; z = 2 + 16;
      break;
    case 56: 
      gasname = "nC4H10"; 
      a = 4 * 12.0107 + 10 * 1.00794; z = 4 * 6 + 10;
      break;
    case 57: 
      gasname = "nC5H12"; 
      a = 5 * 12.0107 + 12 * 1.00794; z = 5 * 6 + 12;
      break;
    case 58: 
      gasname = "N2";      
      a = 2 * 14.0067; z = 2 * 7;
      break;
    case 59: 
      gasname = "GeH4"; 
      a = 72.64   + 4 * 1.00794; z = 32 + 4;
      break;
    case 60: 
      gasname = "SiH4"; 
      a = 28.0855 + 4 * 1.00794; z = 14 + 4;
      break;
    default: gasname = ""; a = 0.; z = 0.; return false; break;
  }
  return true;

}

bool 
MediumMagboltz86::Mixer() {

  // Set constants and parameters in Magboltz common blocks.
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
  if (useAnisotropic) {
    inpt_.nAniso = 2;
  } else {
    inpt_.nAniso = 0;
  }
  inpt_.efinal = eFinal;
  inpt_.estep = eStep;
  
  // Calculate the atomic density (ideal gas law).
  const double dens = LoschmidtNumber * 
                      (pressure / AtmosphericPressure) * 
                      (273.15 / temperature);
  // Prefactor for calculation of scattering rate from cross-section.
  const double prefactor = dens * SpeedOfLight * sqrt(2. / ElectronMass);

  // Fill the electron energy array, reset the collision rates.
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
  
  // Cross-sections
  // 0: total, 1: elastic, 
  // 2: ionisation, 3: attachment, 
  // 4, 5: unused
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
  // Scattering algorithms
  long long kIn[nMaxInelasticTerms] = {0};
  long long kEl[6] = {0};    
  
  char name[15];  
          
  if (debug) {
    std::cout << className << "::Mixer:\n";
    std::cout << "    Creating table of collision rates with " 
              << nEnergySteps << " energy steps \n";
    std::cout << "    between 0 and " << eFinal << " eV.\n";
  }
  nTerms = 0;
  
  std::ofstream outfile;
  if (useCsOutput) {
    outfile.open("cs.txt", std::ios::out);
    outfile << "# \n";
  }

  // Loop over the gases in the mixture.  
  for (int iGas = 0; iGas < nComponents; ++iGas) {
    // Retrieve the cross-section data for this gas from Magboltz.
    long long ngs = gas[iGas];
    gasmix_(&ngs, q[0], qIn[0], &nIn, e, eIn, name, &virial, &w, 
            pEqEl[0], pEqIn[0], penFra[0], kEl, kIn, scrpt);
    if (debug) {
      std::cout << "    " << name << "\n";
      std::cout << "      m / M:                " << 0.5 * e[1] << "\n";
      std::cout << "      Ionisation threshold: " << e[2] << " eV\n";
      std::cout << "      Attachment threshold: " << e[3] << " eV\n";
      std::cout << "      Splitting parameter:  " << w << " eV\n";
    }
    int np0 = nTerms;
    
    // Check if there is still sufficient space.
    if (np0 + nIn + 2 >= nMaxLevels) {
      std::cerr << className << "::Mixer:\n";
      std::cerr << "    Max. number of levels (" << nMaxLevels 
                << ") exceeded.\n";
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
    csType[np] = nCsTypes * iGas;
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
      csType[np] = nCsTypes * iGas + 1;
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
      csType[np] = nCsTypes * iGas + 2;
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
        csType[np] = nCsTypes * iGas + 4;     
      } else if (eIn[j] < 0.) {
        // Super-elastic collision
        csType[np] = nCsTypes * iGas + 5;
      } else {
        // Inelastic collision
        csType[np] = nCsTypes * iGas + 3;
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
        ComputeAngularCut(pEqEl[iE][1], scatCut[iE][np], 
                          scatParameter[iE][np]);
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
        // Scale the excitation cross-sections (for error estimates)
        cf[iE][np] *= scaleExc;
        // Temporary hack for methane dissociative excitations:
        if (description[np][5] == 'D' &&
            description[np][6] == 'I' &&
            description[np][7] == 'S' &&
            energyLoss[np] * r >= 12.) {
        
        }
        if (cf[iE][np] < 0.) {
          std::cerr << className << "::Mixer:\n";
          std::cerr << "    Negative inelastic cross-section at " 
                    << iE * eStep << " eV.\n"; 
          std::cerr << "    Set to zero.\n";
          cf[iE][np] = 0.;
        }
        if (scatModel[np] == 1) {
          ComputeAngularCut(pEqIn[iE][j], scatCut[iE][np], 
                            scatParameter[iE][np]);
        } else if (scatModel[np] == 2) {
          scatParameter[iE][np] = pEqIn[iE][j];
        }
      }
      if (useCsOutput) outfile << "\n";
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
    std::cout << className << "::Mixer:\n";
    std::cout << "    Lowest ionisation threshold in the mixture: " 
              << minIonPot << " eV\n";
  }

  for (int iE = nEnergySteps; iE--;) {
    // Calculate the total collision frequency
    for (int k = nTerms; k--;) {
      if (cf[iE][k] < 0.) {
          std::cerr << className << "::Mixer:\n";
          std::cerr << "    Negative collision rate at " 
                    << iE * eStep << " eV. \n";
          std::cerr << "    Set to zero.\n";
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
    std::cout << className << "::Mixer:\n";
    std::cout << "    Energy [eV]    Collision Rate [ns-1]\n";
    for (int i = 0; i < 8; ++i) { 
      std::cout << "    " << std::setw(10) 
                << (2 * i + 1) * eFinal / 16
                << "    " << std::setw(18) 
                << cfTot[(i + 1) * nEnergySteps / 16] << "\n";
    }
  }

  // If requested, make a table of de-excitation channels
  if (useDeexcitation) ComputeDeexcitationTable();
  // Fill the photon collision rates table
  if (!ComputePhotonCollisionTable()) {
    std::cerr << "MediumMagboltz86: \n";
    std::cerr << "    Photon collision rates could not be calculated.\n"; 
    if (useDeexcitation) {
      std::cerr << "    Deexcitation handling is switched off.\n";
      useDeexcitation = false;
    }
  }

  // Reset the Penning transfer parameters
  for (int i = nTerms; i--;) {
    rPenning[i] = rPenningGlobal;
    int iGas = int(csType[i] / nCsTypes);
    if (rPenningGas[iGas] > Small) {
      rPenning[i] = rPenningGas[iGas];
      lambdaPenning[i] = lambdaPenningGas[iGas];
    }
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

  // Concentrations of "de-excitable" gases
  bool withAr = false, withNe = false;
  double cAr = 0., cNe = 0.;
  int iAr = 0, iNe = 0;
  std::map<std::string, int> mapLevels;
  // Make a mapping of all excitation levels 
  for (int i = 0; i < nTerms; ++i) {
    if (csType[i] % nCsTypes != 4) continue;
    const int ngas = int(csType[i] / nCsTypes);
    if (ngas == 2) {
      // Argon
      if (!withAr) {
        withAr = true;
        iAr = int(csType[i] / nCsTypes);
        cAr = fraction[iAr];
      }
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
      else {
        std::cerr << className << "::ComputeDeexcitationTable:\n";
        std::cerr << "    Unknown excitation level:\n";
        std::cerr << "      Ar " << level << "\n";
      }
    } else if (ngas == 5) {
      // Neon
      if (!withNe) {
        withNe = true;
        iNe = int(csType[i] / nCsTypes);
        cNe = fraction[iNe];
      }
      std::string level = "       ";
      for (int j = 0; j < 7; ++j) level[j] = description[i][3 + j];
      if      (level == "  1S5  ") mapLevels["Ne_1S5"] = i;
      else if (level == "  1S4  ") mapLevels["Ne_1S4"] = i;
      else if (level == "  1S3  ") mapLevels["Ne_1S3"] = i;
      else if (level == "  1S2  ") mapLevels["Ne_1S2"] = i;
      else if (level == " 2P10  ") mapLevels["Ne_2P10"] = i;
      else if (level == "  2P9  ") mapLevels["Ne_2P9"] = i;
      else if (level == "  2P8  ") mapLevels["Ne_2P8"] = i;
      else if (level == "  2P7  ") mapLevels["Ne_2P7"] = i;
      else if (level == "  2P6  ") mapLevels["Ne_2P6"] = i;
      else if (level == "  2P5  ") mapLevels["Ne_2P5"] = i;
      else if (level == "  2P4  ") mapLevels["Ne_2P4"] = i;
      else if (level == "  2P3  ") mapLevels["Ne_2P3"] = i;
      else if (level == "  2P2  ") mapLevels["Ne_2P2"] = i;
      else if (level == "  2P1  ") mapLevels["Ne_2P1"] = i;
      else if (level == "  2S5  ") mapLevels["Ne_2S5"] = i;
      else if (level == "  2S4  ") mapLevels["Ne_2S4"] = i;
      else if (level == "  2S3  ") mapLevels["Ne_2S3"] = i;
      else if (level == "  2S2  ") mapLevels["Ne_2S2"] = i;
      else if (level == "  3D6  ") mapLevels["Ne_3D6"] = i;
      else if (level == "  3D5  ") mapLevels["Ne_3D5"] = i;
      else if (level == " 3D4!  ") mapLevels["Ne_3D4!"] = i;
      else if (level == "  3D4  ") mapLevels["Ne_3D4"] = i;
      else if (level == "  3D3  ") mapLevels["Ne_3D3"] = i;
      else if (level == "  3D2  ") mapLevels["Ne_3D2"] = i;
      else if (level == " 3D1!! ") mapLevels["Ne_3D1!!"] = i;
      else if (level == " 3D1!  ") mapLevels["Ne_3D1!"] = i;
      else if (level == "3S1!!!!") mapLevels["Ne_3S1!!!!"] = i;
      else if (level == "3S1!!! ") mapLevels["Ne_3S1!!!"] = i;
      else if (level == " 3S1!! ") mapLevels["Ne_3S1!!"] = i;
      else if (level == "  3S1! ") mapLevels["Ne_3S1!"] = i;
      else if (level == "SUM 3P1") mapLevels["Ne_3P10_3P6"] = i;
      else if (level == "SUM 3P5") mapLevels["Ne_3P5_3P2"] = i;
      else if (level == "  3P1  ") mapLevels["Ne_3P1"] = i;
      else if (level == "  3S4  ") mapLevels["Ne_3S4"] = i;
      else if (level == "  3S2  ") mapLevels["Ne_3S2"] = i;
      else if (level == "  4D5  ") mapLevels["Ne_4D5"] = i;
      else if (level == "  4D2  ") mapLevels["Ne_4D2"] = i;
      else if (level == " 4S1!  ") mapLevels["Ne_4S1!"] = i;
      else if (level == "  4S4  ") mapLevels["Ne_4S4"] = i;
      else if (level == "  5D5  ") mapLevels["Ne_5D5"] = i;
      else if (level == "  5D2  ") mapLevels["Ne_5D2"] = i;
      else if (level == "  4S2  ") mapLevels["Ne_4S2"] = i;
      else if (level == " 5S1!  ") mapLevels["Ne_5S1!"] = i;
      else if (level == "SUM S H") mapLevels["Ne_Sum_S_High"] = i;
      else if (level == "SUM D H") mapLevels["Ne_Sum_P_High"] = i;
      else {
        std::cerr << className << "::ComputeDeexcitationTable:\n";
        std::cerr << "    Unknown excitation level:\n";
        std::cerr << "      Ne " << level << "\n";
      }
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

  // Radiative de-excitation channels
  // Transition probabilities taken from NIST Atomic Spectra Database 
  // Oscillator strengths taken from 
  // J. Berkowitz, Atomic and Molecular Photoabsorption (2002)
  // Conversion from oscillator strength to transition probability
  const double f2A = 2. * SpeedOfLight * FineStructureConstant / 
                    (3. * ElectronMass * HbarC);
  deexcitation newDxc;
  for (itMap = mapLevels.begin(); itMap != mapLevels.end(); itMap++) {
    std::string level = (*itMap).first;
    newDxc.gas = int(csType[(*itMap).second] / nCsTypes);
    newDxc.label = level;
    newDxc.energy = energyLoss[(*itMap).second] * rgas[(*itMap).second];
    newDxc.osc = 0.;
    newDxc.p.clear(); newDxc.final.clear(); newDxc.type.clear();
    newDxc.nChannels = 0;
    if (level == "Ar_1S3" || level == "Ar_1S5") {
      newDxc.p.clear(); newDxc.final.clear(); newDxc.type.clear(); 
    } else if (level == "Ar_1S4") {
      newDxc.osc = 0.058;
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.119; newDxc.final[0] = -1;
    } else if (level == "Ar_1S2") {
      newDxc.osc = 0.2214;
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.51; newDxc.final[0] = -1;
    } else if (level == "Ar_2P10") {
      int nc = 4; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.0189;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 5.43e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 9.8e-4;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 1.9e-4;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P9") {
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.0331; newDxc.final[0] = mapDxc["Ar_1S5"];
    } else if (level == "Ar_2P8") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 9.28e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 0.0215;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 1.47e-3; newDxc.final[2] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P7") {
      int nc = 4; newDxc.nChannels = nc; 
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 5.18e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 0.025;   newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 2.43e-3; newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 1.06e-3; newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P6") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.0245;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 4.9e-3;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 5.03e-3; newDxc.final[2] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P5") {
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.0402; newDxc.final[0] = mapDxc["Ar_1S4"];
    } else if (level == "Ar_2P4") {
      int nc = 4; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 6.25e-4; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 2.2e-5;  newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0186;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 0.0139;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P3") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 3.8e-3;  newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 8.47e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0223;  newDxc.final[2] = mapDxc["Ar_1S3"];
    } else if (level == "Ar_2P2") {
      int nc = 4; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 6.39e-3; newDxc.final[0] = mapDxc["Ar_1S5"];
      newDxc.p[1] = 1.83e-3; newDxc.final[1] = mapDxc["Ar_1S4"];
      newDxc.p[2] = 0.0117;  newDxc.final[2] = mapDxc["Ar_1S3"];
      newDxc.p[3] = 0.0153;  newDxc.final[3] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_2P1") {
      int nc = 2; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 2.36e-4; newDxc.final[0] = mapDxc["Ar_1S4"];
      newDxc.p[1] = 0.0445;  newDxc.final[1] = mapDxc["Ar_1S2"];
    } else if (level == "Ar_3D6") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 8.1e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P4"];
      newDxc.p[2] = 3.6e-4; newDxc.final[2] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3D5") {
      newDxc.osc = 0.0011;
      int nc = 6; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 7.4e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 3.9e-5; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 3.2e-5; newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 1.4e-4; newDxc.final[3] = mapDxc["Ar_2P3"];
      newDxc.p[4] = 1.7e-4; newDxc.final[4] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[5] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[5] = -1;
    } else if (level == "Ar_3D3") {
      int nc = 6; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 4.9e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.6e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 2.5e-3; newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 3.9e-4; newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.1e-4; newDxc.final[5] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3D4!") {
      newDxc.p.clear(); newDxc.final.clear();  newDxc.type.clear();
    } else if (level == "Ar_3D4") {
      int nc = 2; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.011;  newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 8.8e-5; newDxc.final[1] = mapDxc["Ar_2P6"];
    } else if (level == "Ar_3D1!!") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 1.2e-4; newDxc.final[0] = mapDxc["Ar_2P9"];
      newDxc.p[1] = 5.7e-3; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 7.3e-3; newDxc.final[2] = mapDxc["Ar_2P7"];
    } else if (level == "Ar_2S5") {
      int nc = 8; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 4.9e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 0.011;  newDxc.final[1] = mapDxc["Ar_2P9"];
      newDxc.p[2] = 1.1e-3; newDxc.final[2] = mapDxc["Ar_2P8"];
      newDxc.p[3] = 4.6e-4; newDxc.final[3] = mapDxc["Ar_2P7"];
      newDxc.p[4] = 3.3e-3; newDxc.final[4] = mapDxc["Ar_2P6"];
      newDxc.p[5] = 5.9e-5; newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 1.2e-4; newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 3.1e-4; newDxc.final[7] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_2S4") {
      newDxc.osc = 0.026;
      int nc = 10; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
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
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 3.1e-3; newDxc.final[0] = mapDxc["Ar_2P9"];
      newDxc.p[1] = 2.e-3;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 9.8e-6; newDxc.final[2] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_3D2") {
      newDxc.osc = 0.09;
      int nc = 5; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.27;    newDxc.final[0] = -1;
      newDxc.p[1] = 9.52e-4; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 0.011;   newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.3e-3;  newDxc.final[3] = mapDxc["Ar_2P6"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[4] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[4] = -1;
    } else if (level == "Ar_3S1!!!!") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 8.3e-4; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 0.013;  newDxc.final[1] = mapDxc["Ar_2P4"];
      newDxc.p[2] = 2.2e-3; newDxc.final[2] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_3S1!!") {
      int nc = 3; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 3.69e-4; newDxc.final[0] = mapDxc["Ar_2P7"];
      newDxc.p[1] = 3.76e-3; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 6.2e-3;  newDxc.final[2] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_3S1!!!") {
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.015; newDxc.final[0] = mapDxc["Ar_2P3"];
    } else if (level == "Ar_2S3") {
      int nc = 4; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 3.26e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.22e-3; newDxc.final[1] = mapDxc["Ar_2P7"];
      newDxc.p[2] = 0.01;    newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 5.1e-3;  newDxc.final[3] = mapDxc["Ar_2P2"];
    } else if (level == "Ar_2S2") {
      newDxc.osc = 0.012;
      int nc = 4; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.035;  newDxc.final[0] = -1;
      newDxc.p[1] = 8.9e-3; newDxc.final[1] = mapDxc["Ar_2P3"];
      newDxc.p[2] = 3.4e-3; newDxc.final[2] = mapDxc["Ar_2P2"];
      newDxc.p[3] = 1.9e-3; newDxc.final[3] = mapDxc["Ar_2P1"];
    } else if (level == "Ar_3S1!") {
      newDxc.osc = 0.106;
      int nc = 7; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 0.318;   newDxc.final[0] = -1;
      newDxc.p[1] = 3.96e-4; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 4.2e-4;  newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 4.5e-3;  newDxc.final[3] = mapDxc["Ar_2P4"];
      newDxc.p[4] = 7.1e-3;  newDxc.final[4] = mapDxc["Ar_2P2"];
      newDxc.p[5] = 5.2e-3;  newDxc.final[5] = mapDxc["Ar_2P1"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[6] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[6] = -1;
    } else if (level == "Ar_4D5") {
      newDxc.osc = 0.0019;
      int nc = 7; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 2.78e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.8e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 8.6e-4;  newDxc.final[2] = mapDxc["Ar_2P6"];
      newDxc.p[3] = 9.2e-4;  newDxc.final[3] = mapDxc["Ar_2P5"];
      newDxc.p[4] = 4.6e-4;  newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.6e-4;  newDxc.final[5] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[6] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[6] = -1;
    } else if (level == "Ar_3S4") {
      newDxc.osc = 0.0144;
      int nc = 10; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 4.21e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.e-3;   newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 1.7e-3;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 7.2e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 3.5e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 1.2e-4;  newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 4.2e-6;  newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 3.3e-5;  newDxc.final[7] = mapDxc["Ar_2P2"];
      newDxc.p[8] = 9.7e-5;  newDxc.final[8] = mapDxc["Ar_2P1"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[9] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[9] = -1;
    } else if (level == "Ar_4D2") {
      newDxc.osc = 0.048;
      int nc = 2; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 1.7e-4; newDxc.final[0] = mapDxc["Ar_2P7"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[1] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[1] = -1;
    } else if (level == "Ar_4S1!") {
      newDxc.osc = 0.0209;
      int nc = 7; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 1.05e-3; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 3.1e-5;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.5e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.e-4;   newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 5.8e-5;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 1.2e-4;  newDxc.final[5] = mapDxc["Ar_2P3"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[6] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[6] = -1;
    } else if (level == "Ar_3S2") {
      newDxc.osc = 0.0221;
      int nc = 10; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 2.85e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 5.1e-5;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 5.3e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 1.6e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 1.5e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 6.e-4;   newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 2.48e-3; newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 9.6e-4;  newDxc.final[7] = mapDxc["Ar_2P2"];
      newDxc.p[8] = 3.59e-4; newDxc.final[8] = mapDxc["Ar_2P1"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[9] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[9] = -1;
    } else if (level == "Ar_5D5") {
      newDxc.osc = 0.0041;
      int nc = 9; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 2.2e-3;  newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.1e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 7.6e-5;  newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 4.2e-4;  newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 2.4e-4;  newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 2.1e-4;  newDxc.final[5] = mapDxc["Ar_2P4"];
      newDxc.p[6] = 2.4e-4;  newDxc.final[6] = mapDxc["Ar_2P3"];
      newDxc.p[7] = 1.2e-4;  newDxc.final[7] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[8] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[8] = -1;
    } else if (level == "Ar_4S4") {
      newDxc.osc = 0.0139;
      int nc = 7; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 1.9e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 1.1e-3; newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 5.2e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 5.1e-4; newDxc.final[3] = mapDxc["Ar_2P6"];
      newDxc.p[4] = 9.4e-5; newDxc.final[4] = mapDxc["Ar_2P5"];
      newDxc.p[5] = 5.4e-5; newDxc.final[5] = mapDxc["Ar_2P4"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[6] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[6] = -1;
    } else if (level == "Ar_5D2") {
      newDxc.osc = 0.0426;
      int nc = 5; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 5.9e-5; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 9.e-6;  newDxc.final[1] = mapDxc["Ar_2P7"];
      newDxc.p[2] = 1.5e-4; newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 3.1e-5; newDxc.final[3] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[4] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[4] = -1;
    } else if (level == "Ar_6D5") {
      newDxc.osc = 0.0062;
      int nc = 7; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 1.9e-3;  newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 4.2e-4;  newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 3.e-4;   newDxc.final[2] = mapDxc["Ar_2P5"];
      newDxc.p[3] = 5.1e-5;  newDxc.final[3] = mapDxc["Ar_2P4"];
      newDxc.p[4] = 6.6e-5;  newDxc.final[4] = mapDxc["Ar_2P3"];
      newDxc.p[5] = 1.21e-4; newDxc.final[5] = mapDxc["Ar_2P1"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[6] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[6] = -1;
    } else if (level == "Ar_5S1!") {
      newDxc.osc = 0.0562;
      int nc = 2; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 7.7e-5; newDxc.final[0] = mapDxc["Ar_2P5"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[1] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[1] = -1;
    } else if (level == "Ar_4S2") {
      newDxc.osc = 0.0069;
      int nc = 8; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 4.5e-4; newDxc.final[0] = mapDxc["Ar_2P10"];
      newDxc.p[1] = 2.e-4;  newDxc.final[1] = mapDxc["Ar_2P8"];
      newDxc.p[2] = 2.1e-4; newDxc.final[2] = mapDxc["Ar_2P7"];
      newDxc.p[3] = 1.2e-4; newDxc.final[3] = mapDxc["Ar_2P5"];
      newDxc.p[4] = 1.8e-4; newDxc.final[4] = mapDxc["Ar_2P4"];
      newDxc.p[5] = 9.e-4;  newDxc.final[5] = mapDxc["Ar_2P3"];
      newDxc.p[6] = 3.3e-4; newDxc.final[6] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[7] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[7] = -1;
    } else if (level == "Ar_5S4") {
      newDxc.osc = 0.0211;
      int nc = 5; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      newDxc.p[0] = 3.6e-4; newDxc.final[0] = mapDxc["Ar_2P8"];
      newDxc.p[1] = 1.2e-4; newDxc.final[1] = mapDxc["Ar_2P6"];
      newDxc.p[2] = 1.5e-4; newDxc.final[2] = mapDxc["Ar_2P4"];
      newDxc.p[3] = 1.4e-4; newDxc.final[3] = mapDxc["Ar_2P2"];
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[4] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[4] = -1;
    } else if (level == "Ar_6D2") {
      newDxc.osc = 0.0574;
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[0] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[0] = -1;
    } else if (level == "Ar_High") {
      newDxc.osc = 0.0335;
      int nc = 1; newDxc.nChannels = nc;
      newDxc.p.resize(nc); newDxc.final.resize(nc); newDxc.type.resize(nc, 0);
      // Transition probability to ground state calculated from osc. strength
      newDxc.p[0] = f2A * pow(newDxc.energy, 2) * newDxc.osc; 
      newDxc.final[0] = -1;
    } else {
      std::cerr << className << "::ComputeDeexcitationTable:\n";
      std::cerr << "    Missing de-excitation data for level " 
                << level << ".\n";
      std::cerr << "    Program bug!\n";
      return;
    }
    deexcitations.push_back(newDxc);
  }
  
  if (debug) {
    std::cout << className << "::ComputeDeexcitationTable:\n"; 
    std::cout << "    Found " << nDeexcitations << " levels "
              << "with available radiative de-excitation data.\n";
  }

  // Collisional de-excitation channels
  const double dens = LoschmidtNumber * (pressure / AtmosphericPressure) * 
                         (273.15 / temperature);
  if (withAr) {
    // Add the Ar dimer ground state
    newDxc.label = "Ar_Dimer";
    newDxc.gas = iAr;
    newDxc.energy = 14.71;
    newDxc.osc = 0.;
    newDxc.p.clear(); newDxc.final.clear(); newDxc.type.clear();
    newDxc.nChannels = 0;
    mapDxc["Ar_Dimer"] = nDeexcitations;
    deexcitations.push_back(newDxc);
    ++nDeexcitations;
    // Add an Ar excimer level
    newDxc.label = "Ar_Excimer";
    newDxc.gas = iAr;
    newDxc.energy = 14.71;
    newDxc.osc = 0.;
    newDxc.p.clear(); newDxc.final.clear(); newDxc.type.clear();
    newDxc.nChannels = 0;
    mapDxc["Ar_Excimer"] = nDeexcitations;
    deexcitations.push_back(newDxc);
    ++nDeexcitations;
    // Calculate the collisional transition rates
    //   References:
    //     A. Bogaerts and R. Gijbels, J. Appl. Phys. 86 (1999), 4124-4133
    //     A. Bogaerts and R. Gijbels, Phys. Rev. A 52 (1995), 3743-3751
    // Hornbeck-Molnar ionisation
    const double fHM = 2.e-18 * dens * cAr;
    // Collisional losses
    // Two-body collision
    const double fLoss2b = 2.3e-24 * dens * cAr;
    // Three-body collision
    const double fLoss3b = 1.4e-41 * pow(dens * cAr, 2.);
    for (int j = nDeexcitations; j--;) {
      std::string level = deexcitations[j].label;
      // Collisional losses
      if (deexcitations[j].gas == iAr) {
        deexcitations[j].final.push_back(-1);
        deexcitations[j].final.push_back(mapDxc["Ar_Excimer"]);
        deexcitations[j].type.push_back(-1);
        deexcitations[j].type.push_back(-1);
        deexcitations[j].p.push_back(fLoss2b);
        deexcitations[j].p.push_back(fLoss3b);
        deexcitations[j].nChannels += 2;
      }
      // Hornbeck-Molnar ionisation
      if (level == "Ar_4D5"  || level == "Ar_3S4" || level == "Ar_4D2" ||
          level == "Ar_4S1!" || level == "Ar_3S2" || level == "Ar_5D5" ||
          level == "Ar_4S4"  || level == "Ar_5D2" || level == "Ar_6D5" ||
          level == "Ar_5S1!" || level == "Ar_4S2" || level == "Ar_5S4" ||
          level == "Ar_6D2"  || level == "Ar_High") {
        deexcitations[j].final.push_back(mapDxc["Ar_Dimer"]);
        deexcitations[j].type.push_back(1);
        deexcitations[j].p.push_back(fHM);
        deexcitations[j].nChannels += 1;
      }
    }
  }

  if (nComponents != 2) {
    std::cout << className << "::ComputeDeexcitationTable:\n";
    std::cout << "    Gas mixture has " << nComponents 
              << " components.\n";
    std::cout << "    Penning effects are only implemented for "
              << " binary mixtures.\n";
  } else if ((gas[0] == 2 && gas [1] == 8) || (gas[0] == 8 && gas[1] == 2)) {
    // Ar-CH4
    const double b3 = 22.121274;
    const double b4 = 3.842488;
    double fB = b4 / b3;
    double p = pressure / AtmosphericPressure;
    fB *= p * (1. - cAr);
    for (int j = nDeexcitations; j--;) {
      std::string level = deexcitations[j].label;
      if (level == "Ar_2P10" || level == "Ar_2P9" || level == "Ar_2P8" ||
          level == "Ar_2P7"  || level == "Ar_2P6" || level == "Ar_2P5" ||
          level == "Ar_2P4"  || level == "Ar_2P3" || level == "Ar_2P2" ||
          level == "Ar_2P1") {
        deexcitations[j].p.push_back(fB / 30.);
        deexcitations[j].final.push_back(-1);
        deexcitations[j].type.push_back(1);
        deexcitations[j].nChannels += 1;
      } else if (level == "Ar_3D6"     || level == "Ar_3D5"   ||
                 level == "Ar_3D3"     || level == "Ar_3D4!"  ||
                 level == "Ar_3D4"     || level == "Ar_3D1!!" ||
                 level == "Ar_2S5"     || level == "Ar_2S4"   ||
                 level == "Ar_3D1!"    || level == "Ar_3D2"   ||
                 level == "Ar_3S1!!!!" || level == "Ar_3S1!!" ||
                 level == "Ar_3S1!!!"  || level == "Ar_2S3"   ||
                 level == "Ar_2S2"     || level == "Ar_3S1!") {
        deexcitations[j].p.push_back(fB / 40.);
        deexcitations[j].final.push_back(-1);
        deexcitations[j].type.push_back(1);
        deexcitations[j].nChannels += 1;
      } else if (level == "Ar_4D5" || level == "Ar_3S4"  || 
                 level == "Ar_4D2" || level == "Ar_4S1!" ||
                 level == "Ar_3S2" || level == "Ar_5D5"  || 
                 level == "Ar_4S4" || level == "Ar_5D2"  ||
                 level == "Ar_6D5" || level == "Ar_5S1!" ||
                 level == "Ar_4S2" || level == "Ar_5S4"  ||
                 level == "Ar_6D2" || level == "Ar_High") {
        deexcitations[j].p.push_back(fB / 100.);
        deexcitations[j].final.push_back(-1);
        deexcitations[j].type.push_back(1);
        deexcitations[j].nChannels += 1;
      }
    }
  } else {
    std::cout << className << "::ComputeDeexcitationTable:\n";
    std::cout << "    No data on Penning effects found.\n";
  }

  if (debug) {
    std::cout << className << "::ComputeDeexcitationTable:\n";
    std::cout << "        Level                 Lifetimes [ns]\n";
    std::cout << "                   Total      Radiative         Collisional\n";
    std::cout << "                                         "
              << " Ionisation      Loss\n";
  }
  for (int i = 0; i < nDeexcitations; ++i) {
    deexcitations[i].rate = 0.;
    double fRad = 0., fCollIon = 0., fCollLoss = 0.;
    for (int j = deexcitations[i].nChannels; j--;) {
      deexcitations[i].rate += deexcitations[i].p[j];
      if (deexcitations[i].type[j] == 0) {
        fRad += deexcitations[i].p[j];
      } else if (deexcitations[i].type[j] == 1) {
        fCollIon += deexcitations[i].p[j];
      } else if (deexcitations[i].type[j] == -1) {
        fCollLoss += deexcitations[i].p[j];
      }
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
        if (fCollIon > 0.) {
          std::cout << std::setw(10) << 1. / fCollIon << "  ";
        } else {
          std::cout << "----------  ";
        }
        if (fCollLoss > 0.) {
          std::cout << std::setw(10) << 1. / fCollLoss << "\n";
        } else {
          std::cout << "----------  \n";
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
    if (deexcitations[iLevel].rate <= 0. || 
        deexcitations[iLevel].nChannels <= 0) return;
    // Determine the de-excitation time
    newDxcProd.t += -log(RndmUniformPos()) / deexcitations[iLevel].rate;
    // Select the transition
    int fLevel = -1;
    int type = 0;
    const double r = RndmUniform();
    for (int j = 0; j < deexcitations[iLevel].nChannels; ++j) {
      if (r <= deexcitations[iLevel].p[j]) {
        fLevel = deexcitations[iLevel].final[j];
        type = deexcitations[iLevel].type[j];
        break;
      }
    }
    if (type == 0) {
      // Radiative decay
      newDxcProd.type = DxcTypePhoton;
      newDxcProd.energy = deexcitations[iLevel].energy;
      if (fLevel >= 0) newDxcProd.energy -= deexcitations[fLevel].energy;
      dxcProducts.push_back(newDxcProd);
      ++nDeexcitationProducts;
    } else if (type == 1) {
      // Ionisation electron
      newDxcProd.type = DxcTypeElectron;
      newDxcProd.energy = deexcitations[iLevel].energy;
      if (fLevel >= 0) {
        newDxcProd.energy -= deexcitations[fLevel].energy;
      } else {
        // Penning transfer
        newDxcProd.energy -= minIonPot;
      }
      if (newDxcProd.energy < Small) newDxcProd.energy = Small;
      ++nPenning;
      dxcProducts.push_back(newDxcProd);
      ++nDeexcitationProducts;
    } 
    iLevel = fLevel;
  }

}

bool
MediumMagboltz86::ComputePhotonCollisionTable() {

  OpticalData data;
  double cs;
  double eta;
  std::string gasname;

  // Atomic density
  const double dens = LoschmidtNumber * (pressure / AtmosphericPressure) * 
                                        (273.15 / temperature);
  // Conversion factor from oscillator strength to cross-section
  const double f2cs = FineStructureConstant * 2 * Pi2 * HbarC * HbarC / 
                      ElectronMass;

  // Reset the collision rate arrays
  cfTotGamma.clear(); cfTotGamma.resize(nEnergyStepsGamma, 0.);
  cfGamma.clear(); cfGamma.resize(nEnergyStepsGamma);
  for (int j = nEnergyStepsGamma; j--;) cfGamma[j].clear();
  csTypeGamma.clear();

  nPhotonTerms = 0;
  for (int i = 0; i < nComponents; ++i) {
    const double prefactor = dens * SpeedOfLight * fraction[i];
    double a, z;
    GetGasInfo(gas[i], gasname, a, z);
    if (!data.IsAvailable(gasname)) return false;
    // Continuum
    csTypeGamma.push_back(i * 4 + 1);
    csTypeGamma.push_back(i * 4 + 2);
    nPhotonTerms += 2;
    for (int j = 0; j < nEnergyStepsGamma; ++j) {
      // Retrieve total photoabsorption cross-section and ionisation yield
      data.GetPhotoabsorptionCrossSection(gasname, j * eStepGamma, 
                                          cs, eta);
      cfTotGamma[j] += cs * prefactor;
      // Ionisation
      cfGamma[j].push_back(cs * prefactor * eta);
      // Inelastic absorption
      cfGamma[j].push_back(cs * prefactor * (1. - eta));
    }
    // Discrete excitations
    if (useDeexcitation && useRadTrap) {
      for (int k = 0; k < nDeexcitations; ++k) {
        if (deexcitations[k].gas != i) continue;
        if (deexcitations[k].osc < Small) continue;
        std::string label = deexcitations[k].label;
        if (deexcitations[k].energy < 0 || 
            deexcitations[k].energy > eFinalGamma) {
          std::cerr << className << "::ComputePhotonCollisionTable:\n"; 
          std::cerr << "    Warning: excitation energy of level " 
                    << label << "  "
                    << " is outside the currently set photon energy range.\n"; 
          continue;
        }
        csTypeGamma.push_back(-k - 1);
        for (int j = 0; j < nEnergyStepsGamma; ++j) cfGamma[j].push_back(0.);
        int ie = int(deexcitations[k].energy / eStepGamma);
        if (ie >= nEnergyStepsGamma) ie = nEnergyStepsGamma - 1;
        cfGamma[ie][nPhotonTerms] = prefactor * f2cs * deexcitations[k].osc;
        cfTotGamma[ie] += prefactor * f2cs * deexcitations[k].osc;
        ++nPhotonTerms;
      } 
    }
  }

  if (debug && useRadTrap && useDeexcitation) {
    std::cout << "MediumMagboltz::ComputePhotonCollisionTable:\n";
    std::cout << "   Discrete absorption levels:\n";
    for (int i = 0; i < nPhotonTerms; ++i) {
      if (csTypeGamma[i] < 0) {
        int level = -csTypeGamma[i] - 1;
        std::cout << "    " << std::setw(12) << deexcitations[level].label
                  << std::setw(10) << deexcitations[level].energy  
                  << " eV  " << std::setw(10)
                  << cfGamma[int(deexcitations[level].energy / eStepGamma)][i]
                  << " ns-1\n";
      }
    }
  }

  if (useCsOutput) {
    std::ofstream csfile;
    csfile.open("csgamma.txt", std::ios::out);
    for (int j = 0; j < nEnergyStepsGamma; ++j) {
      csfile << j * eStepGamma << "  ";
      for (int i = 0; i < nPhotonTerms; ++i) csfile << cfGamma[j][i] << "  ";
      csfile << "\n";
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
                              double& alpha, double& eta,
                              double& vxerr, double& vyerr, double& vzerr,
                              double& dlerr, double& dterr, 
                              double& alphaerr, double& etaerr,
                              double& alphapt, double& etapt,
                              double& alphapterr, double& etapterr,
                              double& alphatof) {

  // Set input parameters in Magboltz common blocks
  inpt_.nGas = nComponents;
  inpt_.nStep = 4000;

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

  // Calculate the max. energy in the table
  if (e * temperature / (293.15 * pressure) > 15) {
    // If E/p > 15 start with 8 eV 
    inpt_.efinal = 8.;
  } else {
    inpt_.efinal = 0.5;
  }
  setp_.estart = inpt_.efinal / 50.;

  long long ielow = 1;
  while (ielow == 1) {
    mixer_();
    if (bmag == 0. || btheta == 0. || fabs(btheta) == 180.) {
      elimit_(&ielow);
    } else if (btheta == 90.) {
      elimitb_(&ielow);
    } else {
      elimitc_(&ielow);
    }
    if (ielow == 1) {
      // Increase the max. energy
      inpt_.efinal *= sqrt(2.);
      setp_.estart = inpt_.efinal / 50.;
    }
  }

  if (verbose) prnter_();
  
  // Run the Monte Carlo calculation
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
  // include spatial gradients in the solution
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

  vx = vel_.wx * 1.e-9; vxerr = velerr_.dwx;
  vy = vel_.wy * 1.e-9; vyerr = velerr_.dwy;
  vz = vel_.wz * 1.e-9; vzerr = velerr_.dwz;

  dt = sqrt(0.2 * difvel_.diftr / vz) * 1.e-4; dterr = diferl_.dfter;
  dl = sqrt(0.2 * difvel_.difln / vz) * 1.e-4; dlerr = diferl_.dfler;
 
  alpha = ctowns_.alpha; alphaerr = ctwner_.alper;
  eta   = ctowns_.att;   etaerr = ctwner_.atter;
 
  alphapt = tofout_.ralpha; alphapterr = tofout_.ralper;
  etapt   = tofout_.rattof; etapterr   = tofout_.ratofer;
  double fc1 = 1.e5 * tofout_.tofwr / (2. * tofout_.tofdl);
  double fc2 = 1.e12 * (alphapt - etapt) / tofout_.tofdl;
  alphatof = fc1 - sqrt(fc1 * fc1 - fc2);

  if (verbose) {
    std::cout << className << "::RunMagboltz:\n";
    std::cout << "    Results: \n";
    std::cout << "      Drift velocity along E:   " << vz << " cm/ns "
              << " +/- " << vzerr << "%\n";
    std::cout << "      Drift velocity along Bt:  " << vx << " cm/ns "
              << " +/- " << vxerr << "%\n";
    std::cout << "      Drift velocity along ExB: " << vy << " cm/ns "
              << " +/- " << vyerr << "%\n";
    std::cout << "      Longitudinal diffusion:   " << dl << " sqrt(cm) "
              << " +/- " << dlerr << "%\n";
    std::cout << "      Transverse diffusion:     " << dt << " sqrt(cm) "
              << " +/- " << dterr << "%\n";
    std::cout << "      Townsend coefficient (SST): " << alpha << " cm-1"
              << " +/- " << alphaerr << "%\n";
    std::cout << "      Attachment coefficient (SST): " << eta << " cm-1"
              << " +/- " << etaerr << "%\n";
    std::cout << "      Effective Townsend coefficient (TOF): " 
              << alphatof << " cm-1\n";
  }

}

}
