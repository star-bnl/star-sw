#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

#include "MediumSilicon.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumSilicon::MediumSilicon() :
  Medium(), 
  bandGap(1.12), 
  dopingType('i'), dopingConcentration(0.),
  eEffMass(1.18), hEffMass(0.81),
  eLatticeMobility(1.35e-6), hLatticeMobility(0.48e-6),
  eMobility(1.43e-6), hMobility(0.46e-6),
  eBetaCanali(1.109), hBetaCanali(1.213),
  eSatVel(1.02e-2), hSatVel(0.72e-2),
  eHallFactor(1.15), hHallFactor(0.7),
  eTrapCs(1.e-15), hTrapCs(1.e-15),
  eTrapDensity(1.e13), hTrapDensity(1.e13),
  eTrapTime(0.), hTrapTime(0.),
  trappingModel(0),  
  eImpactA0(3.318e5), eImpactA1(0.703e6), eImpactA2(0.),
  eImpactB0(1.135e6), eImpactB1(1.231e6), eImpactB2(0.),
  hImpactA0(1.582e6), hImpactA1(0.671e6), hImpactA2(0.),
  hImpactB0(2.036e6), hImpactB1(1.693e6), hImpactB2(0.),
  userMobility(false), userSaturationVelocity(false),
  latticeMobilityModel(0),
  dopingMobilityModel(0),
  saturationVelocityModel(0),
  highFieldMobilityModel(0),
  impactIonisationModel(0),
  eFinal(2.), eStep(0.001),
  nLevels(0),
  cfNullElectrons(0.),
  hasOpticalData(false), opticalDataFile("OpticalData_Si.txt") {

  SetName("Si");
  SetTemperature(300.);
  SetDielectricConstant(11.9);
  SetAtomicNumber(14.);
  SetAtomicWeight(28.0855);
  SetMassDensity(2.329);
  
  EnableDrift();
  microscopic = true;

}

void 
MediumSilicon::SetDoping(const char type, const double c) {

  if (toupper(type) == 'N') {
    dopingType = 'n';
    if (c > 1.e-20) {
      dopingConcentration = c;
    } else {
      std::cerr << "MediumSilicon::SetDoping:" << std::endl;
      std::cerr << "    Doping concentration must be greater than zero." << std::endl
                << "    Using default value for n-type silicon (10^12 cm-3) instead." << std::endl;
      dopingConcentration = 1.e12;
    }
  } else if (toupper(type) == 'P') {
    dopingType = 'p';
    if (c > 1.e-20) {
      dopingConcentration = c;
    } else {
      std::cerr << "MediumSilicon::SetDoping:" << std::endl;
      std::cerr << "    Doping concentration must be greater than zero." << std::endl;
      std::cerr << "    Using default value for p-type silicon (10^18 cm-3) instead." << std::endl;
      dopingConcentration = 1.e18;
    }
  } else if (toupper(type) == 'I') {
    dopingType = 'i';
    dopingConcentration = 0.;
  } else {
    std::cerr << "MediumSilicon::SetDoping:" << std::endl;
    std::cerr << "    Unknown dopant type (" << type << ")." << std::endl;
    std::cerr << "    Available types are n, p and i (intrinsic)." << std::endl;
    return;
  }
  
  isChanged = true;
  
}

void 
MediumSilicon::GetDoping(char& type, double& c) const {

  type = dopingType; c = dopingConcentration;
  
}

void
MediumSilicon::SetTrapCrossSection(const double ecs, const double hcs) {

  if (ecs < 0.) {
    std::cerr << "MediumSilicon::SetTrapCrossSection:" << std::endl;
    std::cerr << "    Capture cross-section [cm2] must be greater than zero." << std::endl;
  } else {
    eTrapCs = ecs;
  }
  
  if (hcs < 0.) {
    std::cerr << "MediumSilicon::SetTrapCrossSection:" << std::endl;
    std::cerr << "    Capture cross-section [cm2] must be greater than zero." << std::endl;
  } else {
    hTrapCs = hcs;
  }
  
  trappingModel = 0;

}

void
MediumSilicon::SetTrapDensity(const double n) {

  if (n < 0.) {
    std::cerr << "MediumSilicon::SetTrapDensity:" << std::endl;
    std::cerr << "    Trap density [cm-3] must be greater than zero." << std::endl;
  } else {
    eTrapDensity = n;
    hTrapDensity = n;
  }
  
  trappingModel = 0;

}

void
MediumSilicon::SetTrappingTime(const double etau, const double htau) {

  if (etau <= 0.) {
    std::cerr << "MediumSilicon::SetTrappingTime:" << std::endl;
    std::cerr << "    Trapping time [ns-1] must be greater than zero." << std::endl;
  } else {
    eTrapTime = etau;
  }
  
  if (htau <= 0.) {
    std::cerr << "MediumSilicon::SetTrappingTime:" << std::endl;
    std::cerr << "    Trapping time [ns-1] must be greater than zero." << std::endl;
  } else {
    hTrapTime = htau;
  }
  
  trappingModel = 1;

}

bool 
MediumSilicon::ElectronVelocity(
            const double ex, const double ey, const double ez, 
            const double bx, const double by, const double bz, 
            double& vx, double& vy, double& vz) {

  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }

  const double e = sqrt(ex * ex + ey * ey + ez * ez);

  // Calculate the mobility
  double mu;
  switch (highFieldMobilityModel) {
    case 0:
      ElectronMobilityMinimos(e, mu);
      break;
    case 1:
      ElectronMobilityCanali(e, mu);
      break;
    case 2:
      ElectronMobilityReggiani(e, mu);
      break;
    default:
      mu = eMobility;
      break;
  }
  mu = -mu;

  // Hall mobility
  const double muH = eHallFactor * mu;
  // Compute drift velocity using the Langevin equation
  const double c1 = mu / (1. + muH * muH * (bx * bx + by * by + bz * bz));
  const double c2 = muH * muH * (bx * ex + by * ey + bz * ez);
  vx = c1 * (ex + muH * (ey * bz - ez * by) + c2 * bx);
  vy = c1 * (ey + muH * (ez * bx - ex * bz) + c2 * by);
  vz = c1 * (ez + muH * (ex * by - ey * bx) + c2 * bz);
  return true;

}

bool 
MediumSilicon::ElectronTownsend(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& alpha) {
                         
  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }
  
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  
  switch (impactIonisationModel) {
    case 0:
      return ElectronImpactIonisationVanOverstraetenDeMan(e, alpha);
      break;
    case 1:
      return ElectronImpactIonisationGrant(e, alpha);
      break;
    default:
      std::cerr << "MediumSilicon::ElectronTownsend:" << std::endl;
      std::cerr << "    Unknown model activated. Program bug!" << std::endl;    
      break;
  }  
  return false;
  
}  

bool 
MediumSilicon::ElectronAttachment(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& eta) {

  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }
  
  switch (trappingModel) {
    case 0:
      eta = eTrapCs * eTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = eTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << "MediumSilicon::ElectronAttachment:" << std::endl;
      std::cerr << "    Unknown model activated. Program bug!" << std::endl;
      return false;
      break;      
  }
    
  return true;

}            

bool 
MediumSilicon::HoleVelocity(
            const double ex, const double ey, const double ez, 
            const double bx, const double by, const double bz, 
            double& vx, double& vy, double& vz) {

  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }

  const double e = sqrt(ex * ex + ey * ey + ez * ez);

  // Calculate the mobility
  double mu;
  switch (highFieldMobilityModel) {
    case 0:
      HoleMobilityMinimos(e, mu);
      break;
    case 1:
      HoleMobilityCanali(e, mu);
      break;
    case 2:
      HoleMobilityReggiani(e, mu);
      break;
    default:
      mu = hMobility;
  }

  // Hall mobility
  const double muH = hHallFactor * mu;
  // Compute drift velocity using the Langevin equation
  const double c1 = mu / (1. + muH * muH * (bx * bx + by * by + bz * bz));
  const double c2 = muH * muH * (bx * ex + by * ey + bz * ez);
  vx = c1 * (ex + muH * (ey * bz - ez * by) + c2 * bx);
  vy = c1 * (ey + muH * (ez * bx - ex * bz) + c2 * by);
  vz = c1 * (ez + muH * (ex * by - ey * bx) + c2 * bz);
  return true;

}

bool 
MediumSilicon::HoleTownsend(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& alpha) {
                         
  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }
  
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  
  switch (impactIonisationModel) {
    case 0:
      return HoleImpactIonisationVanOverstraetenDeMan(e, alpha);
      break;
    case 1:
      return HoleImpactIonisationGrant(e, alpha);
      break;
    default:
      std::cerr << "MediumSilicon::HoleTownsend:" << std::endl;
      std::cerr << "    Unknown model activated. Program bug!" << std::endl;      
      break;
  }  
  return false;

}

bool 
MediumSilicon::HoleAttachment(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& eta) {

  if (isChanged) {
    UpdateTransportParameters();
    isChanged = false;
  }
  
  switch (trappingModel) {
    case 0:
      eta = hTrapCs * hTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = hTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << "MediumSilicon::HoleAttachment:" << std::endl;
      std::cerr << "    Unknown model activated. Program bug!" << std::endl;
      return false;
      break;      
  }
  
  return true;

}

void
MediumSilicon::SetLowFieldMobility(const double mue, const double muh) {

  if (mue <= 0. || muh <= 0.) {
    std::cerr << "MediumSilicon::SetLowFieldMobility:" << std::endl;
    std::cerr << "    Mobility must be greater than zero." << std::endl;
    return;
  }
  
  eMobility = mue;
  hMobility = muh;
  userMobility = true;
  isChanged = true;

}

void 
MediumSilicon::SetLatticeMobilityModelMinimos() {

  latticeMobilityModel = 0;  
  userMobility = false;
  isChanged = true;
  
}

void 
MediumSilicon::SetLatticeMobilityModelSentaurus() {

  latticeMobilityModel = 1;
  userMobility = false;
  isChanged = true;
  
}

void 
MediumSilicon::SetLatticeMobilityModelSah() {
  
  latticeMobilityModel = 2;
  userMobility = false;
  isChanged = true;
  
}

void
MediumSilicon::SetLatticeMobilityModelReggiani() {

  latticeMobilityModel = 3;
  userMobility = false;
  isChanged = true;

}

void 
MediumSilicon::SetDopingMobilityModelMinimos() {

  dopingMobilityModel = 0;
  userMobility = false;
  isChanged = true;
  
}

void 
MediumSilicon::SetDopingMobilityModelMasetti() {

  dopingMobilityModel = 1;
  userMobility = false;
  isChanged = true;
  
}

void
MediumSilicon::SetSaturationVelocity(const double vsate, const double vsath) {

  if (vsate <= 0. || vsath <= 0.) {
    std::cout << "MediumSilicon::SetSaturationVelocity:" << std::endl;
    std::cout << "    Restoring default values." << std::endl;
    userSaturationVelocity = false;
  } else {
    eSatVel = vsate; hSatVel = vsath;
    userSaturationVelocity = true;
  }
  
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelMinimos() {

  saturationVelocityModel = 0;
  userSaturationVelocity = false;
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelCanali() {

  saturationVelocityModel = 1;
  userSaturationVelocity = false;
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelReggiani() {

  saturationVelocityModel = 2;
  userSaturationVelocity = false;
  isChanged = true;

}

void 
MediumSilicon::SetHighFieldMobilityModelMinimos() {

  highFieldMobilityModel = 0;
  isChanged = true;
  
}

void 
MediumSilicon::SetHighFieldMobilityModelCanali() {

  highFieldMobilityModel = 1;
  isChanged = true;
  
}

void
MediumSilicon::SetHighFieldMobilityModelReggiani() {

  highFieldMobilityModel = 2;
  isChanged = true;

}

void
MediumSilicon::SetHighFieldMobilityModelConstant() {

  highFieldMobilityModel = 3;
  
}

void 
MediumSilicon::SetImpactIonisationModelVanOverstraetenDeMan() {

  impactIonisationModel = 0;
  isChanged = true;  

}

void
MediumSilicon::SetImpactIonisationModelGrant() {

  impactIonisationModel = 1;
  isChanged = true;
 
}

bool
MediumSilicon::SetMaxEnergy(const double e) {

  if (e <= 1.e-20) {
    std::cerr << "MediumSilicon::SetMaxEnergy:" << std::endl;
    std::cerr << "    Provided upper electron energy limit (" << e
              << " eV) is too small." << std::endl;
    return false;
  }

  eFinal = e;
  // Determine the energy interval size
  eStep = eFinal / nEnergySteps;

  isChanged = true;

  return true;

}

double
MediumSilicon::GetElectronNullCollisionRate() {

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << "MediumSilicon: Error calculating the"
                << " collision rates table." << std::endl;
      return 0.;
    }
    isChanged = false;
  }

  return cfNullElectrons;

}

double
MediumSilicon::GetElectronCollisionRate(const double e) {

  if (e <= 0.) {
    std::cerr << "MediumSilicon: Electron energy must be greater than zero."
              << std::endl;
    return cfTotElectrons[0];
  }

  if (e > eFinal) {
    std::cerr << "MediumSilicon::GetElectronCollisionRate:" << std::endl;
    std::cerr << "    Collision rate at " << e
              << " eV is not included in the current table." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxEnergy(1.05 * e);
  }

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << "MediumSilicon: Error calculating the"
                << "  collision rates table." << std::endl;
      return 0.;
    }
    isChanged = false;
  }

  return cfTotElectrons[int(e / eStep)];

}

bool
MediumSilicon::GetElectronCollision(const double e, int& type, int& level,
                     double& e1, double& ctheta, double& s, double& esec) {

  if (e > eFinal) {
    std::cerr << "MediumSilicon::GetElectronCollision:" << std::endl;
    std::cerr << "    Provided electron energ (" << e;
    std::cerr << " eV) exceeds current energy range (" << eFinal;
    std::cerr << " eV)." << std::endl;
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV." << std::endl;
    SetMaxEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << "MediumSilicon::GetElectronCollision:" << std::endl;
    std::cerr << "    Electron energy must be greater than zero." << std::endl;
    return false;
  }

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << "MediumSilicon: Error calculating"
                << " the collision rates table." << std::endl;
      return false;
    }
    isChanged = false;
  }

  // Energy interval
  const int iE = int(e / eStep);

  const double r = RndmUniform();
  int iLow = 0; 
  int iUp = nLevels - 1;
  if (r <= cfElectrons[iE][iLow]) {
    level = iLow;
  } else if (r >= cfElectrons[iE][nLevels - 1]) {
    level = nLevels - 1;
  } else {
    int iMid;
    while (iUp - iLow > 1) {
      iMid = (iLow + iUp) >> 1;
      if (r < cfElectrons[iE][iMid]) {
        iUp = iMid;
      } else {
        iLow = iMid;
      }
    }
    level = iUp;
  }

  // Collision type
  type = scatTypeElectrons[level];
  
  // Scattering angle
  ctheta = 1. - 2. * RndmUniform();

  // Energy loss
  double loss = energyLossElectrons[level];
  // Secondary electron energy (none by default)
  esec = 0.;
  // Ionising collision
  if (type == 1) {
    esec = RndmUniform() * (e - loss);
    loss += esec;
    if (esec < Small) esec = 1.e-20;
  }

  if (e < loss) loss = e - 0.0001;
  // Update the energy
  e1 = e - loss;
  if (e1 < 1.e-20) e1 = 1.e-20;
  
  return true;

}

bool 
MediumSilicon::GetOpticalDataRange(double& emin, double& emax, const int i) {

  if (i != 0) {
    std::cerr << "MediumSilicon::GetOpticalDataRange:" << std::endl;
    std::cerr << "    Only one component available." << std::endl;
  }

  if (!hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << "MediumSilicon::GetOpticalDataRange:" << std::endl;
      std::cerr << "    Error loading the optical data table." << std::endl;
      return false;
    }
    hasOpticalData = true;
  }
    
  emin = opticalDataTable[0].energy;
  emax = opticalDataTable.back().energy;
  return true;  
  
}

bool 
MediumSilicon::GetDielectricFunction(const double e, 
                                    double& eps1, double& eps2, const int i) {
                        
  if (i != 0) {
    std::cerr << "MediumSilicon::GetDielectricFunction:" << std::endl;
    std::cerr << "    Only one component available." << std::endl;
  }
                        
  if (!hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << "MediumSilicon::GetDielectricFunction:" << std::endl;
      std::cerr << "    Error loading the optical data table." << std::endl;
      return false;
    }
    hasOpticalData = true;
  }
  
  const double emin = opticalDataTable[0].energy;
  const double emax = opticalDataTable.back().energy;    
  if (e < emin || e > emax) {
    std::cerr << "MediumSilicon::GetDielectricFunction:" << std::endl;
    std::cerr << "    Requested energy (" << e << " eV) " 
              << " is outside the range of the optical data table "
              << "(" << emin << " eV < E < " << emax << " eV)." << std::endl;
    eps1 = eps2 = 0.;
    return false;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = opticalDataTable.size() - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (e >= opticalDataTable[iM].energy) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }
  
  // Real part of dielectric function: use linear interpolation if crossing zero, log-log otherwise
  const double logX0 = log(opticalDataTable[iLow].energy);
  const double logX1 = log(opticalDataTable[iUp].energy);
  const double logX = log(e);
  double logY0, logY1;
  if (opticalDataTable[iLow].eps1 <= 0. || opticalDataTable[iUp].eps1 <= 0.) {
    eps1 = opticalDataTable[iLow].eps1 + (e - opticalDataTable[iLow].energy) * 
           (opticalDataTable[iUp].eps1 - opticalDataTable[iLow].eps1) / 
          (opticalDataTable[iUp].energy - opticalDataTable[iLow].energy);  
  } else {
    logY0 = log(opticalDataTable[iLow].eps1);
    logY1 = log(opticalDataTable[iUp].eps1);
    eps1 =  logY0 + (logX - logX0) * (logY1 - logY0) / (logX1 - logX0);
    eps1 = exp(eps1);
  }
      
  // Imaginary part of dielectric function: use log-log interpolation
  logY0 = log(opticalDataTable[iLow].eps2);
  logY1 = log(opticalDataTable[iUp].eps2);  
  eps2 = logY0 + (log(e) - logX0) * (logY1 - logY0) / (logX1 - logX0);
  eps2 = exp(eps2);
  return true;
  
}

bool 
MediumSilicon::UpdateTransportParameters() {

  // Calculate impact ionisation coefficients
  switch (impactIonisationModel) {
    case 0:
      UpdateImpactIonisationVanOverstraetenDeMan();
      break;
    case 1:
      UpdateImpactIonisationGrant();
      break;
    default:
      std::cerr << "MediumSilicon::UpdateTransportParameters:" << std::endl;
      std::cerr << "    Unknown impact ionisation model. Bug!" << std::endl;
      break;
  }
  
  if (!userMobility) {        
    // Calculate lattice mobility
    switch (latticeMobilityModel) {
      case 0:
        UpdateLatticeMobilityMinimos();
        break;
      case 1:
        UpdateLatticeMobilitySentaurus();
        break;   
      case 2:
        UpdateLatticeMobilitySah();
        break;     
      case 3:
        UpdateLatticeMobilityReggiani();
        break; 
      default:
        std::cerr << "MediumSilicon::UpdateTransportParameters:" 
                  << std::endl;
        std::cerr << "    Unknown lattice mobility model. Program bug!" 
                  << std::endl;
        break;
    }

  
    // Calculate doping mobility
    switch (dopingMobilityModel) {
      case 0:
        UpdateDopingMobilityMinimos();
        break;
      case 1:
        UpdateDopingMobilityMasetti();
        break;
      default:
        std::cerr << "MediumSilicon::UpdateTransportParameters:" 
                  << std::endl;
        std::cerr << "    Unknown doping mobility model. Program bug!" 
                  << std::endl;
        break;
    }
  }
      
  // Calculate saturation velocity
  if (!userSaturationVelocity) {
    switch (saturationVelocityModel) {
      case 0:
        UpdateSaturationVelocityMinimos();
        break;
      case 1:
        UpdateSaturationVelocityCanali();
        break;
      case 2:
        UpdateSaturationVelocityReggiani();
        break;
    }
  }

  // Calculate high field saturation parameters
  switch (highFieldMobilityModel) {
    case 1:
      UpdateHighFieldMobilityCanali();
      break;
  }
  
  if (debug) {
    std::cout << "MediumSilicon::UpdateTransportParameters:" << std::endl;
    std::cout << "    Low-field mobility [cm2 V-1 ns-1]" << std::endl;
    std::cout << "      Electrons: " << eMobility << std::endl;
    std::cout << "      Holes:     " << hMobility << std::endl;
    if (highFieldMobilityModel > 2) {
      std::cout << "    Mobility is not field-dependent." << std::endl;
    } else {
      std::cout << "    Saturation velocity [cm / ns]" << std::endl;
      std::cout << "      Electrons: " << eSatVel << std::endl;
      std::cout << "      Holes:     " << hSatVel << std::endl;
    }
  }

  if (!ElectronScatteringRates()) return false;
  return true;

}

void 
MediumSilicon::UpdateLatticeMobilityMinimos() {

  // References:
  // - S. Selberherr, W. Haensch, M. Seavey, J. Slotboom,
  //   Solid State Electronics 33 (1990), 1425-1436
  // - Minimos 6.1 User's Guide (1999)

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.43e-6; 
  const double hMu0 = 0.46e-6;
  // Temperature normalized to 300 K
  const double t = temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.);
  hLatticeMobility = hMu0 * pow(t, -2.18);
  
}

void 
MediumSilicon::UpdateLatticeMobilitySentaurus() {

  // References:
  // - C. Lombardi et al.,
  //   IEEE Trans. CAD 7 (1988), 1164-1171
  // - Sentaurus Device User Guide (2007)

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.417e-6; 
  const double hMu0 = 0.4705e-6;
  // Temperature normalized to 300 K
  const double t = temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.5);
  hLatticeMobility = hMu0 * pow(t, -2.2);
  
}

void 
MediumSilicon::UpdateLatticeMobilitySah() {

  // References:
  // - S. Selberherr, 
  //   Analysis and Simulation of Semiconductor Devices
  //   Springer 1984
  // - C. Sah et al.,
  //   IEEE Trans. Electron Devices 28 (1981), 304-313

  // Temperature normalized to 300 K
  const double t = temperature / 300.;

  eLatticeMobility = 1. / (4.195e-6 * pow(t, -1.5)) + 
                     1. / (2.153e-6 * pow(t, -3.13));
  hLatticeMobility = 1. / (2.502e-6 * pow(t, -1.5)) + 
                     1. / (0.591e-6 * pow(t, -3.25));
  eLatticeMobility = 1. / eLatticeMobility;
  hLatticeMobility = 1. / hLatticeMobility;

}

void
MediumSilicon::UpdateLatticeMobilityReggiani() {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.320e-6; 
  const double hMu0 = 0.460e-6;
  // Temperature normalized to 300 K
  const double t = temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.);
  hLatticeMobility = hMu0 * pow(t, -2.2);

}

void
MediumSilicon::UpdateDopingMobilityMinimos() {

  // References:
  // - S. Selberherr, W. Haensch, M. Seavey, J. Slotboom,
  //   Solid State Electronics 33 (1990), 1425-1436
  // - Minimos 6.1 User's Guide (1999)

  // Mobility reduction due to ionised impurity scattering
  // Surface term not taken into account
  double eMuMin = 0.080e-6;
  double hMuMin = 0.045e-6;
  if (temperature > 200.) {
    eMuMin *= pow(temperature / 300., -0.45);
    hMuMin *= pow(temperature / 300., -0.45);
  } else {
    eMuMin *= pow(2. / 3., -0.45) * pow(temperature / 200., -0.15);
    hMuMin *= pow(2. / 3., -0.45) * pow(temperature / 200., -0.15);
  }
  const double eRefC = 1.12e17 * pow(temperature / 300., 3.2);
  const double hRefC = 2.23e17 * pow(temperature / 300., 3.2);
  const double alpha = 0.72 * pow(temperature / 300., 0.065);
  // Assume impurity concentration equal to doping concentration
  eMobility =  eMuMin + (eLatticeMobility - eMuMin) / 
               (1. + pow(dopingConcentration / eRefC, alpha));
  hMobility =  hMuMin + (hLatticeMobility - hMuMin) / 
               (1. + pow(dopingConcentration / hRefC, alpha));

}

void 
MediumSilicon::UpdateDopingMobilityMasetti() {

  // Reference:
  // - G. Masetti, M. Severi, S. Solmi,
  //   IEEE Trans. Electron Devices 30 (1983), 764-769
  // - Sentaurus Device User Guide (2007)
  // - Minimos NT User Guide (2004)

  if (dopingConcentration < 1.e13) {
    eMobility = eLatticeMobility;
    hMobility = hLatticeMobility;
    return;
  }
  
  // Parameters taken from Minimos NT User Guide
  const double eMuMin1 = 0.0522e-6;
  const double eMuMin2 = 0.0522e-6;
  const double eMu1    = 0.0434e-6;
  const double hMuMin1 = 0.0449e-6;
  const double hMuMin2 = 0.;
  const double hMu1    = 0.029e-6;
  const double eCr = 9.68e16;
  const double eCs = 3.42e20;
  const double hCr = 2.23e17;
  const double hCs = 6.10e20;
  const double hPc = 9.23e16;
  const double eAlpha = 0.68;
  const double eBeta = 2.;
  const double hAlpha = 0.719;
  const double hBeta = 2.;
  
  eMobility = eMuMin1 + (eLatticeMobility - eMuMin2) / 
                        (1. + pow(dopingConcentration / eCr, eAlpha)) - eMu1 / 
                        (1. + pow(eCs / dopingConcentration, eBeta));

  hMobility = hMuMin1 * exp(-hPc / dopingConcentration) + 
                        (hLatticeMobility - hMuMin2) / 
                        (1. + pow(dopingConcentration / hCr, hAlpha)) - hMu1 / 
                        (1. + pow(hCs / dopingConcentration, hBeta));

}
  

void 
MediumSilicon::UpdateSaturationVelocityMinimos() {

  // References:
  // - R. Quay, C. Moglestue, V. Palankovski, S. Selberherr,
  //   Materials Science in Semiconductor Processing 3 (2000), 149-155
  // - Minimos NT User Guide (2004)
  
  // Temperature-dependence of saturation velocities [cm / ns]
  eSatVel = 1.e-2    / (1. + 0.74 * (temperature / 300. - 1.));
  hSatVel = 0.704e-2 / (1. + 0.37 * (temperature / 300. - 1.));
  
}

void 
MediumSilicon::UpdateSaturationVelocityCanali() {

  // References:
  // - C. Canali, G. Majni, R. Minder, G. Ottaviani,
  //   IEEE Transactions on Electron Devices 22 (1975), 1045-1047
  // - Sentaurus Device User Guide (2007)

  eSatVel = 1.07e-2 * pow(300. / temperature, 0.87);
  hSatVel = 8.37e-3 * pow(300. / temperature, 0.52);

}

void
MediumSilicon::UpdateSaturationVelocityReggiani() {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  eSatVel = 1.470e-2 * sqrt(tanh(150. / temperature));
  hSatVel = 0.916e-2 * sqrt(tanh(300. / temperature));

}

void
MediumSilicon::UpdateHighFieldMobilityCanali() {

  // References:
  // - C. Canali, G. Majni, R. Minder, G. Ottaviani,
  //   IEEE Transactions on Electron Devices 22 (1975), 1045-1047
  // - Sentaurus Device User Guide (2007)

  // Temperature dependent exponent in high-field mobility formula
  eBetaCanali = 1.109 * pow(temperature / 300., 0.66);
  hBetaCanali = 1.213 * pow(temperature / 300., 0.17);

}

void 
MediumSilicon::UpdateImpactIonisationVanOverstraetenDeMan() {

  // References:
  //  - R. van Overstraeten and H. de Man, 
  //    Solid State Electronics 13 (1970), 583-608
  //  - W. Maes, K. de Meyer and R. van Overstraeten, 
  //    Solid State Electronics 33 (1990), 705-718
  // - Sentaurus Device User Guide (2007)

  // Temperature dependence as in Sentaurus Device
  // Optical phonon energy
  double hbarOmega = 0.063;
  // Temperature scaling coefficient
  double gamma = tanh(hbarOmega / (2. * BoltzmannConstant * 300.)) /
                 tanh(hbarOmega / (2. * BoltzmannConstant * temperature));
  
  // Low field coefficients taken from Maes, de Meyer, van Overstraeten
  eImpactA0 = gamma * 3.318e5; 
  eImpactB0 = gamma * 1.135e6;
  eImpactA1 = gamma * 7.03e5;
  eImpactB1 = gamma * 1.231e6;
  
  hImpactA0 = gamma * 1.582e6;
  hImpactB0 = gamma * 2.036e6;
  hImpactA1 = gamma * 6.71e5;
  hImpactB1 = gamma * 1.693e6;

}

void 
MediumSilicon::UpdateImpactIonisationGrant() {

  // References:
  // - W. N. Grant, 
  //   Solid State Electronics 16 (1973), 1189 - 1203
  // - Sentaurus Device User Guide (2007)

  // Temperature dependence as in Sentaurus Device
  // Optical phonon energy
  double hbarOmega = 0.063;
  // Temperature scaling coefficient
  double gamma = tanh(hbarOmega / (2. * BoltzmannConstant * 300.)) /
                 tanh(hbarOmega / (2. * BoltzmannConstant * temperature));
                 
  eImpactA0 = 2.60e6 * gamma;
  eImpactB0 = 1.43e6 * gamma;
  eImpactA1 = 6.20e5 * gamma;
  eImpactB1 = 1.08e6 * gamma;
  eImpactA2 = 5.05e5 * gamma;
  eImpactB2 = 9.90e5 * gamma;
  
  hImpactA0 = 2.00e6 * gamma;
  hImpactB0 = 1.97e6 * gamma;
  hImpactA1 = 5.60e5 * gamma;
  hImpactB1 = 1.32e6 * gamma;
  
}

bool 
MediumSilicon::ElectronMobilityMinimos(const double e, double& mu) const {

  // Reference:
  // - Minimos User's Guide (1999)
  
  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = 2. * eMobility / 
         (1. + sqrt(1. + pow(2. * eMobility * e / eSatVel, 2.)));
  }  
  return true;
  
}

bool 
MediumSilicon::ElectronMobilityCanali(const double e, double& mu) const {

  // Reference:
  // - Sentaurus Device User Guide (2007)
  
  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = eMobility / 
         pow(1. + pow(eMobility * e / eSatVel, eBetaCanali), 1. / eBetaCanali);
  }  
  return true;
  
}

bool
MediumSilicon::ElectronMobilityReggiani(const double e, double& mu) const {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = eMobility / pow(1 + pow(eMobility * e / eSatVel, 1.5), 1. / 1.5);
  }
  return true;

}

bool 
MediumSilicon::ElectronImpactIonisationVanOverstraetenDeMan(const double e, 
                                                         double& alpha) const {

  // References:
  //  - R. van Overstraeten and H. de Man, 
  //    Solid State Electronics 13 (1970), 583-608
  //  - W. Maes, K. de Meyer and R. van Overstraeten, 
  //    Solid State Electronics 33 (1990), 705-718

  if (e < 1.e-20) {
    alpha = 0.;
  } else if (e < 1.2786e5) {
    alpha = eImpactA0 * exp(-eImpactB0 / e);
  } else {
    alpha = eImpactA1  * exp(-eImpactB1 / e);
  }
  return true;

}

bool 
MediumSilicon::ElectronImpactIonisationGrant(const double e, 
                                             double& alpha) const {

  // Reference:
  //  - W. N. Grant, Solid State Electronics 16 (1973), 1189 - 1203

  if (e < 1.e-20) {
    alpha = 0.;
  } else if (e < 2.4e5) {
    alpha = eImpactA0 * exp(-eImpactB0 / e);
  } else if (e < 5.3e5) {
    alpha = eImpactA1 * exp(-eImpactB1 / e);
  } else {
    alpha = eImpactA2 * exp(-eImpactB2 / e);
  }
  return true;

}

bool 
MediumSilicon::HoleMobilityMinimos(const double e, double& mu) const {

  // Reference:
  // - Minimos User's Guide (1999)

  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = hMobility / (1. + hMobility * e / eSatVel);
  }  
  return true;
  
}

bool 
MediumSilicon::HoleMobilityCanali(const double e, double& mu) const {

  // Reference:
  // - Sentaurus Device User Guide (2007)
  
  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = hMobility / 
         pow(1. + pow(hMobility * e / hSatVel, hBetaCanali), 1. / hBetaCanali);
  }  
  return true;
  
}

bool 
MediumSilicon::HoleMobilityReggiani(const double e, double& mu) const {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  if (e < 1.e-20) {
    mu = 0.;
  } else {
    mu = hMobility / 
         pow(1. + pow(hMobility * e / hSatVel, 2.), 0.5);
  }
  return true;

}

bool 
MediumSilicon::HoleImpactIonisationVanOverstraetenDeMan(const double e, 
                                                       double& alpha) const {

  // Reference:
  //  - R. van Overstraeten and H. de Man, 
  //    Solid State Electronics 13 (1970), 583-608

  if (e < 1.e-20) {
    alpha = 0.;
  } else {
    alpha = hImpactA1 * exp(-hImpactB1 / e);
  }
  return true;

}

bool 
MediumSilicon::HoleImpactIonisationGrant(const double e, double& alpha) const {

  // Reference:
  //  - W. N. Grant, Solid State Electronics 16 (1973), 1189 - 1203

  if (e < 1.e-20) {
    alpha = 0.;
  } else if (e < 5.3e5) {
    alpha = hImpactA0 * exp(-hImpactB0 / e);
  } else {
    alpha = hImpactA1 * exp(-hImpactB1 / e);
  }
  return true;

}


bool 
MediumSilicon::LoadOpticalData(const std::string filename) {

  // Open the file
  std::ifstream infile;
  infile.open(filename.c_str(), std::ios::in);
  // Check if the file could be opened
  if (!infile) {
    std::cerr << "MediumSilicon::LoadOpticalData:" << std::endl;
    std::cerr << "    Error opening file " << filename 
              << "." << std::endl;
    return false;
  }
  
  // Clear the optical data table
  opticalDataTable.clear();
  
  double lastEnergy = -1.;
  double energy, eps1, eps2, loss;  
  opticalData data;
  // Read the file line by line
  std::string line;
  std::istringstream dataStream;  
  int i = 0;
  while (!infile.eof()) {
    i++;
    std::getline(infile, line);
    // Strip white space from beginning of line
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments
    if (line[0] == '#') continue;
    // Get the values in the line
    dataStream.str(line);
    dataStream >> energy >> eps1 >> eps2 >> loss;
    dataStream.clear();
    // Check if the data has been read correctly
    if (infile.fail()) {
      std::cerr << "MediumSilicon::LoadOpticalData:" << std::endl;
      std::cerr << "    Error reading file "
                << filename << " (line " << i << ")." << std::endl;
      return false;
    }
    // Check if the provided values make sense
    if (energy <= lastEnergy) {
      std::cerr << "MediumSilicon::LoadOpticalData:" << std::endl;
      std::cerr << "    Optical data table is not in monotonically " 
                << "increasing order (line " << i << ")." << std::endl;
      return false;
    }
    if (eps2 <= 0.) {
      std::cerr << "MediumSilicon::LoadOpticalData:" << std::endl;
      std::cerr << "    Negative value of loss function "
                << "(line " << i << ")." << std::endl;
      return false;
    }
    if (energy <= 0.) continue;
    data.energy = energy;
    data.eps1 = eps1;
    data.eps2 = eps2;
    opticalDataTable.push_back(data);
    lastEnergy = energy;
  }
  
  int nEntries = opticalDataTable.size();
  if (nEntries <= 0) return false;
  
  return true;

}


bool
MediumSilicon::ElectronScatteringRates() {

  // Reset the scattering rates to zero
  for (int i = nEnergySteps; i--;) {
    cfTotElectrons[i] = 0.;
    for (int j = nMaxLevels; j--;) {
      cfElectrons[i][j] = 0.;
    }
  }
  cfNullElectrons = 0.;
  
  nLevels = 0;
  // Fill the scattering rates table
  ElectronAcousticScatteringRates(nLevels);
  ElectronImpurityScatteringRates(nLevels);
  ElectronIntervalleyScatteringRates(nLevels);
  ElectronIonisationRates(nLevels);

  std::ofstream outfile;  
  if (debug) outfile.open("rates.txt", std::ios::out);

  for (int i = 0; i < nEnergySteps; ++i) {
    // Sum up the scattering rates of all processes 
    for (int j = nLevels; j--;) cfTotElectrons[i] += cfElectrons[i][j];
    
    if (debug) {
      outfile << i * eStep << " " << cfTotElectrons[i] << " ";
      for (int j = 0; j < nLevels; ++j) outfile << cfElectrons[i][j] << " ";
      outfile << std::endl;
    }

    if (cfTotElectrons[i] > cfNullElectrons) {
      cfNullElectrons = cfTotElectrons[i];
    }
   
    // Make sure the total scattering rate is positive
    if (cfTotElectrons[i] <= 0.) { 
      std::cerr << "MediumSilicon::ElectronScatteringRates:" << std::endl;
      std::cerr << "    Scattering rate at " << i * eStep << " <= 0." 
                << std::endl;
      return false;
    }

    // Normalise the rates
    for (int j = 0; j < nLevels; ++j) {
      cfElectrons[i][j] /= cfTotElectrons[i];
      if (j > 0) cfElectrons[i][j] += cfElectrons[i][j - 1];
    }  

  }
  if (debug) outfile.close();

  return true;

}

bool 
MediumSilicon::ElectronAcousticScatteringRates(int& iLevel) {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [g/cm3]
  const double rho = GetMassDensity();
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  // Band parameters
  // Longitudinal and transverse effective electron masses
  const double ml = 0.98; 
  const double mt = 0.19;
  // Density of states effective mass
  const double md = pow(ml * mt * mt, 1. / 3.);
  // Non-parabolicity coefficient [eV-1]
  const double alpha = 0.5;
   
  // Acoustic phonon intraband scattering  
  // Acoustic deformation potential [eV]
  const double defpot = 9.;
  // Longitudinal and transverse velocity of sound [cm/ns]
  const double ut = 9.0e-4;
  const double ul = 5.3e-4;
  // Average velocity of sound [cm/ns]
  const double u = (ul + 2. * ut) / 3.;    
  // Prefactor for acoustic deformation potential scattering
  const double cIntra = 
    sqrt(2. * md * ElectronMass) * md * ElectronMassGramme * 
    kbt * defpot * defpot /
    (Pi * HbarC * pow(Hbar, 3.) * u * u * rho);

  double en = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    cfElectrons[i][iLevel] = cIntra * sqrt(en * (1. + alpha * en)) * 
                             (1. + 2. * alpha * en);
    en += eStep;
  }  
  
  energyLossElectrons[iLevel] = 0.;
  scatTypeElectrons[iLevel] = 0;
  ++iLevel;

  return true;

}

bool 
MediumSilicon::ElectronIntervalleyScatteringRates(int& iLevel) {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [g/cm3]
  const double rho = GetMassDensity();
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  // Band parameters
  // Longitudinal and transverse effective electron masses
  const double ml = 0.98; 
  const double mt = 0.19;
  // Density of states effective mass
  const double md = pow(ml * mt * mt, 1. / 3.);
  // Non-parabolicity coefficient [eV-1]
  const double alpha = 0.5;

  const int nPhonons = 6;
  // Coupling constants [eV/cm]
  const double dtk[nPhonons] = {0.5e8, 0.8e8, 11.0e8, 
                                0.3e8, 2.0e8,  2.0e8};
  // Phonon energies [eV]
  const double eph[nPhonons] = {12.1e-3, 18.5e-3, 62.0e-3, 
                                19.0e-3, 47.4e-3, 59.0e-3};
  // Occupation numbers
  double nocc[nPhonons];
  // Prefactors
  const double c0 = 
    sqrt(md * ElectronMass) * md * ElectronMassGramme /
    (sqrt(2.) * Pi * HbarC * Hbar * rho);
  double c[nPhonons];

  for (int j = 0; j < 6; ++j) {
    nocc[j] = 1. / (exp(eph[j] / kbt) - 1);
    c[j] = c0 * dtk[j] * dtk[j] / eph[j];
    if (j > 2) c[j] *= 4;
  }

  double en = 0.;
  double ef = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    int k = iLevel;
    for (int j = 0; j < nPhonons; ++j) {
     // Absorption
      ef = en + eph[j];
      cfElectrons[i][k] = c[j] * nocc[j] * 
                          sqrt(ef * (1. + alpha * ef)) * 
                           (1. + 2. * alpha * ef);
      
      ++k;
      // Emission
      ef = en - eph[j];
      if (ef > 1.e-20) {
        cfElectrons[i][k] = c[j] * (nocc[j] + 1) * 
                            sqrt(ef * (1. + alpha * ef)) * 
                            (1. + 2. * alpha * ef);
      }
      ++k;
    }
    en += eStep;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption
    energyLossElectrons[iLevel + 2 * j] = - eph[j];
    scatTypeElectrons[iLevel + 2 * j] = 5;
    // Emission
    energyLossElectrons[iLevel + 2 * j + 1] = eph[j];
    scatTypeElectrons[iLevel + 2 * j + 1] = 4;
  }

  iLevel += nPhonons;

  return true;

}

bool
MediumSilicon::ElectronIonisationRates(int& iLevel) {


  // Reference:
  // - E. Cartier, M. V. Fischetti, E. A. Eklund and F. R. McFeely,
  //   Appl. Phys. Lett 62, 3339-3341

  // Coefficients [ns-1]
  const double p[3] = {6.25e1, 3.e3, 6.8e5};
  // Threshold energies [eV]
  const double eth[3] = {1.1, 1.8, 3.45};

  double en = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    cfElectrons[i][iLevel] = 0.;
    if (en > eth[0]) {
      cfElectrons[i][iLevel] += p[0] * (en - eth[0]) * (en - eth[0]);
    }
    if (en > eth[1]) {
      cfElectrons[i][iLevel] += p[1] * (en - eth[1]) * (en - eth[1]);
    }
    if (en > eth[2]) {
      cfElectrons[i][iLevel] += p[2] * (en - eth[2]) * (en - eth[2]);
    }
    en += eStep;
  }

  energyLossElectrons[iLevel] = eth[0];
  scatTypeElectrons[iLevel] = 1;
  ++iLevel;

  return true;

}

bool
MediumSilicon::ElectronImpurityScatteringRates(int& iLevel) {

  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;

  // Band parameters
  // Longitudinal and transverse effective electron masses
  const double ml = 0.98; 
  const double mt = 0.19;
  // Density of states effective mass
  const double md = pow(ml * mt * mt, 1. / 3.) * ElectronMass;
  // Non-parabolicity coefficient [eV-1]
  const double alpha = 0.5;

  // Dielectric constant
  const double eps = GetDielectricConstant();
  // Impurity concentration
  const double impurityConcentration = dopingConcentration;
  if (impurityConcentration < 1.e-20) return true;

  // Screening length
  const double ls = sqrt(eps * kbt / (4 * Pi * FineStructureConstant * HbarC * 
                                      impurityConcentration));
  const double eb = 0.5 * HbarC * HbarC / (md * ls * ls);

  // Prefactor
  // const double c = pow(2., 2.5) * Pi * impurityConcentration * 
  //                 pow(FineStructureConstant * HbarC, 2) *
  //                 SpeedOfLight / (eps * eps * sqrt(md) * eb * eb);
  // Use momentum-transfer cross-section
  const double c = impurityConcentration * Pi * 
                   pow(FineStructureConstant * HbarC, 2) * 
                   SpeedOfLight / 
                   (sqrt(2 * md) * eps * eps);
  
  double en = 0.;
  double gamma = 0.;
  double b = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    gamma = en * (1 + alpha * en);
    // cfElectrons[i][iLevel] = c * sqrt(gamma) * (1. + 2 * alpha * en) /
    //                         (1. + 4. * gamma / eb);
    if (gamma <= 0.) {
      cfElectrons[i][iLevel] = 0.;
    } else {
      b = 4 * gamma / eb;
      cfElectrons[i][iLevel] = (c / pow(gamma, 1.5)) * 
                               (log(1. + b) - b / (1. + b));
    }
    en += eStep;
  }

  energyLossElectrons[iLevel] = 0.;
  scatTypeElectrons[iLevel] = 0; 
  ++iLevel;

  return true;

}

bool
MediumSilicon::HoleScatteringRates() {

  // Mass density [g/cm3]
  const double rho = GetMassDensity();
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  // Acoustic phonon interband scattering
  // Acoustic deformation potential [eV]
  const double defpot = 5.0;
  // Longitudinal and transverse velocity of sound [cm/ns]
  const double ut = 9.0e-4;
  const double ul = 5.3e-4;
  // Average velocity of sound [cm/ns]
  const double u = (ul + 2. * ut) / 3.;
  // Prefactor
  const double cAc = defpot * defpot * pow(kbt, 3.) /
                     (pow(2., 4.5) * Pi * rho * pow(u, 4.));

  // Impact ionisation
  // Reference:
  // - DAMOCLES web page
  const double p[2] = {2., 1.e3};
  const double eth[2] = {1.1, 1.45};
  const double b[2] = {6., 4.};
  
  double en = 1.e-4;
  const double estep = 0.005;
  double f;
  int nSteps = 2000;
  for (int i = 0; i < nSteps; ++i) {
    en += estep;    
    // Impact ionisation
    f = 0.;
    if (en > eth[0]) f += p[0] * pow(en - eth[0], b[0]);
    if (en > eth[1]) f += p[1] * pow(en - eth[1], b[1]);
  }

  return true;

}

void 
MediumSilicon::ComputeHoleIntegrals(const double x, double& f3, double& f4, 
                                    double& g3, double& g4) {

  const double x2 = x * x;
  const double x3 = x2 * x;
  const double x4 = x2 * x2;
  const double sq2 = sqrt(2);
  
  if (x <= 3. / sq2) {
    f3 = 2. * x2 * (1. - 34. * sq2 * x / 105. + x2 / 12.);
    g3 = 2. * x2 * (1. + 34. * sq2 * x / 105. + x2 / 12.);
    f4 = x3 * (136. * sq2 / 105. - x + 44. * sq2 * x2 / 315.);
    g4 = x3 * (136. * sq2 / 105. + x + 44. * sq2 * x2 / 315.);
    return;
  }
  
  f3 = 1017. / 280. + (68. - 2358. / x2 + 41931. / x4) * exp(-3.) - 
       8. * exp(-sq2 * x) * 
       (x2 + 4. * sq2 * x * 28. + 72. * sq2 / x + 252. / x2 + 270. * sq2 / x3 
       + 270. / x4);
  f4 = 801. / 140. + exp(-3.) * (312. - 13248. / x2 + 300078. / x4) - 
       exp(sq2 * x) * 
       (8. * sq2 * x3 + 72. * x2 + 288. * sq2 * x + 1824. + 4320. * sq2 / x + 
       14400. / x2 + 15120. * sq2 / x3 + 15120. / x4);
  g3 = 5913. / 280. + 136. * sq2 / (105. * x3) - 9. * (4. - 162. / (5. * x2) + 
       729. / (7. * x4));       
  g4 = 6371. / 140. + 2. * x4 - 81. + 729. / x2 - 19683. / (8. * x4);
  
}

}
