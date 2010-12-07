#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>

#include "MediumSilicon.hh"
#include "Random.hh"
#include "GarfieldConstants.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumSilicon::MediumSilicon() :
  Medium(), 
  bandGap(1.12), 
  dopingType('i'), dopingConcentration(0.),
  mLongX(0.916), mTransX(0.191),
  mLongL(1.59),  mTransL(0.12),
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
  hasUserMobility(false), hasUserSaturationVelocity(false),
  latticeMobilityModel(0),
  dopingMobilityModel(0),
  saturationVelocityModel(0),
  highFieldMobilityModel(0),
  impactIonisationModel(0),
  useCfOutput(false),
  useNonParabolicity(true), useFullBandDos(true), useAnisotropy(true),
  eFinal(5.), eStep(eFinal / nEnergySteps),
  nLevelsX(0), nLevelsL(0), nValleysX(6), nValleysL(8),
  eMinL(1.), eMinG(2.24), ieMinL(0), ieMinG(0), 
  hasOpticalData(false), opticalDataFile("OpticalData_Si_V1.txt") {

  className = "MediumSilicon";
  name = "Si";

  SetTemperature(300.);
  SetDielectricConstant(11.9);
  SetAtomicNumber(14.);
  SetAtomicWeight(28.0855);
  SetMassDensity(2.329);
  
  EnableDrift();
  EnablePrimaryIonisation();
  microscopic = true;

  wValue = 3.6;
  fanoFactor = 0.11;  
  
  cfTotElectronsX.clear();
  cfElectronsX.clear();
  energyLossElectronsX.clear();
  scatTypeElectronsX.clear();

  cfTotElectronsL.clear();
  cfElectronsL.clear();
  energyLossElectronsL.clear();
  scatTypeElectronsL.clear();
  
  cfTotElectronsG.clear();
  cfElectronsG.clear();
  energyLossElectronsG.clear();
  scatTypeElectronsG.clear();  

  ieMinL = int(eMinL / eStep) + 1;
  ieMinG = int(eMinG / eStep) + 1;

  // Initialize the collision counters.
  nCollElectronAcoustic = nCollElectronOptical = 0;
  nCollElectronIntervalley = 0;
  nCollElectronImpurity = 0;
  nCollElectronIonisation = 0;
  nCollElectronDetailed.clear();
  nCollElectronBand.clear();

}

void 
MediumSilicon::SetDoping(const char type, const double c) {

  if (toupper(type) == 'N') {
    dopingType = 'n';
    if (c > Small) {
      dopingConcentration = c;
    } else {
      std::cerr << className << "::SetDoping:\n";
      std::cerr << "    Doping concentration must be greater than zero.\n"; 
      std::cerr << "    Using default value for n-type silicon "
                << "(10^12 cm-3) instead.\n";
      dopingConcentration = 1.e12;
    }
  } else if (toupper(type) == 'P') {
    dopingType = 'p';
    if (c > Small) {
      dopingConcentration = c;
    } else {
      std::cerr << className << "::SetDoping:\n";
      std::cerr << "    Doping concentration must be greater than zero.\n"; 
      std::cerr << "    Using default value for p-type silicon "
                << "(10^18 cm-3) instead.\n";
      dopingConcentration = 1.e18;
    }
  } else if (toupper(type) == 'I') {
    dopingType = 'i';
    dopingConcentration = 0.;
  } else {
    std::cerr << className << "::SetDoping:\n";
    std::cerr << "    Unknown dopant type (" << type << ").\n";
    std::cerr << "    Available types are n, p and i (intrinsic).\n";
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
    std::cerr << className << "::SetTrapCrossSection:\n";
    std::cerr << "    Capture cross-section [cm2] must positive.\n"; 
  } else {
    eTrapCs = ecs;
  }
  
  if (hcs < 0.) {
    std::cerr << className << "::SetTrapCrossSection:\n";
    std::cerr << "    Capture cross-section [cm2] must be positive.n"; 
  } else {
    hTrapCs = hcs;
  }
  
  trappingModel = 0;
  isChanged = true;

}

void
MediumSilicon::SetTrapDensity(const double n) {

  if (n < 0.) {
    std::cerr << className << "::SetTrapDensity:\n";
    std::cerr << "    Trap density [cm-3] must be greater than zero.\n"; 
  } else {
    eTrapDensity = n;
    hTrapDensity = n;
  }
  
  trappingModel = 0;
  isChanged = true;

}

void
MediumSilicon::SetTrappingTime(const double etau, const double htau) {

  if (etau <= 0.) {
    std::cerr << className << "::SetTrappingTime:\n";
    std::cerr << "    Trapping time [ns-1] must be positive.\n"; 
  } else {
    eTrapTime = etau;
  }
  
  if (htau <= 0.) {
    std::cerr << className << "::SetTrappingTime:\n";
    std::cerr << "    Trapping time [ns-1] must be positive.\n"; 
  } else {
    hTrapTime = htau;
  }
  
  trappingModel = 1;
  isChanged = true;

}

bool 
MediumSilicon::ElectronVelocity(
            const double ex, const double ey, const double ez, 
            const double bx, const double by, const double bz, 
            double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::ElectronVelocity:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }

  if (hasElectronVelocityE) {
    // Interpolation in user table.
    return Medium::ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }

  const double e = sqrt(ex * ex + ey * ey + ez * ez);

  // Calculate the mobility
  double mu;
  switch (highFieldMobilityModel) {
    case HighFieldMobilityModelMinimos:
      ElectronMobilityMinimos(e, mu);
      break;
    case HighFieldMobilityModelCanali:
      ElectronMobilityCanali(e, mu);
      break;
    case HighFieldMobilityModelReggiani:
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
                         
  alpha = 0.;
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::ElectronTownsend:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }
 
  if (hasElectronTownsend) {
    // Interpolation in user table.
    return Medium::ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
 
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  
  switch (impactIonisationModel) {
    case ImpactIonisationModelVanOverstraeten:
      return ElectronImpactIonisationVanOverstraetenDeMan(e, alpha);
      break;
    case ImpactIonisationModelGrant:
      return ElectronImpactIonisationGrant(e, alpha);
      break;
    default:
      std::cerr << className << "::ElectronTownsend:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";    
      break;
  }  
  return false;
  
}  

bool 
MediumSilicon::ElectronAttachment(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& eta) {

  eta = 0.;
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::ElectronAttachment:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }

  if (hasElectronAttachment) {
    // Interpolation in user table.
    return Medium::ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
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
      std::cerr << className << "::ElectronAttachment:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
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

  vx = vy = vz = 0.;
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::HoleVelocity:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }

  if (hasHoleVelocityE) {
    // Interpolation in user table.
    return Medium::HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }

  const double e = sqrt(ex * ex + ey * ey + ez * ez);

  // Calculate the mobility
  double mu;
  switch (highFieldMobilityModel) {
    case HighFieldMobilityModelMinimos:
      HoleMobilityMinimos(e, mu);
      break;
    case HighFieldMobilityModelCanali:
      HoleMobilityCanali(e, mu);
      break;
    case HighFieldMobilityModelReggiani:
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

  alpha = 0.;                         
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::HoleTownsend:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }
 
  if (hasHoleTownsend) {
    // Interpolation in user table.
    return Medium::HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
 
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  
  switch (impactIonisationModel) {
    case ImpactIonisationModelVanOverstraeten:
      return HoleImpactIonisationVanOverstraetenDeMan(e, alpha);
      break;
    case ImpactIonisationModelGrant:
      return HoleImpactIonisationGrant(e, alpha);
      break;
    default:
      std::cerr << className << "::HoleTownsend:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";      
      break;
  }  
  return false;

}

bool 
MediumSilicon::HoleAttachment(
            const double ex, const double ey, const double ez,
            const double bx, const double by, const double bz,
            double& eta) {

  eta = 0.;
  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::HoleAttachment:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    isChanged = false;
  }

  if (hasHoleAttachment) {
    // Interpolation in user table.
    return Medium::HoleAttachment(ex, ey, ez, bx, by, bz, eta);
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
      std::cerr << className << "::HoleAttachment:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
      return false;
      break;      
  }
  
  return true;

}

void
MediumSilicon::SetLowFieldMobility(const double mue, const double muh) {

  if (mue <= 0. || muh <= 0.) {
    std::cerr << className << "::SetLowFieldMobility:\n";
    std::cerr << "    Mobility must be greater than zero.\n";
    return;
  }
  
  eMobility = mue;
  hMobility = muh;
  hasUserMobility = true;
  isChanged = true;

}

void 
MediumSilicon::SetLatticeMobilityModelMinimos() {

  latticeMobilityModel = LatticeMobilityModelMinimos;  
  hasUserMobility = false;
  isChanged = true;
  
}

void 
MediumSilicon::SetLatticeMobilityModelSentaurus() {

  latticeMobilityModel = LatticeMobilityModelSentaurus;
  hasUserMobility = false;
  isChanged = true;
  
}

void
MediumSilicon::SetLatticeMobilityModelReggiani() {

  latticeMobilityModel = LatticeMobilityModelReggiani;
  hasUserMobility = false;
  isChanged = true;

}

void 
MediumSilicon::SetDopingMobilityModelMinimos() {

  dopingMobilityModel = DopingMobilityModelMinimos;
  hasUserMobility = false;
  isChanged = true;
  
}

void 
MediumSilicon::SetDopingMobilityModelMasetti() {

  dopingMobilityModel = DopingMobilityModelMasetti;
  hasUserMobility = false;
  isChanged = true;
  
}

void
MediumSilicon::SetSaturationVelocity(const double vsate, const double vsath) {

  if (vsate <= 0. || vsath <= 0.) {
    std::cout << className << "::SetSaturationVelocity:\n";
    std::cout << "    Restoring default values.\n";
    hasUserSaturationVelocity = false;
  } else {
    eSatVel = vsate; hSatVel = vsath;
    hasUserSaturationVelocity = true;
  }
  
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelMinimos() {

  saturationVelocityModel = SaturationVelocityModelMinimos;
  hasUserSaturationVelocity = false;
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelCanali() {

  saturationVelocityModel = SaturationVelocityModelCanali;
  hasUserSaturationVelocity = false;
  isChanged = true;

}

void
MediumSilicon::SetSaturationVelocityModelReggiani() {

  saturationVelocityModel = SaturationVelocityModelReggiani;
  hasUserSaturationVelocity = false;
  isChanged = true;

}

void 
MediumSilicon::SetHighFieldMobilityModelMinimos() {

  highFieldMobilityModel = HighFieldMobilityModelMinimos;
  isChanged = true;
  
}

void 
MediumSilicon::SetHighFieldMobilityModelCanali() {

  highFieldMobilityModel = HighFieldMobilityModelCanali;
  isChanged = true;
  
}

void
MediumSilicon::SetHighFieldMobilityModelReggiani() {

  highFieldMobilityModel = HighFieldMobilityModelReggiani;
  isChanged = true;

}

void
MediumSilicon::SetHighFieldMobilityModelConstant() {

  highFieldMobilityModel = HighFieldMobilityModelConstant;
  
}

void 
MediumSilicon::SetImpactIonisationModelVanOverstraetenDeMan() {

  impactIonisationModel = ImpactIonisationModelVanOverstraeten;
  isChanged = true;  

}

void
MediumSilicon::SetImpactIonisationModelGrant() {

  impactIonisationModel = ImpactIonisationModelGrant;
  isChanged = true;
 
}

bool
MediumSilicon::SetMaxElectronEnergy(const double e) {

  if (e <= Small) {
    std::cerr << className << "::SetMaxElectronEnergy:\n";
    std::cerr << "    Provided upper electron energy limit (" << e
              << " eV) is too small.\n";
    return false;
  }

  eFinal = e;
  // Determine the energy interval size.
  eStep = eFinal / nEnergySteps;

  isChanged = true;

  return true;

}

double
MediumSilicon::GetElectronEnergy(
               const double px, const double py, const double pz, 
               double& vx, double& vy, double& vz, const int band) {

  // Effective masses
  double mx = ElectronMass, my = ElectronMass, mz = ElectronMass;
  // Energy offset
  double e0 = 0.;
  if (band >= 0 && band < nValleysX) {
    // X valley
    if (useAnisotropy) {
      switch (band) {
        case 0:
        case 1:
          // X 100, -100
          mx *= mLongX; my *= mTransX; mz *= mTransX;
          break;
        case 2:
        case 3:
          // X 010, 0-10
          mx *= mTransX; my *= mLongX; mz *= mTransX;
          break;
        case 4:
        case 5:
          // X 001, 00-1
          mx *= mTransX; my *= mTransX; mz *= mLongX;
          break;
        default:
          std::cerr << className << "::GetElectronEnergy:\n";
          std::cerr << "    Unexpected band index " << band << "!\n";
          break;
      }
    } else {
      // Conduction effective mass
      const double mc = 3. / (1. / mLongX + 2. / mTransX);
      mx *= mc; my *= mc; mz *= mc;
    }
  } else if (band < nValleysX + nValleysL) {
    // L valley, isotropic approximation
    e0 = eMinL;
    // Effective mass
    const double mc = 3. / (1. / mLongL + 2. / mTransL);
    mx *= mc; my *= mc; mz *= mc;
  } else if (band == nValleysX + nValleysL) {
    // Higher band(s)
  }

  if (useNonParabolicity) {
    // Non-parabolicity parameter
    double alpha = 0.;
    if (band < nValleysX) {
      // X valley
      alpha = 0.5;
    } else if (band < nValleysX + nValleysL) {
      // L valley
      alpha = 0.3;
    } 

    const double p2 = 0.5 * (px * px / mx + py * py / my + pz * pz / mz);
    const double e = 0.5 * (sqrt(1. + 4 * alpha * p2) - 1.) / alpha;
    const double a = SpeedOfLight / (1. + 2 * alpha * e);
    vx = a * px / mx; vy = a * py / my; vz = a * pz / mz;
    return e0 + e;
  } else {
    const double e = 0.5 * (px * px / mx + py * py / my + pz * pz / mz);
    vx = SpeedOfLight * px / mx; 
    vy = SpeedOfLight * py / my; 
    vz = SpeedOfLight * pz / mz;
    return e0 + e;
  }

  vx = SpeedOfLight * px / mx;
  vy = SpeedOfLight * py / my;
  vz = SpeedOfLight * pz / mz;
  return 0.5 * (px * px + py * py + pz * pz) / ElectronMass;

}

void
MediumSilicon::GetElectronMomentum(const double e,
                                   double& px, double& py, double& pz,
                                   int& band) {

  int nBands = nValleysX;
  if (e > eMinL) nBands += nValleysL;
  if (e > eMinG) ++nBands;

  // If the band index is out of range, choose one at random.
  if (band < 0 || band > nValleysX + nValleysL || 
      (e < eMinL || band >= nValleysX) || 
      (e < eMinG || band == nValleysX + nValleysL)) {
    if (e < eMinL) {
      band = int(nValleysX * RndmUniform());
      if (band >= nValleysX) band = nValleysX - 1;
    } else {
      const double dosX = GetConductionBandDensityOfStates(e, 0);
      const double dosL = GetConductionBandDensityOfStates(e, nValleysX);
      const double dosG = GetConductionBandDensityOfStates(e, nValleysX +
                                                              nValleysL);
      const double dosSum = nValleysX * dosX + nValleysL * dosL + dosG;
      if (dosSum < Small) {
        band = nValleysX + nValleysL;
      } else {
        const double r = RndmUniform() * dosSum;
        if (r < dosX) {
          band = int(nValleysX * RndmUniform());
          if (band >= nValleysX) band = nValleysX - 1;
        } else if (r < dosX + dosL) {
          band = nValleysX + int(nValleysL * RndmUniform());
          if (band >= nValleysX + nValleysL) band = nValleysL - 1;
        } else {
          band = nValleysX + nValleysL;
        }
      }
    }
    if (debug) {
      std::cout << className << "::GetElectronMomentum:\n";
      std::cout << "    Randomised band index: " << band << "\n";
    }
  }
  if (band < nValleysX) {
    // X valleys
    double pstar = sqrt(2. * ElectronMass * e);
    if (useNonParabolicity) {
      const double alpha = 0.5;
      pstar *= sqrt(1. + alpha * e);
    }

    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();

    if (useAnisotropy) {
      const double pl = pstar * sqrt(mLongX);
      const double pt = pstar * sqrt(mTransX);
      switch (band) {
        case 0:
        case 1:
          // 100
          px = pl * ctheta;
          py = pt * cos(phi) * stheta;
          pz = pt * sin(phi) * stheta;
          break;
        case 2:
        case 3:
          // 010
          px = pt * sin(phi) * stheta;
          py = pl * ctheta;
          pz = pt * cos(phi) * stheta;
          break;
        case 4:
        case 5:
          // 001
          px = pt * cos(phi) * stheta;
          py = pt * sin(phi) * stheta;
          pz = pl * ctheta;
          break;
        default:
          // Other band; should not occur.
          std::cerr << className << "::GetElectronMomentum:\n";
          std::cerr << "    Unexpected band index (" << band << ").\n";
          px = pstar * stheta * cos(phi);
          py = pstar * stheta * sin(phi);
          pz = pstar * ctheta;
          break;
      }
    } else {
      pstar *= sqrt(3. / (1. / mLongX + 2. / mTransX));
      px = pstar * cos(phi) * stheta;
      py = pstar * sin(phi) * stheta;
      pz = pstar * ctheta;
    }
  } else if (band < nValleysX + nValleysL) {
    // L valleys 
    double pstar = sqrt(2. * ElectronMass * (e - eMinL));
    if (useNonParabolicity) {
      const double alpha = 0.3;
      pstar *= sqrt(1. + alpha * (e - eMinL));
    }
    pstar *= sqrt(3. / (1. / mLongL + 2. / mTransL));

    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();
    
    px = pstar * cos(phi) * stheta;
    py = pstar * sin(phi) * stheta;
    pz = pstar * ctheta;
  } else if (band == nValleysX + nValleysL) {
    // Higher band
    double pstar = sqrt(2. * ElectronMass * e);
    
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();
    
    px = pstar * cos(phi) * stheta;
    py = pstar * sin(phi) * stheta;
    pz = pstar * ctheta;
  }
  
}

double
MediumSilicon::GetElectronNullCollisionRate(const int band) {

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::GetElectronNullCollisionRate:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return 0.;
    }
    isChanged = false;
  }

  if (band >= 0 && band < nValleysX) {
    return cfNullElectronsX;
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    return cfNullElectronsL;
  } else if (band == nValleysX + nValleysL) {
    return cfNullElectronsG;
  }
  std::cerr << className << "::GetElectronNullCollisionRate:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return 0.;

}

double
MediumSilicon::GetElectronCollisionRate(const double e, const int band) {

  if (e <= 0.) {
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Electron energy must be positive.\n";
    return 0.;
  }

  if (e > eFinal) {
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Collision rate at " << e
              << " eV is not included in the current table.\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);
  }

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::GetElectronCollisionRate:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return 0.;
    }
    isChanged = false;
  }

  int iE = int(e / eStep);
  if (iE >= nEnergySteps) {
    iE = nEnergySteps - 1;
  } else if (iE < 0) {
    iE = 0;
  }
  
  if (band >= 0 && band < nValleysX) {
    return cfTotElectronsX[iE];
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    if (iE < ieMinL) iE = ieMinL;
    return cfTotElectronsL[iE];
  } else if (band == nValleysX + nValleysL) {
    if (iE < ieMinG) iE = ieMinG;
    return cfTotElectronsG[iE];
  }
  
  std::cerr << className << "::GetElectronCollisionRate:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return 0.;
  
}

bool
MediumSilicon::GetElectronCollision(const double e, 
                                    int& type, int& level, double& e1, 
                                    double& px, double& py, double& pz, 
                                    int& nsec, double& esec,
                                    int& band) {

  if (e > eFinal) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Requested electron energy (" << e;
    std::cerr << " eV) exceeds current energy range (" << eFinal;
    std::cerr << " eV).\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e
              << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Electron energy must be greater than zero.\n";
    return false;
  }

  if (isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << className << "::GetElectronCollision:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return false;
    }
    isChanged = false;
  }

  // Get the energy interval.
  int iE = int(e / eStep);
  if (iE >= nEnergySteps) iE = nEnergySteps - 1;
  if (iE < 0) iE = 0;
  // Energy loss
  double loss = 0.;
  // Sample the scattering process.
  if (band >= 0 && band < nValleysX) {
    // X valley
    const double r = RndmUniform();
    int iLow = 0; 
    int iUp = nLevelsX - 1;
    if (r <= cfElectronsX[iE][iLow]) {
      level = iLow;
    } else if (r >= cfElectronsX[iE][nLevelsX - 1]) {
      level = iUp;
    } else {
      int iMid;
      while (iUp - iLow > 1) {
        iMid = (iLow + iUp) >> 1;
        if (r < cfElectronsX[iE][iMid]) {
          iUp = iMid;
        } else {
          iLow = iMid;
        }
      }
      level = iUp;
    }

    // Get the collision type.
    type = scatTypeElectronsX[level];
    // Fill the collision counters.
    ++nCollElectronDetailed[level];
    ++nCollElectronBand[band];
    if (type == ElectronCollisionTypeAcousticPhonon) {
      ++nCollElectronAcoustic;
    } else if (type == ElectronCollisionTypeIntervalleyG) {
      // Intervalley scattering (g type)
      ++nCollElectronIntervalley;
      // Final valley is in opposite direction.
      switch (band) {
        case 0: 
          band = 1; break;
        case 1:
          band = 0; break;
        case 2:
          band = 3; break;
        case 3:
          band = 2; break;
        case 4:
          band = 5; break;
        case 5:
          band = 4; break;
        default:
          break;
      }
    } else if (type == ElectronCollisionTypeIntervalleyF) {
      // Intervalley scattering (f type)
      ++nCollElectronIntervalley;
      // Final valley is perpendicular.
      switch (band) {
        case 0:
        case 1:
          band = int(RndmUniform() * 4) + 2;
          break;
        case 2:
        case 3:
          band = int(RndmUniform() * 4);
          if (band > 1) band += 2;
          break;
        case 4:
        case 5:
          band = int(RndmUniform() * 4);          
          break;
      }
    } else if (type == ElectronCollisionTypeInterband) {
      // XL scattering
      ++nCollElectronIntervalley;
      // Final valley is in L band.
      band = nValleysX + int(RndmUniform() * nValleysL);
      if (band >= nValleysX + nValleysL) band = nValleysX + nValleysL - 1;
    } else if (type == ElectronCollisionTypeImpurity) {
      ++nCollElectronImpurity;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    }
      
    // Get the energy loss.
    loss = energyLossElectronsX[level];
    
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    if (iE < ieMinL) iE = ieMinL;
    // L valley
    const double r = RndmUniform();
    int iLow = 0; 
    int iUp = nLevelsL - 1;
    if (r <= cfElectronsL[iE][iLow]) {
      level = iLow;
    } else if (r >= cfElectronsL[iE][nLevelsL - 1]) {
      level = iUp;
    } else {
      int iMid;
      while (iUp - iLow > 1) {
        iMid = (iLow + iUp) >> 1;
        if (r < cfElectronsL[iE][iMid]) {
          iUp = iMid;
        } else {
          iLow = iMid;
        }
      }
      level = iUp;
    }

    // Get the collision type.
    type = scatTypeElectronsL[level];
    // Fill the collision counters.
    ++nCollElectronDetailed[nLevelsX + level];
    ++nCollElectronBand[band];
    if (type == ElectronCollisionTypeAcousticPhonon) {
      ++nCollElectronAcoustic;
    } else if (type == ElectronCollisionTypeIntervalleyG ||
               type == ElectronCollisionTypeIntervalleyF) {
      // Equivalent intervalley scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = nValleysX + int(RndmUniform() * nValleysL);
      while (band < nValleysX || band >= nValleysX + nValleysL) {
        band = nValleysX + int(RndmUniform() * nValleysL);
      }
    } else if (type == ElectronCollisionTypeInterband) {
      // LX scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = int(RndmUniform() * nValleysX);
      if (band >= nValleysX) band = nValleysX - 1;
    } else if (type == ElectronCollisionTypeImpurity) {
      ++nCollElectronImpurity;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    }
      
    // Get the energy loss.
    loss = energyLossElectronsL[level];
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    if (iE < ieMinG) iE = ieMinG;
    // Higher bands
    const double r = RndmUniform();
    int iLow = 0; 
    int iUp = nLevelsG - 1;
    if (r <= cfElectronsG[iE][iLow]) {
      level = iLow;
    } else if (r >= cfElectronsG[iE][nLevelsG - 1]) {
      level = iUp;
    } else {
      int iMid;
      while (iUp - iLow > 1) {
        iMid = (iLow + iUp) >> 1;
        if (r < cfElectronsG[iE][iMid]) {
          iUp = iMid;
        } else {
          iLow = iMid;
        }
      }
      level = iUp;
    }

    // Get the collision type.
    type = scatTypeElectronsG[level];
    // Fill the collision counters.
    ++nCollElectronDetailed[nLevelsX + nLevelsL + level];
    ++nCollElectronBand[band];
    if (type == ElectronCollisionTypeAcousticPhonon) {
      ++nCollElectronAcoustic;
    } else if (type == ElectronCollisionTypeIntervalleyG ||
               type == ElectronCollisionTypeIntervalleyF) {
      // Equivalent intervalley scattering
      ++nCollElectronIntervalley;
    } else if (type == ElectronCollisionTypeImpurity) {
      ++nCollElectronImpurity;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    }
      
    // Get the energy loss.
    loss = energyLossElectronsG[level];
  } else {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Band index (" << band << ") out of range.\n";
    return false;
  }
    
  // Secondary electron energy (none by default)
  esec = 0.;
  nsec = 0;
  // Ionising collision
  if (type == ElectronCollisionTypeIonisation) {
    esec = RndmUniform() * (e - loss);
    loss += esec;
    if (esec < Small) esec = Small;
    nsec = 1;
  }

  if (e < loss) loss = e - 0.00001;
  // Update the energy.
  e1 = e - loss;
  if (e1 < Small) e1 = Small;
  
  // Update the momentum.
  if (band >= 0 && band < nValleysX) {
    double pstar = sqrt(2. * ElectronMass * e1);
    if (useNonParabolicity) {
      const double alpha = 0.5;
      pstar *= sqrt(1. + alpha * e1);
    }

    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();

    if (useAnisotropy) {
      const double pl = pstar * sqrt(mLongX);
      const double pt = pstar * sqrt(mTransX);
      switch (band) {
        case 0:
        case 1:
          // 100
          px = pl * ctheta;
          py = pt * cos(phi) * stheta;
          pz = pt * sin(phi) * stheta;
          break;
        case 2:
        case 3:
          // 010
          px = pt * sin(phi) * stheta;
          py = pl * ctheta;
          pz = pt * cos(phi) * stheta;
          break;
        case 4:
        case 5:
          // 001
          px = pt * cos(phi) * stheta;
          py = pt * sin(phi) * stheta;
          pz = pl * ctheta;
          break;
        default:
          return false;
          break;
      }
    } else {
      pstar *= 3. / (1. / mLongX + 2. / mTransX);
      px = pstar * cos(phi) * stheta;
      py = pstar * sin(phi) * stheta;
      pz = pstar * ctheta;
    }
    return true;

  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
  
    // Update the momentum.
    double pstar = sqrt(2. * ElectronMass * (e1 - eMinL));
    if (useNonParabolicity) {
      const double alpha = 0.3;
      pstar *= sqrt(1. + alpha * (e1 - eMinL));
    }

    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();

    pstar *= sqrt(3. / (1. / mLongL + 2. / mTransL));
    px = pstar * cos(phi) * stheta;
    py = pstar * sin(phi) * stheta;
    pz = pstar * ctheta;
    return true;

  }
  
  std::cerr << className << "::GetElectronCollision:\n";
  std::cerr << "   Band index (" << band << ") out of range.\n";
  e1 = e;
  type = 0;
  return false;

}

void
MediumSilicon::ResetCollisionCounters() {

  nCollElectronAcoustic = nCollElectronOptical = 0;
  nCollElectronIntervalley = 0;
  nCollElectronImpurity = 0;
  nCollElectronIonisation = 0;
  const int nLevels = nLevelsX + nLevelsL + nLevelsG;
  nCollElectronDetailed.resize(nLevels);
  for (int j = nLevels; j--;) nCollElectronDetailed[j] = 0;
  const int nBands = nValleysX + nValleysL + 1;
  nCollElectronBand.resize(nBands);
  for (int j = nBands; j--;) nCollElectronBand[j] = 0;

}

int
MediumSilicon::GetNumberOfElectronCollisions() const {

  return nCollElectronAcoustic + nCollElectronOptical + 
         nCollElectronIntervalley + 
         nCollElectronImpurity + nCollElectronIonisation;

}

int
MediumSilicon::GetNumberOfLevels() {

  const int nLevels = nLevelsX + nLevelsL + nLevelsG;
  return nLevels;

}

int
MediumSilicon::GetNumberOfElectronCollisions(const int level) const {

  const int nLevels = nLevelsX + nLevelsL + nLevelsG;
  if (level < 0 || level >= nLevels) {
    std::cerr << className << "::GetNumberOfElectronCollisions:\n";
    std::cerr << "    Requested scattering rate term ("
              << level << ") does not exist.\n";
    return 0;
  }
  return nCollElectronDetailed[level];

}

int
MediumSilicon::GetNumberOfElectronBands() {

  const int nBands = nValleysX + nValleysL + 1;
  return nBands;

}

int
MediumSilicon::GetElectronBandPopulation(const int band) {

  const int nBands = nValleysX + nValleysL + 1;
  if (band < 0 || band >= nBands) {
    std::cerr << className << "::GetElectronBandPopulation:\n";
    std::cerr << "    Band index (" << band << ") out of range.\n";
    return 0;
  }

  return nCollElectronBand[band];

}

bool 
MediumSilicon::GetOpticalDataRange(double& emin, double& emax, const int i) {

  if (i != 0) {
    std::cerr << className << "::GetOpticalDataRange:\n";
    std::cerr << "    Medium has only one component.\n";
  }

  // Make sure the optical data table has been loaded.
  if (!hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << className << "::GetOpticalDataRange:\n";
      std::cerr << "    Optical data table could not be loaded.\n";
      return false;
    }
    hasOpticalData = true;
  }
   
  emin = opticalDataTable[0].energy;
  emax = opticalDataTable.back().energy;
  if (debug) {
    std::cout << className << "::GetOpticalDataRange:\n";
    std::cout << "    " << emin << " < E [eV] < " << emax << "\n";
  }
  return true;  
  
}

bool 
MediumSilicon::GetDielectricFunction(const double e, 
                                     double& eps1, double& eps2, 
                                     const int i) {
                        
  if (i != 0) {
    std::cerr << className << "::GetDielectricFunction:\n";
    std::cerr << "    Medium has only one component.\n";
    return false;
  }
                        
  // Make sure the optical data table has been loaded.
  if (!hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << className << "::GetDielectricFunction:\n";
      std::cerr << "    Optical data table could not be loaded.\n";
      return false;
    }
    hasOpticalData = true;
  }
  
  // Make sure the requested energy is within the range of the table.
  const double emin = opticalDataTable[0].energy;
  const double emax = opticalDataTable.back().energy;    
  if (e < emin || e > emax) {
    std::cerr << className << "::GetDielectricFunction:\n";
    std::cerr << "    Requested energy (" << e << " eV) " 
              << " is outside the range of the optical data table.\n";
    std::cerr << "    " << emin << " < E [eV] < " << emax << "\n";
    eps1 = eps2 = 0.;
    return false;
  }

  // Locate the requested energy in the table.
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
  
  // Interpolate the real part of dielectric function.
  // Use linear interpolation if one of the values is negative,
  // Otherwise use log-log interpolation.
  const double logX0 = log(opticalDataTable[iLow].energy);
  const double logX1 = log(opticalDataTable[iUp].energy);
  const double logX = log(e);
  if (opticalDataTable[iLow].eps1 <= 0. || opticalDataTable[iUp].eps1 <= 0.) {
    eps1 = opticalDataTable[iLow].eps1 + (e - opticalDataTable[iLow].energy) * 
           (opticalDataTable[iUp].eps1 - opticalDataTable[iLow].eps1) / 
          (opticalDataTable[iUp].energy - opticalDataTable[iLow].energy);  
  } else {
    const double logY0 = log(opticalDataTable[iLow].eps1);
    const double logY1 = log(opticalDataTable[iUp].eps1);
    eps1 = logY0 + (logX - logX0) * (logY1 - logY0) / (logX1 - logX0);
    eps1 = exp(eps1);
  }
      
  // Interpolate the imaginary part of dielectric function,
  // using log-log interpolation.
  const double logY0 = log(opticalDataTable[iLow].eps2);
  const double logY1 = log(opticalDataTable[iUp].eps2);  
  eps2 = logY0 + (log(e) - logX0) * (logY1 - logY0) / (logX1 - logX0);
  eps2 = exp(eps2);
  return true;
  
}

bool 
MediumSilicon::UpdateTransportParameters() {

  // Calculate impact ionisation coefficients
  switch (impactIonisationModel) {
    case ImpactIonisationModelVanOverstraeten:
      UpdateImpactIonisationVanOverstraetenDeMan();
      break;
    case ImpactIonisationModelGrant:
      UpdateImpactIonisationGrant();
      break;
    default:
      std::cerr << className << "::UpdateTransportParameters:\n";
      std::cerr << "    Unknown impact ionisation model. Bug!\n";
      break;
  }
  
  if (!hasUserMobility) {        
    // Calculate lattice mobility
    switch (latticeMobilityModel) {
      case LatticeMobilityModelMinimos:
        UpdateLatticeMobilityMinimos();
        break;
      case LatticeMobilityModelSentaurus:
        UpdateLatticeMobilitySentaurus();
        break;   
      case LatticeMobilityModelReggiani:
        UpdateLatticeMobilityReggiani();
        break; 
      default:
        std::cerr << className << "::UpdateTransportParameters:\n"; 
        std::cerr << "    Unknown lattice mobility model.\n";
        std::cerr << "    Program bug!\n"; 
        break;
    }

  
    // Calculate doping mobility
    switch (dopingMobilityModel) {
      case DopingMobilityModelMinimos:
        UpdateDopingMobilityMinimos();
        break;
      case DopingMobilityModelMasetti:
        UpdateDopingMobilityMasetti();
        break;
      default:
        std::cerr << className << "::UpdateTransportParameters:\n"; 
        std::cerr << "    Unknown doping mobility model.\n";
        std::cerr << "    Program bug!\n"; 
        break;
    }
  }
      
  // Calculate saturation velocity
  if (!hasUserSaturationVelocity) {
    switch (saturationVelocityModel) {
      case SaturationVelocityModelMinimos:
        UpdateSaturationVelocityMinimos();
        break;
      case SaturationVelocityModelCanali:
        UpdateSaturationVelocityCanali();
        break;
      case SaturationVelocityModelReggiani:
        UpdateSaturationVelocityReggiani();
        break;
    }
  }

  // Calculate high field saturation parameters
  switch (highFieldMobilityModel) {
    case HighFieldMobilityModelCanali:
      UpdateHighFieldMobilityCanali();
      break;
  }
  
  if (debug) {
    std::cout << className << "::UpdateTransportParameters:\n";
    std::cout << "    Low-field mobility [cm2 V-1 ns-1]\n";
    std::cout << "      Electrons: " << eMobility << "\n";
    std::cout << "      Holes:     " << hMobility << "\n";
    if (highFieldMobilityModel > 2) {
      std::cout << "    Mobility is not field-dependent.\n";
    } else {
      std::cout << "    Saturation velocity [cm / ns]\n";
      std::cout << "      Electrons: " << eSatVel << "\n";
      std::cout << "      Holes:     " << hSatVel << "\n";
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
  
  // Parameters adopted from Minimos NT User Guide
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
  const double hbarOmega = 0.063;
  // Temperature scaling coefficient
  const double gamma = tanh(hbarOmega / (2. * BoltzmannConstant * 300.)) /
                       tanh(hbarOmega / (2. * BoltzmannConstant * temperature));
  
  // Low field coefficients taken from Maes, de Meyer, van Overstraeten
  eImpactA0 = gamma * 3.318e5; eImpactB0 = gamma * 1.135e6;
  eImpactA1 = gamma * 7.03e5;  eImpactB1 = gamma * 1.231e6;
  
  hImpactA0 = gamma * 1.582e6; hImpactB0 = gamma * 2.036e6;
  hImpactA1 = gamma * 6.71e5;  hImpactB1 = gamma * 1.693e6;

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
                 
  eImpactA0 = 2.60e6 * gamma; eImpactB0 = 1.43e6 * gamma;
  eImpactA1 = 6.20e5 * gamma; eImpactB1 = 1.08e6 * gamma;
  eImpactA2 = 5.05e5 * gamma; eImpactB2 = 9.90e5 * gamma;
  
  hImpactA0 = 2.00e6 * gamma; hImpactB0 = 1.97e6 * gamma;
  hImpactA1 = 5.60e5 * gamma; hImpactB1 = 1.32e6 * gamma;
  
}

bool 
MediumSilicon::ElectronMobilityMinimos(const double e, double& mu) const {

  // Reference:
  // - Minimos User's Guide (1999)
  
  if (e < Small) {
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
  
  if (e < Small) {
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

  if (e < Small) {
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

  if (e < Small) {
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

  if (e < Small) {
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

  if (e < Small) {
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
  
  if (e < Small) {
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

  if (e < Small) {
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

  if (e < Small) {
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

  if (e < Small) {
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

  // Get the path to the data directory.
  char* pPath = getenv("GARFIELD_HOME");
  if (pPath == 0) {
    std::cerr << className << "::LoadOpticalData:\n";
    std::cerr << "    Environment variable GARFIELD_HOME is not set.\n"; 
    return false;
  }
  std::string filepath = pPath;
  filepath = filepath + "/Data/" + filename;

  // Open the file.
  std::ifstream infile;
  infile.open(filepath.c_str(), std::ios::in);
  // Make sure the file could actually be opened.
  if (!infile) {
    std::cerr << className << "::LoadOpticalData:\n";
    std::cerr << "    Error opening file " << filename << ".\n";
    return false;
  }
  
  // Clear the optical data table.
  opticalDataTable.clear();
  
  double lastEnergy = -1.;
  double energy, eps1, eps2, loss;  
  opticalData data;
  // Read the file line by line.
  std::string line;
  std::istringstream dataStream;  
  int i = 0;
  while (!infile.eof()) {
    ++i;
    // Read the next line.
    std::getline(infile, line);
    // Strip white space from the beginning of the line.
    line.erase(line.begin(), std::find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments.
    if (line[0] == '#' ||
        (line[0] == '/' && line[1] == '/')) continue;
    // Extract the values.
    dataStream.str(line);
    dataStream >> energy >> eps1 >> eps2 >> loss;
    if (dataStream.eof()) break;
    // Check if the data has been read correctly.
    if (infile.fail()) {
      std::cerr << className << "::LoadOpticalData:\n";
      std::cerr << "    Error reading file "
                << filename << " (line " << i << ").\n";
      return false;
    }
    // Reset the stringstream.
    dataStream.str("");
    dataStream.clear();
    // Make sure the values make sense.
    // The table has to be in ascending order
    //  with respect to the photon energy.
    if (energy <= lastEnergy) {
      std::cerr << className << "::LoadOpticalData:\n";
      std::cerr << "    Table is not in monotonically " 
                << "increasing order (line " << i << ").\n";
      std::cerr << "    " << lastEnergy << "  " << energy << "  " << eps1 << "  " << eps2 << "\n";
      return false;
    }
    // The imaginary part of the dielectric function has to be positive.
    if (eps2 < 0.) {
      std::cerr << className << "::LoadOpticalData:\n";
      std::cerr << "    Negative value of the loss function "
                << "(line " << i << ").\n";
      return false;
    }
    // Ignore negative photon energies.
    if (energy <= 0.) continue;
    // Add the values to the list.
    data.energy = energy;
    data.eps1 = eps1;
    data.eps2 = eps2;
    opticalDataTable.push_back(data);
    lastEnergy = energy;
  }
  
  const int nEntries = opticalDataTable.size();
  if (nEntries <= 0) {
    std::cerr << className << "::LoadOpticalData:\n";
    std::cerr << "    Import of data from file " << filepath << "failed.\n";
    std::cerr << "    No valid data found.\n";
    return false;
  }
  
  if (debug) {
    std::cout << className << "::LoadOpticalData:\n";
    std::cout << "    Read " << nEntries << " values from file " 
              << filepath << "\n";
  }
  return true;

}


bool
MediumSilicon::ElectronScatteringRates() {

  // Reset the scattering rates
  cfTotElectronsX.resize(nEnergySteps);
  cfTotElectronsL.resize(nEnergySteps);
  cfTotElectronsG.resize(nEnergySteps);
  cfElectronsX.resize(nEnergySteps);
  cfElectronsL.resize(nEnergySteps);
  cfElectronsG.resize(nEnergySteps);
  for (int i = nEnergySteps; i--;) {
    cfTotElectronsX[i] = 0.;
    cfTotElectronsL[i] = 0.;
    cfTotElectronsG[i] = 0.;
    cfElectronsX[i].clear();
    cfElectronsL[i].clear();
    cfElectronsG[i].clear();
  }
  energyLossElectronsX.clear();
  energyLossElectronsL.clear();
  energyLossElectronsG.clear();
  scatTypeElectronsX.clear();
  scatTypeElectronsL.clear();
  scatTypeElectronsG.clear();
  cfNullElectronsX = 0.;
  cfNullElectronsL = 0.;
  cfNullElectronsG = 0.;
  
  nLevelsX = 0;
  nLevelsL = 0;
  nLevelsG = 0;
  // Fill the scattering rates table
  ElectronAcousticScatteringRates();
  ElectronImpurityScatteringRates();
  ElectronIntervalleyScatteringRatesXX();
  ElectronIntervalleyScatteringRatesXL();
  ElectronIntervalleyScatteringRatesLL();
  ElectronIonisationRates();

  std::ofstream outfileX;
  std::ofstream outfileL;
  std::ofstream outfileG;  
  if (useCfOutput) {
    outfileX.open("ratesX.txt", std::ios::out);
    outfileL.open("ratesL.txt", std::ios::out);
    outfileG.open("ratesG.txt", std::ios::out);
  }

  ieMinL = int(eMinL / eStep) + 1;
  for (int i = 0; i < nEnergySteps; ++i) {
    // Sum up the scattering rates of all processes. 
    for (int j = nLevelsX; j--;) cfTotElectronsX[i] += cfElectronsX[i][j];
    for (int j = nLevelsL; j--;) cfTotElectronsL[i] += cfElectronsL[i][j];
    for (int j = nLevelsG; j--;) cfTotElectronsG[i] += cfElectronsG[i][j];
    
    if (useCfOutput) {
      outfileX << i * eStep << " " << cfTotElectronsX[i] << " ";
      for (int j = 0; j < nLevelsX; ++j) {
        outfileX << cfElectronsX[i][j] << " ";
      }
      outfileX << "\n";
      outfileL << i * eStep << " " << cfTotElectronsL[i] << " ";
      for (int j = 0; j < nLevelsL; ++j) {
        outfileL << cfElectronsL[i][j] << " ";
      }
      outfileL << "\n";
      outfileG << i * eStep << " " << cfTotElectronsG[i] << " ";
      for (int j = 0; j < nLevelsG; ++j) {
        outfileG << cfElectronsG[i][j] << " ";
      }
      outfileG << "\n";
    }

    if (cfTotElectronsX[i] > cfNullElectronsX) {
      cfNullElectronsX = cfTotElectronsX[i];
    }
    if (cfTotElectronsL[i] > cfNullElectronsL) {
      cfNullElectronsL = cfTotElectronsL[i];
    }
    if (cfTotElectronsG[i] > cfNullElectronsG) {
      cfNullElectronsG = cfTotElectronsG[i];
    }
   
    // Make sure the total scattering rate is positive
    if (cfTotElectronsX[i] <= 0.) { 
      std::cerr << className << "::ElectronScatteringRates:\n";
      std::cerr << "    X-valley scattering rate at " 
                << i * eStep << " eV <= 0.\n"; 
      return false;
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsX; ++j) {
      cfElectronsX[i][j] /= cfTotElectronsX[i];
      if (j > 0) cfElectronsX[i][j] += cfElectronsX[i][j - 1];
    }  

    if (cfTotElectronsL[i] <= 0.) { 
      if (i < ieMinL) continue;
      std::cerr << className << "::ElectronScatteringRates:\n";
      std::cerr << "    L-valley scattering rate at " 
                << i * eStep << " eV <= 0.\n"; 
      return false;
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsL; ++j) {
      cfElectronsL[i][j] /= cfTotElectronsL[i];
      if (j > 0) cfElectronsL[i][j] += cfElectronsL[i][j - 1];
    }
    
    if (cfTotElectronsG[i] <= 0.) {
      if (i < ieMinG) continue;
      std::cerr << className << "::ElectronScatteringRates:\n";
      std::cerr << "    Higher band scattering rate at "
                << i * eStep << " eV <= 0.\n";
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsG; ++j) {
      cfElectronsG[i][j] /= cfTotElectronsG[i];
      if (j > 0) cfElectronsG[i][j] += cfElectronsG[i][j - 1];
    }

  }
  if (useCfOutput) {
    outfileX.close();
    outfileL.close();
    outfileG.close();
  }

  // Reset the collision counters.
  ResetCollisionCounters();

  return true;

}

bool 
MediumSilicon::ElectronAcousticScatteringRates() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [(eV/c2)/cm3]
  const double rho = density * atomicWeight * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  // Acoustic phonon intraband scattering  
  // Acoustic deformation potential [eV]
  const double defpot = 9.;
  // Longitudinal velocity of sound [cm/ns]
  const double u = 9.04e-4;
  // Prefactor for acoustic deformation potential scattering
  const double cIntra = TwoPi * SpeedOfLight * SpeedOfLight * 
                        kbt * defpot * defpot /
                        (Hbar * u * u * rho); 
  
  double en = Small;
  for (int i = 0; i < nEnergySteps; ++i) {
    const double dosX = GetConductionBandDensityOfStates(en, 0);
    const double dosL = GetConductionBandDensityOfStates(en, nValleysX);
    const double dosG = GetConductionBandDensityOfStates(en, nValleysX + 
                                                             nValleysL);
    cfElectronsX[i].push_back(cIntra * dosX);
    cfElectronsL[i].push_back(cIntra * dosL);
    cfElectronsG[i].push_back(cIntra * dosG);
    en += eStep;
  }
  
  energyLossElectronsX.push_back(0.);
  energyLossElectronsL.push_back(0.);
  energyLossElectronsG.push_back(0.);
  scatTypeElectronsX.push_back(ElectronCollisionTypeAcousticPhonon);
  scatTypeElectronsL.push_back(ElectronCollisionTypeAcousticPhonon);
  scatTypeElectronsG.push_back(ElectronCollisionTypeAcousticPhonon);
  ++nLevelsX;
  ++nLevelsL;
  ++nLevelsG;

  return true;

}

bool 
MediumSilicon::ElectronIntervalleyScatteringRatesXX() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [(eV/c2)/cm3]
  const double rho = density * atomicWeight * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  const int nPhonons = 6;
  // f-type scattering: transition between orthogonal axes (multiplicity 4)
  // g-type scattering: transition between opposite axes (multiplicity 1)
  // Sequence of transitions in the table:
  // TA (g) - LA (g) - LO (g) - TA (f) - LA (f) - TO (f)
  // Coupling constants [eV/cm]
  const double dtk[nPhonons] = {0.5e8, 0.8e8, 1.1e9, 
                                0.3e8, 2.0e8, 2.0e8};
  // Phonon energies [eV]
  const double eph[nPhonons] = {12.06e-3, 18.53e-3, 62.04e-3, 
                                18.86e-3, 47.39e-3, 59.03e-3};
  // Phonon cccupation numbers
  double nocc[nPhonons];
  // Prefactors
  const double c0 = HbarC * SpeedOfLight * Pi / rho;
  double c[nPhonons];

  for (int j = 0; j < nPhonons; ++j) {
    nocc[j] = 1. / (exp(eph[j] / kbt) - 1);
    c[j] = c0 * dtk[j] * dtk[j] / eph[j];
    if (j > 2) c[j] *= 4;
  }

  double en = 0.;
  double dos = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    for (int j = 0; j < nPhonons; ++j) {
      // Absorption
      dos = GetConductionBandDensityOfStates(en + eph[j], 0);
      cfElectronsX[i].push_back(c[j] * nocc[j] * dos);      
      // Emission
      if (en > eph[j]) {
        dos = GetConductionBandDensityOfStates(en - eph[j], 0);
        cfElectronsX[i].push_back(c[j] * (nocc[j] + 1) * dos);
      } else {
        cfElectronsX[i].push_back(0.);
      }
    }
    en += eStep;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption
    energyLossElectronsX.push_back(-eph[j]);
    // Emission
    energyLossElectronsX.push_back(eph[j]);
    if (j <= 2) {
      scatTypeElectronsX.push_back(ElectronCollisionTypeIntervalleyG);
      scatTypeElectronsX.push_back(ElectronCollisionTypeIntervalleyG);
    } else {
      scatTypeElectronsX.push_back(ElectronCollisionTypeIntervalleyF);
      scatTypeElectronsX.push_back(ElectronCollisionTypeIntervalleyF);
    }
  }

  nLevelsX += 2 * nPhonons;

  return true;

}

bool 
MediumSilicon::ElectronIntervalleyScatteringRatesXL() {

  // Reference:
  // - M. Lundstrom, Fundamentals of carrier transport
  // - M. Martin et al.,
  //   Semicond. Sci. Technol. 8, 1291-1297
  
  // Mass density [(eV/c2)/cm3]
  const double rho = density * atomicWeight * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  const int nPhonons = 4;

  // Coupling constants [eV/cm]
  const double dtk[nPhonons] = {2.e8, 2.e8, 2.e8, 2.e8};
  // Phonon energies [eV]
  const double eph[nPhonons] = {58.e-3, 55.e-3, 41.e-3, 17.e-3};
  // Number of equivalent valleys
  const int zX = 6;
  const int zL = 8; 

  // Phonon cccupation numbers
  double nocc[nPhonons] = {0.};
  // Prefactors
  const double c0 = HbarC * SpeedOfLight * Pi / rho;
  double c[nPhonons];

  for (int j = 0; j < nPhonons; ++j) {
    nocc[j] = 1. / (exp(eph[j] / kbt) - 1);
    c[j] = c0 * dtk[j] * dtk[j] / eph[j];
  }

  double en = 0.;
  double dos = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    for (int j = 0; j < nPhonons; ++j) {
      // XL  
      // Absorption
      if (en + eph[j] > eMinL) {
        dos = GetConductionBandDensityOfStates(en + eph[j], nValleysX);
        cfElectronsX[i].push_back(zL * c[j] * nocc[j] * dos);
      } else {
        cfElectronsX[i].push_back(0.);
      }
      // Emission
      if (en - eph[j] > eMinL) {
        dos = GetConductionBandDensityOfStates(en - eph[j], nValleysX);
        cfElectronsX[i].push_back(zL * c[j] * (nocc[j] + 1) * dos);
      } else {
        cfElectronsX[i].push_back(0.);
      }
      // LX
      if (en > eMinL) {
        // Absorption
        dos = GetConductionBandDensityOfStates(en + eph[j], 0);
        cfElectronsL[i].push_back(zX * c[j] * nocc[j] * dos);
        // Emission
        dos = GetConductionBandDensityOfStates(en - eph[j], 0);
        cfElectronsL[i].push_back(zX * c[j] * nocc[j] * dos);
      } else {
        cfElectronsL[i].push_back(0.);
        cfElectronsL[i].push_back(0.);
      }
    }
    en += eStep;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption
    energyLossElectronsX.push_back(-eph[j]);
    energyLossElectronsL.push_back(-eph[j]);
    // Emission
    energyLossElectronsX.push_back(eph[j]);
    energyLossElectronsL.push_back(eph[j]);
    scatTypeElectronsX.push_back(ElectronCollisionTypeInterband);
    scatTypeElectronsX.push_back(ElectronCollisionTypeInterband);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterband);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterband);
  }

  nLevelsX += 2 * nPhonons;
  nLevelsL += 2 * nPhonons;

  return true;

}

bool 
MediumSilicon::ElectronIntervalleyScatteringRatesLL() {

  // Reference:
  //  - M. J. Martin et al.,
  //    Semicond. Sci. Technol. 8, 1291-1297
  
  // Mass density [(eV/c2)/cm3]
  const double rho = density * atomicWeight * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;  

  const int nPhonons = 1;
  // Coupling constant [eV/cm]
  const double dtk[nPhonons] = {2.63e8};
  // Phonon energy [eV]
  const double eph[nPhonons] = {38.87e-3};
  // Phonon cccupation numbers
  double nocc[nPhonons];
  // Prefactors
  const double c0 = HbarC * SpeedOfLight * Pi / rho;
  double c[nPhonons];

  for (int j = 0; j < nPhonons; ++j) {
    nocc[j] = 1. / (exp(eph[j] / kbt) - 1);
    c[j] = c0 * dtk[j] * dtk[j] / eph[j];
    c[j] *= 7;
  }

  double en = 0.;
  double dos = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    for (int j = 0; j < nPhonons; ++j) {
      // Absorption
      dos = GetConductionBandDensityOfStates(en + eph[j], nValleysX);
      cfElectronsL[i].push_back(c[j] * nocc[j] * dos);      
      // Emission
      if (en > eMinL + eph[j]) {
        dos = GetConductionBandDensityOfStates(en - eph[j], nValleysX);
        cfElectronsL[i].push_back(c[j] * (nocc[j] + 1) * dos);
      } else {
        cfElectronsL[i].push_back(0.);
      }
    }
    en += eStep;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption
    energyLossElectronsL.push_back(-eph[j]);
    // Emission
    energyLossElectronsL.push_back(eph[j]);
    scatTypeElectronsL.push_back(ElectronCollisionTypeIntervalleyF);
    scatTypeElectronsL.push_back(ElectronCollisionTypeIntervalleyF);
  }

  nLevelsL += 2 * nPhonons;

  return true;

}

bool
MediumSilicon::ElectronIonisationRates() {

  // References:
  // - E. Cartier, M. V. Fischetti, E. A. Eklund and F. R. McFeely,
  //   Appl. Phys. Lett 62, 3339-3341
  // - DAMOCLES web page: www.research.ibm.com/DAMOCLES

  // Coefficients [ns-1]
  const double p[3] = {6.25e1, 3.e3, 6.8e5};
  // Threshold energies [eV]
  const double eth[3] = {1.2, 1.8, 3.45};

  double en = 0.;
  double fIon = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    fIon = 0.;
    if (en > eth[0]) {
      fIon += p[0] * (en - eth[0]) * (en - eth[0]);
    }
    if (en > eth[1]) {
      fIon += p[1] * (en - eth[1]) * (en - eth[1]);
    }
    if (en > eth[2]) {
      fIon += p[2] * (en - eth[2]) * (en - eth[2]);
    }
    cfElectronsX[i].push_back(fIon);
    cfElectronsL[i].push_back(fIon);
    cfElectronsG[i].push_back(fIon);
    en += eStep;
  }

  energyLossElectronsX.push_back(eth[0]);
  energyLossElectronsL.push_back(eth[0]);
  energyLossElectronsG.push_back(eth[0]);
  scatTypeElectronsX.push_back(ElectronCollisionTypeIonisation);
  scatTypeElectronsL.push_back(ElectronCollisionTypeIonisation);
  scatTypeElectronsG.push_back(ElectronCollisionTypeIonisation);  
  ++nLevelsX;
  ++nLevelsL;
  ++nLevelsG;

  return true;

}

bool
MediumSilicon::ElectronImpurityScatteringRates() {

  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * temperature;

  // Band parameters
  // Density of states effective masses
  const double mdX = ElectronMass * 
                     pow(mLongX * mTransX * mTransX, 1. / 3.);
  const double mdL = ElectronMass *
                     pow(mLongL * mTransL * mTransL, 1. / 3.);
  // Non-parabolicity coefficients [eV-1]
  const double alphaX = 0.5;
  const double alphaL = 0.3;

  // Dielectric constant
  const double eps = GetDielectricConstant();
  // Impurity concentration
  const double impurityConcentration = dopingConcentration;
  if (impurityConcentration < Small) return true;

  // Screening length
  const double ls = sqrt(eps * kbt / (4 * Pi * 
                                      FineStructureConstant * HbarC * 
                                      impurityConcentration));
  const double ebX = 0.5 * HbarC * HbarC / (mdX * ls * ls);
  const double ebL = 0.5 * HbarC * HbarC / (mdL * ls * ls);

  // Prefactor
  // const double c = pow(2., 2.5) * Pi * impurityConcentration * 
  //                 pow(FineStructureConstant * HbarC, 2) *
  //                 SpeedOfLight / (eps * eps * sqrt(md) * eb * eb);
  // Use momentum-transfer cross-section
  const double cX = impurityConcentration * Pi * 
                    pow(FineStructureConstant * HbarC, 2) * 
                    SpeedOfLight / 
                    (sqrt(2 * mdX) * eps * eps);
  const double cL = impurityConcentration * Pi * 
                    pow(FineStructureConstant * HbarC, 2) * 
                    SpeedOfLight / 
                    (sqrt(2 * mdL) * eps * eps);
  
  double en = 0.;
  for (int i = 0; i < nEnergySteps; ++i) {
    const double gammaX = en * (1. + alphaX * en);
    const double gammaL = en * (1. + alphaL * en);
    // cfElectrons[i][iLevel] = c * sqrt(gamma) * (1. + 2 * alpha * en) /
    //                         (1. + 4. * gamma / eb);
    if (gammaX <= 0.) {
      cfElectronsX[i].push_back(0.);
    } else {
      const double b = 4 * gammaX / ebX;
      cfElectronsX[i].push_back((cX / pow(gammaX, 1.5)) * 
                                (log(1. + b) - b / (1. + b)));
    }
    if (gammaL <= 0.) {
      cfElectronsL[i].push_back(0.);
    } else {
      const double b = 4 * gammaL / ebL;
      cfElectronsL[i].push_back((cL / pow(gammaL, 1.5)) *
                                (log(1. + b) - b / (1. + b)));
    }
    en += eStep;
  }

  energyLossElectronsX.push_back(0.);
  energyLossElectronsL.push_back(0.);
  scatTypeElectronsX.push_back(ElectronCollisionTypeImpurity); 
  scatTypeElectronsL.push_back(ElectronCollisionTypeImpurity);
  ++nLevelsX;
  ++nLevelsL;

  return true;

}

double
MediumSilicon::GetConductionBandDensityOfStates(const double e, 
                                                const int band) {
  if (band < 0) {
    // Total (full-band) density of states.
    const int nFbDosEntries = 101;
    const double fbDos[nFbDosEntries] = {
      0.,      1.5114,  2.71026,  3.67114,  4.40173, 
      5.05025, 5.6849,  6.28358,  6.84628,  7.43859, 
      8.00204, 8.80658, 9.84885, 10.9579,  12.0302,
     13.2051, 14.6948, 16.9879,  18.4492,  18.1933,
     17.6747, 16.8135, 15.736,   14.4965,  13.1193,
     12.1817, 12.6109, 15.3148,  19.4936,  23.0093,
     24.4106, 22.2834, 19.521,   18.9894,  18.8015,
     17.9363, 17.0252, 15.9871,  14.8486,  14.3797,
     14.2426, 14.3571, 14.7271,  14.681,   14.3827,
     14.2789, 14.144,  14.1684,  14.1418,  13.9237,
     13.7558, 13.5691, 13.4567,  13.2693,  12.844,
     12.4006, 12.045,  11.7729,  11.3607,  11.14,
     11.0586, 10.5475,  9.73786,  9.34423,  9.4694, 
      9.58071, 9.6967,  9.84854, 10.0204,   9.82705, 
      9.09102, 8.30665, 7.67306,  7.18925,  6.79675, 
      6.40713, 6.21687, 6.33267,  6.5223,   6.17877, 
      5.48659, 4.92208, 4.44239,  4.02941,  3.5692, 
      3.05953, 2.6428,  2.36979,  2.16273,  2.00627, 
      1.85206, 1.71265, 1.59497,  1.46681,  1.34913, 
      1.23951, 1.13439, 1.03789,  0.924155, 0.834962, 
      0.751017};
      
    const double de = 0.1;
    int iE = int(e / de);
    if (iE >= nFbDosEntries || iE < 0) return 0.;
    else if (iE == nFbDosEntries - 1) return fbDos[nFbDosEntries - 1];
    
    const double dos = fbDos[iE] + 
                       (fbDos[iE + 1] - fbDos[iE]) * (e / de - iE);
    return dos * 1.e21;
    
  } else if (band < nValleysX) {
    // X valleys
    if (e <= 0.) return 0.;
    // Density-of-states effective mass (cube)
    const double md3 = pow(ElectronMass, 3) * (mLongX * mTransX * mTransX);
 
    if (useFullBandDos) {
      if (e < eMinL) {
        return GetConductionBandDensityOfStates(e, -1) / nValleysX;
      } else if (e < eMinG) {
        // Subtract the fraction of the full-band density of states 
        // attributed to the L valleys.
        const double dosX = 
          GetConductionBandDensityOfStates(e, -1) -
          GetConductionBandDensityOfStates(e, nValleysX) * nValleysL;
        return dosX / nValleysX;
      } else {
        // Subtract the fraction of the full-band density of states 
        // attributed to the L valleys and the higher bands.
        const double dosX = 
          GetConductionBandDensityOfStates(e, -1) -
          GetConductionBandDensityOfStates(e, nValleysX) * nValleysL -
          GetConductionBandDensityOfStates(e, nValleysX + nValleysL);
        if (dosX <= 0.) return 0.;
        return dosX / nValleysX;
      }
    } else if (useNonParabolicity) {
      const double alpha = 0.5;
      return sqrt(md3 * e * (1. + alpha * e) / 2.) * 
                  (1. + 2 * alpha * e) / (Pi2 * pow(HbarC, 3.));
    } else {
      return sqrt(md3 * e / 2.) / (Pi2 * pow(HbarC, 3.));
    }      
  } else if (band < nValleysX + nValleysL) {
    // L valleys
    if (e <= eMinL) return 0.;
    
    // Density-of-states effective mass (cube)
    const double md3 = pow(ElectronMass, 3) * (mLongL * mTransL * mTransL);
    // Non-parabolicity parameter
    const double alpha = 0.3;
    
    if (useFullBandDos) {
      // Energy up to which the non-parabolic approximation is used.
      const double ej = eMinL + 0.5;
      if (e <= ej) {
        return sqrt(md3 * (e - eMinL) * (1. + alpha * (e - eMinL))) * 
                    (1. + 2 * alpha * (e - eMinL)) / 
                    (Sqrt2 * Pi2 * pow(HbarC, 3.));
      } else {
        // Fraction of full-band density of states attributed to L valleys
        double fL = sqrt(md3 * (ej - eMinL) * (1. + alpha * (ej - eMinL))) * 
                         (1. + 2 * alpha * (ej - eMinL)) / 
                         (Sqrt2 * Pi2 * pow(HbarC, 3.));
        fL = nValleysL * fL / GetConductionBandDensityOfStates(ej, -1);

        double dosXL = GetConductionBandDensityOfStates(e, -1);
        if (e > eMinG) {
          dosXL -= GetConductionBandDensityOfStates(e, nValleysX + nValleysL);
        }
        if (dosXL <= 0.) return 0.;
        return fL * dosXL / 8.;
      }
    } else if (useNonParabolicity) {
      return sqrt(md3 * (e - eMinL) * (1. + alpha * (e - eMinL))) * 
                  (1. + 2 * alpha * (e - eMinL)) / 
                  (Sqrt2 * Pi2 * pow(HbarC, 3.));
    } else {
      return sqrt(md3 * (e - eMinL) / 2.) / (Pi2 * pow(HbarC, 3.));
    }      
  } else if (band == nValleysX + nValleysL) {
    // Higher bands
    const double ej = 2.7;
    if (eMinG >= ej) {
      std::cerr << className << "::GetConductionBandDensityOfStates:\n";
      std::cerr << "    Cannot determine higher band density-of-states.\n";
      std::cerr << "    Program bug. Check offset energy!\n";
      return 0.;
    }
    if (e < eMinG) {
      return 0.;
    } else if (e < ej) {
      // Coexistence of XL and higher bands.
      const double dj = GetConductionBandDensityOfStates(ej, -1);
      // Assume linear increase of density-of-states.
      return dj * (e - eMinG) / (ej - eMinG);
    } else {
      return GetConductionBandDensityOfStates(e, -1);
    }
  }
  
  std::cerr << className << "::GetConductionBandDensityOfStates:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return ElectronMass * sqrt(ElectronMass * e / 2.) / 
         (Pi2 * pow(HbarC, 3.));

}
  
}
