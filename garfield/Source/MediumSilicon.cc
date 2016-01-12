#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <vector>

#include "MediumSilicon.hh"
#include "Random.hh"
#include "GarfieldConstants.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumSilicon::MediumSilicon()
    : Medium(),
      m_bandGap(1.12),
      m_dopingType('i'),
      m_dopingConcentration(0.),
      mLongX(0.916),
      mTransX(0.191),
      mLongL(1.59),
      mTransL(0.12),
      alphaX(0.5),
      alphaL(0.5),
      eLatticeMobility(1.35e-6),
      hLatticeMobility(0.45e-6),
      eMobility(1.35e-6),
      hMobility(0.45e-6),
      eBetaCanali(1.109),
      hBetaCanali(1.213),
      eBetaCanaliInv(1. / 1.109),
      hBetaCanaliInv(1. / 1.213),
      eSatVel(1.02e-2),
      hSatVel(0.72e-2),
      eHallFactor(1.15),
      hHallFactor(0.7),
      eTrapCs(1.e-15),
      hTrapCs(1.e-15),
      eTrapDensity(1.e13),
      hTrapDensity(1.e13),
      eTrapTime(0.),
      hTrapTime(0.),
      trappingModel(0),
      eImpactA0(3.318e5),
      eImpactA1(0.703e6),
      eImpactA2(0.),
      eImpactB0(1.135e6),
      eImpactB1(1.231e6),
      eImpactB2(0.),
      hImpactA0(1.582e6),
      hImpactA1(0.671e6),
      hImpactB0(2.036e6),
      hImpactB1(1.693e6),
      m_hasUserMobility(false),
      m_hasUserSaturationVelocity(false),
      latticeMobilityModel(LatticeMobilityModelSentaurus),
      dopingMobilityModel(DopingMobilityModelMasetti),
      saturationVelocityModel(SaturationVelocityModelCanali),
      highFieldMobilityModel(HighFieldMobilityModelCanali),
      impactIonisationModel(ImpactIonisationModelVanOverstraeten),
      useCfOutput(false),
      useNonParabolicity(true),
      useFullBandDos(true),
      useAnisotropy(true),
      eFinalXL(4.),
      eStepXL(eFinalXL / nEnergyStepsXL),
      eFinalG(10.),
      eStepG(eFinalG / nEnergyStepsG),
      eFinalV(8.5),
      eStepV(eFinalV / nEnergyStepsV),
      nLevelsX(0),
      nLevelsL(0),
      nLevelsG(0),
      nLevelsV(0),
      nValleysX(6),
      nValleysL(8),
      eMinL(1.05),
      eMinG(2.24),
      ieMinL(0),
      ieMinG(0),
      m_hasOpticalData(false),
      opticalDataFile("OpticalData_Si.txt") {

  m_className = "MediumSilicon";
  m_name = "Si";

  SetTemperature(300.);
  SetDielectricConstant(11.9);
  SetAtomicNumber(14.);
  SetAtomicWeight(28.0855);
  SetMassDensity(2.329);

  EnableDrift();
  EnablePrimaryIonisation();
  m_microscopic = true;

  m_w = 3.6;
  m_fano = 0.11;

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

  ieMinL = int(eMinL / eStepXL) + 1;
  ieMinG = int(eMinG / eStepG) + 1;

  cfTotHoles.clear();
  cfHoles.clear();
  energyLossHoles.clear();
  scatTypeHoles.clear();

  // Load the density of states table.
  InitialiseDensityOfStates();

  // Initialize the collision counters.
  nCollElectronAcoustic = nCollElectronOptical = 0;
  nCollElectronIntervalley = 0;
  nCollElectronImpurity = 0;
  nCollElectronIonisation = 0;
  nCollElectronDetailed.clear();
  nCollElectronBand.clear();

  nIonisationProducts = 0;
  ionProducts.clear();
}

void MediumSilicon::SetDoping(const char& type, const double& c) {

  if (toupper(type) == 'N') {
    m_dopingType = 'n';
    if (c > Small) {
      m_dopingConcentration = c;
    } else {
      std::cerr << m_className << "::SetDoping:\n";
      std::cerr << "    Doping concentration must be greater than zero.\n";
      std::cerr << "    Using default value for n-type silicon "
                << "(10^12 cm-3) instead.\n";
      m_dopingConcentration = 1.e12;
    }
  } else if (toupper(type) == 'P') {
    m_dopingType = 'p';
    if (c > Small) {
      m_dopingConcentration = c;
    } else {
      std::cerr << m_className << "::SetDoping:\n";
      std::cerr << "    Doping concentration must be greater than zero.\n";
      std::cerr << "    Using default value for p-type silicon "
                << "(10^18 cm-3) instead.\n";
      m_dopingConcentration = 1.e18;
    }
  } else if (toupper(type) == 'I') {
    m_dopingType = 'i';
    m_dopingConcentration = 0.;
  } else {
    std::cerr << m_className << "::SetDoping:\n";
    std::cerr << "    Unknown dopant type (" << type << ").\n";
    std::cerr << "    Available types are n, p and i (intrinsic).\n";
    return;
  }

  m_isChanged = true;
}

void MediumSilicon::GetDoping(char& type, double& c) const {

  type = m_dopingType;
  c = m_dopingConcentration;
}

void MediumSilicon::SetTrapCrossSection(const double& ecs, const double& hcs) {

  if (ecs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n";
    std::cerr << "    Capture cross-section [cm2] must positive.\n";
  } else {
    eTrapCs = ecs;
  }

  if (hcs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n";
    std::cerr << "    Capture cross-section [cm2] must be positive.n";
  } else {
    hTrapCs = hcs;
  }

  trappingModel = 0;
  m_isChanged = true;
}

void MediumSilicon::SetTrapDensity(const double& n) {

  if (n < 0.) {
    std::cerr << m_className << "::SetTrapDensity:\n";
    std::cerr << "    Trap density [cm-3] must be greater than zero.\n";
  } else {
    eTrapDensity = n;
    hTrapDensity = n;
  }

  trappingModel = 0;
  m_isChanged = true;
}

void MediumSilicon::SetTrappingTime(const double& etau, const double& htau) {

  if (etau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n";
    std::cerr << "    Trapping time [ns-1] must be positive.\n";
  } else {
    eTrapTime = etau;
  }

  if (htau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n";
    std::cerr << "    Trapping time [ns-1] must be positive.\n";
  } else {
    hTrapTime = htau;
  }

  trappingModel = 1;
  m_isChanged = true;
}

bool MediumSilicon::ElectronVelocity(const double ex, const double ey,
                                     const double ez, const double bx,
                                     const double by, const double bz,
                                     double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::ElectronVelocity:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasElectronVelocityE) {
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

  const double b = sqrt(bx * bx + by * by + bz * bz);
  if (b < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = eHallFactor * mu;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(muH * b, 2);
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH * muH * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH * muH * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH * muH * bz * eb) / nom;
  }
  return true;
}

bool MediumSilicon::ElectronTownsend(const double ex, const double ey,
                                     const double ez, const double bx,
                                     const double by, const double bz,
                                     double& alpha) {

  alpha = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::ElectronTownsend:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasElectronTownsend) {
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
      std::cerr << m_className << "::ElectronTownsend:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
      break;
  }
  return false;
}

bool MediumSilicon::ElectronAttachment(const double ex, const double ey,
                                       const double ez, const double bx,
                                       const double by, const double bz,
                                       double& eta) {

  eta = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::ElectronAttachment:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasElectronAttachment) {
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
      std::cerr << m_className << "::ElectronAttachment:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }

  return true;
}

bool MediumSilicon::HoleVelocity(const double ex, const double ey,
                                 const double ez, const double bx,
                                 const double by, const double bz, double& vx,
                                 double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::HoleVelocity:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasHoleVelocityE) {
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

  const double b = sqrt(bx * bx + by * by + bz * bz);
  if (b < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = hHallFactor * mu;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(muH * b, 2);
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH * muH * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH * muH * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH * muH * bz * eb) / nom;
  }
  return true;
}

bool MediumSilicon::HoleTownsend(const double ex, const double ey,
                                 const double ez, const double bx,
                                 const double by, const double bz,
                                 double& alpha) {

  alpha = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::HoleTownsend:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasHoleTownsend) {
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
      std::cerr << m_className << "::HoleTownsend:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
      break;
  }
  return false;
}

bool MediumSilicon::HoleAttachment(const double ex, const double ey,
                                   const double ez, const double bx,
                                   const double by, const double bz,
                                   double& eta) {

  eta = 0.;
  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::HoleAttachment:\n";
      std::cerr << "    Error calculating the transport parameters.\n";
      return false;
    }
    m_isChanged = false;
  }

  if (m_hasHoleAttachment) {
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
      std::cerr << m_className << "::HoleAttachment:\n";
      std::cerr << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }

  return true;
}

void MediumSilicon::SetLowFieldMobility(const double mue, const double muh) {

  if (mue <= 0. || muh <= 0.) {
    std::cerr << m_className << "::SetLowFieldMobility:\n";
    std::cerr << "    Mobility must be greater than zero.\n";
    return;
  }

  eMobility = mue;
  hMobility = muh;
  m_hasUserMobility = true;
  m_isChanged = true;
}

void MediumSilicon::SetLatticeMobilityModelMinimos() {

  latticeMobilityModel = LatticeMobilityModelMinimos;
  m_hasUserMobility = false;
  m_isChanged = true;
}

void MediumSilicon::SetLatticeMobilityModelSentaurus() {

  latticeMobilityModel = LatticeMobilityModelSentaurus;
  m_hasUserMobility = false;
  m_isChanged = true;
}

void MediumSilicon::SetLatticeMobilityModelReggiani() {

  latticeMobilityModel = LatticeMobilityModelReggiani;
  m_hasUserMobility = false;
  m_isChanged = true;
}

void MediumSilicon::SetDopingMobilityModelMinimos() {

  dopingMobilityModel = DopingMobilityModelMinimos;
  m_hasUserMobility = false;
  m_isChanged = true;
}

void MediumSilicon::SetDopingMobilityModelMasetti() {

  dopingMobilityModel = DopingMobilityModelMasetti;
  m_hasUserMobility = false;
  m_isChanged = true;
}

void MediumSilicon::SetSaturationVelocity(const double vsate,
                                          const double vsath) {

  if (vsate <= 0. || vsath <= 0.) {
    std::cout << m_className << "::SetSaturationVelocity:\n";
    std::cout << "    Restoring default values.\n";
    m_hasUserSaturationVelocity = false;
  } else {
    eSatVel = vsate;
    hSatVel = vsath;
    m_hasUserSaturationVelocity = true;
  }

  m_isChanged = true;
}

void MediumSilicon::SetSaturationVelocityModelMinimos() {

  saturationVelocityModel = SaturationVelocityModelMinimos;
  m_hasUserSaturationVelocity = false;
  m_isChanged = true;
}

void MediumSilicon::SetSaturationVelocityModelCanali() {

  saturationVelocityModel = SaturationVelocityModelCanali;
  m_hasUserSaturationVelocity = false;
  m_isChanged = true;
}

void MediumSilicon::SetSaturationVelocityModelReggiani() {

  saturationVelocityModel = SaturationVelocityModelReggiani;
  m_hasUserSaturationVelocity = false;
  m_isChanged = true;
}

void MediumSilicon::SetHighFieldMobilityModelMinimos() {

  highFieldMobilityModel = HighFieldMobilityModelMinimos;
  m_isChanged = true;
}

void MediumSilicon::SetHighFieldMobilityModelCanali() {

  highFieldMobilityModel = HighFieldMobilityModelCanali;
  m_isChanged = true;
}

void MediumSilicon::SetHighFieldMobilityModelReggiani() {

  highFieldMobilityModel = HighFieldMobilityModelReggiani;
  m_isChanged = true;
}

void MediumSilicon::SetHighFieldMobilityModelConstant() {

  highFieldMobilityModel = HighFieldMobilityModelConstant;
}

void MediumSilicon::SetImpactIonisationModelVanOverstraetenDeMan() {

  impactIonisationModel = ImpactIonisationModelVanOverstraeten;
  m_isChanged = true;
}

void MediumSilicon::SetImpactIonisationModelGrant() {

  impactIonisationModel = ImpactIonisationModelGrant;
  m_isChanged = true;
}

bool MediumSilicon::SetMaxElectronEnergy(const double e) {

  if (e <= eMinG + Small) {
    std::cerr << m_className << "::SetMaxElectronEnergy:\n";
    std::cerr << "    Provided upper electron energy limit (" << e
              << " eV) is too small.\n";
    return false;
  }

  eFinalG = e;
  // Determine the energy interval size.
  eStepG = eFinalG / nEnergyStepsG;

  m_isChanged = true;

  return true;
}

double MediumSilicon::GetElectronEnergy(const double px, const double py,
                                        const double pz, double& vx, double& vy,
                                        double& vz, const int band) {

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
          mx *= mLongX;
          my *= mTransX;
          mz *= mTransX;
          break;
        case 2:
        case 3:
          // X 010, 0-10
          mx *= mTransX;
          my *= mLongX;
          mz *= mTransX;
          break;
        case 4:
        case 5:
          // X 001, 00-1
          mx *= mTransX;
          my *= mTransX;
          mz *= mLongX;
          break;
        default:
          std::cerr << m_className << "::GetElectronEnergy:\n";
          std::cerr << "    Unexpected band index " << band << "!\n";
          break;
      }
    } else {
      // Conduction effective mass
      const double mc = 3. / (1. / mLongX + 2. / mTransX);
      mx *= mc;
      my *= mc;
      mz *= mc;
    }
  } else if (band < nValleysX + nValleysL) {
    // L valley, isotropic approximation
    e0 = eMinL;
    // Effective mass
    const double mc = 3. / (1. / mLongL + 2. / mTransL);
    mx *= mc;
    my *= mc;
    mz *= mc;
  } else if (band == nValleysX + nValleysL) {
    // Higher band(s)
  }

  if (useNonParabolicity) {
    // Non-parabolicity parameter
    double alpha = 0.;
    if (band < nValleysX) {
      // X valley
      alpha = alphaX;
    } else if (band < nValleysX + nValleysL) {
      // L valley
      alpha = alphaL;
    }

    const double p2 = 0.5 * (px * px / mx + py * py / my + pz * pz / mz);
    if (alpha > 0.) {
      const double e = 0.5 * (sqrt(1. + 4 * alpha * p2) - 1.) / alpha;
      const double a = SpeedOfLight / (1. + 2 * alpha * e);
      vx = a * px / mx;
      vy = a * py / my;
      vz = a * pz / mz;
      return e0 + e;
    }
  }

  const double e = 0.5 * (px * px / mx + py * py / my + pz * pz / mz);
  vx = SpeedOfLight * px / mx;
  vy = SpeedOfLight * py / my;
  vz = SpeedOfLight * pz / mz;
  return e0 + e;
}

void MediumSilicon::GetElectronMomentum(const double e, double& px, double& py,
                                        double& pz, int& band) {

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
      const double dosG =
          GetConductionBandDensityOfStates(e, nValleysX + nValleysL);
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
    if (m_debug) {
      std::cout << m_className << "::GetElectronMomentum:\n";
      std::cout << "    Randomised band index: " << band << "\n";
    }
  }
  if (band < nValleysX) {
    // X valleys
    double pstar = sqrt(2. * ElectronMass * e);
    if (useNonParabolicity) {
      const double alpha = alphaX;
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
          std::cerr << m_className << "::GetElectronMomentum:\n";
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
      const double alpha = alphaL;
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

double MediumSilicon::GetElectronNullCollisionRate(const int band) {

  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::GetElectronNullCollisionRate:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return 0.;
    }
    m_isChanged = false;
  }

  if (band >= 0 && band < nValleysX) {
    return cfNullElectronsX;
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    return cfNullElectronsL;
  } else if (band == nValleysX + nValleysL) {
    return cfNullElectronsG;
  }
  std::cerr << m_className << "::GetElectronNullCollisionRate:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return 0.;
}

double MediumSilicon::GetElectronCollisionRate(const double e, const int band) {

  if (e <= 0.) {
    std::cerr << m_className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Electron energy must be positive.\n";
    return 0.;
  }

  if (e > eFinalG) {
    std::cerr << m_className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Collision rate at " << e << " eV (band " << band
              << ") is not included"
              << " in the current table.\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);
  }

  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::GetElectronCollisionRate:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return 0.;
    }
    m_isChanged = false;
  }

  if (band >= 0 && band < nValleysX) {
    int iE = int(e / eStepXL);
    if (iE >= nEnergyStepsXL)
      iE = nEnergyStepsXL - 1;
    else if (iE < 0)
      iE = 0;
    return cfTotElectronsX[iE];
  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    int iE = int(e / eStepXL);
    if (iE >= nEnergyStepsXL)
      iE = nEnergyStepsXL - 1;
    else if (iE < ieMinL)
      iE = ieMinL;
    return cfTotElectronsL[iE];
  } else if (band == nValleysX + nValleysL) {
    int iE = int(e / eStepG);
    if (iE >= nEnergyStepsG)
      iE = nEnergyStepsG - 1;
    else if (iE < ieMinG)
      iE = ieMinG;
    return cfTotElectronsG[iE];
  }

  std::cerr << m_className << "::GetElectronCollisionRate:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return 0.;
}

bool MediumSilicon::GetElectronCollision(const double e, int& type, int& level,
                                         double& e1, double& px, double& py,
                                         double& pz, int& nion, int& ndxc,
                                         int& band) {

  if (e > eFinalG) {
    std::cerr << m_className << "::GetElectronCollision:\n";
    std::cerr << "    Requested electron energy (" << e;
    std::cerr << " eV) exceeds current energy range (" << eFinalG;
    std::cerr << " eV).\n";
    std::cerr << "    Increasing energy range to " << 1.05 * e << " eV.\n";
    SetMaxElectronEnergy(1.05 * e);
  } else if (e <= 0.) {
    std::cerr << m_className << "::GetElectronCollision:\n";
    std::cerr << "    Electron energy must be greater than zero.\n";
    return false;
  }

  if (m_isChanged) {
    if (!UpdateTransportParameters()) {
      std::cerr << m_className << "::GetElectronCollision:\n";
      std::cerr << "    Error calculating the collision rates table.\n";
      return false;
    }
    m_isChanged = false;
  }

  // Energy loss
  double loss = 0.;
  // Sample the scattering process.
  if (band >= 0 && band < nValleysX) {
    // X valley
    // Get the energy interval.
    int iE = int(e / eStepXL);
    if (iE >= nEnergyStepsXL) iE = nEnergyStepsXL - 1;
    if (iE < 0) iE = 0;
    // Select the scattering process.
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
          band = 1;
          break;
        case 1:
          band = 0;
          break;
        case 2:
          band = 3;
          break;
        case 3:
          band = 2;
          break;
        case 4:
          band = 5;
          break;
        case 5:
          band = 4;
          break;
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
    } else if (type == ElectronCollisionTypeInterbandXL) {
      // XL scattering
      ++nCollElectronIntervalley;
      // Final valley is in L band.
      band = nValleysX + int(RndmUniform() * nValleysL);
      if (band >= nValleysX + nValleysL) band = nValleysX + nValleysL - 1;
    } else if (type == ElectronCollisionTypeInterbandXG) {
      ++nCollElectronIntervalley;
      band = nValleysX + nValleysL;
    } else if (type == ElectronCollisionTypeImpurity) {
      ++nCollElectronImpurity;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    } else {
      std::cerr << m_className << "::GetElectronCollision:\n";
      std::cerr << "    Unexpected collision type (" << type << ").\n";
    }

    // Get the energy loss.
    loss = energyLossElectronsX[level];

  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    // L valley
    // Get the energy interval.
    int iE = int(e / eStepXL);
    if (iE >= nEnergyStepsXL) iE = nEnergyStepsXL - 1;
    if (iE < ieMinL) iE = ieMinL;
    // Select the scattering process.
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
    } else if (type == ElectronCollisionTypeOpticalPhonon) {
      ++nCollElectronOptical;
    } else if (type == ElectronCollisionTypeIntervalleyG ||
               type == ElectronCollisionTypeIntervalleyF) {
      // Equivalent intervalley scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = nValleysX + int(RndmUniform() * nValleysL);
      if (band >= nValleysX + nValleysL) band = nValleysX + nValleysL;
    } else if (type == ElectronCollisionTypeInterbandXL) {
      // LX scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = int(RndmUniform() * nValleysX);
      if (band >= nValleysX) band = nValleysX - 1;
    } else if (type == ElectronCollisionTypeInterbandLG) {
      // LG scattering
      ++nCollElectronIntervalley;
      band = nValleysX + nValleysL;
    } else if (type == ElectronCollisionTypeImpurity) {
      ++nCollElectronImpurity;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    } else {
      std::cerr << m_className << "::GetElectronCollision:\n";
      std::cerr << "    Unexpected collision type (" << type << ").\n";
    }

    // Get the energy loss.
    loss = energyLossElectronsL[level];
  } else if (band == nValleysX + nValleysL) {
    // Higher bands
    // Get the energy interval.
    int iE = int(e / eStepG);
    if (iE >= nEnergyStepsG) iE = nEnergyStepsG - 1;
    if (iE < ieMinG) iE = ieMinG;
    // Select the scattering process.
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
    } else if (type == ElectronCollisionTypeOpticalPhonon) {
      ++nCollElectronOptical;
    } else if (type == ElectronCollisionTypeIntervalleyG ||
               type == ElectronCollisionTypeIntervalleyF) {
      // Equivalent intervalley scattering
      ++nCollElectronIntervalley;
    } else if (type == ElectronCollisionTypeInterbandXG) {
      // GX scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = int(RndmUniform() * nValleysX);
      if (band >= nValleysX) band = nValleysX - 1;
    } else if (type == ElectronCollisionTypeInterbandLG) {
      // GL scattering
      ++nCollElectronIntervalley;
      // Randomise the final valley.
      band = nValleysX + int(RndmUniform() * nValleysL);
      if (band >= nValleysX + nValleysL) band = nValleysX + nValleysL - 1;
    } else if (type == ElectronCollisionTypeIonisation) {
      ++nCollElectronIonisation;
    } else {
      std::cerr << m_className << "::GetElectronCollision:\n";
      std::cerr << "    Unexpected collision type (" << type << ").\n";
    }

    // Get the energy loss.
    loss = energyLossElectronsG[level];
  } else {
    std::cerr << m_className << "::GetElectronCollision:\n";
    std::cerr << "    Band index (" << band << ") out of range.\n";
    return false;
  }

  // Secondaries
  nion = ndxc = 0;
  // Ionising collision
  if (type == ElectronCollisionTypeIonisation) {
    double ee = 0., eh = 0.;
    ComputeSecondaries(e, ee, eh);
    loss = ee + eh + m_bandGap;
    ionProducts.clear();
    // Add the secondary electron.
    ionProd newIonProd;
    newIonProd.type = IonProdTypeElectron;
    newIonProd.energy = ee;
    ionProducts.push_back(newIonProd);
    // Add the hole.
    newIonProd.type = IonProdTypeHole;
    newIonProd.energy = eh;
    ionProducts.push_back(newIonProd);
    nion = nIonisationProducts = 2;
  }

  if (e < loss) loss = e - 0.00001;
  // Update the energy.
  e1 = e - loss;
  if (e1 < Small) e1 = Small;

  // Update the momentum.
  if (band >= 0 && band < nValleysX) {
    // X valleys
    double pstar = sqrt(2. * ElectronMass * e1);
    if (useNonParabolicity) {
      const double alpha = alphaX;
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
      pstar *= sqrt(3. / (1. / mLongX + 2. / mTransX));
      px = pstar * cos(phi) * stheta;
      py = pstar * sin(phi) * stheta;
      pz = pstar * ctheta;
    }
    return true;

  } else if (band >= nValleysX && band < nValleysX + nValleysL) {
    // L valleys
    double pstar = sqrt(2. * ElectronMass * (e1 - eMinL));
    if (useNonParabolicity) {
      const double alpha = alphaL;
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

  } else {
    double pstar = sqrt(2. * ElectronMass * e1);

    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();

    px = pstar * cos(phi) * stheta;
    py = pstar * sin(phi) * stheta;
    pz = pstar * ctheta;
    return true;
  }

  std::cerr << m_className << "::GetElectronCollision:\n";
  std::cerr << "   Band index (" << band << ") out of range.\n";
  e1 = e;
  type = 0;
  return false;
}

bool MediumSilicon::GetIonisationProduct(const int i, int& type,
                                         double& energy) {

  if (i < 0 || i >= nIonisationProducts) {
    std::cerr << m_className << "::GetIonisationProduct:\n";
    std::cerr << "    Index (" << i << ") out of range.\n";
    return false;
  }

  type = ionProducts[i].type;
  energy = ionProducts[i].energy;
  return true;
}

void MediumSilicon::ResetCollisionCounters() {

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

int MediumSilicon::GetNumberOfElectronCollisions() const {

  return nCollElectronAcoustic + nCollElectronOptical +
         nCollElectronIntervalley + nCollElectronImpurity +
         nCollElectronIonisation;
}

int MediumSilicon::GetNumberOfLevels() {

  const int nLevels = nLevelsX + nLevelsL + nLevelsG;
  return nLevels;
}

int MediumSilicon::GetNumberOfElectronCollisions(const int level) const {

  const int nLevels = nLevelsX + nLevelsL + nLevelsG;
  if (level < 0 || level >= nLevels) {
    std::cerr << m_className << "::GetNumberOfElectronCollisions:\n";
    std::cerr << "    Requested scattering rate term (" << level
              << ") does not exist.\n";
    return 0;
  }
  return nCollElectronDetailed[level];
}

int MediumSilicon::GetNumberOfElectronBands() {

  const int nBands = nValleysX + nValleysL + 1;
  return nBands;
}

int MediumSilicon::GetElectronBandPopulation(const int band) {

  const int nBands = nValleysX + nValleysL + 1;
  if (band < 0 || band >= nBands) {
    std::cerr << m_className << "::GetElectronBandPopulation:\n";
    std::cerr << "    Band index (" << band << ") out of range.\n";
    return 0;
  }
  return nCollElectronBand[band];
}

bool MediumSilicon::GetOpticalDataRange(double& emin, double& emax,
                                        const unsigned int& i) {

  if (i != 0) {
    std::cerr << m_className << "::GetOpticalDataRange:\n";
    std::cerr << "    Medium has only one component.\n";
  }

  // Make sure the optical data table has been loaded.
  if (!m_hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << m_className << "::GetOpticalDataRange:\n";
      std::cerr << "    Optical data table could not be loaded.\n";
      return false;
    }
    m_hasOpticalData = true;
  }

  emin = opticalDataTable[0].energy;
  emax = opticalDataTable.back().energy;
  if (m_debug) {
    std::cout << m_className << "::GetOpticalDataRange:\n";
    std::cout << "    " << emin << " < E [eV] < " << emax << "\n";
  }
  return true;
}

bool MediumSilicon::GetDielectricFunction(const double& e, double& eps1, 
                                          double& eps2, const unsigned int& i) {

  if (i != 0) {
    std::cerr << m_className << "::GetDielectricFunction:\n";
    std::cerr << "    Medium has only one component.\n";
    return false;
  }

  // Make sure the optical data table has been loaded.
  if (!m_hasOpticalData) {
    if (!LoadOpticalData(opticalDataFile)) {
      std::cerr << m_className << "::GetDielectricFunction:\n";
      std::cerr << "    Optical data table could not be loaded.\n";
      return false;
    }
    m_hasOpticalData = true;
  }

  // Make sure the requested energy is within the range of the table.
  const double emin = opticalDataTable[0].energy;
  const double emax = opticalDataTable.back().energy;
  if (e < emin || e > emax) {
    std::cerr << m_className << "::GetDielectricFunction:\n";
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
    eps1 = opticalDataTable[iLow].eps1 +
           (e - opticalDataTable[iLow].energy) *
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

bool MediumSilicon::Initialise() {

  if (!m_isChanged) {
    if (m_debug) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Nothing changed.\n";
    }
    return true;
  }
  if (!UpdateTransportParameters()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Error preparing transport parameters/"
              << "calculating collision rates.\n";
    return false;
  }
  return true;
}

bool MediumSilicon::UpdateTransportParameters() {

  // Calculate impact ionisation coefficients
  switch (impactIonisationModel) {
    case ImpactIonisationModelVanOverstraeten:
      UpdateImpactIonisationVanOverstraetenDeMan();
      break;
    case ImpactIonisationModelGrant:
      UpdateImpactIonisationGrant();
      break;
    default:
      std::cerr << m_className << "::UpdateTransportParameters:\n";
      std::cerr << "    Unknown impact ionisation model. Bug!\n";
      break;
  }

  if (!m_hasUserMobility) {
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
        std::cerr << m_className << "::UpdateTransportParameters:\n";
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
        std::cerr << m_className << "::UpdateTransportParameters:\n";
        std::cerr << "    Unknown doping mobility model.\n";
        std::cerr << "    Program bug!\n";
        break;
    }
  }

  // Calculate saturation velocity
  if (!m_hasUserSaturationVelocity) {
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

  if (m_debug) {
    std::cout << m_className << "::UpdateTransportParameters:\n";
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
  if (!HoleScatteringRates()) return false;

  // Reset the collision counters.
  ResetCollisionCounters();

  return true;
}

void MediumSilicon::UpdateLatticeMobilityMinimos() {

  // References:
  // - S. Selberherr, W. Haensch, M. Seavey, J. Slotboom,
  //   Solid State Electronics 33 (1990), 1425-1436
  // - Minimos 6.1 User's Guide (1999)

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.43e-6;
  const double hMu0 = 0.46e-6;
  // Temperature normalized to 300 K
  const double t = m_temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.);
  hLatticeMobility = hMu0 * pow(t, -2.18);
}

void MediumSilicon::UpdateLatticeMobilitySentaurus() {

  // References:
  // - C. Lombardi et al.,
  //   IEEE Trans. CAD 7 (1988), 1164-1171
  // - Sentaurus Device User Guide (2007)

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.417e-6;
  const double hMu0 = 0.4705e-6;
  // Temperature normalized to 300 K
  const double t = m_temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.5);
  hLatticeMobility = hMu0 * pow(t, -2.2);
}

void MediumSilicon::UpdateLatticeMobilityReggiani() {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  // Lattice mobilities at 300 K [cm2 / (V ns)]
  const double eMu0 = 1.320e-6;
  const double hMu0 = 0.460e-6;
  // Temperature normalized to 300 K
  const double t = m_temperature / 300.;
  // Temperature dependence of lattice mobility
  eLatticeMobility = eMu0 * pow(t, -2.);
  hLatticeMobility = hMu0 * pow(t, -2.2);
}

void MediumSilicon::UpdateDopingMobilityMinimos() {

  // References:
  // - S. Selberherr, W. Haensch, M. Seavey, J. Slotboom,
  //   Solid State Electronics 33 (1990), 1425-1436
  // - Minimos 6.1 User's Guide (1999)

  // Mobility reduction due to ionised impurity scattering
  // Surface term not taken into account
  double eMuMin = 0.080e-6;
  double hMuMin = 0.045e-6;
  if (m_temperature > 200.) {
    eMuMin *= pow(m_temperature / 300., -0.45);
    hMuMin *= pow(m_temperature / 300., -0.45);
  } else {
    eMuMin *= pow(2. / 3., -0.45) * pow(m_temperature / 200., -0.15);
    hMuMin *= pow(2. / 3., -0.45) * pow(m_temperature / 200., -0.15);
  }
  const double eRefC = 1.12e17 * pow(m_temperature / 300., 3.2);
  const double hRefC = 2.23e17 * pow(m_temperature / 300., 3.2);
  const double alpha = 0.72 * pow(m_temperature / 300., 0.065);
  // Assume impurity concentration equal to doping concentration
  eMobility = eMuMin + (eLatticeMobility - eMuMin) /
                           (1. + pow(m_dopingConcentration / eRefC, alpha));
  hMobility = hMuMin + (hLatticeMobility - hMuMin) /
                           (1. + pow(m_dopingConcentration / hRefC, alpha));
}

void MediumSilicon::UpdateDopingMobilityMasetti() {

  // Reference:
  // - G. Masetti, M. Severi, S. Solmi,
  //   IEEE Trans. Electron Devices 30 (1983), 764-769
  // - Sentaurus Device User Guide (2007)
  // - Minimos NT User Guide (2004)

  if (m_dopingConcentration < 1.e13) {
    eMobility = eLatticeMobility;
    hMobility = hLatticeMobility;
    return;
  }

  // Parameters adopted from Minimos NT User Guide
  const double eMuMin1 = 0.0522e-6;
  const double eMuMin2 = 0.0522e-6;
  const double eMu1 = 0.0434e-6;
  const double hMuMin1 = 0.0449e-6;
  const double hMuMin2 = 0.;
  const double hMu1 = 0.029e-6;
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
                            (1. + pow(m_dopingConcentration / eCr, eAlpha)) -
              eMu1 / (1. + pow(eCs / m_dopingConcentration, eBeta));

  hMobility = hMuMin1 * exp(-hPc / m_dopingConcentration) +
              (hLatticeMobility - hMuMin2) /
                  (1. + pow(m_dopingConcentration / hCr, hAlpha)) -
              hMu1 / (1. + pow(hCs / m_dopingConcentration, hBeta));
}

void MediumSilicon::UpdateSaturationVelocityMinimos() {

  // References:
  // - R. Quay, C. Moglestue, V. Palankovski, S. Selberherr,
  //   Materials Science in Semiconductor Processing 3 (2000), 149-155
  // - Minimos NT User Guide (2004)

  // Temperature-dependence of saturation velocities [cm / ns]
  eSatVel = 1.e-2 / (1. + 0.74 * (m_temperature / 300. - 1.));
  hSatVel = 0.704e-2 / (1. + 0.37 * (m_temperature / 300. - 1.));
}

void MediumSilicon::UpdateSaturationVelocityCanali() {

  // References:
  // - C. Canali, G. Majni, R. Minder, G. Ottaviani,
  //   IEEE Transactions on Electron Devices 22 (1975), 1045-1047
  // - Sentaurus Device User Guide (2007)

  eSatVel = 1.07e-2 * pow(300. / m_temperature, 0.87);
  hSatVel = 8.37e-3 * pow(300. / m_temperature, 0.52);
}

void MediumSilicon::UpdateSaturationVelocityReggiani() {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  eSatVel = 1.470e-2 * sqrt(tanh(150. / m_temperature));
  hSatVel = 0.916e-2 * sqrt(tanh(300. / m_temperature));
}

void MediumSilicon::UpdateHighFieldMobilityCanali() {

  // References:
  // - C. Canali, G. Majni, R. Minder, G. Ottaviani,
  //   IEEE Transactions on Electron Devices 22 (1975), 1045-1047
  // - Sentaurus Device User Guide (2007)

  // Temperature dependent exponent in high-field mobility formula
  eBetaCanali = 1.109 * pow(m_temperature / 300., 0.66);
  hBetaCanali = 1.213 * pow(m_temperature / 300., 0.17);
  eBetaCanaliInv = 1. / eBetaCanali;
  hBetaCanaliInv = 1. / hBetaCanali;
}

void MediumSilicon::UpdateImpactIonisationVanOverstraetenDeMan() {

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
                       tanh(hbarOmega / (2. * BoltzmannConstant * m_temperature));

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

void MediumSilicon::UpdateImpactIonisationGrant() {

  // References:
  // - W. N. Grant,
  //   Solid State Electronics 16 (1973), 1189 - 1203
  // - Sentaurus Device User Guide (2007)

  // Temperature dependence as in Sentaurus Device
  // Optical phonon energy
  double hbarOmega = 0.063;
  // Temperature scaling coefficient
  double gamma = tanh(hbarOmega / (2. * BoltzmannConstant * 300.)) /
                 tanh(hbarOmega / (2. * BoltzmannConstant * m_temperature));

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

bool MediumSilicon::ElectronMobilityMinimos(const double e, double& mu) const {

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

bool MediumSilicon::ElectronMobilityCanali(const double e, double& mu) const {

  // Reference:
  // - Sentaurus Device User Guide (2007)

  if (e < Small) {
    mu = 0.;
  } else {
    mu = eMobility /
         pow(1. + pow(eMobility * e / eSatVel, eBetaCanali), eBetaCanaliInv);
  }
  return true;
}

bool MediumSilicon::ElectronMobilityReggiani(const double e, double& mu) const {

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

bool MediumSilicon::ElectronImpactIonisationVanOverstraetenDeMan(
    const double e, double& alpha) const {

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
    alpha = eImpactA1 * exp(-eImpactB1 / e);
  }
  return true;
}

bool MediumSilicon::ElectronImpactIonisationGrant(const double e,
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

bool MediumSilicon::HoleMobilityMinimos(const double e, double& mu) const {

  // Reference:
  // - Minimos User's Guide (1999)

  if (e < Small) {
    mu = 0.;
  } else {
    mu = hMobility / (1. + hMobility * e / eSatVel);
  }
  return true;
}

bool MediumSilicon::HoleMobilityCanali(const double e, double& mu) const {

  // Reference:
  // - Sentaurus Device User Guide (2007)

  if (e < Small) {
    mu = 0.;
  } else {
    mu = hMobility /
         pow(1. + pow(hMobility * e / hSatVel, hBetaCanali), hBetaCanaliInv);
  }
  return true;
}

bool MediumSilicon::HoleMobilityReggiani(const double e, double& mu) const {

  // Reference:
  // - M. A. Omar, L. Reggiani
  //   Solid State Electronics 30 (1987), 693-697

  if (e < Small) {
    mu = 0.;
  } else {
    mu = hMobility / pow(1. + pow(hMobility * e / hSatVel, 2.), 0.5);
  }
  return true;
}

bool MediumSilicon::HoleImpactIonisationVanOverstraetenDeMan(
    const double e, double& alpha) const {

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

bool MediumSilicon::HoleImpactIonisationGrant(const double e,
                                              double& alpha) const {

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

bool MediumSilicon::LoadOpticalData(const std::string filename) {

  // Get the path to the data directory.
  char* pPath = getenv("GARFIELD_HOME");
  if (pPath == 0) {
    std::cerr << m_className << "::LoadOpticalData:\n";
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
    std::cerr << m_className << "::LoadOpticalData:\n";
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
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments.
    if (line[0] == '#' || line[0] == '*' || (line[0] == '/' && line[1] == '/'))
      continue;
    // Extract the values.
    dataStream.str(line);
    dataStream >> energy >> eps1 >> eps2 >> loss;
    if (dataStream.eof()) break;
    // Check if the data has been read correctly.
    if (infile.fail()) {
      std::cerr << m_className << "::LoadOpticalData:\n";
      std::cerr << "    Error reading file " << filename << " (line " << i
                << ").\n";
      return false;
    }
    // Reset the stringstream.
    dataStream.str("");
    dataStream.clear();
    // Make sure the values make sense.
    // The table has to be in ascending order
    //  with respect to the photon energy.
    if (energy <= lastEnergy) {
      std::cerr << m_className << "::LoadOpticalData:\n";
      std::cerr << "    Table is not in monotonically "
                << "increasing order (line " << i << ").\n";
      std::cerr << "    " << lastEnergy << "  " << energy << "  " << eps1
                << "  " << eps2 << "\n";
      return false;
    }
    // The imaginary part of the dielectric function has to be positive.
    if (eps2 < 0.) {
      std::cerr << m_className << "::LoadOpticalData:\n";
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
    std::cerr << m_className << "::LoadOpticalData:\n";
    std::cerr << "    Import of data from file " << filepath << "failed.\n";
    std::cerr << "    No valid data found.\n";
    return false;
  }

  if (m_debug) {
    std::cout << m_className << "::LoadOpticalData:\n";
    std::cout << "    Read " << nEntries << " values from file " << filepath
              << "\n";
  }
  return true;
}

bool MediumSilicon::ElectronScatteringRates() {

  // Reset the scattering rates
  cfTotElectronsX.resize(nEnergyStepsXL);
  cfTotElectronsL.resize(nEnergyStepsXL);
  cfTotElectronsG.resize(nEnergyStepsG);
  cfElectronsX.resize(nEnergyStepsXL);
  cfElectronsL.resize(nEnergyStepsXL);
  cfElectronsG.resize(nEnergyStepsG);
  for (int i = nEnergyStepsXL; i--;) {
    cfTotElectronsX[i] = 0.;
    cfTotElectronsL[i] = 0.;
    cfElectronsX[i].clear();
    cfElectronsL[i].clear();
  }
  for (int i = nEnergyStepsG; i--;) {
    cfTotElectronsG[i] = 0.;
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
  // Fill the scattering rate tables.
  ElectronAcousticScatteringRates();
  ElectronOpticalScatteringRates();
  ElectronImpurityScatteringRates();
  ElectronIntervalleyScatteringRatesXX();
  ElectronIntervalleyScatteringRatesXL();
  ElectronIntervalleyScatteringRatesLL();
  ElectronIntervalleyScatteringRatesXGLG();
  ElectronIonisationRatesXL();
  ElectronIonisationRatesG();

  if (m_debug) {
    std::cout << m_className << "::ElectronScatteringRates:\n";
    std::cout << "    " << nLevelsX << " X-valley scattering terms\n";
    std::cout << "    " << nLevelsL << " L-valley scattering terms\n";
    std::cout << "    " << nLevelsG << " higher band scattering terms\n";
  }

  std::ofstream outfileX;
  std::ofstream outfileL;
  if (useCfOutput) {
    outfileX.open("ratesX.txt", std::ios::out);
    outfileL.open("ratesL.txt", std::ios::out);
  }

  ieMinL = int(eMinL / eStepXL) + 1;
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    // Sum up the scattering rates of all processes.
    for (int j = nLevelsX; j--;) cfTotElectronsX[i] += cfElectronsX[i][j];
    for (int j = nLevelsL; j--;) cfTotElectronsL[i] += cfElectronsL[i][j];

    if (useCfOutput) {
      outfileX << i* eStepXL << " " << cfTotElectronsX[i] << " ";
      for (int j = 0; j < nLevelsX; ++j) {
        outfileX << cfElectronsX[i][j] << " ";
      }
      outfileX << "\n";
      outfileL << i* eStepXL << " " << cfTotElectronsL[i] << " ";
      for (int j = 0; j < nLevelsL; ++j) {
        outfileL << cfElectronsL[i][j] << " ";
      }
      outfileL << "\n";
    }

    if (cfTotElectronsX[i] > cfNullElectronsX) {
      cfNullElectronsX = cfTotElectronsX[i];
    }
    if (cfTotElectronsL[i] > cfNullElectronsL) {
      cfNullElectronsL = cfTotElectronsL[i];
    }

    // Make sure the total scattering rate is positive.
    if (cfTotElectronsX[i] <= 0.) {
      std::cerr << m_className << "::ElectronScatteringRates:\n";
      std::cerr << "    X-valley scattering rate at " << i* eStepXL
                << " eV <= 0.\n";
      return false;
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsX; ++j) {
      cfElectronsX[i][j] /= cfTotElectronsX[i];
      if (j > 0) cfElectronsX[i][j] += cfElectronsX[i][j - 1];
    }

    // Make sure the total scattering rate is positive.
    if (cfTotElectronsL[i] <= 0.) {
      if (i < ieMinL) continue;
      std::cerr << m_className << "::ElectronScatteringRates:\n";
      std::cerr << "    L-valley scattering rate at " << i* eStepXL
                << " eV <= 0.\n";
      return false;
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsL; ++j) {
      cfElectronsL[i][j] /= cfTotElectronsL[i];
      if (j > 0) cfElectronsL[i][j] += cfElectronsL[i][j - 1];
    }
  }

  if (useCfOutput) {
    outfileX.close();
    outfileL.close();
  }

  std::ofstream outfileG;
  if (useCfOutput) {
    outfileG.open("ratesG.txt", std::ios::out);
  }
  ieMinG = int(eMinG / eStepG) + 1;
  for (int i = 0; i < nEnergyStepsG; ++i) {
    // Sum up the scattering rates of all processes.
    for (int j = nLevelsG; j--;) cfTotElectronsG[i] += cfElectronsG[i][j];

    if (useCfOutput) {
      outfileG << i* eStepG << " " << cfTotElectronsG[i] << " ";
      for (int j = 0; j < nLevelsG; ++j) {
        outfileG << cfElectronsG[i][j] << " ";
      }
      outfileG << "\n";
    }

    if (cfTotElectronsG[i] > cfNullElectronsG) {
      cfNullElectronsG = cfTotElectronsG[i];
    }

    // Make sure the total scattering rate is positive.
    if (cfTotElectronsG[i] <= 0.) {
      if (i < ieMinG) continue;
      std::cerr << m_className << "::ElectronScatteringRates:\n";
      std::cerr << "    Higher band scattering rate at " << i* eStepG
                << " eV <= 0.\n";
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsG; ++j) {
      cfElectronsG[i][j] /= cfTotElectronsG[i];
      if (j > 0) cfElectronsG[i][j] += cfElectronsG[i][j - 1];
    }
  }

  if (useCfOutput) {
    outfileG.close();
  }

  return true;
}

bool MediumSilicon::ElectronAcousticScatteringRates() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  // Acoustic phonon intraband scattering
  // Acoustic deformation potential [eV]
  const double defpot = 9.;
  // Longitudinal velocity of sound [cm/ns]
  const double u = 9.04e-4;
  // Prefactor for acoustic deformation potential scattering
  const double cIntra = TwoPi * SpeedOfLight * SpeedOfLight * kbt * defpot *
                        defpot / (Hbar * u * u * rho);

  // Fill the scattering rate tables.
  double en = Small;
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    const double dosX = GetConductionBandDensityOfStates(en, 0);
    const double dosL = GetConductionBandDensityOfStates(en, nValleysX);

    cfElectronsX[i].push_back(cIntra * dosX);
    cfElectronsL[i].push_back(cIntra * dosL);
    en += eStepXL;
  }

  en = Small;
  for (int i = 0; i < nEnergyStepsG; ++i) {
    const double dosG =
        GetConductionBandDensityOfStates(en, nValleysX + nValleysL);
    cfElectronsG[i].push_back(cIntra * dosG);
    en += eStepG;
  }

  // Assume that energy loss is negligible.
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

bool MediumSilicon::ElectronOpticalScatteringRates() {

  // Reference:
  //  - K. Hess (editor),
  //    Monte Carlo device simulation: full band and beyond
  //    Chapter 5
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705
  //  - M. Lundstrom,
  //    Fundamentals of carrier transport

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  // Coupling constant [eV/cm]
  const double dtk = 2.2e8;
  // Phonon energy [eV]
  const double eph = 63.0e-3;
  // Phonon cccupation numbers
  const double nocc = 1. / (exp(eph / kbt) - 1);
  // Prefactors
  const double c0 = HbarC * SpeedOfLight * Pi / rho;
  double c = c0 * dtk * dtk / eph;

  double en = 0.;
  double dos = 0.;
  // L valleys
  /*
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    // Absorption
    if (en > eMinL) {
      dos = GetConductionBandDensityOfStates(en + eph, nValleysX);
      cfElectronsL[i].push_back(c * nocc * dos);
    } else {
      cfElectronsL[i].push_back(0.);
    }
    // Emission
    if (en > eMinL + eph) {
      dos = GetConductionBandDensityOfStates(en - eph, nValleysX);
      cfElectronsL[i].push_back(c * (nocc + 1) * dos);
    } else {
      cfElectronsL[i].push_back(0.);
    }
    en += eStepXL;
  }
  //*/

  en = 0.;
  // Higher band(s)
  for (int i = 0; i < nEnergyStepsG; ++i) {
    // Absorption
    if (en > eMinG) {
      dos = GetConductionBandDensityOfStates(en + eph, nValleysX + nValleysL);
      cfElectronsG[i].push_back(c * nocc * dos);
    } else {
      cfElectronsG[i].push_back(0.);
    }
    // Emission
    if (en > eMinG + eph) {
      dos = GetConductionBandDensityOfStates(en - eph, nValleysX + nValleysL);
      cfElectronsG[i].push_back(c * (nocc + 1) * dos);
    } else {
      cfElectronsG[i].push_back(0.);
    }
    en += eStepG;
  }

  // Absorption
  // energyLossElectronsL.push_back(-eph);
  energyLossElectronsG.push_back(-eph);
  // Emission
  // energyLossElectronsL.push_back(eph);
  energyLossElectronsG.push_back(eph);
  // scatTypeElectronsL.push_back(ElectronCollisionTypeOpticalPhonon);
  // scatTypeElectronsL.push_back(ElectronCollisionTypeOpticalPhonon);
  scatTypeElectronsG.push_back(ElectronCollisionTypeOpticalPhonon);
  scatTypeElectronsG.push_back(ElectronCollisionTypeOpticalPhonon);

  // nLevelsL += 2;
  nLevelsG += 2;

  return true;
}

bool MediumSilicon::ElectronIntervalleyScatteringRatesXX() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  const int nPhonons = 6;
  // f-type scattering: transition between orthogonal axes (multiplicity 4)
  // g-type scattering: transition between opposite axes (multiplicity 1)
  // Sequence of transitions in the table:
  // TA (g) - LA (g) - LO (g) - TA (f) - LA (f) - TO (f)
  // Coupling constants [eV/cm]
  const double dtk[nPhonons] = {0.5e8, 0.8e8, 1.1e9, 0.3e8, 2.0e8, 2.0e8};
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
  for (int i = 0; i < nEnergyStepsXL; ++i) {
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
    en += eStepXL;
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

bool MediumSilicon::ElectronIntervalleyScatteringRatesXL() {

  // Reference:
  // - M. Lundstrom, Fundamentals of carrier transport
  // - M. Martin et al.,
  //   Semicond. Sci. Technol. 8, 1291-1297

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

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
  for (int i = 0; i < nEnergyStepsXL; ++i) {
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
        cfElectronsL[i].push_back(zX * c[j] * (nocc[j] + 1) * dos);
      } else {
        cfElectronsL[i].push_back(0.);
        cfElectronsL[i].push_back(0.);
      }
    }
    en += eStepXL;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption
    energyLossElectronsX.push_back(-eph[j]);
    energyLossElectronsL.push_back(-eph[j]);
    // Emission
    energyLossElectronsX.push_back(eph[j]);
    energyLossElectronsL.push_back(eph[j]);
    scatTypeElectronsX.push_back(ElectronCollisionTypeInterbandXL);
    scatTypeElectronsX.push_back(ElectronCollisionTypeInterbandXL);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterbandXL);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterbandXL);
  }

  nLevelsX += 2 * nPhonons;
  nLevelsL += 2 * nPhonons;

  return true;
}

bool MediumSilicon::ElectronIntervalleyScatteringRatesLL() {

  // Reference:
  //  - K. Hess (editor),
  //    Monte Carlo device simulation: full band and beyond
  //    Chapter 5
  //  - M. J. Martin et al.,
  //    Semicond. Sci. Technol. 8, 1291-1297

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

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
  for (int i = 0; i < nEnergyStepsXL; ++i) {
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
    en += eStepXL;
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

bool MediumSilicon::ElectronIntervalleyScatteringRatesXGLG() {

  // Reference:
  //  - K. Hess (editor),
  //    Monte Carlo device simulation: full band and beyond
  //    Chapter 5

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  const int nPhonons = 1;

  // Coupling constants [eV/cm]
  // Average of XG and LG
  const double dtk[nPhonons] = {2.43e8};
  // Phonon energies [eV]
  const double eph[nPhonons] = {37.65e-3};
  // Number of equivalent valleys
  const int zX = 6;
  const int zL = 8;
  const int zG = 1;

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
  // XG, LG
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    for (int j = 0; j < nPhonons; ++j) {
      // Absorption
      if (en + eph[j] > eMinG) {
        dos = GetConductionBandDensityOfStates(en + eph[j],
                                               nValleysX + nValleysL);
        cfElectronsX[i].push_back(zG * c[j] * nocc[j] * dos);
        cfElectronsL[i].push_back(zG * c[j] * nocc[j] * dos);
      } else {
        cfElectronsX[i].push_back(0.);
        cfElectronsL[i].push_back(0.);
      }
      // Emission
      if (en - eph[j] > eMinG) {
        dos = GetConductionBandDensityOfStates(en - eph[j],
                                               nValleysX + nValleysL);
        cfElectronsX[i].push_back(zG * c[j] * (nocc[j] + 1) * dos);
        cfElectronsL[i].push_back(zG * c[j] * (nocc[j] + 1) * dos);
      } else {
        cfElectronsX[i].push_back(0.);
        cfElectronsL[i].push_back(0.);
      }
    }
    en += eStepXL;
  }

  // GX, GL
  en = 0.;
  double dosX = 0., dosL = 0.;
  for (int i = 0; i < nEnergyStepsG; ++i) {
    for (int j = 0; j < nPhonons; ++j) {
      // Absorption
      dosX = GetConductionBandDensityOfStates(en + eph[j], 0);
      dosL = GetConductionBandDensityOfStates(en + eph[j], nValleysX);
      cfElectronsG[i].push_back(zX * c[j] * nocc[j] * dosX);
      if (en > eMinL) {
        cfElectronsG[i].push_back(zL * c[j] * nocc[j] * dosL);
      } else {
        cfElectronsG[i].push_back(0.);
      }
      // Emission
      dosX = GetConductionBandDensityOfStates(en - eph[j], 0);
      dosL = GetConductionBandDensityOfStates(en - eph[j], nValleysX);
      if (en > eph[j]) {
        cfElectronsG[i].push_back(zX * c[j] * (nocc[j] + 1) * dosX);
      } else {
        cfElectronsG[i].push_back(0.);
      }
      if (en - eph[j] > eMinL) {
        cfElectronsG[i].push_back(zL * c[j] * (nocc[j] + 1) * dosL);
      } else {
        cfElectronsG[i].push_back(0.);
      }
    }
    en += eStepG;
  }

  for (int j = 0; j < nPhonons; ++j) {
    // Absorption (XL)
    energyLossElectronsX.push_back(-eph[j]);
    energyLossElectronsL.push_back(-eph[j]);
    // Emission (XL)
    energyLossElectronsX.push_back(eph[j]);
    energyLossElectronsL.push_back(eph[j]);
    // Absorption (G)
    energyLossElectronsG.push_back(-eph[j]);
    energyLossElectronsG.push_back(-eph[j]);
    // Emission (G)
    energyLossElectronsG.push_back(eph[j]);
    energyLossElectronsG.push_back(eph[j]);

    scatTypeElectronsX.push_back(ElectronCollisionTypeInterbandXG);
    scatTypeElectronsX.push_back(ElectronCollisionTypeInterbandXG);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterbandLG);
    scatTypeElectronsL.push_back(ElectronCollisionTypeInterbandLG);

    scatTypeElectronsG.push_back(ElectronCollisionTypeInterbandXG);
    scatTypeElectronsG.push_back(ElectronCollisionTypeInterbandLG);
    scatTypeElectronsG.push_back(ElectronCollisionTypeInterbandXG);
    scatTypeElectronsG.push_back(ElectronCollisionTypeInterbandLG);
  }

  nLevelsX += 2 * nPhonons;
  nLevelsL += 2 * nPhonons;
  nLevelsG += 4 * nPhonons;

  return true;
}

bool MediumSilicon::ElectronIonisationRatesXL() {

  // References:
  // - E. Cartier, M. V. Fischetti, E. A. Eklund and F. R. McFeely,
  //   Appl. Phys. Lett 62, 3339-3341
  // - DAMOCLES web page: www.research.ibm.com/DAMOCLES

  // Coefficients [ns-1]
  const double p[3] = {6.25e1, 3.e3, 6.8e5};
  // Threshold energies [eV]
  const double eth[3] = {1.2, 1.8, 3.45};

  double en = 0.;
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    double fIon = 0.;
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
    en += eStepXL;
  }

  energyLossElectronsX.push_back(eth[0]);
  energyLossElectronsL.push_back(eth[0]);
  scatTypeElectronsX.push_back(ElectronCollisionTypeIonisation);
  scatTypeElectronsL.push_back(ElectronCollisionTypeIonisation);
  ++nLevelsX;
  ++nLevelsL;

  return true;
}

bool MediumSilicon::ElectronIonisationRatesG() {

  // References:
  // - E. Cartier, M. V. Fischetti, E. A. Eklund and F. R. McFeely,
  //   Appl. Phys. Lett 62, 3339-3341
  // - S. Tanuma, C. J. Powell and D. R. Penn
  //   Surf. Interface Anal. (2010)

  // Coefficients [ns-1]
  const double p[3] = {6.25e1, 3.e3, 6.8e5};
  // Threshold energies [eV]
  const double eth[3] = {1.2, 1.8, 3.45};

  double en = 0.;
  for (int i = 0; i < nEnergyStepsG; ++i) {
    double fIon = 0.;
    if (en > eth[0]) {
      fIon += p[0] * (en - eth[0]) * (en - eth[0]);
    }
    if (en > eth[1]) {
      fIon += p[1] * (en - eth[1]) * (en - eth[1]);
    }
    if (en > eth[2]) {
      fIon += p[2] * (en - eth[2]) * (en - eth[2]);
    }
    if (en >= eMinG) {
      cfElectronsG[i].push_back(fIon);
    } else {
      cfElectronsG[i].push_back(0.);
    }
    en += eStepG;
  }

  energyLossElectronsG.push_back(eth[0]);
  scatTypeElectronsG.push_back(ElectronCollisionTypeIonisation);
  ++nLevelsG;

  return true;
}

bool MediumSilicon::ElectronImpurityScatteringRates() {

  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  // Band parameters
  // Density of states effective masses
  const double mdX = ElectronMass * pow(mLongX * mTransX * mTransX, 1. / 3.);
  const double mdL = ElectronMass * pow(mLongL * mTransL * mTransL, 1. / 3.);

  // Dielectric constant
  const double eps = GetDielectricConstant();
  // Impurity concentration
  const double impurityConcentration = m_dopingConcentration;
  if (impurityConcentration < Small) return true;

  // Screening length
  const double ls = sqrt(eps * kbt / (4 * Pi * FineStructureConstant * HbarC *
                                      impurityConcentration));
  const double ebX = 0.5 * HbarC * HbarC / (mdX * ls * ls);
  const double ebL = 0.5 * HbarC * HbarC / (mdL * ls * ls);

  // Prefactor
  // const double c = pow(2., 2.5) * Pi * impurityConcentration *
  //                 pow(FineStructureConstant * HbarC, 2) *
  //                 SpeedOfLight / (eps * eps * sqrt(md) * eb * eb);
  // Use momentum-transfer cross-section
  const double cX = impurityConcentration * Pi *
                    pow(FineStructureConstant * HbarC, 2) * SpeedOfLight /
                    (sqrt(2 * mdX) * eps * eps);
  const double cL = impurityConcentration * Pi *
                    pow(FineStructureConstant * HbarC, 2) * SpeedOfLight /
                    (sqrt(2 * mdL) * eps * eps);

  double en = 0.;
  for (int i = 0; i < nEnergyStepsXL; ++i) {
    const double gammaX = en * (1. + alphaX * en);
    const double gammaL = (en - eMinL) * (1. + alphaL * (en - eMinL));
    // cfElectrons[i][iLevel] = c * sqrt(gamma) * (1. + 2 * alpha * en) /
    //                         (1. + 4. * gamma / eb);
    if (gammaX <= 0.) {
      cfElectronsX[i].push_back(0.);
    } else {
      const double b = 4 * gammaX / ebX;
      cfElectronsX[i]
          .push_back((cX / pow(gammaX, 1.5)) * (log(1. + b) - b / (1. + b)));
    }
    if (en <= eMinL || gammaL <= 0.) {
      cfElectronsL[i].push_back(0.);
    } else {
      const double b = 4 * gammaL / ebL;
      cfElectronsL[i]
          .push_back((cL / pow(gammaL, 1.5)) * (log(1. + b) - b / (1. + b)));
    }
    en += eStepXL;
  }

  energyLossElectronsX.push_back(0.);
  energyLossElectronsL.push_back(0.);
  scatTypeElectronsX.push_back(ElectronCollisionTypeImpurity);
  scatTypeElectronsL.push_back(ElectronCollisionTypeImpurity);
  ++nLevelsX;
  ++nLevelsL;

  return true;
}

bool MediumSilicon::HoleScatteringRates() {

  // Reset the scattering rates
  cfTotHoles.resize(nEnergyStepsV);
  cfHoles.resize(nEnergyStepsV);
  for (int i = nEnergyStepsV; i--;) {
    cfTotHoles[i] = 0.;
    cfHoles[i].clear();
  }
  energyLossHoles.clear();
  scatTypeHoles.clear();
  cfNullHoles = 0.;

  nLevelsV = 0;
  // Fill the scattering rates table
  HoleAcousticScatteringRates();
  HoleOpticalScatteringRates();
  // HoleImpurityScatteringRates();
  HoleIonisationRates();

  std::ofstream outfile;
  if (useCfOutput) {
    outfile.open("ratesV.txt", std::ios::out);
  }

  for (int i = 0; i < nEnergyStepsV; ++i) {
    // Sum up the scattering rates of all processes.
    for (int j = nLevelsV; j--;) cfTotHoles[i] += cfHoles[i][j];

    if (useCfOutput) {
      outfile << i* eStepV << " " << cfTotHoles[i] << " ";
      for (int j = 0; j < nLevelsV; ++j) {
        outfile << cfHoles[i][j] << " ";
      }
      outfile << "\n";
    }

    if (cfTotHoles[i] > cfNullHoles) {
      cfNullHoles = cfTotHoles[i];
    }

    // Make sure the total scattering rate is positive.
    if (cfTotHoles[i] <= 0.) {
      std::cerr << m_className << "::HoleScatteringRates:\n";
      std::cerr << "    Scattering rate at " << i* eStepV << " eV <= 0.\n";
      return false;
    }
    // Normalise the rates.
    for (int j = 0; j < nLevelsV; ++j) {
      cfHoles[i][j] /= cfTotHoles[i];
      if (j > 0) cfHoles[i][j] += cfHoles[i][j - 1];
    }
  }

  if (useCfOutput) {
    outfile.close();
  }

  return true;
}

bool MediumSilicon::HoleAcousticScatteringRates() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705
  //  - DAMOCLES web page: www.research.ibm.com/DAMOCLES
  //  - M. Lundstrom, Fundamentals of carrier transport

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  // Acoustic phonon intraband scattering
  // Acoustic deformation potential [eV]
  // DAMOCLES: 4.6 eV; Lundstrom: 5 eV
  const double defpot = 4.6;
  // Longitudinal velocity of sound [cm/ns]
  const double u = 9.04e-4;
  // Prefactor for acoustic deformation potential scattering
  const double cIntra = TwoPi * SpeedOfLight * SpeedOfLight * kbt * defpot *
                        defpot / (Hbar * u * u * rho);

  // Fill the scattering rate tables.
  double en = Small;
  for (int i = 0; i < nEnergyStepsV; ++i) {
    const double dos = GetValenceBandDensityOfStates(en, 0);
    cfHoles[i].push_back(cIntra * dos);
    en += eStepV;
  }

  // Assume that energy loss is negligible.
  energyLossHoles.push_back(0.);
  scatTypeHoles.push_back(ElectronCollisionTypeAcousticPhonon);
  ++nLevelsV;

  return true;
}

bool MediumSilicon::HoleOpticalScatteringRates() {

  // Reference:
  //  - C. Jacoboni and L. Reggiani,
  //    Rev. Mod. Phys. 55, 645-705
  //  - DAMOCLES web page: www.research.ibm.com/DAMOCLES
  //  - M. Lundstrom, Fundamentals of carrier transport

  // Mass density [(eV/c2)/cm3]
  const double rho = m_density * m_a * AtomicMassUnitElectronVolt;
  // Lattice temperature [eV]
  const double kbt = BoltzmannConstant * m_temperature;

  // Coupling constant [eV/cm]
  // DAMOCLES: 6.6, Lundstrom: 6.0
  const double dtk = 6.6e8;
  // Phonon energy [eV]
  const double eph = 63.0e-3;
  // Phonon cccupation numbers
  const double nocc = 1. / (exp(eph / kbt) - 1);
  // Prefactors
  const double c0 = HbarC * SpeedOfLight * Pi / rho;
  double c = c0 * dtk * dtk / eph;

  double en = 0.;
  double dos = 0.;
  for (int i = 0; i < nEnergyStepsV; ++i) {
    // Absorption
    dos = GetValenceBandDensityOfStates(en + eph, 0);
    cfHoles[i].push_back(c * nocc * dos);
    // Emission
    if (en > eph) {
      dos = GetValenceBandDensityOfStates(en - eph, 0);
      cfHoles[i].push_back(c * (nocc + 1) * dos);
    } else {
      cfHoles[i].push_back(0.);
    }
    en += eStepV;
  }

  // Absorption
  energyLossHoles.push_back(-eph);
  // Emission
  energyLossHoles.push_back(eph);
  scatTypeHoles.push_back(ElectronCollisionTypeOpticalPhonon);
  scatTypeHoles.push_back(ElectronCollisionTypeOpticalPhonon);

  nLevelsV += 2;

  return true;
}

bool MediumSilicon::HoleIonisationRates() {

  // References:
  //  - DAMOCLES web page: www.research.ibm.com/DAMOCLES

  // Coefficients [ns-1]
  const double p[2] = {2., 1.e3};
  // Threshold energies [eV]
  const double eth[2] = {1.1, 1.45};
  // Exponents
  const double b[2] = {6., 4.};

  double en = 0.;
  for (int i = 0; i < nEnergyStepsV; ++i) {
    double fIon = 0.;
    if (en > eth[0]) {
      fIon += p[0] * pow(en - eth[0], b[0]);
    }
    if (en > eth[1]) {
      fIon += p[1] * pow(en - eth[1], b[1]);
    }
    cfHoles[i].push_back(fIon);
    en += eStepV;
  }

  energyLossHoles.push_back(eth[0]);
  scatTypeHoles.push_back(ElectronCollisionTypeIonisation);
  ++nLevelsV;

  return true;
}

double MediumSilicon::GetConductionBandDensityOfStates(const double e,
                                                       const int band) {
  if (band < 0) {
    int iE = int(e / eStepDos);
    if (iE >= nFbDosEntriesConduction || iE < 0) {
      return 0.;
    } else if (iE == nFbDosEntriesConduction - 1) {
      return fbDosConduction[nFbDosEntriesConduction - 1];
    }

    const double dos =
        fbDosConduction[iE] +
        (fbDosConduction[iE + 1] - fbDosConduction[iE]) * (e / eStepDos - iE);
    return dos * 1.e21;

  } else if (band < nValleysX) {
    // X valleys
    if (e <= 0.) return 0.;
    // Density-of-states effective mass (cube)
    const double md3 = pow(ElectronMass, 3) * mLongX * mTransX * mTransX;

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
      const double alpha = alphaX;
      return sqrt(md3 * e * (1. + alpha * e) / 2.) * (1. + 2 * alpha * e) /
             (Pi2 * pow(HbarC, 3.));
    } else {
      return sqrt(md3 * e / 2.) / (Pi2 * pow(HbarC, 3.));
    }
  } else if (band < nValleysX + nValleysL) {
    // L valleys
    if (e <= eMinL) return 0.;

    // Density-of-states effective mass (cube)
    const double md3 = pow(ElectronMass, 3) * mLongL * mTransL * mTransL;
    // Non-parabolicity parameter
    const double alpha = alphaL;

    if (useFullBandDos) {
      // Energy up to which the non-parabolic approximation is used.
      const double ej = eMinL + 0.5;
      if (e <= ej) {
        return sqrt(md3 * (e - eMinL) * (1. + alpha * (e - eMinL))) *
               (1. + 2 * alpha * (e - eMinL)) / (Sqrt2 * Pi2 * pow(HbarC, 3.));
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
             (1. + 2 * alpha * (e - eMinL)) / (Sqrt2 * Pi2 * pow(HbarC, 3.));
    } else {
      return sqrt(md3 * (e - eMinL) / 2.) / (Pi2 * pow(HbarC, 3.));
    }
  } else if (band == nValleysX + nValleysL) {
    // Higher bands
    const double ej = 2.7;
    if (eMinG >= ej) {
      std::cerr << m_className << "::GetConductionBandDensityOfStates:\n";
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

  std::cerr << m_className << "::GetConductionBandDensityOfStates:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return ElectronMass * sqrt(ElectronMass * e / 2.) / (Pi2 * pow(HbarC, 3.));
}

double MediumSilicon::GetValenceBandDensityOfStates(const double e,
                                                    const int band) {

  if (band <= 0) {
    // Total (full-band) density of states.

    int iE = int(e / eStepDos);
    if (iE >= nFbDosEntriesValence || iE < 0) {
      return 0.;
    } else if (iE == nFbDosEntriesValence - 1) {
      return fbDosValence[nFbDosEntriesValence - 1];
    }

    const double dos =
        fbDosValence[iE] +
        (fbDosValence[iE + 1] - fbDosValence[iE]) * (e / eStepDos - iE);
    return dos * 1.e21;
  }

  std::cerr << m_className << "::GetConductionBandDensityOfStates:\n";
  std::cerr << "    Band index (" << band << ") out of range.\n";
  return 0.;
}

void MediumSilicon::ComputeSecondaries(const double e0, double& ee,
                                       double& eh) {

  const double widthValenceBand = eStepDos * nFbDosEntriesValence;
  const double widthConductionBand = eStepDos * nFbDosEntriesConduction;

  bool ok = false;
  while (!ok) {
    // Sample a hole energy according to the valence band DOS.
    eh = RndmUniformPos() * std::min(widthValenceBand, e0);
    int ih = std::min(int(eh / eStepDos), nFbDosEntriesValence - 1);
    while (RndmUniform() > fbDosValence[ih] / fbDosMaxV) {
      eh = RndmUniformPos() * std::min(widthValenceBand, e0);
      ih = std::min(int(eh / eStepDos), nFbDosEntriesValence - 1);
    }
    // Sample an electron energy according to the conduction band DOS.
    ee = RndmUniformPos() * std::min(widthConductionBand, e0);
    int ie = std::min(int(ee / eStepDos), nFbDosEntriesConduction - 1);
    while (RndmUniform() > fbDosConduction[ie] / fbDosMaxC) {
      ee = RndmUniformPos() * std::min(widthConductionBand, e0);
      ie = std::min(int(ee / eStepDos), nFbDosEntriesConduction - 1);
    }
    // Calculate the energy of the primary electron.
    const double ep = e0 - m_bandGap - eh - ee;
    if (ep < Small) continue;
    if (ep > 5.) return;
    // Check if the primary electron energy is consistent with the DOS.
    int ip = std::min(int(ep / eStepDos), nFbDosEntriesConduction - 1);
    if (RndmUniform() > fbDosConduction[ip] / fbDosMaxC) continue;
    ok = true;
  }
}

void MediumSilicon::InitialiseDensityOfStates() {

  eStepDos = 0.1;

  const int nFbDosEntriesV = 83;
  const double fbDosV[nFbDosEntriesV] = {
      0.,      1.28083,  2.08928, 2.70763, 3.28095, 3.89162, 4.50547, 5.15043,
      5.89314, 6.72667,  7.67768, 8.82725, 10.6468, 12.7003, 13.7457, 14.0263,
      14.2731, 14.5527,  14.8808, 15.1487, 15.4486, 15.7675, 16.0519, 16.4259,
      16.7538, 17.0589,  17.3639, 17.6664, 18.0376, 18.4174, 18.2334, 16.7552,
      15.1757, 14.2853,  13.6516, 13.2525, 12.9036, 12.7203, 12.6104, 12.6881,
      13.2862, 14.0222,  14.9366, 13.5084, 9.77808, 6.15266, 3.47839, 2.60183,
      2.76747, 3.13985,  3.22524, 3.29119, 3.40868, 3.6118,  3.8464,  4.05776,
      4.3046,  4.56219,  4.81553, 5.09909, 5.37616, 5.67297, 6.04611, 6.47252,
      6.9256,  7.51254,  8.17923, 8.92351, 10.0309, 11.726,  16.2853, 18.2457,
      12.8879, 7.86019,  6.02275, 5.21777, 4.79054, 3.976,   3.11855, 2.46854,
      1.65381, 0.830278, 0.217735};

  // Total (full-band) density of states.
  const int nFbDosEntriesC = 101;
  const double fbDosC[nFbDosEntriesC] = {
      0.,      1.5114,  2.71026,  3.67114,  4.40173, 5.05025, 5.6849,  6.28358,
      6.84628, 7.43859, 8.00204,  8.80658,  9.84885, 10.9579, 12.0302, 13.2051,
      14.6948, 16.9879, 18.4492,  18.1933,  17.6747, 16.8135, 15.736,  14.4965,
      13.1193, 12.1817, 12.6109,  15.3148,  19.4936, 23.0093, 24.4106, 22.2834,
      19.521,  18.9894, 18.8015,  17.9363,  17.0252, 15.9871, 14.8486, 14.3797,
      14.2426, 14.3571, 14.7271,  14.681,   14.3827, 14.2789, 14.144,  14.1684,
      14.1418, 13.9237, 13.7558,  13.5691,  13.4567, 13.2693, 12.844,  12.4006,
      12.045,  11.7729, 11.3607,  11.14,    11.0586, 10.5475, 9.73786, 9.34423,
      9.4694,  9.58071, 9.6967,   9.84854,  10.0204, 9.82705, 9.09102, 8.30665,
      7.67306, 7.18925, 6.79675,  6.40713,  6.21687, 6.33267, 6.5223,  6.17877,
      5.48659, 4.92208, 4.44239,  4.02941,  3.5692,  3.05953, 2.6428,  2.36979,
      2.16273, 2.00627, 1.85206,  1.71265,  1.59497, 1.46681, 1.34913, 1.23951,
      1.13439, 1.03789, 0.924155, 0.834962, 0.751017};

  nFbDosEntriesValence = nFbDosEntriesV;
  nFbDosEntriesConduction = nFbDosEntriesC;
  fbDosValence.resize(nFbDosEntriesValence);
  fbDosConduction.resize(nFbDosEntriesConduction);
  fbDosMaxV = fbDosV[nFbDosEntriesV - 1];
  fbDosMaxC = fbDosC[nFbDosEntriesC - 1];
  for (int i = nFbDosEntriesV; i--;) {
    fbDosValence[i] = fbDosV[i];
    if (fbDosV[i] > fbDosMaxV) fbDosMaxV = fbDosV[i];
  }
  for (int i = nFbDosEntriesC; i--;) {
    fbDosConduction[i] = fbDosC[i];
    if (fbDosC[i] > fbDosMaxC) fbDosMaxC = fbDosC[i];
  }
}
}
