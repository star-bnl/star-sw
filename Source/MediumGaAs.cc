#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>

#include "MediumGaAs.hh"
#include "Random.hh"
#include "GarfieldConstants.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumGaAs::MediumGaAs() : Medium() {

  m_className = "MediumGaAs";
  m_name = "GaAs";

  SetTemperature(300.);
  SetDielectricConstant(12.9);
  SetAtomicNumber(32);
  SetAtomicWeight(72.32);
  SetMassDensity(5.317);

  EnableDrift();
  EnablePrimaryIonisation();
  m_microscopic = false;

  m_w = 4.35;
  m_fano = 0.1;
}

void MediumGaAs::GetComponent(const unsigned int i, std::string& label, double& f) {

  if (i == 0) {
    label = "Ga";
    f = 0.5;
  } else if (i == 1) {
    label = "As";
    f = 0.5;
  }
}

void MediumGaAs::SetTrapCrossSection(const double ecs, const double hcs) {

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

void MediumGaAs::SetTrapDensity(const double n) {

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

void MediumGaAs::SetTrappingTime(const double etau, const double htau) {

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

bool MediumGaAs::ElectronVelocity(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz, double& vx,
                                  double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!m_eVelocityE.empty()) {
    // Interpolation in user table.
    return Medium::ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  double mu = eMobility;
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

bool MediumGaAs::ElectronTownsend(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz,
                                  double& alpha) {

  alpha = 0.;
  if (!m_eTownsend.empty()) {
    // Interpolation in user table.
    return Medium::ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool MediumGaAs::ElectronAttachment(const double ex, const double ey,
                                    const double ez, const double bx,
                                    const double by, const double bz,
                                    double& eta) {

  eta = 0.;
  if (!m_eAttachment.empty()) {
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

bool MediumGaAs::HoleVelocity(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!m_hVelocityE.empty()) {
    // Interpolation in user table.
    return Medium::HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  double mu = hMobility;
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

bool MediumGaAs::HoleTownsend(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& alpha) {

  alpha = 0.;
  if (!m_hTownsend.empty()) {
    // Interpolation in user table.
    return Medium::HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool MediumGaAs::HoleAttachment(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& eta) {

  eta = 0.;
  if (!m_hAttachment.empty()) {
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

void MediumGaAs::SetLowFieldMobility(const double mue, const double muh) {

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

}
