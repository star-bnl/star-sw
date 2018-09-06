#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <vector>

#include "MediumCdTe.hh"
#include "Random.hh"
#include "GarfieldConstants.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumCdTe::MediumCdTe() : Medium() {

  m_className = "MediumCdTe";
  m_name = "CdTe";

  SetTemperature(300.);
  SetDielectricConstant(11.);
  SetAtomicNumber(48.52);
  SetAtomicWeight(240.01);
  SetMassDensity(5.85);

  EnableDrift();
  EnablePrimaryIonisation();
  m_microscopic = false;

  m_w = 4.43;
  m_fano = 0.1;
}

void MediumCdTe::GetComponent(const unsigned int i, 
                              std::string& label, double& f) {

  if (i == 0) {
    label = "Cd";
    f = 0.5;
  } else if (i == 1) {
    label = "Te";
    f = 0.5;
  } else {
    std::cerr << m_className << "::GetComponent: Index out of range.\n";
  }
}

void MediumCdTe::SetTrapCrossSection(const double ecs, const double hcs) {

  if (ecs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n"
              << "    Capture cross-section [cm2] must positive.\n";
  } else {
    m_eTrapCs = ecs;
  }

  if (hcs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n"
              << "    Capture cross-section [cm2] must be positive.n";
  } else {
    m_hTrapCs = hcs;
  }

  m_trappingModel = 0;
  m_isChanged = true;
}

void MediumCdTe::SetTrapDensity(const double n) {

  if (n < 0.) {
    std::cerr << m_className << "::SetTrapDensity:\n"
              << "    Trap density [cm-3] must be greater than zero.\n";
  } else {
    m_eTrapDensity = n;
    m_hTrapDensity = n;
  }

  m_trappingModel = 0;
  m_isChanged = true;
}

void MediumCdTe::SetTrappingTime(const double etau, const double htau) {

  if (etau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n"
              << "    Trapping time [ns-1] must be greater than zero.\n";
  } else {
    m_eTrapTime = etau;
  }

  if (htau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n"
              << "    Trapping time [ns-1] must be greater than zero.\n";
  } else {
    m_hTrapTime = htau;
  }

  m_trappingModel = 1;
  m_isChanged = true;
}

bool MediumCdTe::ElectronVelocity(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz, double& vx,
                                  double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!m_eVelocityE.empty()) {
    // Interpolation in user table.
    return Medium::ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  const double mu = -m_eMobility;
  const double b2 = bx * bx + by * by + bz * bz;
  if (b2 < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = m_eHallFactor * mu;
    const double muH2 = muH * muH;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + muH2 * b2;
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH2 * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH2 * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH2 * bz * eb) / nom;
  }
  return true;
}

bool MediumCdTe::ElectronTownsend(const double ex, const double ey,
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

bool MediumCdTe::ElectronAttachment(const double ex, const double ey,
                                    const double ez, const double bx,
                                    const double by, const double bz,
                                    double& eta) {

  eta = 0.;
  if (!m_eAttachment.empty()) {
    // Interpolation in user table.
    return Medium::ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
  }

  switch (m_trappingModel) {
    case 0:
      eta = m_eTrapCs * m_eTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = m_eTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << m_className << "::ElectronAttachment:\n"
                << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }

  return true;
}

bool MediumCdTe::HoleVelocity(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!m_hVelocityE.empty()) {
    // Interpolation in user table.
    return Medium::HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  const double mu = m_hMobility;
  const double b2 = bx * bx + by * by + bz * bz;
  if (b2 < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = m_hHallFactor * mu;
    const double muH2 = muH * muH;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + muH2 * b2;
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH2 * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH2 * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH2 * bz * eb) / nom;
  }
  return true;
}

bool MediumCdTe::HoleTownsend(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& alpha) {

  alpha = 0.;
  if (!m_hTownsend.empty()) {
    // Interpolation in user table.
    return Medium::HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool MediumCdTe::HoleAttachment(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& eta) {

  eta = 0.;
  if (!m_hAttachment.empty()) {
    // Interpolation in user table.
    return Medium::HoleAttachment(ex, ey, ez, bx, by, bz, eta);
  }
  switch (m_trappingModel) {
    case 0:
      eta = m_hTrapCs * m_hTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = m_hTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << m_className << "::HoleAttachment:\n"
                << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }
  return true;
}

void MediumCdTe::SetLowFieldMobility(const double mue, const double muh) {

  if (mue <= 0. || muh <= 0.) {
    std::cerr << m_className << "::SetLowFieldMobility:\n"
              << "    Mobility must be greater than zero.\n";
    return;
  }

  m_eMobility = mue;
  m_hMobility = muh;
  m_hasUserMobility = true;
  m_isChanged = true;
}

void MediumCdTe::SetSaturationVelocity(const double vsate, const double vsath) {

  if (vsate <= 0. || vsath <= 0.) {
    std::cout << m_className << "::SetSaturationVelocity:\n"
              << "    Restoring default values.\n";
    m_hasUserSaturationVelocity = false;
  } else {
    m_eSatVel = vsate;
    m_hSatVel = vsath;
    m_hasUserSaturationVelocity = true;
  }
  m_isChanged = true;
}

}
