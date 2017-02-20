#include <iostream>

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "Track.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

Track::Track()
    : m_className("Track"),
      m_q(-1.),
      m_spin(1),
      m_mass(MuonMass),
      m_energy(0.),
      m_isElectron(false),
      m_particleName("mu-"),
      m_sensor(NULL),
      m_isChanged(true),
      m_usePlotting(false),
      m_viewer(NULL),
      m_debug(false),
      m_plotId(-1) {

  SetBetaGamma(3.);
}

void Track::SetParticle(const std::string& part) {

  m_isElectron = false;
  if (part == "electron" || part == "Electron" || part == "e-") {
    m_q = -1;
    m_mass = ElectronMass;
    m_spin = 1;
    m_isElectron = true;
    m_particleName = "e-";
  } else if (part == "positron" || part == "Positron" || part == "e+") {
    m_q = 1;
    m_mass = ElectronMass;
    m_spin = 1;
    m_particleName = "e+";
  } else if (part == "muon" || part == "Muon" || part == "mu" ||
             part == "mu-") {
    m_q = -1;
    m_mass = MuonMass;
    m_spin = 1;
    m_particleName = "mu-";
  } else if (part == "mu+") {
    m_q = 1;
    m_mass = MuonMass;
    m_spin = 1;
    m_particleName = "mu+";
  } else if (part == "pion" || part == "Pion" || part == "pi" ||
             part == "pi-") {
    m_q = -1;
    m_mass = 139.57018e6;
    m_spin = 0;
    m_particleName = "pi-";
  } else if (part == "pi+") {
    m_q = 1;
    m_mass = 139.57018e6;
    m_spin = 0;
    m_particleName = "pi+";
  } else if (part == "kaon" || part == "Kaon" || part == "K" || part == "K-") {
    m_q = -1;
    m_mass = 493.677e6;
    m_spin = 0;
    m_particleName = "K-";
  } else if (part == "K+") {
    m_q = 1;
    m_mass = 493.677e6;
    m_spin = 0;
    m_particleName = "K+";
  } else if (part == "proton" || part == "Proton" || part == "p") {
    m_q = 1;
    m_mass = ProtonMass;
    m_spin = 1;
    m_particleName = "p";
  } else if (part == "anti-proton" || part == "Anti-Proton" ||
             part == "antiproton" || part == "Antiproton" || part == "p-bar") {
    m_q = -1;
    m_mass = ProtonMass;
    m_spin = 1;
    m_particleName = "pbar";
  } else if (part == "deuteron" || part == "Deuteron" || part == "d") {
    m_q = 1;
    m_mass = 1875.612793e6;
    m_spin = 2;
    m_particleName = "d";
  } else if (part == "alpha" || part == "Alpha") {
    m_q = 2;
    m_mass = 3.727379240e9;
    m_spin = 0;
    m_particleName = "alpha";
  } else {
    std::cerr << m_className << "::SetParticle:\n"
              << "    Particle " << part << " is not defined.\n";
  }
}

void Track::SetEnergy(const double e) {

  if (e <= m_mass) {
    std::cerr << m_className << "::SetEnergy:\n"
              << "    Particle energy must be greater than the mass.\n";
    return;
  }

  m_energy = e;
  const double gamma = m_energy / m_mass;
  m_beta2 = 1. - 1. / (gamma * gamma);
  m_isChanged = true;
}

void Track::SetBetaGamma(const double bg) {

  if (bg <= 0.) {
    std::cerr << m_className << "::SetBetaGamma:\n"
              << "    Particle speed must be greater than zero.\n";
    return;
  }

  const double bg2 = bg * bg;
  m_energy = m_mass * sqrt(1. + bg2);
  m_beta2 = bg2 / (1. + bg2);
  m_isChanged = true;
}

void Track::SetBeta(const double beta) {

  if (beta <= 0. && beta >= 1.) {
    std::cerr << m_className << "::SetBeta:\n";
    std::cerr << "    Particle speed must be between zero"
              << " and speed of light.\n";
    return;
  }

  m_beta2 = beta * beta;
  m_energy = m_mass * sqrt(1. / (1. - m_beta2));
  m_isChanged = true;
}

void Track::SetGamma(const double gamma) {

  if (gamma <= 1.) {
    std::cerr << m_className << "::SetGamma:\n";
    std::cerr << "    Particle speed must be greater than zero.\n";
    return;
  }

  m_energy = m_mass * gamma;
  m_beta2 = 1. - 1. / (gamma * gamma);
  m_isChanged = true;
}

void Track::SetMomentum(const double p) {

  if (p <= 0.) {
    std::cerr << m_className << "::SetMomentum:\n";
    std::cerr << "    Particle momentum must be greater than zero.\n";
    return;
  }

  m_energy = sqrt(m_mass * m_mass + p * p);
  const double bg = p / m_mass;
  m_beta2 = bg * bg / (1. + bg * bg);
  m_isChanged = true;
}

void Track::SetKineticEnergy(const double ekin) {

  if (ekin <= 0.) {
    std::cerr << m_className << "::SetKineticEnergy:\n";
    std::cerr << "    Kinetic energy must be greater than zero.\n";
    return;
  }

  m_energy = m_mass + ekin;
  const double gamma = 1. + ekin / m_mass;
  m_beta2 = 1. - 1. / (gamma * gamma);
  m_isChanged = true;
}

void Track::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  m_sensor = s;
}

void Track::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n";
    std::cerr << "    Pointer is null.\n";
    return;
  }

  m_viewer = view;
  m_usePlotting = true;
}

void Track::DisablePlotting() {

  m_usePlotting = false;
  m_viewer = NULL;
}

void Track::PlotNewTrack(const double x0, const double y0, const double z0) {

  if (!m_usePlotting || !m_viewer) return;

  m_viewer->NewChargedParticleTrack(1, m_plotId, x0, y0, z0);
}

void Track::PlotCluster(const double x0, const double y0, const double z0) {

  if (m_plotId < 0 || !m_usePlotting || !m_viewer) {
    std::cerr << m_className << "::PlotCluster:\n";
    std::cerr << "    No track set. Program bug!\n";
    return;
  }
  m_viewer->AddTrackPoint(m_plotId, x0, y0, z0);
}
}
