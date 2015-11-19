#include <iostream>

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "Track.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

Track::Track()
    : className("Track"),
      q(-1.),
      spin(1),
      mass(MuonMass),
      energy(0.),
      isElectron(false),
      particleName("mu-"),
      sensor(0),
      isChanged(true),
      usePlotting(false),
      viewer(0),
      debug(false),
      plotId(-1) {

  SetBetaGamma(3.);
}

void Track::SetParticle(std::string part) {

  isElectron = false;
  if (part == "electron" || part == "Electron" || part == "e-") {
    q = -1;
    mass = ElectronMass;
    spin = 1;
    isElectron = true;
    particleName = "e-";
  } else if (part == "positron" || part == "Positron" || part == "e+") {
    q = 1;
    mass = ElectronMass;
    spin = 1;
    particleName = "e+";
  } else if (part == "muon" || part == "Muon" || part == "mu" ||
             part == "mu-") {
    q = -1;
    mass = MuonMass;
    spin = 1;
    particleName = "mu-";
  } else if (part == "mu+") {
    q = 1;
    mass = MuonMass;
    spin = 1;
    particleName = "mu+";
  } else if (part == "pion" || part == "Pion" || part == "pi" ||
             part == "pi-") {
    q = -1;
    mass = 139.57018e6;
    spin = 0;
    particleName = "pi-";
  } else if (part == "pi+") {
    q = 1;
    mass = 139.57018e6;
    spin = 0;
    particleName = "pi+";
  } else if (part == "kaon" || part == "Kaon" || part == "K" || part == "K-") {
    q = -1;
    mass = 493.677e6;
    spin = 0;
    particleName = "K-";
  } else if (part == "K+") {
    q = 1;
    mass = 493.677e6;
    spin = 0;
    particleName = "K+";
  } else if (part == "proton" || part == "Proton" || part == "p") {
    q = 1;
    mass = ProtonMass;
    spin = 1;
    particleName = "p";
  } else if (part == "anti-proton" || part == "Anti-Proton" ||
             part == "antiproton" || part == "Antiproton" || part == "p-bar") {
    q = -1;
    mass = ProtonMass;
    spin = 1;
    particleName = "pbar";
  } else if (part == "deuteron" || part == "Deuteron" || part == "d") {
    q = 1;
    mass = 1875.612793e6;
    spin = 2;
    particleName = "d";
  } else if (part == "alpha" || part == "Alpha") {
    q = 2;
    mass = 3.727379240e9;
    spin = 0;
    particleName = "alpha";
  } else {
    std::cerr << className << "::SetParticle:\n";
    std::cerr << "    Particle " << part << " is not defined.\n";
  }
}

void Track::SetEnergy(const double e) {

  if (e <= mass) {
    std::cerr << className << "::SetEnergy:\n";
    std::cerr << "    Particle energy must be greater than the mass.\n";
    return;
  }

  energy = e;
  const double gamma = energy / mass;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;
}

void Track::SetBetaGamma(const double bg) {

  if (bg <= 0.) {
    std::cerr << className << "::SetBetaGamma:\n";
    std::cerr << "    Particle speed must be greater than zero.\n";
    return;
  }

  const double bg2 = bg * bg;
  energy = mass * sqrt(1. + bg2);
  beta2 = bg2 / (1. + bg2);
  isChanged = true;
}

void Track::SetBeta(const double beta) {

  if (beta <= 0. && beta >= 1.) {
    std::cerr << className << "::SetBeta:\n";
    std::cerr << "    Particle speed must be between zero"
              << " and speed of light.\n";
    return;
  }

  beta2 = beta * beta;
  energy = mass * sqrt(1. / (1. - beta2));
  isChanged = true;
}

void Track::SetGamma(const double gamma) {

  if (gamma <= 1.) {
    std::cerr << className << "::SetGamma:\n";
    std::cerr << "    Particle speed must be greater than zero.\n";
    return;
  }

  energy = mass * gamma;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;
}

void Track::SetMomentum(const double p) {

  if (p <= 0.) {
    std::cerr << className << "::SetMomentum:\n";
    std::cerr << "    Particle momentum must be greater than zero.\n";
    return;
  }

  energy = sqrt(mass * mass + p * p);
  const double bg = p / mass;
  beta2 = bg * bg / (1. + bg * bg);
  isChanged = true;
}

void Track::SetKineticEnergy(const double ekin) {

  if (ekin <= 0.) {
    std::cerr << className << "::SetKineticEnergy:\n";
    std::cerr << "    Kinetic energy must be greater than zero.\n";
    return;
  }

  energy = mass + ekin;
  const double gamma = 1. + ekin / mass;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;
}

void Track::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  sensor = s;
}

void Track::EnablePlotting(ViewDrift* view) {

  if (view == 0) {
    std::cerr << className << "::EnablePlotting:\n";
    std::cerr << "    Pointer is null.\n";
    return;
  }

  viewer = view;
  usePlotting = true;
}

void Track::DisablePlotting() {

  usePlotting = false;
  viewer = 0;
}

void Track::PlotNewTrack(const double x0, const double y0, const double z0) {

  if (!usePlotting || viewer == 0) return;

  viewer->NewChargedParticleTrack(1, plotId, x0, y0, z0);
}

void Track::PlotCluster(const double x0, const double y0, const double z0) {

  if (plotId < 0 || !usePlotting || viewer == 0) {
    std::cerr << className << "::PlotCluster:\n";
    std::cerr << "    No track set. Program bug!\n";
    return;
  }
  viewer->AddTrackPoint(plotId, x0, y0, z0);
}
}
