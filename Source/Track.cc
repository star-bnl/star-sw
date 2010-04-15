#include <iostream>

#include "Track.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

Track::Track() :
  mass(MuonMass), energy(1.e9),
  sensor(0),
  isChanged(true), debug(false) {


}

void
Track::SetParticle(std::string part) {

  if (part == "electron" || part == "e-") {
    q = -1; mass = ElectronMass;
  } else if (part == "positron" || part == "e+") {
    q =  1; mass = ElectronMass;
  } else if (part == "muon" || part == "mu" || part == "mu-") {
    q = -1; mass = MuonMass;
  } else if (part == "mu+") {
    q =  1; mass = MuonMass;
  } else if (part == "pion" || part == "pi" || part == "pi-") {
    q = -1; mass = 139.57018e6;
  } else if (part == "pi+") {
    q =  1; mass = 139.57018e6;
  } else if (part == "kaon" || part == "K" || part == "K-") {
    q = -1; mass = 493.677e6;
  } else if (part == "K+") {
    q =  1; mass = 493.677e6;
  } else if (part == "proton" || part == "p") {
    q =  1; mass = ProtonMass;
  } else if (part == "anti-proton" || part == "p-bar") {
    q = -1; mass = ProtonMass;
  } else {
    std::cerr << "Track::SetParticle:" << std::endl;
    std::cerr << "    Particle " << part << " is not defined." << std::endl;
  }

}

void
Track::SetEnergy(const double e) {
  
  if (energy <= mass) {
    std::cerr << "Track::SetEnergy:" << std::endl;
    std::cerr << "    Particle energy must be greater than the mass." 
              << std::endl;
    return;
  }

  energy = e;
  const double gamma = energy / mass;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;

}

void
Track::SetBetaGamma(const double bg) {

  if (bg <= 0.) {
    std::cerr << "Track::SetBetaGamma:" << std::endl;
    std::cerr << "    Particle speed must be greater than zero." << std::endl;
    return;
  }

  const double bg2 = bg * bg;
  energy = mass * sqrt(1. + bg2);
  beta2 = bg2 / (1. + bg2);
  isChanged = true;

}

void
Track::SetBeta(const double beta) {

  if (beta <= 0. && beta >= 1.) {
    std::cerr << "Track::SetBeta:" << std::endl;
    std::cerr << "    Particle speed must be between zero and speed of light." 
              << std::endl;
    return;
  }

  beta2 = beta * beta;
  energy = mass * sqrt(1. / (1. - beta2));
  isChanged = true;

}

void 
Track::SetGamma(const double gamma) {

  if (gamma <= 1.) {
    std::cerr << "Track::SetGamma:" << std::endl;
    std::cerr << "    Particle speed must be greater than zero." << std::endl;
    return;
  }
  
  energy = mass * gamma;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;

}

void
Track::SetMomentum(const double p) {

  if (p <= 0.) {
    std::cerr << "Track::SetMomentum:" << std::endl;
    std::cerr << "    Particle momentum must be greater than zero." 
              << std::endl;
    return;
  }

  energy = sqrt(mass * mass + p * p);
  const double bg = p / mass;
  beta2 = bg * bg / (1. + bg * bg);
  isChanged = true;

}

void
Track::SetKineticEnergy(const double ekin) {

  if (ekin <= 0.) {
    std::cerr << "Track::SetKineticEnergy:" << std::endl;
    std::cerr << "    Kinetic energy must be greater than zero." << std::endl;
    return;
  }

  energy = mass + ekin;
  const double gamma = 1. + ekin / mass;
  beta2 = 1. - 1. / (gamma * gamma);
  isChanged = true;

}

void
Track::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << "Track::SetSensor:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }

  sensor = s;

}

}
