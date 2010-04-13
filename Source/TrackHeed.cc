#include <iostream>

#include "TrackHeed.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

void
TrackHeed::SetParticle(std::string part) {

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
    std::cerr << "TrackHeed::SetParticle:" << std::endl;
    std::cerr << "    Particle " << part << " is not defined." << std::endl;
  }

}

void
TrackHeed::NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx, const double dy, const double dz) {

  if (debug) {
    std::cout << "TrackHeed::NewTrack:" << std::endl;
    std::cout << "    Starting point at (" 
              << x0 << ", " << y0 << ", " << z0 << ")" << std::endl;
  }

}

bool
TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls,
                      int& n, double& e, double& extra) {

  n = 1;
  e = 1000.;
  return true;

}

}
