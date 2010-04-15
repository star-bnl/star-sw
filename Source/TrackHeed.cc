#include <iostream>

#include "TrackHeed.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

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
