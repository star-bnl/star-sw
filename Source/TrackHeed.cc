#include <iostream>

#include "TrackHeed.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

void
TrackHeed::NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx0, const double dy0, const double dz0) {

  if (debug) {
    std::cout << "TrackHeed::NewTrack:" << std::endl;
    std::cout << "    Starting point at (" 
              << x0 << ", " << y0 << ", " << z0 << ") at time " 
              << t0 << std::endl;
    std::cout << "    Initial direction: ("
              << dx0 << ", " << dy0 << ", " << dz0 << ")" << std::endl;
    
  }

}

bool
TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                      int& n, double& e, double& extra) {

  xcls = ycls = zcls = tcls = 0.;
  extra = 0.;
  n = 1;
  e = 1000.;
  return true;

}

}
