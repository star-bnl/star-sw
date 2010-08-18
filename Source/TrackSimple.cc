#include <iostream>

#include "TrackSimple.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackSimple::TrackSimple() :
  isReady(false),
  x(0.), y(0.), z(0.), t(0.),
  dx(0.), dy(0.), dz(1.),
  mfp(0.04), eloss(2530.), useEqualSpacing(false) {

}

void
TrackSimple::SetClusterDensity(const double d) {

  if (d < Small) {
    std::cerr << "TrackSimple::SetClusterDensity:\n";
    std::cerr << "    Cluster density (number of clusters per cm)"
              << " must be positive.\n"
              << std::endl;
    return;
  }

  mfp = 1. / d;

}

void
TrackSimple::SetStoppingPower(const double dedx) {

  if (dedx < Small) {
    std::cerr << "TrackSimple::SetStoppingPower:\n";
    std::cerr << "    Stopping power (average energy loss [eV] per cm)"
              << " must be positive.\n";
    return;
  }

  eloss = dedx;

}

void
TrackSimple::NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx0, const double dy0, const double dz0) {

  // Check if a sensor has been defined
  if (sensor == 0) {
    std::cerr << "TrackSimple::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    isReady = false;
    return;
  }

  // Make sure we are inside a medium
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << "TrackSimple::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    isReady = false;
    return;
  }

  isReady = true;

  x = x0; y = y0; z = z0; t = t0;

  // Normalise the direction
  const double d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (d < Small) {
    // Choose random direction
    double phi = TwoPi * RndmUniform();
    double ctheta = 1. - 2. * RndmUniform();
    double stheta = sqrt(1. - ctheta * ctheta);
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
   dx = dx0 / d; dy = dy0 / d; dz = dz0 / d;
  }

}

bool
TrackSimple::GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                        int& n, double& e, double& extra) {

  extra = 0.;
  if (!isReady) return false;

  if (useEqualSpacing) {
    x += dx * mfp; 
    y += dy * mfp;
    z += dz * mfp;
  } else {
    const double d = - mfp * log(RndmUniformPos());
    x += dx * d;
    y += dy * d;
    z += dz * d;
  }
 
  xcls = x; ycls = y; zcls = z; tcls = t;

  n = 1;
  e = eloss * mfp;
 
  Medium* medium;
  if (!sensor->GetMedium(x, y, z, medium)) {
    isReady = false;
    if (debug) {
      std::cout << "TrackSimple::GetCluster:\n";
      std::cout << "    Particle left the medium.\n";
    }
    return false;
  }

  return true;

}

}
