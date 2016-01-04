#include <iostream>
#include <cmath>

#include "DriftLineRKF.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

DriftLineRKF::DriftLineRKF()
    : m_sensor(NULL),
      m_medium(NULL),
      m_maxStepSize(1.e8),
      m_accuracy(1.e-8),
      m_maxSteps(1000),
      m_usePlotting(false),
      m_view(NULL),
      m_scaleElectronSignal(1.),
      m_scaleHoleSignal(1.),
      m_scaleIonSignal(1.),
      m_debug(false),
      m_verbose(false) {

  m_className = "DriftLineRKF";
  m_path.clear();
}

void DriftLineRKF::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }
  m_sensor = s;
}

void DriftLineRKF::SetIntegrationAccuracy(const double a) {

  if (a > 0.) {
    m_accuracy = a;
  } else {
    std::cerr << m_className << "::SetIntegrationAccuracy:\n";
    std::cerr << "    Accuracy must be greater than zero.\n";
  }
}

void DriftLineRKF::SetMaximumStepSize(const double ms) {

  if (ms > 0.) {
    m_maxStepSize = ms;
  } else {
    std::cerr << m_className << "::SetMaximumStepSize:\n";
    std::cerr << "    Step size must be greater than zero.\n";
  }
}

void DriftLineRKF::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n";
    std::cerr << "    Viewer pointer is null.\n";
    return;
  }
  m_usePlotting = true;
  m_view = view;
}

void DriftLineRKF::DisablePlotting() {

  m_view = NULL;
  m_usePlotting = false;
}

bool DriftLineRKF::DriftElectron(const double x0, const double y0,
                                 const double z0, const double t0) {

  m_particleType = ParticleTypeElectron;
  if (!DriftLine(x0, y0, z0, t0)) return false;
  // GetGain();
  ComputeSignal();
  return true;
}

bool DriftLineRKF::DriftHole(const double x0, const double y0, 
                             const double z0, const double t0) {

  m_particleType = ParticleTypeHole;
  if (!DriftLine(x0, y0, z0, t0)) return false;
  ComputeSignal();
  return true;
}

bool DriftLineRKF::DriftIon(const double x0, const double y0, 
                            const double z0, const double t0) {

  m_particleType = ParticleTypeIon;
  if (!DriftLine(x0, y0, z0, t0)) return false;
  ComputeSignal();
  return true;
}

bool DriftLineRKF::DriftLine(const double x0, const double y0, 
                             const double z0, const double t0) {

  // Check if the sensor is defined.
  if (!m_sensor) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }
  // Get electric and magnetic field at initial position.
  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0.;
  int status = 0;
  m_sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  m_sensor->ElectricField(x0, y0, z0, ex, ey, ez, m_medium, status);
  // Make sure the initial position is at a valid location.
  if (status != 0) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    No valid field at initial position.\n";
    return false;
  }

  // Start plotting a new line if requested.
  int iLine = 0;
  if (m_usePlotting) {
    if (m_particleType == ParticleTypeIon) {
      m_view->NewIonDriftLine(1, iLine, x0, y0, z0);
    } else if (m_particleType == ParticleTypeElectron) {
      m_view->NewElectronDriftLine(1, iLine, x0, y0, z0);
    } else if (m_particleType == ParticleTypeHole) {
      m_view->NewHoleDriftLine(1, iLine, x0, y0, z0);
    }
  }

  // Setup numerical constants for RKF integration.
  const double c10 = 214. / 891.;
  const double c11 = 1. / 33.;
  const double c12 = 650. / 891.;
  const double c20 = 533. / 2106.;
  const double c22 = 800. / 1053.;
  const double c23 = -1. / 78.;

  const double b10 = 1. / 4.;
  const double b20 = -189. / 800.;
  const double b21 = 729. / 800.;
  const double b30 = 214. / 891.;
  const double b31 = 1. / 33.;
  const double b32 = 650. / 891.;

  // Charge of the drifting particle
  const double charge = m_particleType == ParticleTypeElectron ? -1 : 1;
  // Current position
  double x = x0;
  double y = y0;
  double z = z0;
  // Initial velocity
  double v0x = 0., v0y = 0., v0z = 0.;
  // Velocities at mid-points
  double v1[3] = {0., 0., 0.};
  double v2[3] = {0., 0., 0.};
  double v3[3] = {0., 0., 0.};
  // Position where particle has crossed the trap radius of a wire
  double rc[3] = {0., 0., 0.};
  // Centre and radius of the wire at which the particle is to be terminated
  double xWire = 0., yWire = 0.;
  double rWire = 0.;
  // Final velocity estimates
  double phi1[3] = {0., 0., 0.};
  double phi2[3] = {0., 0., 0.};

  // Initialize particle velocity.
  if (!GetVelocity(ex, ey, ez, bx, by, bz, v0x, v0y, v0z)) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Failed to retrieve drift velocity.\n";
    return false;
  }
  const double vTot = sqrt(v0x * v0x + v0y * v0y + v0z * v0z);
  if (vTot < Small) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Zero velocity at initial position.\n";
    return false;
  }
  // Initialise time step and previous time step.
  double dt = m_accuracy / vTot;
  double pdt = 0.;

  // Count the number of steps
  unsigned int counter = 0;
  // Flag whether to continue with the next drift step or not
  bool keepGoing = true;
  m_path.clear();
  while (counter <= m_maxSteps && keepGoing) {
    step tempStep;
    m_path.push_back(tempStep);
    m_path[counter].xi = x;
    m_path[counter].yi = y;
    m_path[counter].zi = z;
    if (counter == 0) {
      m_path[counter].ti = t0;
    } else {
      m_path[counter].ti = m_path[counter - 1].tf;
    }
    // Get first estimate of new drift velocity.
    double x1 = x + dt * b10 * v0x;
    double y1 = y + dt * b10 * v0y;
    double z1 = z + dt * b10 * v0z;
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      if (!EndDriftLine()) return false;
      break;
    }
    if (m_sensor->IsWireCrossed(m_path.back().xi, m_path.back().yi,
                                m_path.back().zi, x1, y1, z1, rc[0], rc[1],
                                rc[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Drift line crossed wire. Abandoning.\n";
      m_path[counter].status = StatusCalculationAbandoned;
      break;
    }
    if (m_sensor->IsInTrapRadius(charge, x1, y1, z1, xWire, yWire, rWire) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(x1, y1, z1, xWire, yWire, rWire)) return false;
      break;
    }
    if (!GetVelocity(ex, ey, ez, bx, by, bz, v1[0], v1[1], v1[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return false;
    }
    // Get second estimate of new drift velocity.
    x1 = x + dt * (b20 * v0x + b21 * v1[0]);
    y1 = y + dt * (b20 * v0y + b21 * v1[1]);
    z1 = z + dt * (b20 * v0z + b21 * v1[2]);
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      if (!EndDriftLine()) return false;
      break;
    }
    if (m_sensor->IsWireCrossed(m_path.back().xi, m_path.back().yi,
                                m_path.back().zi, x1, y1, z1, rc[0], rc[1],
                                rc[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Drift line crossed wire. Abandoning.\n";
      m_path[counter].status = StatusCalculationAbandoned;
      break;
    }
    if (m_sensor->IsInTrapRadius(charge, x1, y1, z1, xWire, yWire, rWire) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(x1, y1, z1, xWire, yWire, rWire)) return false;
      break;
    }
    if (!GetVelocity(ex, ey, ez, bx, by, bz, v2[0], v2[1], v2[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return false;
    }
    // Get third estimate of new drift velocity.
    x1 = x + dt * (b30 * v0x + b31 * v1[0] + b32 * v2[0]);
    y1 = y + dt * (b30 * v0y + b31 * v1[1] + b32 * v2[1]);
    z1 = z + dt * (b30 * v0z + b31 * v1[2] + b32 * v2[2]);
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      if (!EndDriftLine()) return false;
      break;
    }
    if (m_sensor->IsWireCrossed(m_path.back().xi, m_path.back().yi,
                                m_path.back().zi, x1, y1, z1, rc[0], rc[1],
                                rc[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Drift line crossed wire. Abandoning.\n";
      m_path[counter].status = StatusCalculationAbandoned;
      break;
    }
    if (m_sensor->IsInTrapRadius(charge, x1, y1, z1, xWire, yWire, rWire) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(x1, y1, z1, xWire, yWire, rWire)) return false;
      break;
    }
    if (!GetVelocity(ex, ey, ez, bx, by, bz, v3[0], v3[1], v3[2])) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return false;
    }

    // Calculate estimates of velocity over step.
    phi1[0] = c10 * v0x + c11 * v1[0] + c12 * v2[0];
    phi1[1] = c10 * v0y + c11 * v1[1] + c12 * v2[1];
    phi1[2] = c10 * v0z + c11 * v1[2] + c12 * v2[2];

    phi2[0] = c20 * v0x + c22 * v2[0] + c23 * v3[0];
    phi2[1] = c20 * v0y + c22 * v2[1] + c23 * v3[1];
    phi2[2] = c20 * v0z + c22 * v2[2] + c23 * v3[2];

    // Check step length is valid.
    const double phi1Tot =
        sqrt(phi1[0] * phi1[0] + phi1[1] * phi1[1] + phi1[2] * phi1[2]);
    if (phi1Tot <= 0.0) {
      std::cerr << m_className << "::DriftLine:\n"
                << "    Zero velocity. Abandoning drift.\n";
      keepGoing = false;
    } else if (dt * phi1Tot > m_maxStepSize) {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Step too long, reduce.\n";
      }
      dt = 0.5 * m_maxStepSize / phi1Tot;
    } else {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Step good.\n";
      }
    }
    pdt = dt;
    // Update position.
    x += dt * phi1[0];
    y += dt * phi1[1];
    z += dt * phi1[2];

    m_path[counter].xf = x;
    m_path[counter].yf = y;
    m_path[counter].zf = z;
    m_path[counter].tf = m_path[counter].ti + dt;
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
    if (status != 0) {
      if (!EndDriftLine()) return false;
      break;
    }

    // Adjust step size depending on accuracy.
    const double dphi0 = fabs(phi1[0] - phi2[0]);
    const double dphi1 = fabs(phi1[1] - phi2[1]);
    const double dphi2 = fabs(phi1[2] - phi2[2]);
    if (dphi0 > Small || dphi1 > Small || dphi2 > Small) {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Adapting step size.\n";
      }
      dt = sqrt(dt * m_accuracy / (dphi0 + dphi1 + dphi2));
    } else {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Increase step size.\n";
      }
      dt *= 2.;
    }
    // Make sure that dt is different from zero;
    // this should always be ok.
    if (dt <= 0.) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Step size is zero (program bug).\n";
      std::cerr << "    The calculation is abandoned.\n";
      return false;
    }
    // Prevent step size from growing too fast.
    if (dt > 10. * pdt) dt = 10. * pdt;
    // Stop in case dt tends to become too small.
    if (dt * (fabs(phi1[0]) + fabs(phi1[1]) + fabs(phi1[2])) < m_accuracy) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Step size has become smaller than int. accuracy.\n";
      std::cerr << "    The calculation is abandoned.\n";
      return false;
    }
    // Update velocity.
    v0x = v3[0];
    v0y = v3[1];
    v0z = v3[2];

    if (keepGoing && counter <= m_maxSteps) {
      m_path[counter].status = StatusAlive;
    } else if (counter > m_maxSteps) {
      m_path[counter].status = StatusTooManySteps;
    } else {
      m_path[counter].status = StatusCalculationAbandoned;
    }
    // Increase counter (default counter max = 1000)
    ++counter;
  }
  const unsigned int nSteps = m_path.size();
  if (m_verbose) {
    // If requested, print step history.
    std::cout << "    Step #    time    Xi    Yi    Zi    Xf    Yf    Zf    dt "
                 "   Status\n";
    for (unsigned int i = 0; i < nSteps; ++i) {
      std::cout.precision(8);
      std::cout << i << "    " << m_path[i].ti << "    " << m_path[i].xi
                << "    " << m_path[i].yi << "    " << m_path[i].zi << "    "
                << m_path[i].xf << "    " << m_path[i].yf << "    "
                << m_path[i].zf << "    " << fabs(m_path[i].tf - m_path[i].ti)
                << "    " << m_path[i].status << "\n";
    }
  }
  if (m_usePlotting) {
    for (unsigned int i = 0; i < nSteps; ++i) {
      m_view->AddDriftLinePoint(iLine, m_path[i].xi, m_path[i].yi,
                                m_path[i].zi);
    }
    m_view->AddDriftLinePoint(iLine, m_path.back().xf, m_path.back().yf,
                              m_path.back().zf);
  }
  return true;
}

double DriftLineRKF::GetArrivalTimeSpread() {

  const unsigned int nSteps = m_path.size();
  double sum = 0.;
  for (unsigned int i = 0; i < nSteps; ++i) {
    sum += IntegrateDiffusion(m_path[i].xi, m_path[i].yi, m_path[i].zi,
                              m_path[i].xf, m_path[i].yf, m_path[i].zf);
  }
  return sqrt(sum);
}

double DriftLineRKF::GetGain() {

  if (m_path.empty()) return 0.;
  const unsigned int nSteps = m_path.size();
  // First get a rough estimate of the result.
  double crude = 0.;
  double alphaPrev = 0.;
  for (unsigned int i = 0; i < nSteps; ++i) {
    // Get the Townsend coefficient at this step.
    const double x = m_path[i].xi;
    const double y = m_path[i].yi;
    const double z = m_path[i].zi;
    double ex, ey, ez;
    double bx, by, bz;
    int status;
    m_sensor->MagneticField(x, y, z, bx, by, bz, status);
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::GetGain:\n";
      std::cerr << "    Invalid initial drift line point.\n";
      return 0.;
    }
    double alpha = 0.;
    if (!GetTownsend(ex, ey, ez, bx, by, bz, alpha)) {
      std::cerr << m_className << "::GetGain:\n";
      std::cerr << "    Unable to retrieve Townsend coefficient.\n";
      return 0.;
    }
    if (i == 0) {
      alphaPrev = alpha;
      continue;
    }
    const double dx = x - m_path[i - 1].xi;
    const double dy = y - m_path[i - 1].yi;
    const double dz = z - m_path[i - 1].zi;
    const double d = sqrt(dx * dx + dy * dy + dz * dz);
    crude += 0.5 * d * (alpha + alphaPrev);
    alphaPrev = alpha;
  }
  // Calculate the integration tolerance based on the rough estimate.
  const double tol = 1.e-4 * crude;
  double sum = 0.;
  for (unsigned int i = 0; i < nSteps; ++i) {
    sum += IntegrateTownsend(m_path[i].xi, m_path[i].yi, m_path[i].zi,
                             m_path[i].xf, m_path[i].yf, m_path[i].zf, tol);
    m_path[i].alphaint = sum;
  }
  return exp(sum);
}

bool DriftLineRKF::GetVelocity(const double ex, const double ey,
                               const double ez, const double bx,
                               const double by, const double bz, double& vx,
                               double& vy, double& vz) const {

  if (m_particleType == ParticleTypeElectron) {
    return m_medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (m_particleType == ParticleTypeIon) {
    return m_medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (m_particleType == ParticleTypeHole) {
    return m_medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  return false;
}

bool DriftLineRKF::GetDiffusion(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& dl,
                                double& dt) const {

  if (m_particleType == ParticleTypeElectron) {
    return m_medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  } else if (m_particleType == ParticleTypeIon) {
    return m_medium->IonDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  } else if (m_particleType == ParticleTypeHole) {
    return m_medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  }
  return false;
}

bool DriftLineRKF::GetTownsend(const double ex, const double ey,
                               const double ez, const double bx,
                               const double by, const double bz,
                               double& alpha) const {

  if (m_particleType == ParticleTypeElectron) {
    return m_medium->ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
  } else if (m_particleType == ParticleTypeHole) {
    return m_medium->HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool DriftLineRKF::EndDriftLine() {

  m_path.back().status = StatusCalculationAbandoned;
  m_path.back().xf = m_path.back().xi;
  m_path.back().yf = m_path.back().yi;
  m_path.back().zf = m_path.back().zi;
  double x0 = m_path.back().xi;
  double y0 = m_path.back().yi;
  double z0 = m_path.back().zi;
  double bx = 0., by = 0., bz = 0.;
  double ex = 0., ey = 0., ez = 0.;
  int status = 0;
  m_sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  m_sensor->ElectricField(x0, y0, z0, ex, ey, ez, m_medium, status);
  if (status != 0) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    No valid field at initial point. Program bug!\n";
    return false;
  }
  double vx = 0., vy = 0., vz = 0.;
  if (!GetVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz)) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    Failed to retrieve initial drift velocity.\n";
    return false;
  }
  const double speed = sqrt(vx * vx + vy * vy + vz * vz);
  if (speed < Small) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    Zero velocity at initial position.\n";
    return false;
  }
  // Calculate the initial step size.
  if (m_path.size() > 1) {
    const double dx = x0 - m_path[m_path.size() - 2].xi;
    const double dy = y0 - m_path[m_path.size() - 2].yi;
    const double dz = z0 - m_path[m_path.size() - 2].zi;
    const double scale = sqrt(dx * dx + dy * dy + dz * dz) / speed;
    vx *= scale;
    vy *= scale;
    vz *= scale;
  } else {
    // This is the first step. Start with a small step size.
    vx *= m_accuracy / speed;
    vy *= m_accuracy / speed;
    vz *= m_accuracy / speed;
  }
  double x1 = x0;
  double y1 = y0;
  double z1 = z0;
  // Step along the initial direction until we leave the volume.
  bool inside = true;
  while (inside) {
    x1 += vx;
    y1 += vy;
    z1 += vz;
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) inside = false;
  }
  double x = 0., y = 0., z = 0.;
  const unsigned int nBisections = 100;
  for (unsigned int i = 0; i < nBisections; ++i) {
    x = x0 + 0.5 * (x1 - x0);
    y = y0 + 0.5 * (y1 - y0);
    z = z0 + 0.5 * (z1 - z0);
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
    if (status == 0) {
      x0 = x;
      y0 = y;
      z0 = z;
    } else {
      x1 = x;
      y1 = y;
      z1 = z;
    }
  }
  // Set final position.
  m_path.back().xf = x;
  m_path.back().yf = y;
  m_path.back().zf = z;
  const double dx = x - m_path.back().xi;
  const double dy = y - m_path.back().yi;
  const double dz = z - m_path.back().zi;
  const double dt = sqrt(dx * dx + dy * dy + dz * dz) / speed;
  m_path.back().tf = m_path.back().ti + dt;
  m_path.back().status = StatusLeftDriftMedium;
  return true;
}

bool DriftLineRKF::DriftToWire(double x0, double y0, double z0,
                               const double& xw, const double& yw,
                               const double& rw) {

  if (m_debug) {
    std::cout << m_className << "::DriftToWire:\n";
    std::cout << "    Drifting particle at (" << x0 << ", " << y0 << ")\n";
    std::cout << "    to wire at (" << xw << ", " << yw
              << ") with physical radius " << rw << " cm.\n";
  }
  const double t0 = m_path.front().ti;
  m_path.back().xf = x0;
  m_path.back().yf = y0;
  m_path.back().zf = z0;
  m_path.back().tf = m_path.back().ti;
  m_path.back().status = StatusCalculationAbandoned;

  // Check initial position.
  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0.;
  int status = 0;
  m_sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  m_sensor->ElectricField(x0, y0, z0, ex, ey, ez, m_medium, status);
  if (status != 0) {
    std::cerr << m_className << "::DriftToWire:\n";
    std::cerr << "    Initial position not valid. Abandoning.\n";
    return false;
  }
  // Calculate the drift velocity at the initial position.
  double vx0 = 0.;
  double vz0 = 0.;
  double vy0 = 0.;
  if (!GetVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
    std::cerr << m_className << "::DriftToWire:\n";
    std::cerr << "    Cannot retrieve drift velocity at initial position.\n";
    return false;
  }
  // Get a coarse estimate of the drift time
  // assuming a straight-line trajectory and constant velocity.
  double dx0 = xw - x0;
  double dy0 = yw - y0;
  double dt0 = (sqrt(dx0 * dx0 + dy0 * dy0) - rw - BoundaryDistance) / 
               sqrt(vx0 * vx0 + vy0 * vy0);
  const unsigned int nMaxSteps = 10;
  for (unsigned int i = 0; i < nMaxSteps; ++i) {
    // JAMES MOTT HACK (5th Oct 2015) - m_path.back().xf,yf,zf not being set
    // Put in flag here, remove return statement and check when looking for onwire flag
    bool smallTimeStep = false;
    if (dt0 < 1.e-6) {
      if (m_debug) {
        std::cout << m_className << "::DriftToWire:\n";
        std::cout << "    Estimated time step: " << dt0 << " ns. Stop.\n";
      }
      // Estimated time step is very small. Stop.
      m_path.back().tf = m_path.back().ti + dt0;
      m_path.back().status = StatusLeftDriftMedium;
      // JAMES MOTT HACK (5th Oct 2015)
      //      return true;
      smallTimeStep = true;
    }
    // Calculate the estimated end-point.
    double x1 = x0 + dt0 * vx0;
    double y1 = y0 + dt0 * vy0;
    double z1 = z0 + dt0 * vz0;
    if (m_debug) {
      std::cout << m_className << "::DriftToWire:\n";
      std::cout << "    Step " << i << " from (" << x0 << ", " << y0 
                << ") to (" << x1 << ", " << y1 << ").\n";
    }
    // Make sure we are not moving away from the wire.
    const double xin0 = (x1 - x0) * (xw - x0) + (y1 - y0) * (yw - y0);
    if (xin0 < 0.) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Particle moves away from the wire. Abandoning.\n";
      return false;
    }
    // Check if the wire was crossed.
    bool onwire = false;
    double xc = 0., yc = 0., zc = 0.;
    if (m_sensor->IsWireCrossed(x0, y0, z0, x1, y1, z1, xc, yc, zc)) {
      if (m_debug) std::cout << m_className << "::DriftToWire: Wire crossed.\n";
      x1 = xc;
      y1 = yc;
      z1 = zc;
      onwire = true;
      const double dx10 = x1 - x0;
      const double dy10 = y1 - y0;
      dt0 = sqrt(dx10 * dx10 + dy10 * dy10) / (vx0 * vx0 + vy0 * vy0);
    }
    m_path.back().xf = x1;
    m_path.back().yf = y1;
    m_path.back().zf = z1;
    m_path.back().tf = m_path.back().ti + dt0;
    // Get the field at this point.
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    End point is not in a valid drift medium.\n"; 
      std::cerr << "    Abandoning.\n";
      m_path.back().status = StatusCalculationAbandoned;
      return false;
    }
    // Calculate the drift velocity at this point.
    double vx1 = 0., vy1 = 0., vz1 = 0.;
    if (!GetVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Cannot retrieve drift velocity at end point.\n";
      m_path.back().status = StatusCalculationAbandoned;
      return false;
    }

    // Get a point halfway between.
    const double xm = 0.5 * (x0 + x1);
    const double ym = 0.5 * (y0 + y1);
    const double zm = 0.5 * (z0 + z1);
    // Get the field at this point.
    m_sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
    m_sensor->ElectricField(xm, ym, zm, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Mid point is not in a valid drift medium.\n";
      std::cerr << "    Abandoning.\n";
      m_path.back().status = StatusCalculationAbandoned;
      return false;
    }
    // Calculate the drift velocity at this point.
    double vxm = 0., vym = 0., vzm = 0.;
    if (!GetVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Cannot retrieve drift velocity at mid point.\n";
      m_path.back().status = StatusCalculationAbandoned;
      return false;
    }
    const double speed0 = sqrt(vx0 * vx0 + vy0 * vy0);
    const double speed1 = sqrt(vx1 * vx1 + vy1 * vy1);
    const double speedm = sqrt(vxm * vxm + vym * vym);
    // Make sure the velocities are non-zero.
    if (speed0 < Small || speed1 < Small || speedm < Small) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Zero velocity. Abandoning.\n";
      return false;
    }
    // Compare first and second order estimates.
    const double dx = x0 - x1;
    const double dy = y0 - y1;
    const double dxy = sqrt(dx * dx + dy * dy);
    if (dxy * fabs(1. / speed0 - 2. / speedm + 1. / speed1) / 3. >
        1.e-4 * (1. + fabs(m_path.back().ti - t0)) &&
        i < nMaxSteps - 1) {
      // Accuracy was not good enough so halve the step time.
      if (m_debug) {
        std::cout << m_className << "::DriftToWire: Reducing step size.\n";
      }
      dt0 *= 0.5;
      continue;
    }
    // Add point to the drift line.
    m_path.back().tf = m_path.back().ti + 
                       dxy * (1. / speed0 + 4. / speedm + 1. / speed1) / 6.; 
    m_path.back().xf = x1;
    m_path.back().yf = y1;
    m_path.back().zf = z1;
    if (onwire) break;
    // JAMES MOTT HACK (5th Oct 2015)
    if(smallTimeStep) break;
    step newStep;
    newStep.xi = x1;
    newStep.yi = y1;
    newStep.zi = z1;
    newStep.ti = m_path.back().tf;
    m_path.push_back(newStep);
    // Proceed to the next step
    x0 = x1;
    y0 = y1;
    z0 = z1;
    vx0 = vx1;
    vy0 = vy1;
    vz0 = vz1;
    dx0 = xw - x0;
    dy0 = yw - y0;
    dt0 = (sqrt(dx0 * dx0 + dy0 * dy0) - rw - BoundaryDistance) / 
          sqrt(vx0 * vx0 + vy0 * vy0);
  }
  return true;
}

void DriftLineRKF::GetEndPoint(double& x, double& y, double& z, double& t,
                               int& stat) const {

  x = m_path.back().xi;
  y = m_path.back().yi;
  z = m_path.back().zi;
  t = m_path.back().ti;
  stat = m_path.back().status;
}

void DriftLineRKF::GetDriftLinePoint(const unsigned int i, double& x, double& y,
                                    double& z, double& t) const {

  if (i >= m_path.size()) {
    std::cerr << m_className << "::GetDriftLinePoint:\n";
    std::cerr << "    Index is outside the range.\n";
    return;
  }

  // Return midpoint of drift line stage
  x = 0.5*(m_path.at(i).xi+m_path.at(i).xf);
  y = 0.5*(m_path.at(i).yi+m_path.at(i).yf);
  z = 0.5*(m_path.at(i).zi+m_path.at(i).zf);
  t = 0.5*(m_path.at(i).ti+m_path.at(i).tf);
}

double DriftLineRKF::IntegrateDiffusion(const double x, const double y,
                                        const double z, const double xe,
                                        const double ye, const double ze) {

  if (m_debug) {
    std::cout << m_className << "::IntegrateDiffusion:\n";
    std::cout << "  Integrating from ";
    std::cout << x << ", " << y << ", " << z << " to " << xe << ", " << ye
              << ", " << ze << "\n";
  }
  // Make sure initial position is valid.
  double ex, ey, ez;
  double bx, by, bz;
  int status;
  m_sensor->MagneticField(x, y, z, bx, by, bz, status);
  m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
  if (status != 0) {
    std::cerr << m_className << "::IntegrateDiffusion:\n";
    std::cerr << "    Initial position not valid.\n";
    return 0.;
  }
  // Determine drift velocity at initial position.
  double vx0 = 0., vy0 = 0., vz0 = 0.;
  if (!GetVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
    std::cerr << m_className << "::IntegrateDiffusion:\n";
    std::cerr << "    Unable to retrieve drift velocity.\n";
    return 0.;
  }
  double speed0 = sqrt(vx0 * vx0 + vy0 * vy0 + vz0 * vz0);
  if (speed0 < Small) {
    std::cerr << m_className << "::IntegrateDiffusion:\n";
    std::cerr << "    Zero velocity at initial position.\n";
    return 0.;
  }
  // Determine diffusion coefficients at initial position.
  double dL0 = 0.;
  double dT0 = 0.;
  if (!GetDiffusion(ex, ey, ez, bx, by, bz, dL0, dT0)) {
    std::cerr << m_className << "::IntegrateDiffusion:\n";
    std::cerr << "    Unable to retrieve diffusion coefficients.\n";
    return 0.;
  }

  // Start and end point coordinates of initial step.
  double x0 = x;
  double y0 = y;
  double z0 = z;
  double x1 = xe;
  double y1 = ye;
  double z1 = ze;

  double integral = 0.;
  bool keepGoing = true;
  while (keepGoing) {
    const double dx = x1 - x0;
    const double dy = y1 - y0;
    const double dz = z1 - z0;
    const double d = sqrt(dx * dx + dy * dy + dz * dz);
    if (d < 1.e-6) {
      // Step length has become very small.
      if (m_debug) {
        std::cout << m_className << "::IntegrateDiffusion: Small step.\n";
      }
      const double s = dL0 / speed0;
      integral += s * s * d;
      // Check if we are close to the end point.
      const double dxe = xe - x1;
      const double dye = ye - y1;
      const double dze = ze - z1;
      if (sqrt(dxe * dxe + dye * dye + dze * dze) < 1.e-6) break;
      // Proceed with the next step.
      x0 = x1;
      y0 = y1;
      z0 = z1;
      x1 = xe;
      y1 = ye;
      z1 = ze;
      continue;
    }
    // Determine drift velocity and diffusion at the end point of the step.
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Invalid end point.\n";
      break;
    }
    double vx1 = 0.;
    double vy1 = 0.;
    double vz1 = 0.;
    if (!GetVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve drift velocity.\n";
      break;
    }
    double speed1 = sqrt(vx1 * vx1 + vy1 * vy1 + vz1 * vz1);
    double dL1 = 0.;
    double dT1 = 0.;
    if (!GetDiffusion(ex, ey, ez, bx, by, bz, dL1, dT1)) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve diffusion.\n";
      break;
    }
    // Determine drift velocity and diffusion at the mid point of the step.
    const double xm = 0.5 * (x0 + x1);
    const double ym = 0.5 * (y0 + y1);
    const double zm = 0.5 * (z0 + z1);
    m_sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
    m_sensor->ElectricField(xm, ym, zm, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Invalid mid point.\n";
      break;
    }
    double vxm = 0.;
    double vym = 0.;
    double vzm = 0.;
    if (!GetVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve drift velocity.\n";
      break;
    }
    double speedm = sqrt(vxm * vxm + vym * vym + vzm * vzm);
    double dLm = 0.;
    double dTm = 0.;
    if (!GetDiffusion(ex, ey, ez, bx, by, bz, dLm, dTm)) {
      std::cerr << m_className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve diffusion.\n";
      break;
    }
    const double tolerance = 1.e-3;
    const double s0 = pow(dL0 / speed0, 2);
    const double s1 = pow(dL1 / speed1, 2);
    const double sm = pow(dLm / speedm, 2);
    // Calculate integral using Simpsons rule.
    const double simpson = d * (s0 + 4. * sm + s1) / 6.;
    // Calculate integral using trapezoidal rule.
    const double trapez = d * (s0 + s1) / 2.;
    // Compare the two estimates.
    if (fabs(trapez - simpson) * sqrt(2. * d / (s0 + s1)) / 6. < tolerance) {
      // Accuracy is good enough.
      integral += simpson;
      // Proceed to the next step.
      x0 = x1;
      y0 = y1;
      z0 = z1;
      dL0 = dL1;
      dT0 = dT1;
      speed0 = speed1;
      x1 = xe;
      y1 = ye;
      z1 = ze;
    } else {
      // Accuracy is not good enough, so halve the step.
      x1 = xm;
      y1 = ym;
      z1 = zm;
      dL1 = dLm;
      dT1 = dTm;
    }
  }
  return integral;
}

double DriftLineRKF::IntegrateTownsend(const double xi, const double yi,
                                       const double zi, 
                                       const double xe, const double ye, 
                                       const double ze,
                                       const double tol) {

  // Make sure the initial position is valid.
  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0.;
  int status = 0;
  m_sensor->MagneticField(xi, yi, zi, bx, by, bz, status);
  m_sensor->ElectricField(xi, yi, zi, ex, ey, ez, m_medium, status);
  if (status != 0) {
    std::cerr << m_className << "::IntegrateTownsend:\n";
    std::cerr << "    Initial position (" << xi << ", " << yi << ", "
              << zi << ") not valid.\n";
    return 0.;
  }
  // Determine the Townsend coefficient at the initial point.
  double alpha0 = 0.;
  if (!GetTownsend(ex, ey, ez, bx, by, bz, alpha0)) {
    std::cerr << m_className << "::IntegrateTownsend:\n";
    std::cerr << "    Cannot retrieve Townsend coefficient at initial point.\n";
    return 0.;
  }
  // Make sure the end position is valid.
  m_sensor->MagneticField(xe, ye, ze, bx, by, bz, status);
  m_sensor->ElectricField(xe, ye, ze, ex, ey, ez, m_medium, status);
  if (status != 0) {
    std::cerr << m_className << "::IntegrateTownsend:\n";
    std::cerr << "    End position (" << xi << ", " << yi << ", "
              << zi << ") not valid.\n";
    return 0.;
  }
  // Determine Townsend coefficient at end point.
  double alpha1 = 0.;
  if (!GetTownsend(ex, ey, ez, bx, by, bz, alpha1)) {
    std::cerr << m_className << "::IntegrateTownsend:\n";
    std::cerr << "    Cannot retrieve Townsend coefficient at end point.\n";
    return 0.;
  }
  // Start and end point coordinates of initial step.
  double x0 = xi;
  double y0 = yi;
  double z0 = zi;
  double x1 = xe;
  double y1 = ye;
  double z1 = ze;
  double dx = x1 - x0;
  double dy = y1 - y0;
  double dz = z1 - z0;
  double d = sqrt(dx * dx + dy * dy + dz * dz);
  // Calculate the convergence criterium.
  const double eps = tol / d;
  unsigned int stepCounter = 0;
  double integral = 0.;
  bool keepGoing = true;
  while (keepGoing) {
    dx = x1 - x0;
    dy = y1 - y0;
    dz = z1 - z0;
    d = sqrt(dx * dx + dy * dy + dz * dz);
    const double tol = 1.e-6;
    if (d < tol) {
      // Step length has become very small.
      if (m_debug) {
        std::cout << m_className << "::IntegrateTownsend: Small step.\n";
      }
      integral += alpha0 * d;
      // Check if we are close to the end point.
      const double dxe = xe - x1;
      const double dye = ye - y1;
      const double dze = ze - z1;
      const double tol2 = tol * tol;
      if (dxe * dxe + dye * dye + dze * dze < tol2) break;
      // Proceed with the next step.
      x0 = x1;
      y0 = y1;
      z0 = z1;
      x1 = xe;
      y1 = ye;
      z1 = ze;
      continue;
    }
    ++stepCounter;
    // Calculate the Townsend coefficient at the end point of the step.
    m_sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::IntegrateTownsend:\n";
      std::cerr << "    Invalid end point.\n";
      break;
    }
    double alpha1 = 0.;
    if (!GetTownsend(ex, ey, ez, bx, by, bz, alpha1)) {
      std::cerr << m_className << "::IntegrateTownsend:\n";
      std::cerr << "    Cannot retrieve Townsend coefficient at end point.\n";
      break;
    }
    // Calculate the Townsend coefficient at the mid point of the step.
    const double xm = 0.5 * (x0 + x1);
    const double ym = 0.5 * (y0 + y1);
    const double zm = 0.5 * (z0 + z1);
    m_sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
    m_sensor->ElectricField(xm, ym, zm, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::IntegrateTownsend:\n";
      std::cerr << "    Invalid mid point.\n";
      break;
    }
    double alpham = 0.;
    if (!GetTownsend(ex, ey, ez, bx, by, bz, alpham)) {
      std::cerr << m_className << "::IntegrateTownsend:\n";
      std::cerr << "    Cannot retrieve Townsend coefficient at mid point.\n";
      break;
    }
    // Check the accuracy of the result.
    if (fabs(alpha0 - 2. * alpham + alpha1) / 3. < eps) {
      // Accuracy is good enough.
      integral += d * (alpha0 + 4. * alpham + alpha1) / 6.;
      // Proceed to the next step.
      x0 = x1;
      y0 = y1;
      z0 = z1;
      alpha0 = alpha1;
      if (fabs(x0 - xe) < BoundaryDistance &&
          fabs(y0 - ye) < BoundaryDistance &&
          fabs(z0 - ze) < BoundaryDistance) {
        keepGoing = false;
        break;
      }
      x1 += dx;
      y1 += dy;
      z1 += dz;
    } else {
      // Accuracy is not good enough, so halve the step.
      x1 = xm;
      y1 = ym;
      z1 = zm;
      alpha1 = alpham;
    }
  }
  return integral;
}

void DriftLineRKF::ComputeSignal() const {

  const unsigned int nSteps = m_path.size();
  if (nSteps < 2) return;
  for (unsigned int i = 0; i < nSteps; ++i) {
    // Calculate step length.
    const double dt = m_path[i].tf - m_path[i].ti;
    const double dx = m_path[i].xf - m_path[i].xi;
    const double dy = m_path[i].yf - m_path[i].yi;
    const double dz = m_path[i].zf - m_path[i].zi;
    // Calculate average velocity.
    const double vx = dx / dt;
    const double vy = dy / dt;
    const double vz = dz / dt;
    // Calculate midpoint.
    const double xm = m_path[i].xi + 0.5 * dx;
    const double ym = m_path[i].yi + 0.5 * dy;
    const double zm = m_path[i].zi + 0.5 * dz;
    if (m_particleType == ParticleTypeElectron) {
      // Signal is weighted by avalanche size at this step.
      const double g = exp(m_path[i].alphaint);
      m_sensor->AddSignal(-1. * g * m_scaleElectronSignal, m_path[i].ti, dt, xm, ym, zm, vx, vy, vz);
    } else if (m_particleType == ParticleTypeIon) {
      m_sensor->AddSignal(1. * m_scaleIonSignal, m_path[i].ti, dt, xm, ym, zm, vx, vy, vz);
    } else if (m_particleType == ParticleTypeHole) {
      m_sensor->AddSignal(1. * m_scaleHoleSignal, m_path[i].ti, dt, xm, ym, zm, vx, vy, vz);
    }
  }
}
}
