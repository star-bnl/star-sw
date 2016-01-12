#include <iostream>
#include <iomanip>
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
      m_maxStepsToWire(1000),
      m_rejectKinks(true),
      m_useStepSizeLimit(false),
      m_usePlotting(false),
      m_view(NULL),
      m_status(0),
      m_nPoints(0),
      m_scaleElectronSignal(1.),
      m_scaleHoleSignal(1.),
      m_scaleIonSignal(1.),
      m_debug(false),
      m_verbose(false) {

  const unsigned int nMaxPoints = m_maxSteps + m_maxStepsToWire + 10;
  m_path.resize(nMaxPoints);
  m_className = "DriftLineRKF";
}

void DriftLineRKF::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n"
              << "    Sensor pointer is a null pointer.\n";
    return;
  }
  m_sensor = s;
}

void DriftLineRKF::SetIntegrationAccuracy(const double a) {

  if (a > 0.) {
    m_accuracy = a;
  } else {
    std::cerr << m_className << "::SetIntegrationAccuracy:\n"
              << "    Accuracy must be greater than zero.\n";
  }
}

void DriftLineRKF::SetMaximumStepSize(const double ms) {

  if (ms > 0.) {
    m_maxStepSize = ms;
    if (!m_useStepSizeLimit) {
      std::cout << m_className << "::SetMaximumStepSize:\n"
                << "    Don't forget to call EnableStepSizeLimit.\n";
    } 
  } else {
    std::cerr << m_className << "::SetMaximumStepSize:\n"
              << "    Step size must be greater than zero.\n";
  }
}

void DriftLineRKF::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n"
              << "    Viewer pointer is a null pointer.\n";
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
  GetGain();
  ComputeSignal(-1., m_scaleElectronSignal);
  return true;
}

bool DriftLineRKF::DriftHole(const double x0, const double y0, 
                             const double z0, const double t0) {

  m_particleType = ParticleTypeHole;
  if (!DriftLine(x0, y0, z0, t0)) return false;
  ComputeSignal(1., m_scaleHoleSignal);
  return true;
}

bool DriftLineRKF::DriftIon(const double x0, const double y0, 
                            const double z0, const double t0) {

  m_particleType = ParticleTypeIon;
  if (!DriftLine(x0, y0, z0, t0)) return false;
  ComputeSignal(1., m_scaleIonSignal);
  return true;
}

bool DriftLineRKF::DriftLine(const double x0, const double y0, 
                             const double z0, const double t0) {

  // Increase the size of the drift line vector if needed.
  const unsigned int nMaxPoints = m_maxSteps + m_maxStepsToWire + 10;
  if (nMaxPoints > m_path.size()) m_path.resize(nMaxPoints);

  // Reset the number of points on the drift line.
  m_nPoints = 0;
  // Reset the status flag.
  m_status = StatusAlive;
  
  // Check if the sensor is defined.
  if (!m_sensor) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Sensor is not defined.\n";
    m_status = StatusCalculationAbandoned;
    return false;
  }

  // Get the sensor's bounding box.
  double xmin = 0., xmax = 0.;
  double ymin = 0., ymax = 0.;
  double zmin = 0., zmax = 0.;
  bool bbox = m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax);

  // Get the electric and magnetic field at the initial position.
  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0.;
  int status = 0;
  m_sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  m_sensor->ElectricField(x0, y0, z0, ex, ey, ez, m_medium, status);
  // Make sure the initial position is at a valid location.
  if (status != 0) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    No valid field at initial position.\n";
    m_status = StatusLeftDriftMedium;
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

  // Set the numerical constants for the RKF integration.
  const double c10 = 214. /  891.;
  const double c11 =   1. /   33.;
  const double c12 = 650. /  891.;
  const double c20 = 533. / 2106.;
  const double c22 = 800. / 1053.;
  const double c23 =  -1. /   78.;

  const double b10 =    1. /   4.;
  const double b20 = -189. / 800.;
  const double b21 =  729. / 800.;
  const double b30 =  214. / 891.;
  const double b31 =    1. /  33.;
  const double b32 =  650. / 891.;

  // Set the charge of the drifting particle.
  const double charge = m_particleType == ParticleTypeElectron ? -1 : 1;
  // Initialise the current position.
  double x = x0;
  double y = y0;
  double z = z0;
  // Initialise the particle velocity.
  double v0[3] = {0., 0., 0.};
  if (!GetVelocity(ex, ey, ez, bx, by, bz, v0[0], v0[1], v0[2])) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Failed to retrieve drift velocity.\n";
    return false;
  }
  const double vTot = sqrt(v0[0] * v0[0] + v0[1] * v0[1] + v0[2] * v0[2]);
  if (vTot < Small) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Zero velocity at initial position.\n";
    return false;
  }

  // Initialise time step and previous time step.
  double dt = m_accuracy / vTot;
  double pdt = dt;

  // Set the initial point.
  m_nPoints = 1;
  m_path[0].x = x;
  m_path[0].y = y;
  m_path[0].z = z;
  m_path[0].t = t0;
  int initCycle = 3;
  while (m_nPoints <= m_maxSteps) {
    // Get first estimate of the new drift velocity.
    const double x1 = x + dt * b10 * v0[0];
    const double y1 = y + dt * b10 * v0[1];
    const double z1 = z + dt * b10 * v0[2];
    double v1[3] = {0., 0., 0.};
    if (!GetVelocity(x1, y1, z1, v1[0], v1[1], v1[2], status)) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      m_status = StatusCalculationAbandoned;
      break;
    } 
    if (status != 0) {
      if (status > 0) {
        if (m_debug) { 
          std::cout << m_className << "::DriftLine: Inside wire, halve.\n";
        }
        dt *= 0.5;
        continue;
      }
      if (!EndDriftLine()) m_status = StatusCalculationAbandoned;
      break;
    }
    // Get second estimate of the new drift velocity.
    const double x2 = x + dt * (b20 * v0[0] + b21 * v1[0]);
    const double y2 = y + dt * (b20 * v0[1] + b21 * v1[1]);
    const double z2 = z + dt * (b20 * v0[2] + b21 * v1[2]);
    double v2[3] = {0., 0., 0.};
    if (!GetVelocity(x2, y2, z2, v2[0], v2[1], v2[2], status)) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      m_status = StatusCalculationAbandoned;
      break;
    } 
    if (status != 0) {
      if (status > 0) {
        if (m_debug) { 
          std::cout << m_className << "::DriftLine: Inside wire, halve.\n";
        }
        dt *= 0.5;
        continue;
      }
      if (!EndDriftLine()) m_status = StatusCalculationAbandoned;
      break;
    }
    // Get third estimate of the new drift velocity.
    double v3[3] = {0., 0., 0.};
    const double x3 = x + dt * (b30 * v0[0] + b31 * v1[0] + b32 * v2[0]);
    const double y3 = y + dt * (b30 * v0[1] + b31 * v1[1] + b32 * v2[1]);
    const double z3 = z + dt * (b30 * v0[2] + b31 * v1[2] + b32 * v2[2]);
    if (!GetVelocity(x3, y3, z3, v3[0], v3[1], v3[2], status)) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      m_status = StatusCalculationAbandoned;
      break;
    } 
    if (status != 0) {
      if (status > 0) {
        if (m_debug) { 
          std::cout << m_className << "::DriftLine: Inside wire, halve.\n";
        }
        dt *= 0.5;
        continue;
      }
      if (!EndDriftLine()) m_status = StatusCalculationAbandoned;
      break;
    }

    // Check if we crossed a wire.
    double xw = 0., yw = 0., zw = 0.;
    if (m_sensor->IsWireCrossed(x, y, z, x1, y1, z1, xw, yw, zw) ||
        m_sensor->IsWireCrossed(x, y, z, x2, y2, z2, xw, yw, zw) ||
        m_sensor->IsWireCrossed(x, y, z, x3, y3, z3, xw, yw, zw)) { 
      if (m_debug) { 
        std::cout << m_className << "::DriftLine:\n"
                  << "    Drift line crossed wire. Reduce step size.\n";
      }
      if (dt < Small) {
        std::cerr << m_className << "::DriftLine:\n"
                  << "    Step size too small. Abandon.\n";
        m_status = StatusCalculationAbandoned;
        break;
      }
      dt *= 0.5;
      continue;
    }
    // Check if we are inside the trap radius of a wire.
    double rw = 0.;
    if (m_sensor->IsInTrapRadius(charge, x1, y1, z1, xw, yw, rw) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(xw, yw, rw)) m_status = StatusCalculationAbandoned;
      break;
    }
    if (m_sensor->IsInTrapRadius(charge, x2, y2, z2, xw, yw, rw) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(xw, yw, rw)) m_status = StatusCalculationAbandoned;
      break;
    }
    if (m_sensor->IsInTrapRadius(charge, x3, y3, z3, xw, yw, rw) &&
        m_particleType != ParticleTypeIon) {
      if (!DriftToWire(xw, yw, rw)) m_status = StatusCalculationAbandoned;
      break;
    }
    // Calculate the correction terms.
    double phi1[3] = {0., 0., 0.};
    double phi2[3] = {0., 0., 0.};
    for (unsigned int i = 0; i < 3; ++i) {
      phi1[i] = c10 * v0[i] + c11 * v1[i] + c12 * v2[i];
      phi2[i] = c20 * v0[i] + c22 * v2[i] + c23 * v3[i];
    }
    // Check if the step length is valid.
    const double phi1Tot =
        sqrt(phi1[0] * phi1[0] + phi1[1] * phi1[1] + phi1[2] * phi1[2]);
    if (phi1Tot < Small) {
      std::cerr << m_className << "::DriftLine:\n"
                << "    Step has zero length. Abandoning drift.\n";
      m_status = StatusCalculationAbandoned;
      break;
    } else if (m_useStepSizeLimit && dt * phi1Tot > m_maxStepSize) {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Step too long, reduce.\n";
      }
      dt = 0.5 * m_maxStepSize / phi1Tot;
      continue;
    } else if (bbox) {
      // Don't allow dt to become too large in view of the time resolution.
      if (dt * fabs(phi1[0]) > 0.1 * fabs(xmax - xmin) ||
          dt * fabs(phi1[1]) > 0.1 * fabs(ymax - ymin)) {
        dt *= 0.5;
        if (m_debug) {
          std::cout << m_className << "::DriftLine: Step too long, halve.\n";
        }
        continue;
      }
    } else if (m_rejectKinks && m_nPoints > 1) {
      if (phi1[0] * (m_path[m_nPoints - 1].x - m_path[m_nPoints - 2].x) +
          phi1[1] * (m_path[m_nPoints - 1].y - m_path[m_nPoints - 2].y) +
          phi1[2] * (m_path[m_nPoints - 1].z - m_path[m_nPoints - 2].z) < 0.) {
        std::cerr << m_className << "::DriftLine:\n"
                  << "    Bending angle exceeds pi/2.\n";
        m_status = StatusSharpKink; 
        break;
      }
    } 
    if (m_debug) std::cout << m_className << "::DriftLine: Step size ok.\n";
    // Update the position.
    x += dt * phi1[0];
    y += dt * phi1[1];
    z += dt * phi1[2];
    // Add a new point to the drift line.
    ++m_nPoints;
    m_path[m_nPoints - 1].x = x;
    m_path[m_nPoints - 1].y = y;
    m_path[m_nPoints - 1].z = z;
    m_path[m_nPoints - 1].t = m_path[m_nPoints - 2].t + dt;
    // Check the new position.
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
    if (status != 0) {
      // The new position is not inside a valid drift medium.
      // Go back one step and terminate the drift line.
      --m_nPoints;
      if (m_debug) {
        std::cout << m_className << "::DriftLine:\n"
                  << "     New point is outside. Terminate.\n";
      } 
      if (!EndDriftLine()) m_status = StatusCalculationAbandoned;
      break;
    }
    // Adjust the step size depending on the accuracy of the two estimates.
    pdt = dt;
    const double dphi0 = fabs(phi1[0] - phi2[0]);
    const double dphi1 = fabs(phi1[1] - phi2[1]);
    const double dphi2 = fabs(phi1[2] - phi2[2]);
    if (dphi0 > Small || dphi1 > Small || dphi2 > Small) {
      dt = sqrt(dt * m_accuracy / (dphi0 + dphi1 + dphi2));
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Adapting step size to "
                  << dt << " ns (from " << pdt << ").\n";
      }
    } else {
      dt *= 2.;
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Double step size.\n";
      }
    }
    // Make sure that dt is different from zero;
    // this should always be ok.
    if (dt < Small) {
      std::cerr << m_className << "::DriftLine:\n"
                << "    Step size is zero (program bug).\n"
                << "    The calculation is abandoned.\n";
      m_status = StatusCalculationAbandoned;
      break;
    }
    // Check the initial step size.
    if (initCycle > 0 && dt < pdt / 5.) {
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Reinitialise step size.\n";
      }
      --initCycle;
      x = m_path[0].x;
      y = m_path[0].y;
      z = m_path[0].z;
      m_nPoints = 1;
      continue;
    }
    initCycle = 0;
    // Prevent the step size from growing too fast.
    if (dt > 10. * pdt) {
      dt = 10. * pdt;
      if (m_debug) {
        std::cout << m_className << "::DriftLine: Limit step size to " << dt << "\n";
      }
    }
    // Stop in case dt tends to become too small.
    if (dt * (fabs(phi1[0]) + fabs(phi1[1]) + fabs(phi1[2])) < m_accuracy) {
      std::cerr << m_className << "::DriftLine:\n"
                << "    Step size has become smaller than int. accuracy.\n"
                << "    The calculation is abandoned.\n";
      m_status = StatusCalculationAbandoned; 
      break;
    }
    // Update the velocity.
    v0[0] = v3[0];
    v0[1] = v3[1];
    v0[2] = v3[2];

    if (m_nPoints > m_maxSteps) {
      m_status = StatusTooManySteps;
      break;
    } 
  }
  if (m_verbose) {
    // If requested, print step history.
    std::cout << m_className << "::DriftLine:\n";
    std::cout << "    Drift line status: " << m_status << "\n";
    std::cout << "   Step          time         time step     "
              << "       x               y                 z\n";
    for (unsigned int i = 0; i < m_nPoints; ++i) {
      std::cout << std::setw(8) << i << " " 
                << std::fixed << std::setprecision(7) 
                << std::setw(15) << m_path[i].t << "  ";
      if (i > 0) {
        std::cout << std::setw(15) << m_path[i].t - m_path[i - 1].t << "  ";
      } else {
        std::cout << std::string(17, ' ');
      } 
      std::cout << std::setw(15) << m_path[i].x << "  " 
                << std::setw(15) << m_path[i].y << "  " 
                << std::setw(15) << m_path[i].z << "\n";
    }
  }
  if (m_usePlotting) {
    for (unsigned int i = 0; i < m_nPoints; ++i) {
      m_view->AddDriftLinePoint(iLine, m_path[i].x, m_path[i].y, m_path[i].z);
    }
  }
  if (status == StatusCalculationAbandoned) return false;
  return true;
}

double DriftLineRKF::GetArrivalTimeSpread() {

  if (m_nPoints < 2) return 0.;
  double sum = 0.;
  for (unsigned int i = 0; i < m_nPoints - 1; ++i) {
    const unsigned int j = i + 1;
    sum += IntegrateDiffusion(m_path[i].x, m_path[i].y, m_path[i].z,
                              m_path[j].x, m_path[j].y, m_path[j].z);
  }
  return sqrt(sum);
}

double DriftLineRKF::GetGain() {

  if (m_nPoints < 2) return 0.;
  if (m_status == StatusCalculationAbandoned) return 0.;
  // First get a rough estimate of the result.
  double crude = 0.;
  double alphaPrev = 0.;
  for (unsigned int i = 0; i < m_nPoints; ++i) {
    // Get the Townsend coefficient at this step.
    const double x = m_path[i].x;
    const double y = m_path[i].y;
    const double z = m_path[i].z;
    double ex, ey, ez;
    double bx, by, bz;
    int status;
    m_sensor->MagneticField(x, y, z, bx, by, bz, status);
    m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
    if (status != 0) {
      std::cerr << m_className << "::GetGain:\n";
      std::cerr << "    Invalid drift line point " << i << ".\n";
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
    const double dx = x - m_path[i - 1].x;
    const double dy = y - m_path[i - 1].y;
    const double dz = z - m_path[i - 1].z;
    const double d = sqrt(dx * dx + dy * dy + dz * dz);
    crude += 0.5 * d * (alpha + alphaPrev);
    alphaPrev = alpha;
  }
  // Calculate the integration tolerance based on the rough estimate.
  const double tol = 1.e-4 * crude;
  double sum = 0.;
  m_path[0].alphaint = 0.;
  for (unsigned int i = 0; i < m_nPoints - 1; ++i) {
    const unsigned int j = i + 1;
    sum += IntegrateTownsend(m_path[i].x, m_path[i].y, m_path[i].z,
                             m_path[j].x, m_path[j].y, m_path[j].z, tol);
    m_path[j].alphaint = sum;
  }
  return exp(sum);
}

bool DriftLineRKF::GetVelocity(const double x, const double y, const double z,
                               double& vx, double& vy, double& vz,
                               int& status) {

  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0.;
  m_sensor->MagneticField(x, y, z, bx, by, bz, status);
  m_sensor->ElectricField(x, y, z, ex, ey, ez, m_medium, status);
  // Stop if we are outside a valid drift medium.
  if (status != 0) return true;
  if (m_particleType == ParticleTypeElectron) {
    return m_medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (m_particleType == ParticleTypeIon) {
    return m_medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (m_particleType == ParticleTypeHole) {
    return m_medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  return false;

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

  // Start at the last valid point of the drift line.
  double x0 = m_path[m_nPoints - 1].x;
  double y0 = m_path[m_nPoints - 1].y;
  double z0 = m_path[m_nPoints - 1].z;
  // Get the initial drift velocity.
  double vx = 0., vy = 0., vz = 0.;
  int status = 0;
  if (!GetVelocity(x0, y0, z0, vx, vy, vz, status)) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    Failed to retrieve initial drift velocity.\n";
    return false;
  } else if (status != 0) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    No valid field at initial point. Program bug!\n";
    return false;
  }
  const double speed = sqrt(vx * vx + vy * vy + vz * vz);
  if (speed < Small) {
    std::cerr << m_className << "::EndDriftLine:\n";
    std::cerr << "    Zero velocity at initial position.\n";
    return false;
  }
  // Calculate the initial step size.
  if (m_nPoints > 1) {
    const double dx = x0 - m_path[m_nPoints - 2].x;
    const double dy = y0 - m_path[m_nPoints - 2].y;
    const double dz = z0 - m_path[m_nPoints - 2].z;
    const double scale = sqrt(dx * dx + dy * dy + dz * dz) / speed;
    vx *= scale;
    vy *= scale;
    vz *= scale;
  } else {
    // This is the very first step. Start with a small step size.
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
    double ex = 0., ey = 0., ez = 0.;
    m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, m_medium, status);
    if (status != 0) inside = false;
  }
  const unsigned int nBisections = 100;
  const double tol = BoundaryDistance * BoundaryDistance;
  for (unsigned int i = 0; i < nBisections; ++i) {
    const double x = x0 + 0.5 * (x1 - x0);
    const double y = y0 + 0.5 * (y1 - y0);
    const double z = z0 + 0.5 * (z1 - z0);
    double ex = 0., ey = 0., ez = 0.;
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
    const double dx = x1 - x0;
    const double dy = y1 - y0;
    const double dz = z1 - z0;
    const double d = dx * dx + dy * dy + dz * dz;
    if (d < tol) break; 
  }
  // Calculate the time step.
  const double dx = x0 - m_path[m_nPoints - 1].x;
  const double dy = y0 - m_path[m_nPoints - 1].y;
  const double dz = z0 - m_path[m_nPoints - 1].z;
  const double dt = sqrt(dx * dx + dy * dy + dz * dz) / speed;
  // Add the last point, just inside the drift area.
  ++m_nPoints;
  m_path[m_nPoints - 1].x = x0;
  m_path[m_nPoints - 1].y = y0;
  m_path[m_nPoints - 1].z = z0;
  m_path[m_nPoints - 1].t = m_path[m_nPoints - 2].t + dt;
  m_status = StatusLeftDriftMedium;
  return true;
}

bool DriftLineRKF::DriftToWire(const double xw, const double yw, 
                               const double rw) {

  // Get the starting point.
  double x0 = m_path[m_nPoints - 1].x;
  double y0 = m_path[m_nPoints - 1].y;
  double z0 = m_path[m_nPoints - 1].z;
  double t0 = m_path[m_nPoints - 1].t - m_path[0].t;
  if (m_debug) {
    std::cout << m_className << "::DriftToWire:\n";
    std::cout << "    Drifting particle at (" << x0 << ", " << y0 << ")\n";
    std::cout << "    to wire at (" << xw << ", " << yw
              << ") with physical radius " << rw << " cm.\n";
  }

  // Get the initial drift velocity.
  double vx0 = 0., vy0 = 0., vz0 = 0.;
  int status = 0;
  if (!GetVelocity(x0, y0, z0, vx0, vy0, vz0, status)) {
    std::cerr << m_className << "::DriftToWire:\n";
    std::cerr << "    Failed to retrieve initial drift velocity.\n";
    return false;
  } else if (status != 0) {
    std::cerr << m_className << "::DriftToWire:\n";
    std::cerr << "    No valid field at initial point. Program bug!\n";
    return false;
  }

  // Get a coarse estimate of the drift time
  // assuming a straight-line trajectory and constant velocity.
  double dx0 = xw - x0;
  double dy0 = yw - y0;
  double dt = (sqrt(dx0 * dx0 + dy0 * dy0) - rw) / 
               sqrt(vx0 * vx0 + vy0 * vy0);
  // If the time needed to reach the wire is very small, stop here.
  if (dt < 1.e-6 * t0) {
    m_status = StatusLeftDriftMedium;
    return true;
  }

  const double r2 = rw * rw;
  const unsigned int nMaxSteps = m_maxStepsToWire;
  for (unsigned int i = 0; i < nMaxSteps; ++i) {
    // Check the time step.
    bool smallTimeStep = false;
    /*
    if (dt < 1.e-6) {
      // Estimated time step is very small. Stop.
      if (m_debug) {
        std::cout << m_className << "::DriftToWire:\n";
        std::cout << "    Estimated time step: " << dt << " ns. Stop.\n";
      }
      smallTimeStep = true;
    }
    */
    // Calculate the estimated end-point.
    double x1 = x0 + dt * vx0;
    double y1 = y0 + dt * vy0;
    double z1 = z0 + dt * vz0;
    if (m_debug) {
      std::cout << m_className << "::DriftToWire:\n";
      std::cout << "    Step " << i << " from (" << x0 << ", " << y0 
                << ") to (" << x1 << ", " << y1 << ").\n";
    }
    // Make sure we are not moving away from the wire.
    const double dxw0 = xw - x0;
    const double dyw0 = yw - y0;
    const double xinp0 = (x1 - x0) * dxw0 + (y1 - y0) * dyw0;
    if (xinp0 < 0.) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Particle moves away from the wire. Abandoning.\n";
      return false;
    }
    // Check if the end-point is inside the wire or the wire was crossed.
    bool onwire = false;
    const double dxw1 = xw - x1;
    const double dyw1 = yw - y1;
    const double xinp1 = (x0 - x1) * dxw1 + (y0 - y1) * dyw1;
    if (xinp1 < 0.) {
      const double d2 = dxw1 * dxw1 + dyw1 * dyw1;
      if (d2 <= r2) {
        onwire = true;
        if (m_debug) std::cout << m_className << "::DriftToWire: Inside.\n";
      } 
    } else {
      if (m_debug) std::cout << m_className << "::DriftToWire: Wire crossed.\n";
      onwire = true;
    }
    if (onwire) {
      const double dw0 = sqrt(dxw0 * dxw0 + dyw0 * dyw0);
      x1 = xw - (rw + BoundaryDistance) * dxw0 / dw0; 
      y1 = yw - (rw + BoundaryDistance) * dyw0 / dw0;  
      const double dx10 = x1 - x0;
      const double dy10 = y1 - y0;
      dt = sqrt((dx10 * dx10 + dy10 * dy10) / (vx0 * vx0 + vy0 * vy0));
      z1 = z0 + dt * vz0;
    }
    // Calculate the drift velocity at the end point.
    double vx1 = 0., vy1 = 0., vz1 = 0.;
    if (!GetVelocity(x1, y1, z1, vx1, vy1, vz1, status)) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Cannot retrieve drift velocity at end point. Quit.\n";
      return false;
    } else if (status != 0) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    End point is not in a valid drift medium. Quit.\n";
      return false;
    }
    // Get a point halfway between.
    const double xm = 0.5 * (x0 + x1);
    const double ym = 0.5 * (y0 + y1);
    const double zm = 0.5 * (z0 + z1);
    // Calculate the drift velocity at the mid point.
    double vxm = 0., vym = 0., vzm = 0.;
    if (!GetVelocity(xm, ym, zm, vxm, vym, vzm, status)) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Cannot retrieve drift velocity at mid point. Quit.\n";
      return false;
    } else if (status != 0) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Mid point is not in a valid drift medium. Quit.\n";
      return false;
    }
    const double v0 = sqrt(vx0 * vx0 + vy0 * vy0);
    const double v1 = sqrt(vx1 * vx1 + vy1 * vy1);
    const double vm = sqrt(vxm * vxm + vym * vym);
    // Make sure the velocities are non-zero.
    if (v0 < Small || v1 < Small || vm < Small) {
      std::cerr << m_className << "::DriftToWire:\n";
      std::cerr << "    Zero velocity. Abandoning.\n";
      return false;
    }
    // Compare first and second order estimates.
    const double dx01 = x0 - x1;
    const double dy01 = y0 - y1;
    const double d10 = sqrt(dx01 * dx01 + dy01 * dy01);
    if (d10 * fabs(1. / v0 - 2. / vm + 1. / v1) / 3. > 1.e-4 * (1. + fabs(t0)) && 
        i < nMaxSteps - 1) {
      // Accuracy was not good enough so halve the step time.
      if (m_debug) {
        std::cout << m_className << "::DriftToWire: Reducing step size.\n";
      }
      dt *= 0.5;
      continue;
    }
    const double t1 = t0 + d10 * (1. / v0 + 4. / vm + 1. / v1) / 6.;
    // Add a new point to the drift line.
    ++m_nPoints;
    m_path[m_nPoints - 1].x = x1;
    m_path[m_nPoints - 1].y = y1;
    m_path[m_nPoints - 1].z = z1;
    m_path[m_nPoints - 1].t = m_path[0].t + t1;
    if (onwire) break;
    // JAMES MOTT HACK (5th Oct 2015)
    if(smallTimeStep) break;
    // Proceed to the next step.
    x0 = x1;
    y0 = y1;
    z0 = z1;
    t0 = t1;
    vx0 = vx1;
    vy0 = vy1;
    vz0 = vz1;
  }
  m_status = StatusLeftDriftMedium;
  return true;
}

void DriftLineRKF::GetEndPoint(double& x, double& y, double& z, double& t,
                               int& stat) const {

  if (m_nPoints == 0) {
    x = y = z = t = 0.;
    stat = m_status;
    return;
  }
  x = m_path[m_nPoints - 1].x;
  y = m_path[m_nPoints - 1].y;
  z = m_path[m_nPoints - 1].z;
  t = m_path[m_nPoints - 1].t;
  stat = m_status;
}

void DriftLineRKF::GetDriftLinePoint(const unsigned int i, double& x, double& y,
                                    double& z, double& t) const {

  if (i >= m_nPoints) {
    std::cerr << m_className << "::GetDriftLinePoint:\n";
    std::cerr << "    Index is outside the range.\n";
    return;
  }

  // Return midpoint of drift line stage
  x = m_path[i].x;
  y = m_path[i].y;
  z = m_path[i].z;
  t = m_path[i].t;
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

void DriftLineRKF::ComputeSignal(const double q, const double scale) const {

  if (m_nPoints < 2) return;
  double g = 1.;
  for (unsigned int i = 0; i < m_nPoints - 1; ++i) {
    // Calculate step length.
    const double dt = m_path[i + 1].t - m_path[i].t;
    const double dx = m_path[i + 1].x - m_path[i].x;
    const double dy = m_path[i + 1].y - m_path[i].y;
    const double dz = m_path[i + 1].z - m_path[i].z;
    // Calculate average velocity.
    const double vx = dx / dt;
    const double vy = dy / dt;
    const double vz = dz / dt;
    // Calculate midpoint.
    const double xm = m_path[i].x + 0.5 * dx;
    const double ym = m_path[i].y + 0.5 * dy;
    const double zm = m_path[i].z + 0.5 * dz;
    if (q < 0.) {
      // Electron signal is weighted by avalanche size at this step.
      g = exp(m_path[i].alphaint);
    }
    m_sensor->AddSignal(q * g * scale, m_path[i].t, dt, xm, ym, zm, vx, vy, vz);
  }
}
}
