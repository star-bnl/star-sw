#include <iostream>
#include <fstream>
#include <cmath>
#include <string>

#include "AvalancheMC.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

double AvalancheMC::c1 = ElectronMass / (SpeedOfLight * SpeedOfLight);

AvalancheMC::AvalancheMC()
    : m_sensor(NULL),
      m_nDrift(0),
      m_stepModel(2),
      m_tMc(0.02),
      m_dMc(0.001),
      m_nMc(100),
      m_hasTimeWindow(false),
      m_tMin(0.),
      m_tMax(0.),
      m_nElectrons(0),
      m_nHoles(0),
      m_nIons(0),
      m_nEndpointsElectrons(0),
      m_nEndpointsHoles(0),
      m_nEndpointsIons(0),
      m_usePlotting(false),
      m_viewer(NULL),
      m_useSignal(false),
      m_useInducedCharge(false),
      m_useEquilibration(true),
      m_useDiffusion(true),
      m_useAttachment(false),
      m_useBfield(false),
      m_useIons(true),
      m_withElectrons(true),
      m_withHoles(true),
      m_scaleElectronSignal(1.),
      m_scaleHoleSignal(1.),
      m_scaleIonSignal(1.),
      m_debug(false) {

  m_className = "AvalancheMC";

  m_drift.reserve(10000);
}

void AvalancheMC::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  m_sensor = s;
}

void AvalancheMC::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n";
    std::cerr << "    Viewer pointer is null.\n";
    return;
  }

  m_usePlotting = true;
  m_viewer = view;
}

void AvalancheMC::DisablePlotting() {

  m_viewer = NULL;
  m_usePlotting = false;
}

void AvalancheMC::SetTimeSteps(const double d) {

  m_stepModel = 0;
  if (d < Small) {
    std::cerr << m_className << "::SetTimeSteps:\n";
    std::cerr << "    Specified step size is too small.\n";
    std::cerr << "    Using default (20 ps) instead.\n";
    m_tMc = 0.02;
  } else {
    if (m_debug) {
      std::cout << m_className << "::SetTimeSteps:\n";
      std::cout << "    Step size set to " << d << " ns.\n";
    }
    m_tMc = d;
  }
}

void AvalancheMC::SetDistanceSteps(const double d) {

  m_stepModel = 1;
  if (d < Small) {
    std::cerr << m_className << "::SetDistanceSteps:\n";
    std::cerr << "    Specified step size is too small.\n";
    std::cerr << "    Using default (10 um) instead.\n";
    m_dMc = 0.001;
  } else {
    if (m_debug) {
      std::cout << m_className << "::SetDistanceSteps:\n";
      std::cout << "    Step size set to " << d << " cm.\n";
    }
    m_dMc = d;
  }
}

void AvalancheMC::SetCollisionSteps(const int n) {

  m_stepModel = 2;
  if (n < 1) {
    std::cerr << m_className << "::SetCollisionSteps:\n";
    std::cerr << "    Number of collisions to be skipped set to "
              << " default value (100).\n";
    m_nMc = 100;
  } else {
    if (m_debug) {
      std::cout << m_className << "::SetCollisionSteps:\n";
      std::cout << "    Number of collisions to be skipped set to " << n
                << ".\n";
    }
    m_nMc = n;
  }
}

void AvalancheMC::SetTimeWindow(const double t0, const double t1) {

  if (fabs(t1 - t0) < Small) {
    std::cerr << m_className << "::SetTimeWindow:\n";
    std::cerr << "    Time interval must be greater than zero.\n";
    return;
  }

  m_tMin = std::min(t0, t1);
  m_tMax = std::max(t0, t1);
  m_hasTimeWindow = true;
}

void AvalancheMC::UnsetTimeWindow() { m_hasTimeWindow = false; }

void AvalancheMC::GetDriftLinePoint(const unsigned int i, double& x, double& y,
                                    double& z, double& t) {

  if (i >= m_nDrift) {
    std::cerr << m_className << "::GetDriftLinePoint:\n";
    std::cerr << "    Index is outside the range.\n";
    return;
  }

  x = m_drift[i].x;
  y = m_drift[i].y;
  z = m_drift[i].z;
  t = m_drift[i].t;
}

void AvalancheMC::GetHoleEndpoint(const unsigned int i, double& x0, double& y0,
                                  double& z0, double& t0, double& x1,
                                  double& y1, double& z1, double& t1,
                                  int& status) const {

  if (i >= m_nEndpointsHoles) {
    std::cerr << m_className << "::GetHoleEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsHoles[i].x0;
  x1 = m_endpointsHoles[i].x1;
  y0 = m_endpointsHoles[i].y0;
  y1 = m_endpointsHoles[i].y1;
  z0 = m_endpointsHoles[i].z0;
  z1 = m_endpointsHoles[i].z1;
  t0 = m_endpointsHoles[i].t0;
  t1 = m_endpointsHoles[i].t1;
  status = m_endpointsHoles[i].status;
}

void AvalancheMC::GetIonEndpoint(const unsigned int i, double& x0, double& y0,
                                 double& z0, double& t0, double& x1, double& y1,
                                 double& z1, double& t1, int& status) const {

  if (i >= m_nEndpointsIons) {
    std::cerr << m_className << "::GetIonEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsIons[i].x0;
  x1 = m_endpointsIons[i].x1;
  y0 = m_endpointsIons[i].y0;
  y1 = m_endpointsIons[i].y1;
  z0 = m_endpointsIons[i].z0;
  z1 = m_endpointsIons[i].z1;
  t0 = m_endpointsIons[i].t0;
  t1 = m_endpointsIons[i].t1;
  status = m_endpointsIons[i].status;
}

void AvalancheMC::GetElectronEndpoint(const unsigned int i, 
                                      double& x0, double& y0,
                                      double& z0, double& t0, double& x1,
                                      double& y1, double& z1, double& t1,
                                      int& status) const {

  if (i >= m_nEndpointsElectrons) {
    std::cerr << m_className << "::GetElectronEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsElectrons[i].x0;
  x1 = m_endpointsElectrons[i].x1;
  y0 = m_endpointsElectrons[i].y0;
  y1 = m_endpointsElectrons[i].y1;
  z0 = m_endpointsElectrons[i].z0;
  z1 = m_endpointsElectrons[i].z1;
  t0 = m_endpointsElectrons[i].t0;
  t1 = m_endpointsElectrons[i].t1;
  status = m_endpointsElectrons[i].status;
}

bool AvalancheMC::DriftElectron(const double x0, const double y0,
                                const double z0, const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftElectron:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 1;
  m_nHoles = 0;
  m_nIons = 0;

  if (!DriftLine(x0, y0, z0, t0, -1)) return false;

  return true;
}

bool AvalancheMC::DriftHole(const double x0, const double y0, 
                            const double z0, const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftHole:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 0;
  m_nHoles = 1;
  m_nIons = 0;

  if (!DriftLine(x0, y0, z0, t0, 1)) return false;

  return true;
}

bool AvalancheMC::DriftIon(const double x0, const double y0, 
                           const double z0, const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftIon:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 0;
  m_nHoles = 0;
  m_nIons = 1;

  if (!DriftLine(x0, y0, z0, t0, 2)) return false;

  return true;
}

bool AvalancheMC::DriftLine(const double x0, const double y0, 
                            const double z0, const double t0,
                            const int type, const bool aval) {

  // Current position
  double x = x0, y = y0, z = z0;
  // Time step
  double delta;
  // Medium
  Medium* medium = 0;
  // Electric field
  int status = 0;
  double ex = 0., ey = 0., ez = 0.;
  // Magnetic field
  double bx = 0., by = 0., bz = 0.;
  // Drift velocity
  double vx = 0., vy = 0., vz = 0., v = 0., vt = 0.;
  // Longitudinal and transverse diffusion coefficients
  double dl = 0., dt = 0.;
  // Diffusion vector
  double dx = 0., dy = 0., dz = 0., d = 0.;
  // Rotation angles
  double phi, cphi, sphi;
  double theta, ctheta, stheta;
  // Collision time
  double tau = 0.;

  // Reset the drift line.
  m_drift.clear();
  // Add the starting point to the drift line.
  driftPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = 0;
  point.nh = 0;
  point.ni = 0;
  m_drift.push_back(point);
  m_nDrift = 1;

  bool ok = true;
  bool trapped = false;
  bool validAlphaEta = false;
  int abortReason = 0;

  if (m_hasTimeWindow && (t0 < m_tMin || t0 > m_tMax)) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Starting time " << t0 << " is outside the specified\n";
    std::cerr << "    time window (" << m_tMin << ", " << m_tMax << ").\n";
    ok = false;
    abortReason = StatusOutsideTimeWindow;
  }

  // Get the electric field at the starting point.
  m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  // Make sure the starting point is inside a drift medium.
  if (status != 0) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    No drift medium at initial position (" << x << ", " << y
              << ", " << z << ").\n";
    ok = false;
    abortReason = StatusLeftDriftMedium;
  }

  double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) {
    std::cerr << m_className << "::DriftLine:\n";
    std::cerr << "    Electric field at initial position is too small:\n";
    std::cerr << "      ex = " << ex << " V/cm\n";
    std::cerr << "      ey = " << ey << " V/cm\n";
    std::cerr << "      ez = " << ez << " V/cm\n";
    ok = false;
    abortReason = StatusCalculationAbandoned;
  }

  if (m_useBfield) {
    m_sensor->MagneticField(x, y, z, bx, by, bz, status);
    bx *= Tesla2Internal;
    by *= Tesla2Internal;
    bz *= Tesla2Internal;
  }

  while (ok) {

    // Compute the drift velocity and the diffusion coefficients.
    if (type < 0) {
      if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) ||
          !medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << m_className << "::DriftLine:\n";
        std::cerr << "    Error calculating electron"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n";
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else if (type == 1) {
      if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) ||
          !medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << m_className << "::DriftLine:\n";
        std::cerr << "    Error calculating hole"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n";
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else if (type == 2) {
      if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) ||
          !medium->IonDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << m_className << "::DriftLine:\n";
        std::cerr << "    Error calculating ion"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n";
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Unknown drift line type (" << type << ").\n";
      std::cerr << "    Program bug!\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      return false;
    }
    if (m_debug) {
      std::cout << m_className << "::DriftLine:\n";
      std::cout << "    Drift velocity at " << x << ", " << y << ", " << z
                << ": " << vx << ", " << vy << ", " << vz << "\n";
    }
    v = sqrt(vx * vx + vy * vy + vz * vz);
    if (v < Small) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Drift velocity at (" << x << ", " << y << ", " << z
                << ") is too small:\n";
      std::cerr << "      vx = " << vx << " cm/ns\n";
      std::cerr << "      vy = " << vy << " cm/ns\n";
      std::cerr << "      vz = " << vz << " cm/ns\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      break;
    }

    // Determine the time step.
    switch (m_stepModel) {
      case 0:
        // Fixed time steps
        delta = m_tMc;
        break;
      case 1:
        // Fixed distance steps
        delta = m_dMc / v;
        break;
      case 2:
        // Steps based on collision time
        tau = c1 * v / e;
        delta = -m_nMc * tau * log(RndmUniformPos());
        break;
      default:
        std::cerr << m_className << "::DriftLine:\n";
        std::cerr << "    Unknown stepping model.\n";
        return false;
    }

    // Draw a random diffusion direction in the particle frame.
    if (m_useDiffusion) {
      d = sqrt(v * delta);
      dx = d * RndmGaussian(0., dl);
      dy = d * RndmGaussian(0., dt);
      dz = d * RndmGaussian(0., dt);
    }
    if (m_debug) {
      std::cout << m_className << "::DriftLine:\n";
      std::cout << "    Adding diffusion step " 
                << dx << ", " << dy << ", " << dz << "\n";
    }
    // Compute the rotation angles to align the diffusion
    // and drift velocity vectors
    vt = sqrt(vx * vx + vy * vy);
    if (vt < Small) {
      phi = 0.;
      theta = vz < 0. ? -HalfPi : HalfPi;
    } else {
      phi = atan2(vy, vx);
      theta = atan2(vz, vt);
    }
    cphi = cos(phi);
    sphi = sin(phi);
    ctheta = cos(theta);
    stheta = sin(theta);

    // Compute the proposed end-point of this step.
    x += delta * vx + cphi * ctheta * dx - sphi * dy - cphi * stheta * dz;
    y += delta * vy + sphi * ctheta * dx + cphi * dy - sphi * stheta * dz;
    z += delta * vz + stheta * dx + ctheta * dz;

    if (m_debug) {
      std::cout << m_className << "::DriftLine:\n";
      std::cout << "    New point: " 
                << x << ", " << y << ", " << z << "\n";
    }
    // Compute the electric field at the new point.
    m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);

    // Check if the new position is inside a drift medium.
    if (status != 0) {
      // Try to terminate the drift line
      // close to the boundary.
      dx = x - point.x;
      dy = y - point.y;
      dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d;
        dy /= d;
        dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d;
        y = point.y + dy * d;
        z = point.z + dz * d;
        // Check if the mid-point is inside the drift medium.
        m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
        if (status == 0) {
          point.x = x;
          point.y = y;
          point.z = z;
          point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift medium.
      point.x += dx * d;
      point.y += dy * d;
      point.z += dz * d;
      m_drift.push_back(point);
      ++m_nDrift;
      abortReason = StatusLeftDriftMedium;
      if (m_debug) {
        std::cout << m_className << "::DriftLine:\n";
        std::cout << "    Particle left the drift medium.\n";
        std::cout << "    At " << point.x << ", " << point.y << ", " << point.z
                  << "\n";
      }
      break;
    }

    // Check if the new position is inside the drift area.
    if (!m_sensor->IsInArea(x, y, z)) {
      // Try to terminate the drift line
      // close to the boundary.
      dx = x - point.x;
      dy = y - point.y;
      dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d;
        dy /= d;
        dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d;
        y = point.y + dy * d;
        z = point.z + dz * d;
        // Check if the mid-point is inside the drift area.
        if (m_sensor->IsInArea(x, y, z)) {
          point.x = x;
          point.y = y;
          point.z = z;
          point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift area.
      point.x += dx * d;
      point.y += dy * d;
      point.z += dz * d;
      m_drift.push_back(point);
      ++m_nDrift;
      abortReason = StatusLeftDriftArea;
      if (m_debug) {
        std::cout << m_className << "::DriftLine:\n";
        std::cout << "    Particle left the drift area.\n";
        std::cout << "    At " << point.x << ", " << point.y << ", " << point.z
                  << "\n";
      }
      break;
    }

    // Check if the particle has crossed a wire.
    double xCross = point.x, yCross = point.y, zCross = point.z;
    if (m_sensor->IsWireCrossed(point.x, point.y, point.z, x, y, z, xCross,
                                yCross, zCross)) {
      delta *=
          sqrt(pow(xCross - point.x, 2) + pow(yCross - point.y, 2) +
               pow(zCross - point.z, 2)) /
          sqrt(pow(x - point.x, 2) + pow(y - point.y, 2) + pow(z - point.z, 2));
      point.x = xCross;
      point.y = yCross;
      point.z = zCross;
      point.t += delta;
      m_drift.push_back(point);
      ++m_nDrift;
      abortReason = StatusLeftDriftMedium;
      if (m_debug) {
        std::cout << m_className << "::DriftLine:\n";
        std::cout << "    Particle hit a wire.\n";
        std::cout << "    At " << xCross << ", " << yCross << ", " << zCross
                  << "\n";
      }
      break;
    }

    e = sqrt(ex * ex + ey * ey + ez * ez);
    if (e < Small) {
      std::cerr << m_className << "::DriftLine:\n";
      std::cerr << "    Electric field at (" << x << ", " << y << ", " << z
                << ") is too small:\n";
      std::cerr << "      ex = " << ex << " V/cm\n";
      std::cerr << "      ey = " << ey << " V/cm\n";
      std::cerr << "      ez = " << ez << " V/cm\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      break;
    }
    // Add the new point to drift line.
    point.x = x;
    point.y = y;
    point.z = z;
    point.t += delta;
    m_drift.push_back(point);
    ++m_nDrift;

    // Check if the time is still within the specified interval.
    if (m_hasTimeWindow && point.t > m_tMax) {
      abortReason = StatusOutsideTimeWindow;
      break;
    }

    if (m_useBfield) {
      m_sensor->MagneticField(x, y, z, bx, by, bz, status);
      bx *= Tesla2Internal;
      by *= Tesla2Internal;
      bz *= Tesla2Internal;
    }
  }

  // Compute Townsend and attachment coefficients for each drift step.
  unsigned int nElectronsOld = m_nElectrons;
  unsigned int nHolesOld = m_nHoles;
  unsigned int nIonsOld = m_nIons;
  if ((type == -1 || type == 1) && (aval || m_useAttachment)) {
    // Compute Townsend and attachment coefficient
    validAlphaEta = ComputeAlphaEta(type);
    if (ok) ok = validAlphaEta;
    // Subdivision of a step
    const double probth = 0.01;

    // Set initial number of electrons/ions.
    int ne = 1, ni = 0;
    // Loop over the drift line.
    for (unsigned int i = 0; i < m_nDrift - 1; ++i) {
      m_drift[i].ne = 0;
      m_drift[i].nh = 0;
      m_drift[i].ni = 0;
      // Only attempt avalanche calculation if alpha and eta are valid.
      if (validAlphaEta) {
        // Compute the number of subdivisions.
        int nDiv = int((m_drift[i].alpha + m_drift[i].eta) / probth);
        if (nDiv < 1) nDiv = 1;
        // Probabilities for gain and loss.
        const double alpha = std::max(m_drift[i].alpha / nDiv, 0.);
        const double eta = std::max(m_drift[i].eta / nDiv, 0.);
        // Set initial number of electrons/ions.
        int neInit = ne, niInit = ni;
        // Loop over the subdivisions.
        for (int j = 0; j < nDiv; ++j) {
          if (ne > 1000) {
            // Gaussian approximation.
            const int gain = int(
                ne * alpha + RndmGaussian() * sqrt(ne * alpha * (1. - alpha)));
            const int loss =
                int(ne * eta + RndmGaussian() * sqrt(ne * eta * (1. - eta)));
            ne += gain - loss;
            ni += gain;
          } else {
            // Binomial approximation
            for (int k = ne; k--;) {
              if (RndmUniform() < alpha) {
                ++ne;
                ++ni;
              }
              if (RndmUniform() < eta) {
                --ne;
              }
            }
          }
          // Check if the particle has survived.
          if (ne <= 0) {
            trapped = true;
            if (type == -1) {
              --m_nElectrons;
            } else if (type == 1) {
              --m_nHoles;
            } else {
              --m_nIons;
            }
            m_nDrift = i + 2;
            m_drift[m_nDrift - 1].x = 0.5 * (m_drift[i].x + m_drift[i + 1].x);
            m_drift[m_nDrift - 1].y = 0.5 * (m_drift[i].y + m_drift[i + 1].y);
            m_drift[m_nDrift - 1].z = 0.5 * (m_drift[i].z + m_drift[i + 1].z);
            break;
          }
        }
        // If at least one new electron has been created,
        // add the new electrons to the table.
        if (ne - neInit >= 1) {
          if (type == -1) {
            m_drift[i].ne = ne - neInit;
            m_nElectrons += ne - neInit;
          } else if (type == 1) {
            m_drift[i].nh = ne - neInit;
            m_nHoles += ne - neInit;
          } else {
            m_drift[i].ni = ne - neInit;
            m_nIons += ne - neInit;
          }
        }
        if (ni - niInit >= 1) {
          if (type == -1) {
            if (m_useIons) {
              m_drift[i].ni = ni - niInit;
              m_nIons += ni - niInit;
            } else {
              m_drift[i].nh = ni - niInit;
              m_nHoles += ni - niInit;
            }
          } else {
            m_drift[i].ne = ni - niInit;
            m_nElectrons += ni - niInit;
          }
        }
        // If trapped, exit the loop over the drift line.
        if (trapped) {
          abortReason = StatusAttached;
          if (m_debug) {
            std::cout << m_className << "::DriftLine:\n";
            std::cout << "    Particle attached.\n";
            std::cout << "    At " << m_drift[m_nDrift - 1].x << ", "
                      << m_drift[m_nDrift - 1].y << ", " << m_drift[m_nDrift - 1].z
                      << "\n";
          }
          break;
        }
      }
    }
  }

  // Create an "endpoint"
  endpoint endPoint;
  endPoint.x0 = x0;
  endPoint.y0 = y0;
  endPoint.z0 = z0;
  endPoint.t0 = t0;
  endPoint.status = abortReason;

  endPoint.x1 = m_drift[m_nDrift - 1].x;
  endPoint.y1 = m_drift[m_nDrift - 1].y;
  endPoint.z1 = m_drift[m_nDrift - 1].z;
  endPoint.t1 = m_drift[m_nDrift - 1].t;

  if (m_debug) {
    const int nNewElectrons = m_nElectrons - nElectronsOld;
    const int nNewHoles = m_nHoles - nHolesOld;
    const int nNewIons = m_nIons - nIonsOld;
    std::cout << m_className << "::DriftLine:\n";
    std::cout << "    Produced\n"
              << "      " << nNewElectrons << " electrons,\n"
              << "      " << nNewHoles << " holes, and\n"
              << "      " << nNewIons << " ions\n"
              << "    along the drift line from \n"
              << "      (" << endPoint.x0 << ", " << endPoint.y0 << ", "
              << endPoint.z0 << ") to \n"
              << "      (" << endPoint.x1 << ", " << endPoint.y1 << ", "
              << endPoint.z1 << ").\n";
  }

  if (type == -1) {
    m_endpointsElectrons.push_back(endPoint);
    ++m_nEndpointsElectrons;
  } else if (type == 1) {
    m_endpointsHoles.push_back(endPoint);
    ++m_nEndpointsHoles;
  } else if (type == 2) {
    m_endpointsIons.push_back(endPoint);
    ++m_nEndpointsIons;
  }

  // Compute the induced signals if requested.
  if (m_useSignal) {
    if (type == 2) {
      ComputeSignal(1. * m_scaleIonSignal);
    } else if (type == 1) {
      ComputeSignal(1. * m_scaleHoleSignal);
    } else if (type < 0) {
      ComputeSignal(-1. * m_scaleElectronSignal);
    }
  }
  if (m_useInducedCharge) {
    if (type == 2) {
      ComputeInducedCharge(1. * m_scaleIonSignal);
    } else if (type == 1) {
      ComputeInducedCharge(1. * m_scaleHoleSignal);
    } else if (type < 0) {
      ComputeInducedCharge(-1. * m_scaleElectronSignal);
    }
  }

  // Plot the drift line if requested.
  if (m_usePlotting && m_nDrift > 0) {
    int jL;
    if (type < 0) {
      m_viewer->NewElectronDriftLine(m_nDrift, jL, m_drift[0].x, m_drift[0].y,
                                   m_drift[0].z);
    } else if (type == 1) {
      m_viewer->NewHoleDriftLine(m_nDrift, jL, m_drift[0].x, m_drift[0].y, m_drift[0].z);
    } else {
      m_viewer->NewIonDriftLine(m_nDrift, jL, m_drift[0].x, m_drift[0].y, m_drift[0].z);
    }
    for (unsigned int iP = 0; iP < m_nDrift; ++iP) {
      m_viewer->SetDriftLinePoint(jL, iP, m_drift[iP].x, m_drift[iP].y, m_drift[iP].z);
    }
  }

  if (!ok) return false;

  return true;
}

bool AvalancheMC::AvalancheElectron(const double x0, const double y0,
                                    const double z0, const double t0,
                                    const bool holes) {

  // Initialise the avalanche table
  m_aval.clear();
  avalPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = 1;
  point.nh = 0;
  point.ni = 0;
  m_aval.push_back(point);

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 1;
  m_nHoles = 0;
  m_nIons = 0;

  m_withHoles = holes;
  return Avalanche();
}

bool AvalancheMC::AvalancheHole(const double x0, const double y0,
                                const double z0, const double t0,
                                const bool electrons) {

  // Initialise the avalanche table
  m_aval.clear();
  avalPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = 0;
  point.nh = 1;
  point.ni = 0;
  m_aval.push_back(point);

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 0;
  m_nHoles = 1;
  m_nIons = 0;

  m_withElectrons = electrons;
  return Avalanche();
}

bool AvalancheMC::AvalancheElectronHole(const double x0, const double y0,
                                        const double z0, const double t0) {

  // Initialise the avalanche table
  m_aval.clear();
  avalPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = 1;
  point.nh = 1;
  point.ni = 0;
  m_aval.push_back(point);

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();
  m_nEndpointsElectrons = 0;
  m_nEndpointsHoles = 0;
  m_nEndpointsIons = 0;

  m_nElectrons = 1;
  m_nHoles = 1;
  m_nIons = 0;

  m_withElectrons = m_withHoles = true;
  return Avalanche();
}

bool AvalancheMC::Avalanche() {

  // Make sure that the sensor is defined
  if (!m_sensor) {
    std::cerr << m_className << "::Avalanche:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  avalPoint point;

  if (!m_withHoles && !m_withElectrons) {
    std::cerr << m_className << "::Avalanche:\n";
    std::cerr << "    Neither electron nor hole/ion component"
              << " are activated.\n";
  }

  int nAval = m_aval.size();
  while (nAval > 0) {
    for (int iAval = nAval; iAval--;) {
      if (m_withElectrons) {
        // Loop over the electrons at this location.
        for (int iE = m_aval[iAval].ne; iE--;) {
          // Compute an electron drift line.
          if (!DriftLine(m_aval[iAval].x, m_aval[iAval].y, m_aval[iAval].z,
                         m_aval[iAval].t, -1, true)) {
            continue;
          }
          // Loop over the drift line.
          for (unsigned int iDrift = 0; iDrift < m_nDrift - 2; ++iDrift) {
            if (m_drift[iDrift].ne > 0 || m_drift[iDrift].nh > 0 ||
                m_drift[iDrift].ni > 0) {
              // Add the point to the table.
              point.x = m_drift[iDrift + 1].x;
              point.y = m_drift[iDrift + 1].y;
              point.z = m_drift[iDrift + 1].z;
              point.t = m_drift[iDrift + 1].t;
              point.ne = m_drift[iDrift].ne;
              point.nh = m_drift[iDrift].nh;
              point.ni = m_drift[iDrift].ni;
              m_aval.push_back(point);
            }
          }
        }
      }

      if (m_withHoles) {
        // Loop over the ions at this location.
        for (int iI = 0; iI < m_aval[iAval].ni; ++iI) {
          // Compute an ion drift line.
          DriftLine(m_aval[iAval].x, m_aval[iAval].y, m_aval[iAval].z, m_aval[iAval].t,
                    2, false);
          continue;
        }

        // Loop over the holes at this location.
        for (int iH = 0; iH < m_aval[iAval].nh; ++iH) {
          // Compute a hole drift line.
          if (!DriftLine(m_aval[iAval].x, m_aval[iAval].y, m_aval[iAval].z,
                         m_aval[iAval].t, +1, true))
            continue;
          // Loop over the drift line.
          for (unsigned int iDrift = 0; iDrift < m_nDrift - 1; ++iDrift) {
            if (m_drift[iDrift].ne > 0 || m_drift[iDrift].nh > 0 ||
                m_drift[iDrift].ni > 0) {
              // Add the point to the table.
              point.x = m_drift[iDrift + 1].x;
              point.y = m_drift[iDrift + 1].y;
              point.z = m_drift[iDrift + 1].z;
              point.t = m_drift[iDrift + 1].t;
              point.ne = m_drift[iDrift].ne;
              point.nh = m_drift[iDrift].nh;
              point.ni = m_drift[iDrift].ni;
              m_aval.push_back(point);
            }
          }
        }
      }
      // Remove the avalanche point.
      m_aval.erase(m_aval.begin() + iAval);
    }
    nAval = m_aval.size();
  }
  return true;
}

bool AvalancheMC::ComputeAlphaEta(const int type) {

  // Locations and weights for 6-point Gaussian integration
  const double tg[6] = {-0.932469514203152028, -0.661209386466264514,
                        -0.238619186083196909, 0.238619186083196909,
                        0.661209386466264514,  0.932469514203152028};
  const double wg[6] = {0.171324492379170345, 0.360761573048138608,
                        0.467913934572691047, 0.467913934572691047,
                        0.360761573048138608, 0.171324492379170345};

  // Medium
  Medium* medium;
  int status = 0;
  // Position
  double x, y, z;
  // Electric field
  double ex = 0., ey = 0., ez = 0.;
  // Magnetic field
  double bx = 0., by = 0., bz = 0.;
  // Drift velocity
  double vx = 0., vy = 0., vz = 0.;
  // Townsend and attachment coefficient
  double alpha = 0., eta = 0.;

  // Integrated drift velocity
  double vdx = 0., vdy = 0., vdz = 0.;

  // Loop a first time over the drift line.
  for (int i = m_nDrift - 1; i--;) {
    // Compute the step length.
    const double delx = m_drift[i + 1].x - m_drift[i].x;
    const double dely = m_drift[i + 1].y - m_drift[i].y;
    const double delz = m_drift[i + 1].z - m_drift[i].z;
    const double del = sqrt(delx * delx + dely * dely + delz * delz);
    // Compute the integrated drift velocity,
    // Townsend coefficient and attachment coefficient.
    vdx = 0., vdy = 0., vdz = 0.;
    m_drift[i].alpha = 0.;
    m_drift[i].eta = 0.;
    for (int j = 6; j--;) {
      x = m_drift[i].x + 0.5 * (1. + tg[j]) * delx;
      y = m_drift[i].y + 0.5 * (1. + tg[j]) * dely;
      z = m_drift[i].z + 0.5 * (1. + tg[j]) * delz;
      m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Make sure that we are in a drift medium.
      if (status != 0) {
        // Check if this point is the last but one.
        const int lastButOne = m_nDrift - 2;
        if (i < lastButOne) {
          std::cerr << m_className << "::ComputeAlphaEta:\n";
          std::cerr << "    Got status value " << status << " at segment "
                    << j + 1 << "/6, drift point " << i + 1 << "/" << m_nDrift
                    << ".\n";
          return false;
        }
        continue;
      }
      if (m_useBfield) {
        m_sensor->MagneticField(x, y, z, bx, by, bz, status);
        bx *= Tesla2Internal;
        by *= Tesla2Internal;
        bz *= Tesla2Internal;
      }
      if (type < 0) {
        medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
        medium->ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
        medium->ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
      } else {
        medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
        medium->HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
        medium->HoleAttachment(ex, ey, ez, bx, by, bz, eta);
      }
      vdx += wg[j] * vx;
      vdy += wg[j] * vy;
      vdz += wg[j] * vz;
      m_drift[i].alpha += wg[j] * alpha;
      m_drift[i].eta += wg[j] * eta;
    }
    // Compute the scaling factor for the projected length.
    double scale = 1.;
    if (m_useEquilibration) {
      const double vd = sqrt(vdx * vdx + vdy * vdy + vdz * vdz);
      if (vd * del <= 0.) {
        scale = 0.;
      } else {
        const double dinv = delx * vdx + dely * vdy + delz * vdz;
        if (dinv < 0.) {
          scale = 0.;
        } else {
          scale = (delx * vdx + dely * vdy + delz * vdz) / (vd * del);
        }
      }
    }
    m_drift[i].alpha *= 0.5 * del * scale;
    m_drift[i].eta *= 0.5 * del * scale;
  }

  // Skip equilibration if projection has not been requested.
  if (!m_useEquilibration) return true;

  double sub1 = 0., sub2 = 0.;
  bool try1 = false, try2 = false, done = false;
  // Try to alpha-equilibrate the returning parts.
  for (unsigned int i = 0; i < m_nDrift - 1; ++i) {
    if (m_drift[i].alpha < 0.) {
      // Targets for subtracting
      sub1 = sub2 = -m_drift[i].alpha / 2.;
      try1 = try2 = false;
      // Try to subtract half in earlier points.
      for (unsigned int j = 0; j < i - 1; ++j) {
        if (m_drift[i - j].alpha > sub1) {
          m_drift[i - j].alpha -= sub1;
          m_drift[i].alpha += sub1;
          sub1 = 0.;
          try1 = true;
          break;
        } else if (m_drift[i - j].alpha > 0.) {
          m_drift[i].alpha += m_drift[i - j].alpha;
          sub1 -= m_drift[i - j].alpha;
          m_drift[i - j].alpha = 0.;
        }
      }
      // Try to subtract the other half in later points.
      for (unsigned int j = 0; j < m_nDrift - i - 1; ++j) {
        if (m_drift[i + j].alpha > sub2) {
          m_drift[i + j].alpha -= sub2;
          m_drift[i].alpha += sub2;
          sub2 = 0.;
          try2 = true;
          break;
        } else if (m_drift[i + j].alpha > 0.) {
          m_drift[i].alpha += m_drift[i + j].alpha;
          sub2 -= m_drift[i + j].alpha;
          m_drift[i + j].alpha = 0.;
        }
      }

      // Done if both sides have margin left.
      done = false;
      if (try1 && try2) {
        done = true;
      } else if (try1) {
        sub1 = -m_drift[i].alpha;
        for (unsigned int j = 0; j < i - 1; ++j) {
          if (m_drift[i - j].alpha > sub1) {
            m_drift[i - j].alpha -= sub1;
            m_drift[i].alpha += sub1;
            sub1 = 0.;
            done = true;
            break;
          } else if (m_drift[i - j].alpha > 0.) {
            m_drift[i].alpha += m_drift[i - j].alpha;
            sub1 -= m_drift[i - j].alpha;
            m_drift[i - j].alpha = 0.;
          }
        }
      } else if (try2) {
        // Try upper side again.
        sub2 = -m_drift[i].alpha;
        for (unsigned int j = 0; j < m_nDrift - i - 1; ++j) {
          if (m_drift[i + j].alpha > sub2) {
            m_drift[i + j].alpha -= sub2;
            m_drift[i].alpha += sub2;
            sub2 = 0.;
            done = true;
            break;
          } else if (m_drift[i + j].alpha > 0.) {
            m_drift[i].alpha += m_drift[i + j].alpha;
            sub2 -= m_drift[i + j].alpha;
            m_drift[i + j].alpha = 0.;
          }
        }
      }
      // See whether we succeeded.
      if (!done) {
        if (m_debug) {
          std::cerr << m_className << "::ComputeAlphaEta:\n";
          std::cerr << "    Unable to even out backwards alpha steps.\n";
          std::cerr << "    Calculation is probably inaccurate.\n";
        }
        return false;
      }
    }
  }

  // Try to eta-equilibrate the returning parts.
  for (unsigned int i = 0; i < m_nDrift - 1; ++i) {
    if (m_drift[i].eta < 0.) {
      // Targets for subtracting
      sub1 = -m_drift[i].eta / 2.;
      sub2 = -m_drift[i].eta / 2.;
      try1 = false;
      try2 = false;
      // Try to subtract half in earlier points.
      for (unsigned int j = 0; j < i - 1; ++j) {
        if (m_drift[i - j].eta > sub1) {
          m_drift[i - j].eta -= sub1;
          m_drift[i].eta += sub1;
          sub1 = 0.;
          try1 = true;
          break;
        } else if (m_drift[i - j].eta > 0.) {
          m_drift[i].eta += m_drift[i - j].eta;
          sub1 -= m_drift[i - j].eta;
          m_drift[i - j].eta = 0.;
        }
      }
      // Try to subtract the other half in later points.
      for (unsigned int j = 0; j < m_nDrift - i - 1; ++j) {
        if (m_drift[i + j].eta > sub2) {
          m_drift[i + j].eta -= sub2;
          m_drift[i].eta += sub2;
          sub2 = 0.;
          try2 = true;
          break;
        } else if (m_drift[i + j].eta > 0.) {
          m_drift[i].eta += m_drift[i + j].eta;
          sub2 -= m_drift[i + j].eta;
          m_drift[i + j].eta = 0.;
        }
      }
      done = false;
      if (try1 && try2) {
        done = true;
      } else if (try1) {
        // Try lower side again.
        sub1 = -m_drift[i].eta;
        for (unsigned int j = 0; j < i - 1; ++j) {
          if (m_drift[i - j].eta > sub1) {
            m_drift[i - j].eta -= sub1;
            m_drift[i].eta += sub1;
            sub1 = 0.;
            done = true;
            break;
          } else if (m_drift[i - j].eta > 0.) {
            m_drift[i].eta += m_drift[i - j].eta;
            sub1 -= m_drift[i - j].eta;
            m_drift[i - j].eta = 0.;
          }
        }
      } else if (try2) {
        // Try upper side again.
        sub2 = -m_drift[i].eta;
        for (unsigned int j = 0; j < m_nDrift - i - 1; ++j) {
          if (m_drift[i + j].eta > sub2) {
            m_drift[i + j].eta -= sub2;
            m_drift[i].eta += sub2;
            sub2 = 0.;
            done = true;
            break;
          } else if (m_drift[i + j].eta > 0.) {
            m_drift[i].eta += m_drift[i + j].eta;
            sub2 -= m_drift[i + j].eta;
            m_drift[i + j].eta = 0.;
          }
        }
      }
      if (!done) {
        if (m_debug) {
          std::cerr << m_className << "::ComputeAlphaEta:\n";
          std::cerr << "    Unable to even out backwards eta steps.\n";
          std::cerr << "    Calculation is probably inaccurate.\n";
        }
        return false;
      }
    }
  }

  // Seems to have worked.
  return true;
}

void AvalancheMC::ComputeSignal(const double q) {

  if (m_nDrift < 2) return;
  double dt, dx, dy, dz;
  for (unsigned int i = 0; i < m_nDrift - 1; ++i) {
    dt = m_drift[i + 1].t - m_drift[i].t;
    dx = m_drift[i + 1].x - m_drift[i].x;
    dy = m_drift[i + 1].y - m_drift[i].y;
    dz = m_drift[i + 1].z - m_drift[i].z;
    m_sensor->AddSignal(q, m_drift[i].t, dt, m_drift[i].x + 0.5 * dx,
                        m_drift[i].y + 0.5 * dy, m_drift[i].z + 0.5 * dz, 
                        dx / dt, dy / dt, dz / dt);
  }
}

void AvalancheMC::ComputeInducedCharge(const double q) {

  if (m_nDrift < 2) return;
  m_sensor->AddInducedCharge(q, m_drift[0].x, m_drift[0].y, m_drift[0].z,
                             m_drift[m_nDrift - 1].x, m_drift[m_nDrift - 1].y,
                             m_drift[m_nDrift - 1].z);
}
}
