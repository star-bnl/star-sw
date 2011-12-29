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

AvalancheMC::AvalancheMC() :
  sensor(0),
  nDrift(0),
  stepModel(2), tMc(0.02), dMc(0.001), nMc(100),
  hasTimeWindow(false), tMin(0.), tMax(0.),
  nElectrons(0), nHoles(0), nIons(0), 
  nEndpointsElectrons(0), nEndpointsHoles(0), nEndpointsIons(0),
  usePlotting(false), viewer(0), 
  useSignal(false), useInducedCharge(false), useEquilibration(true), 
  useDiffusion(true), useAttachment(false), useBfield(false), useIons(true), 
  withElectrons(true), withHoles(true),
  debug(false) {
  
  className = "AvalancheMC";
   
}

AvalancheMC::~AvalancheMC() {

}

void 
AvalancheMC::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }
  
  sensor = s;

}

void
AvalancheMC::EnablePlotting(ViewDrift* view) {
  
  if (view == 0) {
    std::cerr << className << "::EnablePlotting:\n";
    std::cerr << "    Viewer pointer is null.\n";
    return;
  }

  usePlotting = true;
  viewer = view;

}

void
AvalancheMC::DisablePlotting() {

  viewer = 0;
  usePlotting = false;

}

void 
AvalancheMC::SetTimeSteps(const double d) {

  stepModel = 0;
  if (d < Small) {
    std::cerr << className << "::SetTimeSteps:\n";
    std::cerr << "    Specified step size is too small.\n";
    std::cerr << "    Using default (20 ps) instead.\n";
    tMc = 0.02;
  } else {
    if (debug) {
      std::cout << className << "::SetTimeSteps:\n";
      std::cout << "    Step size set to " << d << " ns.\n";
    }
    tMc = d;
  }
  
}

void
AvalancheMC::SetDistanceSteps(const double d) {

  stepModel = 1;
  if (d < Small) {
    std::cerr << className << "::SetDistanceSteps:\n";
    std::cerr << "    Specified step size is too small.\n";
    std::cerr << "    Using default (10 um) instead.\n";
    dMc = 0.001;
  } else {
    if (debug) {
      std::cout << className << "::SetDistanceSteps:\n";
      std::cout << "    Step size set to " << d << " cm.\n";
    }
    dMc = d;
  }
  
}
  
void
AvalancheMC::SetCollisionSteps(const int n) {

  stepModel = 2;
  if (n < 1) {
    std::cerr << className << "::SetCollisionSteps:\n";
    std::cerr << "    Number of collisions to be skipped set to "
              << " default value (100).\n";
    nMc = 100;
  } else {
    if (debug) {
      std::cout << className << "::SetCollisionSteps:\n";
      std::cout << "    Number of collisions to be skipped set to " 
                << n << ".\n";
    }
    nMc = n;
  }
  
}

void
AvalancheMC::SetTimeWindow(const double t0, const double t1) {

  if (fabs(t1 - t0) < Small) {
    std::cerr << className << "::SetTimeWindow:\n";
    std::cerr << "    Time interval must be greater than zero.\n";
    return;
  }

  tMin = std::min(t0, t1);
  tMax = std::max(t0, t1);
  hasTimeWindow = true;

}

void
AvalancheMC::UnsetTimeWindow() {

  hasTimeWindow = false;

}

void 
AvalancheMC::GetDriftLinePoint(const int i, double& x, double& y, double& z, double& t) {

  if (i < 0 || i >= nDrift) {
    std::cerr << className << "::GetDriftLinePoint:\n";
    std::cerr << "    Index is outside the range.\n";
    return;
  }
  
  x = drift[i].x;
  y = drift[i].y;
  z = drift[i].z;
  t = drift[i].t;

}

void
AvalancheMC::GetHoleEndpoint(const int i,
                             double& x0, double& y0, double& z0, double& t0,
                             double& x1, double& y1, double& z1, double& t1,
                             int& status) const {

  if (i < 0 || i >= nEndpointsHoles) {
    std::cerr << className << "::GetHoleEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = endpointsHoles[i].x0; x1 = endpointsHoles[i].x1;
  y0 = endpointsHoles[i].y0; y1 = endpointsHoles[i].y1;
  z0 = endpointsHoles[i].z0; z1 = endpointsHoles[i].z1;
  t0 = endpointsHoles[i].t0; t1 = endpointsHoles[i].t1;
  status = endpointsHoles[i].status;

}

void
AvalancheMC::GetIonEndpoint(const int i,
                             double& x0, double& y0, double& z0, double& t0,
                             double& x1, double& y1, double& z1, double& t1,
                             int& status) const {

  if (i < 0 || i >= nEndpointsIons) {
    std::cerr << className << "::GetIonEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = endpointsIons[i].x0; x1 = endpointsIons[i].x1;
  y0 = endpointsIons[i].y0; y1 = endpointsIons[i].y1;
  z0 = endpointsIons[i].z0; z1 = endpointsIons[i].z1;
  t0 = endpointsIons[i].t0; t1 = endpointsIons[i].t1;
  status = endpointsIons[i].status;

}

void
AvalancheMC::GetElectronEndpoint(const int i,
                             double& x0, double& y0, double& z0, double& t0,
                             double& x1, double& y1, double& z1, double& t1,
                             int& status) const {

  if (i < 0 || i >= nEndpointsElectrons) {
    std::cerr << className << "::GetElectronEndpoint:\n";
    std::cerr << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = endpointsElectrons[i].x0; x1 = endpointsElectrons[i].x1;
  y0 = endpointsElectrons[i].y0; y1 = endpointsElectrons[i].y1;
  z0 = endpointsElectrons[i].z0; z1 = endpointsElectrons[i].z1;
  t0 = endpointsElectrons[i].t0; t1 = endpointsElectrons[i].t1;
  status = endpointsElectrons[i].status;

}

bool
AvalancheMC::DriftElectron(
    const double x0, const double y0, const double z0, const double t0) {
  
  if (sensor == 0) {
    std::cerr << className << "::DriftElectron:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 1;
  nHoles = 0;
  nIons = 0;
  
  if (!DriftLine(x0, y0, z0, t0, -1)) return false;
  
  return true;

}

bool
AvalancheMC::DriftHole(
    const double x0, const double y0, const double z0, const double t0) {
    
  if (sensor == 0) {
    std::cerr << className << "::DriftHole:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 0;
  nHoles = 1;
  nIons = 0;

  if (!DriftLine(x0, y0, z0, t0, 1)) return false;
  
  return true;

}

bool 
AvalancheMC::DriftIon(
    const double x0, const double y0, const double z0, const double t0) {
    
  if (sensor == 0) {
    std::cerr << className << "::DriftIon:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  } 

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 0;
  nHoles = 0;
  nIons = 1;

  if (!DriftLine(x0, y0, z0, t0, 2)) return false;  
  
  return true;

}  

bool 
AvalancheMC::DriftLine(const double x0, const double y0, const double z0, 
                       const double t0, const int q, const bool aval) {

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
  double phi,   cphi,   sphi;
  double theta, ctheta, stheta;
  // Collision time
  double tau = 0.;

  // Reset the drift line.
  drift.clear();
  // Add the starting point to the drift line.
  driftPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0;
  point.ne = 0; point.nh = 0; point.ni = 0;
  drift.push_back(point);
  nDrift = 1;

  bool ok = true;
  bool trapped = false;
  bool validAlphaEta = false;
  int abortReason = 0;

  if (hasTimeWindow && (t0 < tMin || t0 > tMax)) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    Starting time " << t0 << " is outside the specified\n";
    std::cerr << "    time window (" << tMin << ", " << tMax << ").\n";
    ok = false;
    abortReason = StatusOutsideTimeWindow;
  }

  // Get the electric field at the starting point.
  sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  // Make sure the starting point is inside a drift medium.
  if (status != 0) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    No drift medium at initial position ("
              << x << ", " << y << ", " << z << ").\n";
    ok = false;
    abortReason = StatusLeftDriftMedium;
  }

  double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    Electric field at initial position is too small:\n";
    std::cerr << "      ex = " << ex << " V/cm\n";
    std::cerr << "      ey = " << ey << " V/cm\n";
    std::cerr << "      ez = " << ez << " V/cm\n";
    ok = false;
    abortReason = StatusCalculationAbandoned;
  } 

  if (useBfield) {
    sensor->MagneticField(x, y, z, bx, by, bz, status);
    bx *= Tesla2Internal; by *= Tesla2Internal; bz *= Tesla2Internal;
  }

  while (ok) {
  
    // Compute the drift velocity and the diffusion coefficients.
    if (q < 0) {
      if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) || 
          !medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Error calculating electron"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n"; 
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else if (q == 1) {
      if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) || 
          !medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Error calculating hole"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n"; 
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else if (q == 2) {
      if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz) ||
          !medium->IonDiffusion(ex, ey, ez, bx, by, bz, dl, dt)) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Error calculating ion"
                  << " velocity or diffusion\n";
        std::cerr << "    at (" << x << ", " << y << ", " << z << ")\n"; 
        ok = false;
        abortReason = StatusCalculationAbandoned;
        break;
      }
    } else {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Unknown drift line type (" << q << ").\n";
      std::cerr << "    Program bug!\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      return false;
    }    
    v = sqrt(vx * vx + vy * vy + vz * vz);
    if (v < Small) {
      std::cerr << className << "::DriftLine:\n"; 
      std::cerr << "    Drift velocity at (" 
                << x << ", " << y << ", " << z << ") is too small:\n";
      std::cerr << "      vx = " << vx << " cm/ns\n";
      std::cerr << "      vy = " << vy << " cm/ns\n";
      std::cerr << "      vz = " << vz << " cm/ns\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      break;
    }

    // Determine the time step.
    switch (stepModel) {
      case 0:
        // Fixed time steps
        delta = tMc;
        break;
      case 1:
        // Fixed distance steps
        delta = dMc / v;
        break;
      case 2:
        // Steps based on collision time
        tau = c1 * v / e;
        delta = - nMc * tau * log(RndmUniformPos());
        break;
      default:
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Unknown stepping model.\n";
        return false;
    }
 
    // Draw a random diffusion direction in the particle frame.
    if (useDiffusion) {
      d = sqrt(v * delta);
      dx = d * RndmGaussian(0., dl);
      dy = d * RndmGaussian(0., dt);
      dz = d * RndmGaussian(0., dt);
    }

    // Compute the rotation angles to align the diffusion 
    // and drift velocity vectors
    vt = sqrt(vx * vx + vy * vy);
    if (vt < Small) {
      phi = 0.; theta = HalfPi;
      if (vz < 0.) theta = - theta;
    } else {
      phi = atan2(vy, vx);
      theta = atan2(vz, vt);
    }    
    cphi = cos(phi);     sphi = sin(phi);
    ctheta = cos(theta); stheta = sin(theta);
    
    // Compute the proposed end-point of this step.  
    x += delta * vx + cphi * ctheta * dx - sphi * dy - cphi * stheta * dz;
    y += delta * vy + sphi * ctheta * dx + cphi * dy - sphi * stheta * dz;
    z += delta * vz + stheta * dx + ctheta * dz;

    // Compute the electric field at the new point.
    sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
   
    // Check if the new position is inside a drift medium.
    if (status != 0) {
      // Try to terminate the drift line
      // close to the boundary.
      dx = x - point.x; dy = y - point.y; dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d; dy /= d; dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d; y = point.y + dy * d; z = point.z + dz * d;
        // Check if the mid-point is inside the drift medium.
        sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
        if (status == 0) {
          point.x = x; point.y = y; point.z = z; point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift medium.
      point.x += dx * d; point.y += dy * d; point.z += dz * d;
      drift.push_back(point);
      ++nDrift;
      abortReason = StatusLeftDriftMedium;
      if (debug) {
        std::cout << className << "::DriftLine:\n";
        std::cout << "    Particle left the drift medium.\n";
        std::cout << "    At " << point.x << ", " 
                               << point.y << ", " << point.z << "\n";
      }
      break;   
    }
    
    // Check if the new position is inside the drift area.
    if (!sensor->IsInArea(x, y, z)) {
      // Try to terminate the drift line
      // close to the boundary.
      dx = x - point.x; dy = y - point.y; dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d; dy /= d; dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d; y = point.y + dy * d; z = point.z + dz * d;
        // Check if the mid-point is inside the drift area.
        if (sensor->IsInArea(x, y, z)) {
          point.x = x; point.y = y; point.z = z; point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift area.
      point.x += dx * d; point.y += dy * d; point.z += dz * d;      
      drift.push_back(point);
      ++nDrift;
      abortReason = StatusLeftDriftArea;
      if (debug) {
        std::cout << className << "::DriftLine:\n";
        std::cout << "    Particle left the drift area.\n";
        std::cout << "    At " << point.x << ", " 
                               << point.y << ", " << point.z << "\n";
      }
      break;
    }
    
    // Check if the particle has crossed a wire.
    double xCross = point.x, yCross = point.y, zCross = point.z;
    if (sensor->IsWireCrossed(point.x, point.y, point.z, 
                              x, y, z,
                              xCross, yCross, zCross)) {
      delta *= sqrt(pow(xCross - point.x, 2) + 
                    pow(yCross - point.y, 2) +
                    pow(zCross - point.z, 2)) /
               sqrt(pow(x - point.x, 2) +
                    pow(y - point.y, 2) +
                    pow(z - point.z, 2));
      point.x = xCross; point.y = yCross; point.z = zCross;
      point.t += delta;      
      drift.push_back(point);
      ++nDrift;
      abortReason = StatusLeftDriftMedium;
      if (debug) {
        std::cout << className << "::DriftLine:\n";
        std::cout << "    Particle hit a wire.\n";
        std::cout << "    At " << xCross << ", " 
                               << yCross << ", " << zCross << "\n";
      }
      break;
    }

    e = sqrt(ex * ex + ey * ey + ez * ez);
    if (e < Small) {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Electric field at (" 
                << x << ", " << y << ", " << z << ") is too small:\n";
      std::cerr << "      ex = " << ex << " V/cm\n";
      std::cerr << "      ey = " << ey << " V/cm\n";
      std::cerr << "      ez = " << ez << " V/cm\n";
      ok = false;
      abortReason = StatusCalculationAbandoned;
      break;
    }
    // Add the new point to drift line.
    point.x = x; point.y = y; point.z = z; point.t += delta;
    drift.push_back(point);
    ++nDrift;
 
    // Check if the time is still within the specified interval.
    if (hasTimeWindow && point.t > tMax) {
      abortReason = StatusOutsideTimeWindow;
      break;
    }

    if (useBfield) {
      sensor->MagneticField(x, y, z, bx, by, bz, status);
      bx *= Tesla2Internal; by *= Tesla2Internal; bz *= Tesla2Internal;
    } 

  }
  
  // Compute Townsend and attachment coefficients for each drift step.
  int nElectronsOld = nElectrons;
  int nHolesOld = nHoles;
  int nIonsOld = nIons;
  if ((q == -1 || q == 1) && (aval || useAttachment)) {
  
    // Compute Townsend and attachment coefficient
    validAlphaEta = ComputeAlphaEta(q);
    if (ok) ok = validAlphaEta;
    
    // Subdivision of a step
    const double probth = 0.01;

    // Set initial number of electrons/ions.
    int ne = 1, ni = 0;
    // Loop over the drift line.
    for (int i = 0; i < nDrift - 1; ++i) {
      drift[i].ne = 0; drift[i].nh = 0; drift[i].ni = 0;
      // Only attempt avalanche calculation if alpha and eta are valid.
      if (validAlphaEta) {
        // Compute the number of subdivisions.
        int nDiv = int((drift[i].alpha + drift[i].eta) / probth);
        if (nDiv < 1) nDiv = 1;
        // Probabilities for gain and loss.
        const double alpha = std::max(drift[i].alpha / nDiv, 0.);
        const double eta   = std::max(drift[i].eta   / nDiv, 0.);
        // Set initial number of electrons/ions.
        int neInit = ne, niInit = ni;
        // Loop over the subdivisions.
        for (int j = 0; j < nDiv; ++j) {
          if (ne > 1000) {
            // Gaussian approximation.
            const int gain = int(ne * alpha + RndmGaussian() * 
                                 sqrt(ne * alpha * (1. - alpha)));
            const int loss = int(ne * eta   + RndmGaussian() * 
                                 sqrt(ne * eta   * (1. - eta)));
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
            if (q == -1) {
              --nElectrons;
            } else if (q == 1) {
              --nHoles;
            } else {
              --nIons;
            }
            nDrift = i + 2;
            drift[nDrift - 1].x = 0.5 * (drift[i].x + drift[i + 1].x);
            drift[nDrift - 1].y = 0.5 * (drift[i].y + drift[i + 1].y);
            drift[nDrift - 1].z = 0.5 * (drift[i].z + drift[i + 1].z);
            break;
          }
        }
        // If at least one new electron has been created,
        // add the new electrons to the table.
        if (ne - neInit >= 1) {
          if (q == -1) {
            drift[i].ne = ne - neInit;
            nElectrons += ne - neInit;
          } else if (q == 1) {
            drift[i].nh = ne - neInit;
            nHoles += ne - neInit;
          } else {
            drift[i].ni = ne - neInit;
            nIons += ne - neInit;
          }
        }
        if (ni - niInit >= 1) {
          if (q == -1) {
            if (useIons) {
              drift[i].ni = ni - niInit;
              nIons += ni - niInit;
            } else {
              drift[i].nh = ni - niInit;
              nHoles += ni - niInit;
            }
          } else {
            drift[i].ne = ni - niInit;
            nElectrons += ni - niInit;
          }
        }
        // If trapped, exit the loop over the drift line.
        if (trapped) {
          abortReason = StatusAttached;
          if (debug) {
            std::cout << className << "::DriftLine:\n";
            std::cout << "    Particle attached.\n";
            std::cout << "    At " << drift[nDrift - 1].x << ", " 
                                   << drift[nDrift - 1].y << ", " 
                                   << drift[nDrift - 1].z << "\n";
          }
          break;
        }
      }
    }
  }

  // Create an "endpoint"
  endpoint endPoint;
  endPoint.x0 = x0; endPoint.y0 = y0; endPoint.z0 = z0; endPoint.t0 = t0;
  endPoint.status = abortReason;

  endPoint.x1 = drift[nDrift - 1].x;
  endPoint.y1 = drift[nDrift - 1].y;
  endPoint.z1 = drift[nDrift - 1].z;
  endPoint.t1 = drift[nDrift - 1].t;
 
  if (debug) {
    const int nNewElectrons = nElectrons - nElectronsOld;
    const int nNewHoles = nHoles - nHolesOld;
    const int nNewIons = nIons - nIonsOld;
    std::cout << className << "::DriftLine:\n";
    std::cout << "    Produced\n" 
              << "      " << nNewElectrons << " electrons,\n"
              << "      " << nNewHoles << " holes, and\n"
              << "      " << nNewIons << " ions\n"
              << "    along the drift line from \n"
              << "      (" << endPoint.x0 << ", " 
                           << endPoint.y0 << ", " 
                           << endPoint.z0 << ") to \n"
              << "      (" << endPoint.x1 << ", " 
                           << endPoint.y1 << ", "
                           << endPoint.z1 << ").\n";
  }

  if (q == -1) {
    endpointsElectrons.push_back(endPoint);
    ++nEndpointsElectrons;
  } else if (q == 1) {
    endpointsHoles.push_back(endPoint);
    ++nEndpointsHoles;
  } else if (q == 2) {
    endpointsIons.push_back(endPoint);
    ++nEndpointsIons;
  }

  // Compute the induced signals if requested.
  if (useSignal) ComputeSignal(q);
  if (useInducedCharge) ComputeInducedCharge(q);

  // Plot the drift line if requested.
  if (usePlotting && nDrift > 0) {
    int jL;
    if (q < 0) {
      viewer->NewElectronDriftLine(nDrift, jL, 
                                   drift[0].x, drift[0].y, drift[0].z);
    } else if (q == 1) {
      viewer->NewHoleDriftLine(nDrift, jL,
                               drift[0].x, drift[0].y, drift[0].z);
    } else {
      viewer->NewIonDriftLine(nDrift, jL,
                              drift[0].x, drift[0].y, drift[0].z);
    }
    for (int iP = 0; iP < nDrift; ++iP) {
      viewer->SetDriftLinePoint(jL, iP, 
                                drift[iP].x, drift[iP].y, drift[iP].z);
    }
  }

  if (!ok) return false;

  return true;

}

bool 
AvalancheMC::AvalancheElectron(
                        const double x0, const double y0, const double z0,
                        const double t0, const bool holes) {
                               
  // Initialise the avalanche table
  aval.clear();
  avalPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0; 
  point.ne = 1; point.nh = 0; point.ni = 0;
  aval.push_back(point);

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 1;
  nHoles = 0;
  nIons = 0;
  
  withHoles = holes;
  return Avalanche();
  
}

bool 
AvalancheMC::AvalancheHole(const double x0, const double y0, const double z0,
                           const double t0, const bool electrons) {
                               
  // Initialise the avalanche table
  aval.clear();
  avalPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0; 
  point.ne = 0; point.nh = 1; point.ni = 0;
  aval.push_back(point);

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 0;
  nHoles = 1;
  nIons = 0;

  withElectrons = electrons;
  return Avalanche();
 
}

bool 
AvalancheMC::AvalancheElectronHole(
          const double x0, const double y0, const double z0, const double t0) {
                               
  // Initialise the avalanche table
  aval.clear();
  avalPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0; 
  point.ne = 1; point.nh = 1; point.ni = 0;
  aval.push_back(point);  

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 1;
  nHoles = 1;
  nIons = 0;

  withElectrons = withHoles = true;
  return Avalanche();
  
}

bool 
AvalancheMC::Avalanche() {

  // Make sure that the sensor is defined
  if (sensor == 0) {
    std::cerr << className << "::Avalanche:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }
  
  avalPoint point;

  if (!withHoles && !withElectrons) {
    std::cerr << className << "::Avalanche:\n";
    std::cerr << "    Neither electron nor hole/ion component"
              << " are activated.\n"; 
  }

  int nAval = aval.size();
  while (nAval > 0) {
    for (int iAval = nAval; iAval--;) {
      if (withElectrons) {
        // Loop over the electrons at this location.
        for (int iE = aval[iAval].ne; iE--;) {
          // Compute an electron drift line.
          if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z, 
                         aval[iAval].t, -1, true)) {
            continue;
          }
          // Loop over the drift line.
          for (int iDrift = 0; iDrift < nDrift - 2; ++iDrift) {
            if (drift[iDrift].ne > 0 || 
                drift[iDrift].nh > 0 || 
                drift[iDrift].ni > 0) {
              // Add the point to the table.
              point.x = drift[iDrift + 1].x;
              point.y = drift[iDrift + 1].y;
              point.z = drift[iDrift + 1].z;
              point.t = drift[iDrift + 1].t;
              point.ne = drift[iDrift].ne;
              point.nh = drift[iDrift].nh;
              point.ni = drift[iDrift].ni;
              aval.push_back(point);
            }
          }
        }
      }

      if (withHoles) { 
        // Loop over the ions at this location.
        for (int iI = 0; iI < aval[iAval].ni; ++iI) {
          // Compute an ion drift line.
          DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z,
                    aval[iAval].t, 2, false);
          continue;
        }
        
        // Loop over the holes at this location.
        for (int iH = 0; iH < aval[iAval].nh; ++iH) {
          // Compute a hole drift line.
          if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z,
                         aval[iAval].t, +1, true)) continue;
          // Loop over the drift line.
          for (int iDrift = 0; iDrift < nDrift - 1; ++iDrift) {
            if (drift[iDrift].ne > 0 || 
                drift[iDrift].nh > 0 ||
                drift[iDrift].ni > 0) {
              // Add the point to the table.
              point.x = drift[iDrift + 1].x;
              point.y = drift[iDrift + 1].y;
              point.z = drift[iDrift + 1].z;
              point.t = drift[iDrift + 1].t;
              point.ne = drift[iDrift].ne;
              point.nh = drift[iDrift].nh;
              point.ni = drift[iDrift].ni;
              aval.push_back(point);
            }
          }
        }
      }
      // Remove the avalanche point.
      aval.erase(aval.begin() + iAval);
    }
    nAval = aval.size();
  }
  return true;

}

bool 
AvalancheMC::ComputeAlphaEta(const int q) {
 
  // Locations and weights for 6-point Gaussian integration
  const double tg[6] = {
    -0.932469514203152028, -0.661209386466264514, -0.238619186083196909,
     0.238619186083196909,  0.661209386466264514,  0.932469514203152028};
  const double wg[6] = {
    0.171324492379170345, 0.360761573048138608, 0.467913934572691047, 
    0.467913934572691047, 0.360761573048138608, 0.171324492379170345};

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
  double vdx = 0., vdy = 0., vdz = 0., vd = 0.;
  // Length of the step
  double delx = 0., dely = 0., delz = 0., del = 0.;

  // Scaling factor for projected length
  double scale = 1.;
    
  // Loop a first time over the drift line.
  for (int i = nDrift - 1; i--;) {
    // Compute the step length.
    delx = drift[i + 1].x - drift[i].x;
    dely = drift[i + 1].y - drift[i].y;
    delz = drift[i + 1].z - drift[i].z;
    del = sqrt(delx * delx + dely * dely + delz * delz);
    // Compute the integrated drift velocity, 
    // Townsend coefficient and attachment coefficient.
    vdx = 0., vdy = 0., vdz = 0., vd = 0.;
    drift[i].alpha = 0.;
    drift[i].eta = 0.;        
    for (int j = 6; j--;) {
      x = drift[i].x + 0.5 * (1. + tg[j]) * delx;
      y = drift[i].y + 0.5 * (1. + tg[j]) * dely;
      z = drift[i].z + 0.5 * (1. + tg[j]) * delz;
      sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Make sure that we are in a drift medium.
      if (status != 0) {
        // Check if this point is the last but one.
        if (i < nDrift - 2) {
          std::cerr << className << "::ComputeAlphaEta:\n";
          std::cerr << "    Got status value " << status 
                    << " at segment " << j + 1
                    << "/6, drift point " << i + 1 << "/" << nDrift 
                    << ".\n";
          return false;
        }
        continue;
      }
      if (useBfield) {
        sensor->MagneticField(x, y, z, bx, by, bz, status);
        bx *= Tesla2Internal; by *= Tesla2Internal; bz *= Tesla2Internal;
      }
      if (q < 0) {
        medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
        medium->ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
        medium->ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
      } else {
        medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
        medium->HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
        medium->HoleAttachment(ex, ey, ez, bx, by, bz, eta);
      }
      vdx += wg[j] * vx; vdy += wg[j] * vy; vdz += wg[j] * vz;
      drift[i].alpha += wg[j] * alpha;
      drift[i].eta += wg[j] * eta;
    }
    // Compute the scaling factor for the projected length.
    scale = 1.;
    if (useEquilibration) {
      vd = sqrt(vdx * vdx + vdy * vdy + vdz * vdz);
      if (vd * del <= 0.) {
        scale = 0.;
      } else {
        scale = (delx * vdx + dely * vdy + delz * vdz) / (vd * del);
      }
    }
    drift[i].alpha *= del * scale / 2.;
    drift[i].eta *= del * scale / 2.;
  }
  
  // Skip equilibration if projection has not been requested.
  if (!useEquilibration) return true;
  
  double sub1 = 0., sub2 = 0.;
  bool try1 = false, try2 = false, done = false;
  // Try to alpha-equilibrate the returning parts. 
  for (int i = 0; i < nDrift - 1; ++i) {
    if (drift[i].alpha < 0.) {
      // Targets for subtracting
      sub1 = sub2 = - drift[i].alpha / 2.;
      try1 = try2 = false;
      // Try to subtract half in earlier points.
      for (int j = 0; j < i - 1; ++j) {
        if (drift[i - j].alpha > sub1) {
          drift[i - j].alpha -= sub1; 
          drift[i].alpha += sub1;
          sub1 = 0.; 
          try1 = true; 
          break;
        } else if (drift[i- j].alpha > 0.) {
          drift[i].alpha += drift[i - j].alpha; 
          sub1 -= drift[i - j].alpha;
          drift[i - j].alpha = 0.;
        }
      }
      // Try to subtract the other half in later points.
      for (int j = 0; j < nDrift - i - 1; ++j) {
        if (drift[i + j].alpha > sub2) {
          drift[i + j].alpha -= sub2;
          drift[i].alpha += sub2;
          sub2 = 0.;
          try2 = true;
          break;
        } else if (drift[i + j].alpha > 0.) {
          drift[i].alpha += drift[i + j].alpha;
          sub2 -= drift[i + j].alpha;
          drift[i + j].alpha = 0.;
        }
      }
      
      // Done if both sides have margin left.
      done = false;
      if (try1 && try2) {
        done = true;
      } else if (try1) {
        sub1 = - drift[i].alpha;
        for (int j = 0; j < i - 1; ++j) {
          if (drift[i - j].alpha > sub1) {
            drift[i - j].alpha -= sub1;
            drift[i].alpha += sub1;
            sub1 = 0.;
            done = true;
            break;
          } else if (drift[i - j].alpha > 0.) {
            drift[i].alpha += drift[i - j].alpha;
            sub1 -= drift[i - j].alpha;
            drift[i - j].alpha = 0.;
          }
        }
      } else if (try2) {
        // Try upper side again.
        sub2 = -drift[i].alpha;
        for (int j = 0; j < nDrift - i - 1; ++j) {
          if (drift[i + j].alpha > sub2) {
            drift[i + j].alpha -= sub2;
            drift[i].alpha += sub2;
            sub2 = 0.;
            done = true;
            break;
          } else if (drift[i + j].alpha > 0.) {
            drift[i].alpha += drift[i + j].alpha;
            sub2 -= drift[i + j].alpha;
            drift[i + j].alpha = 0.;
          }
        }
      }
      // See whether we succeeded.
      if (!done) {
        if (debug) {
          std::cerr << className << "::ComputeAlphaEta:\n";
          std::cerr << "    Unable to even out backwards alpha steps.\n"; 
          std::cerr << "    Calculation is probably inaccurate.\n";
        } 
        return false;
      }
    }
  }
  
  // Try to eta-equilibrate the returning parts.
  for (int i = 0; i < nDrift - 1; ++i) {
    if (drift[i].eta < 0.) {
      // Targets for subtracting
      sub1 = -drift[i].eta / 2.;
      sub2 = -drift[i].eta / 2.;
      try1 = false;
      try2 = false;
      // Try to subtract half in earlier points.
      for (int j = 0; j < i - 1; ++j) {
        if (drift[i - j].eta > sub1) {
          drift[i - j].eta -= sub1;
          drift[i].eta += sub1;
          sub1 = 0.;
          try1 = true;
          break;
        } else if (drift[i - j].eta > 0.) {
          drift[i].eta += drift[i - j].eta;
          sub1 -= drift[i - j].eta;
          drift[i - j].eta = 0.;
        }
      }
      // Try to subtract the other half in later points.
      for (int j = 0; j < nDrift - i - 1; ++j) {
        if (drift[i + j].eta > sub2) {
          drift[i + j].eta -= sub2;
          drift[i].eta += sub2;
          sub2 = 0.;
          try2 = true;
          break;
        } else if (drift[i + j].eta > 0.) {
          drift[i].eta += drift[i + j].eta;
          sub2 -= drift[i + j].eta;
          drift[i + j].eta = 0.;
        }
      }
      done = false;
      if (try1 && try2) {
        done = true;
      } else if (try1) {
        // Try lower side again.
        sub1 = -drift[i].eta;
        for (int j = 0; j < i - 1; ++j) {
          if (drift[i - j].eta > sub1) {
            drift[i - j].eta -= sub1;
            drift[i].eta += sub1;
            sub1 = 0.;
            done = true;
            break;
          } else if (drift[i - j].eta > 0.) {
            drift[i].eta += drift[i - j].eta;
            sub1 -= drift[i - j].eta;
            drift[i - j].eta = 0.;
          }
        }
      } else if (try2) {
        // Try upper side again.
        sub2 = -drift[i].eta;
        for (int j = 0; j < nDrift - i - 1; ++j) {
          if (drift[i + j].eta > sub2) {
            drift[i + j].eta -= sub2;
            drift[i].eta += sub2;
            sub2 = 0.;
            done = true;
            break;
          } else if (drift[i + j].eta > 0.) {
            drift[i].eta += drift[i + j].eta;
            sub2 -= drift[i + j].eta;
            drift[i + j].eta = 0.;
          }
        }
      }
      if (!done) {
        if (debug) {
          std::cerr << className << "::ComputeAlphaEta:\n";
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

void 
AvalancheMC::ComputeSignal(const int q) {

  if (nDrift < 2) return;  
  double dt, dx, dy, dz;
  for (int i = 0; i < nDrift - 1; ++i) {
    dt = drift[i + 1].t - drift[i].t;
    dx = drift[i + 1].x - drift[i].x;
    dy = drift[i + 1].y - drift[i].y;
    dz = drift[i + 1].z - drift[i].z;
    sensor->AddSignal(q, drift[i].t, dt, 
                      drift[i].x + 0.5 * dx, 
                      drift[i].y + 0.5 * dy,
                      drift[i].z + 0.5 * dz,
                      dx / dt, dy / dt, dz / dt);
  }

}

void
AvalancheMC::ComputeInducedCharge(const int q) {

  if (nDrift < 2) return;
  sensor->AddInducedCharge(q, 
                drift[0].x,          drift[0].y,          drift[0].z, 
                drift[nDrift - 1].x, drift[nDrift - 1].y, drift[nDrift - 1].z);

}

}
