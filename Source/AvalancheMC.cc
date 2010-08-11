#include <iostream>
#include <fstream>
#include <cmath>
#include <string>

#include "AvalancheMC.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

double AvalancheMC::c1 = ElectronMass / (SpeedOfLight * SpeedOfLight);

AvalancheMC::AvalancheMC() :
  sensor(0),
  nDrift(0), nAval(0),
  stepModel(2), tMc(0.02), dMc(0.001), nMc(100),
  nElectrons(0), nIons(0), 
  nEndpointsElectrons(0), nEndpointsHoles(0), nEndpointsIons(0),
  usePlotting(false), viewer(0), 
  useSignal(false), useInducedCharge(false), useEquilibration(true), 
  useDiffusion(true), useAttachment(true), useIons(true), 
  withElectrons(true), withHoles(true),
  debug(false) {
  
  className = "AvalancheMC";
   
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
  nIons = 1;

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

  // Reset the drift line
  drift.clear();
  // Add the starting point to the drift line
  driftPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0;
  point.ne = 0; point.ni = 0;
  drift.push_back(point);
  nDrift = 1;

  bool ok = true;
  bool trapped = false;

  // Get the electric field at the starting point
  sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  // Make sure the starting point is inside a drift medium
  if (status != 0) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    No drift medium at initial position.\n";
    ok = false;
  }

  double e = Max(sqrt(ex * ex + ey * ey + ez * ez), Small);

  while (ok) {
  
    // Compute the drift velocity and the diffusion coefficients
    if (q < 0) {
      if (!medium->ElectronVelocity(ex, ey, ez, 0., 0., 0., vx, vy, vz) || 
          !medium->ElectronDiffusion(ex, ey, ez, 0., 0., 0., dl, dt)) {
        ok = false;
        break;
      }
    } else if (q == 1) {
      if (!medium->HoleVelocity(ex, ey, ez, 0., 0., 0., vx, vy, vz) || 
          !medium->HoleDiffusion(ex, ey, ez, 0., 0., 0., dl, dt)) {
        ok = false;
        break;
      }
    } else if (q == 2) {
      if (!medium->IonVelocity(ex, ey, ez, 0., 0., 0., vx, vy, vz) ||
          !medium->IonDiffusion(ex, ey, ez, 0., 0., 0., dl, dt)) {
        ok = false;
        break;
      }
    } else {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Unknown drift line type (" << q << ").\n";
      std::cerr << "    Program bug!\n"; 
      return false;
    }    
    v = Max(sqrt(vx * vx + vy * vy + vz * vz), Small);
    
    // Determine the time step
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
        
    // Draw a random diffusion direction in the particle frame
    if (useDiffusion) {
      d = sqrt(v * delta);
      dx = d * RndmGaussian(0., dl);
      dy = d * RndmGaussian(0., dt);
      dz = d * RndmGaussian(0., dt);
    }

    // Compute the rotation to align the diffusion and drift velocity vectors
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
    
    // Compute the proposed end-point of this step    
    x += delta * vx + cphi * ctheta * dx - sphi * dy - cphi * stheta * dz;
    y += delta * vy + sphi * ctheta * dx + cphi * dy - sphi * stheta * dz;
    z += delta * vz + stheta * dx + ctheta * dz;

    // Compute the electric field at the new point
    sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
   
    // Check if the new position is inside a drift medium 
    if (status != 0) {
      // Try to terminate the drift line
      // close to the boundary 
      dx = x - point.x; dy = y - point.y; dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d; dy /= d; dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d; y = point.y + dy * d; z = point.z + dz * d;
        // Check if the mid-point is inside the drift medium
        sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
        if (status == 0) {
          point.x = x; point.y = y; point.z = z; point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift medium
      point.x += dx * d; point.y += dy * d; point.z += dz * d;
      drift.push_back(point);
      ++nDrift;
      break;   
    }
    
    // Check if the new position is inside the drift area
    if (!sensor->IsInArea(x, y, z)) {
      // Try to terminate the drift line
      // close to the boundary 
      dx = x - point.x; dy = y - point.y; dz = z - point.z;
      d = sqrt(dx * dx + dy * dy + dz * dz);
      if (d > 0.) {
        dx /= d; dy /= d; dz /= d;
      }
      while (d > BoundaryDistance) {
        delta *= 0.5;
        d *= 0.5;
        x = point.x + dx * d; y = point.y + dy * d; z = point.z + dz * d;
        // Check if the mid-point is inside the drift area
        if (sensor->IsInArea(x, y, z)) {
          point.x = x; point.y = y; point.z = z; point.t += delta;
        }
      }
      // Place the particle OUTSIDE the drift area
      point.x += dx * d; point.y += dy * d; point.z += dz * d;      
      drift.push_back(point);
      ++nDrift;
      break;
    }

    e = Max(sqrt(ex * ex + ey * ey + ez * ez), Small);
    // Add the new point to drift line
    point.x = x; point.y = y; point.z = z; point.t += delta;
    drift.push_back(point);
    ++nDrift;

  }
  
  // Compute Townsend and attachment coefficients for each drift step
  if ((q == -1 || q == 1) && (aval || useAttachment)) {
    ComputeAlphaEta(q);

    // Gain and loss over a step
    int gain = 0;
    int loss = 0;
    int n = 1;

    double alpha = 0.;
    double eta = 0.;

    // Subdivision of a step
    const double probth = 0.01;
    int nDiv = 1;

    // Loop over the drift line
    for (int i = 0; i < nDrift - 1; ++i) {
      n = 1;
      gain = loss = 0;
      // Compute the number of subdivisions
      nDiv = int((drift[i].alpha + drift[i].eta) / probth);
      if (nDiv < 1) nDiv = 1;
      // Probabilities for gain and loss
      alpha = drift[i].alpha / nDiv;
      eta   = drift[i].eta   / nDiv;
      // Loop over the subdivisions
      for (int j = nDiv; j--;) {
        // Binomial approximation
        for (int k = n; k--;) {
          if (RndmUniform() < alpha) ++gain;
          if (RndmUniform() < eta)   ++loss;
        }
        n += gain - loss;
        // Check if the electron/hole has survived
        if (n <= 0) {
          trapped = true;
          if (q == -1) {
            --nElectrons;
          } else {
            --nIons;
          }
          nDrift = i + 1;
          break;
        }
      }
      // Abort the loop over the drift line if the e/h has been trapped
      if (trapped) {
        if (q == -1) {
          drift[i].ne = 0;
          drift[i].ni = gain;
        } else {
          drift[i].ne = gain;
          drift[i].ni = 0;
        }
        break;
      }
      if (q == -1) {
        drift[i].ne = gain - loss;
        drift[i].ni = gain;
        nElectrons += gain - loss;
        nIons += gain;
      } else {
        drift[i].ne = gain;
        drift[i].ni = gain - loss;
        nElectrons += gain;
        nIons += gain - loss;
      }
    }
  }

  // Create an "endpoint"
  endpoint endPoint;
  endPoint.x0 = x0; endPoint.y0 = y0; endPoint.z0 = z0; endPoint.t0 = t0;
  if (!ok) {
    endPoint.status = -3;
  } else if (trapped) {
    endPoint.status = -7;
  } else {
    endPoint.status = -1;
  }

  endPoint.x1 = drift[nDrift - 1].x;
  endPoint.y1 = drift[nDrift - 1].y;
  endPoint.z1 = drift[nDrift - 1].z;

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

  // Compute the induced signals if requested
  if (useSignal) ComputeSignal(q);
  if (useInducedCharge) ComputeInducedCharge(q);

  // Plot the drift line if requested
  if (usePlotting) {
    if (q < 0) {
      viewer->NewElectronDriftLine(nDrift);
    } else {
      viewer->NewIonDriftLine(nDrift);
    }
    for (int i = 0; i < nDrift; ++i) {
      viewer->SetPoint(i, drift[i].x, drift[i].y, drift[i].z);
    }
  }

  if (!ok) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    Error calculating the transport parameters.\n";
    return false;
  }

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
  point.ne = 1; point.ni = 0;
  aval.push_back(point);
  nAval = 1;

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 1;
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
  point.ne = 0; point.ni = 1;
  aval.push_back(point);
  nAval = 1;

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 0;
  nIons = 1;

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
  point.ne = 1; point.ni = 1;
  aval.push_back(point);  
  nAval = 1;

  endpointsElectrons.clear();
  endpointsHoles.clear();
  endpointsIons.clear();
  nEndpointsElectrons = 0;
  nEndpointsHoles = 0;
  nEndpointsIons = 0;

  nElectrons = 1;
  nIons = 1;

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

  for (int iAval = 0; iAval < nAval; ++iAval) {
  
    if (withElectrons) {
      // Loop over the electrons at this location
      for (int iE = aval[iAval].ne; iE--;) {
        if (debug) { 
          std::cout << "      Electron drift line " << iE << "\n";
        }
        // Compute an electron drift line
        if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z, 
                  aval[iAval].t, -1, true)) continue;
        // Loop over the drift line
        for (int iDrift = 0; iDrift < nDrift; ++iDrift) {
          if (drift[iDrift].ne > 0 || drift[iDrift].ni > 0) {
            // Add the point to the table
            if (iDrift < nDrift - 1) {
              point.x = 0.5 * (drift[iDrift].x + drift[iDrift + 1].x);
              point.y = 0.5 * (drift[iDrift].y + drift[iDrift + 1].y);
              point.z = 0.5 * (drift[iDrift].z + drift[iDrift + 1].z);
              point.t = 0.5 * (drift[iDrift].t + drift[iDrift + 1].t);
            } else {
              point.x = drift[iDrift].x;
              point.y = drift[iDrift].y;
              point.z = drift[iDrift].z;
              point.t = drift[iDrift].t;
            }
            point.ne = drift[iDrift].ne;
            point.ni = drift[iDrift].ni;
            aval.push_back(point);
            ++nAval;
          }
        }
      }
    }

    if (withHoles) { 
      // Loop over the holes at this location
      for (int iH = 0; iH < aval[iAval].ni; ++iH) {
        // Compute a hole drift line
        if (useIons) {
          DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z,
                    aval[iAval].t, 2, false);
          continue;
        }
        if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z,
                  aval[iAval].t, +1, true)) continue;
        // Loop over the drift line
        for (int iDrift = 0; iDrift < nDrift; ++iDrift) {
          if (drift[iDrift].ne > 0 || drift[iDrift].ni > 0) {
            // Add the point to the table
            if (iDrift < nDrift - 1) {
              point.x = 0.5 * (drift[iDrift].x + drift[iDrift + 1].x);
              point.y = 0.5 * (drift[iDrift].y + drift[iDrift + 1].y);
              point.z = 0.5 * (drift[iDrift].z + drift[iDrift + 1].z);
              point.t = 0.5 * (drift[iDrift].t + drift[iDrift + 1].t);
            } else {
              point.x = drift[iDrift].x;
              point.y = drift[iDrift].y;
              point.z = drift[iDrift].z;
              point.t = drift[iDrift].t;
            }
            point.ne = drift[iDrift].ne;
            point.ni = drift[iDrift].ni;
            aval.push_back(point);
            ++nAval;
          }
        }
      }
    }
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
    
  // Loop a first time over the drift line
  for (int i = nDrift - 1; i--;) {
    // Compute the step length
    delx = drift[i + 1].x - drift[i].x;
    dely = drift[i + 1].y - drift[i].y;
    delz = drift[i + 1].z - drift[i].z;
    del = sqrt(delx * delx + dely * dely + delz * delz);
    // Compute the integrated drift velocity, 
    // Townsend coefficient and attachment coefficient
    vdx = 0., vdy = 0., vdz = 0., vd = 0.;
    drift[i].alpha = 0.;
    drift[i].eta = 0.;        
    for (int j = 6; j--;) {
      x = drift[i].x + 0.5 * (1. + tg[j]) * delx;
      y = drift[i].y + 0.5 * (1. + tg[j]) * dely;
      z = drift[i].x + 0.5 * (1. + tg[j]) * delz;
      sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Make sure that we are in a drift medium
      if (status != 0) {
        // Check if this point is the last but one
        if (i < nDrift - 2) {
          std::cerr << className << "::ComputeAlphaEta:\n";
          std::cerr << "    Got status value != 0 at segment " << j + 1
                    << "/6, drift point " << i + 1 << "/" << nDrift 
                    << ".\n";
          return false;
        }
        continue;
      }
      if (q < 0) {
        medium->ElectronVelocity(ex, ey, ez, 0., 0., 0., vx, vy, vz);
        medium->ElectronTownsend(ex, ey, ez, 0., 0., 0., alpha);
        medium->ElectronAttachment(ex, ey, ez, 0., 0., 0., eta);
      } else {
        medium->HoleVelocity(ex, ey, ez, 0., 0., 0., vx, vy, vz);
        medium->HoleTownsend(ex, ey, ez, 0., 0., 0., alpha);
        medium->HoleAttachment(ex, ey, ez, 0., 0., 0., eta);
      }
      vdx += wg[j] * vx; vdy += wg[j] * vy; vdz += wg[j] * vz;
      drift[i].alpha += wg[j] * alpha;
      drift[i].eta += wg[j] * eta;
    }
    // Compute the scaling factor for the projected length
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
  
  // Skip equilibration if projection has not been requested
  if (!useEquilibration) return true;
  
  double sub1 = 0., sub2 = 0.;
  bool try1 = false, try2 = false, done = false;
  // Try to alpha-equilibrate the returning parts  
  for (int i = 0; i < nDrift - 1; ++i) {
    if (drift[i].alpha < 0.) {
      // Targets for subtracting
      sub1 = sub2 = - drift[i].alpha / 2.;
      try1 = try2 = false;
      // Try to subtract half in earlier points
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
      // Try to subtract the other half in later points
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
        // Try upper side again
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
      // See whether we succeeded
      if (!done) {
        std::cerr << className << "::ComputeAlphaEta:\n";
        std::cerr << "    Unable to even out backwards alpha steps.\n"; 
        std::cerr << "    Avalanche calculation is probably inaccurate.\n"; 
        return false;
      }
    }
  }
  
  // Try to eta-equilibrate the returning parts
  for (int i = 0; i < nDrift - 1; ++i) {
    if (drift[i].eta < 0.) {
      // Targets for subtracting
      sub1 = -drift[i].eta / 2.;
      sub2 = -drift[i].eta / 2.;
      try1 = false;
      try2 = false;
      // Try to subtract half in earlier points
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
      // Try to subtract the other half in later points
      for (int j = 0; j < nDrift - i - 1; ++j) {
        if (drift[i + j].eta > sub2) {
          drift[i + j].eta -= sub2;
          drift[i].eta += sub2;
          sub2 = 0.;
          try2 = true;
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
        // Try upper side again
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
        std::cerr << className << "::ComputeAlphaEta:\n";
        std::cerr << "    Unable to even out backwards eta steps.\n";
        std::cerr << "    Avalanche calculation is probably inaccurate.\n"; 
        return false;
      }
    }
  }
  
  // Seems to have worked
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
