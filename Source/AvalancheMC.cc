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
  useSignal(false), useEquilibration(true), 
  useDiffusion(true), useIons(true), 
  withElectrons(true), withHoles(true),
  debug(false) {
   
}

void 
AvalancheMC::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << "AvalancheMC::SetSensor:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }
  
  sensor = s;

}

void 
AvalancheMC::SetTimeSteps(const double d) {

  stepModel = 0;
  if (d < Small) {
    std::cerr << "AvalancheMC::SetTimeSteps:" << std::endl;
    std::cerr << "    Specified step size is too small." << std::endl;
    std::cerr << "    Using default (20 ps) instead." << std::endl;
    tMc = 0.02;
  } else {
    if (debug) {
      std::cout << "AvalancheMC::SetTimeSteps:" << std::endl;
      std::cout << "    Step size set to " << d << " ns." << std::endl;
    }
    tMc = d;
  }
  
}

void
AvalancheMC::SetDistanceSteps(const double d) {

  stepModel = 1;
  if (d < Small) {
    std::cerr << "AvalancheMC::SetDistanceSteps:" << std::endl;
    std::cerr << "    Specified step size is too small." << std::endl;
    std::cerr << "    Using default (10 um) instead." << std::endl;
    dMc = 0.001;
  } else {
    if (debug) {
      std::cout << "AvalancheMC::SetDistanceSteps:" << std::endl;
      std::cout << "    Step size set to " << d << " cm." << std::endl;
    }
    dMc = d;
  }
  
}
  
void
AvalancheMC::SetCollisionSteps(const int n) {

  stepModel = 2;
  if (n < 1) {
    std::cerr << "AvalancheMC::SetCollisionSteps:" << std::endl;
    std::cerr << "    Number of collisions to be skipped set to "
              << " default value (100)." << std::endl;
    nMc = 100;
  } else {
    if (debug) {
      std::cout << "AvalancheMC::SetCollisionSteps:" << std::endl;
      std::cout << "    Number of collisions to be skipped set to " 
                << n << "." << std::endl;
    }
    nMc = n;
  }
  
}

void 
AvalancheMC::GetDriftLinePoint(const int i, double& x, double& y, double& z, double& t) {

  if (i < 0 || i >= nDrift) {
    std::cerr << "AvalancheMC::GetDriftLinePoint:" << std::endl;
    std::cerr << "    Index is outside the range." << std::endl;
    return;
  }
  
  x = drift[i].x;
  y = drift[i].y;
  z = drift[i].z;
  t = drift[i].t;

}

bool 
AvalancheMC::DriftElectron(
    const double x0, const double y0, const double z0, const double t0) {
  
  if (sensor == 0) {
    std::cerr << "AvalancheMC::DriftElectron:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }
  
  if (!DriftLine(x0, y0, z0, t0, -1)) return false;
  
  if (useSignal) ComputeSignal(-1);

  return true;

}

bool
AvalancheMC::DriftHole(
    const double x0, const double y0, const double z0, const double t0) {
    
  if (sensor == 0) {
    std::cerr << "AvalancheMC::DriftHole:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }

  if (!DriftLine(x0, y0, z0, t0, 1)) return false;
  
  if (useSignal) ComputeSignal(1);
  
  return true;

}

bool 
AvalancheMC::DriftIon(
    const double x0, const double y0, const double z0, const double t0) {
    
  if (sensor == 0) {
    std::cerr << "AvalancheMC::DriftIon:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }    

  if (!DriftLine(x0, y0, z0, t0, 2)) return false;  
  
  if (useSignal) ComputeSignal(1);
  
  return true;

}  

bool 
AvalancheMC::DriftLine(const double x0, const double y0, const double z0, 
                       const double t0, const int q) {

  // Current position
  double x = x0, y = y0, z = z0, t = t0;
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

  // Get the electric field at the starting point
  sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  // Make sure the starting point is inside a drift medium
  if (status != 0) {
    std::cerr << "AvalancheMC::DriftLine:" << std::endl;
    std::cerr << "    No drift medium at initial position." << std::endl;
    return false;
  }

  double e = Max(sqrt(ex * ex + ey * ey + ez * ez), Small);

  // Reset the drift line
  drift.clear();
  // Add the starting point to the drift line
  driftPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0;
  drift.push_back(point);
  nDrift = 1;

  bool ok = true;
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
      std::cerr << "AvalancheMC::DriftLine:" << std::endl;
      std::cerr << "    Unknown drift line type (" << q << "). Program bug!" 
                << std::endl;
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
        std::cerr << "AvalancheMC::DriftLine:" << std::endl;
        std::cerr << "    Unknown stepping model." << std::endl;
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
    
    if (status != 0) {
      // If the new point is not inside a drift medium,
      // reduce the step size to bring it back into the medium    
      for (int i = 5; i--;) {
        delta *= 0.5;
        x = point.x + 
            delta * vx + cphi * ctheta * dx - sphi * dy - cphi * stheta * dz;
        y = point.y +
            delta * vy + sphi * ctheta * dx + cphi * dy - sphi * stheta * dz;
        z = point.z + delta * vz + stheta * dx + ctheta * dz;
        sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
        if (status == 0) break;
      }
    }
    // Attempt to bring the point back into a drift medium failed
    // Terminate the drift line
    if (status != 0) break;
    
    t += delta;
    e = Max(sqrt(ex * ex + ey * ey + ez * ez), Small);
    // Add the new point to drift line
    point.x = x; point.y = y; point.z = z; point.t = t;
    drift.push_back(point);
    ++nDrift;

  }
  
  if (!ok) {
    std::cerr << "AvalancheMC::DriftLine:" << std::endl;
    std::cerr << "    Error calculating the transport parameters." << std::endl;
    return false;
  }
  return true;

}

bool 
AvalancheMC::AvalancheElectron(const double x0, const double y0, const double z0,
                               const double t0, const bool holes) {
                               
  // Initialise the avalanche table
  aval.clear();
  avalPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0; 
  point.ne = 1; point.ni = 0;
  aval.push_back(point);
  nAval = 1;

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

  nElectrons = 0;
  nIons = 1;

  withElectrons = electrons;
  return Avalanche();
 
}

bool 
AvalancheMC::AvalancheElectronHole(const double x0, const double y0, const double z0,
                                   const double t0) {
                               
  // Initialise the avalanche table
  aval.clear();
  avalPoint point;
  point.x = x0; point.y = y0; point.z = z0; point.t = t0; 
  point.ne = 1; point.ni = 1;
  aval.push_back(point);  
  nAval = 1;

  nElectrons = 1;
  nIons = 1;

  withElectrons = withHoles = true;
  return Avalanche();
  
}

bool 
AvalancheMC::Avalanche() {

  // Make sure that the sensor is defined
  if (sensor == 0) {
    std::cerr << "AvalancheMC::Avalanche:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }
  
  avalPoint point;

  double alpha, eta;
  int gain, loss;
  int ne, ni;
  
  // Subdivisions of a drift line step
  int nDiv;
  const double probth = 0.01;

  // Loop counters
  int iAval = 0, iDrift = 0;
  int iE = 0, iH = 0;
  int iDiv = 0;
  
  if (!withHoles && !withElectrons) {
    std::cerr << "AvalancheMC::Avalanche:" << std::endl;
    std::cerr << "    Neither electron nor hole component are activated." << std::endl;
  }

  // Loop over the table
  while (iAval < nAval) {
  
    if (withElectrons) {
      // Loop over the electrons at this location
      for (iE = aval[iAval].ne; iE--;) {
        if (debug) {        
          std::cout << "      Electron drift line " << iE << std::endl;
        }
        // Compute an electron drift line
        if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z, 
                  aval[iAval].t, -1)) continue;
        // Compute alpha and eta for each drift step
        if (!ComputeAlphaEta(-1)) continue;
        // Loop over the drift line and follow the avalanche development
        for (iDrift = 0; iDrift < nDrift - 1; ++iDrift) {
          // Set initial number of electrons and ions
          ne = 1; ni = 0;
          // Compute the number of subdivisions
          nDiv = int((drift[iDrift].alpha + drift[iDrift].eta) / probth);
          if (nDiv < 1) nDiv = 1;
          // Probabilities for gain and loss
          alpha = drift[iDrift].alpha / nDiv;
          eta   = drift[iDrift].eta   / nDiv;
          // Loop over the subdivisions
          for (iDiv = nDiv; iDiv--;) {
            gain = loss = 0;
            if (ne > 100) {
              // Gaussian approximation
              gain = int(ne * alpha + RndmGaussian() * sqrt(ne * alpha * (1. - alpha)));
              loss = int(ne * eta   + RndmGaussian() * sqrt(ne * eta   * (1. - eta)));
            } else {
              // Binomial approximation
              for (int k = ne; k--;) {
                if (RndmUniform() < alpha) ++gain;
                if (RndmUniform() < eta)   ++loss;
              }
            }
            ne += gain - loss;
            ni += gain;
            if (ne <= 0) {
              --nElectrons;
              break;
            }
          }

          if (ne > 1 || ni >= 1) {
            // Add the point to the table
            point.x = drift[iDrift + 1].x;
            point.y = drift[iDrift + 1].y;
            point.z = drift[iDrift + 1].z;
            point.t = drift[iDrift + 1].t;
            if (ne > 1) {
              nElectrons += ne - 1;          
              point.ne = ne - 1;
            }
            if (ni >= 1) {
              nIons += ni;
              point.ni = ni;
            }
            aval.push_back(point);
            ++nAval;
          }
          // Make sure the electron is still alive
          if (ne <= 0) {
            nDrift = iDrift + 1;
            break;
          }
        }
        if (useSignal) ComputeSignal(-1);
      }

      if (!withHoles) {
        ++iAval;
        continue;
      }
    }
    
    if (useIons) continue;
    
    // Loop over the holes at this location
    for (iH = 0; iH < aval[iAval].ni; ++iH) {
      // Compute a hole drift line
      if (!DriftLine(aval[iAval].x, aval[iAval].y, aval[iAval].z,
                aval[iAval].t, +1)) continue;
      // Compute alpha and eta for each drift step
      if (!ComputeAlphaEta(+1)) continue;
      // Loop over the drift line and follow the avalanche development
      for (iDrift = 0; iDrift < nDrift - 1; ++iDrift) {
        // Set initial number of electrons and ions
        ne = 0; ni = 1;
        // Compute the number of subdivisions
        nDiv = int((drift[iDrift].alpha + drift[iDrift].eta) / probth);
        if (nDiv < 1) nDiv = 1;
        // Probabilities for gain and loss
        alpha = drift[iDrift].alpha / nDiv;
        eta   = drift[iDrift].eta   / nDiv;
        // Loop over the subdivisions
        for (iDiv = nDiv; iDiv--;) {
          gain = loss = 0;
          if (ni > 100) {
            // Gaussian approximation
            gain = int(ni * alpha + RndmGaussian() * sqrt(ni * alpha * (1. - alpha)));
            loss = int(ni * eta   + RndmGaussian() * sqrt(ni * eta   * (1. - eta)));
          } else {
            // Binomial approximation
            for (int k = ni; k--;) {
              if (RndmUniform() < alpha) ++gain;
              if (RndmUniform() < eta)   ++loss;
            }
          }
          ne += gain;
          ni += gain - loss;
          if (ni <= 0) {
            --nIons;
            break;
          }
        }

        if (ni > 1 || ne >= 1) {
          // Add the point to the table
          point.x = drift[iDrift + 1].x;
          point.y = drift[iDrift + 1].y;
          point.z = drift[iDrift + 1].z;
          point.t = drift[iDrift + 1].t;
          if (ne >= 1) {
            nElectrons += ne;
            point.ne = ne;
          }
          if (ni > 1) {
            nIons += ni - 1;
            point.ni = ni - 1;            
          }
          aval.push_back(point);
          ++nAval;
        }
        // Make sure the hole is still alive
        if (ni <= 0) {
          nDrift = iDrift + 1;
          break;
        }
      }
      if (useSignal) ComputeSignal(1);
    }
    ++iAval;
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
          std::cerr << "AvalancheMC::ComputeAlphaEta:" << std::endl;
          std::cerr << "    Got status value != 0 at segment " << j + 1
                    << "/6, drift point " << i + 1 << "/" << nDrift 
                    << "." << std::endl;
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
  
  // Skip equilibration if there projection has not been requested
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
        std::cerr << "AvalancheMC::ComputeAlphaEta:" << std::endl;
        std::cerr << "    Unable to even out backwards alpha steps." << std::endl;
        std::cerr << "    Avalanche calculation is probably inaccurate." 
                  << std::endl;
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
        std::cerr << "AvalancheMC::ComputeAlphaEta:" << std::endl;
        std::cerr << "    Unable to even out backwards eta steps." << std::endl;
        std::cerr << "    Avalanche calculation is probably inaccurate." 
                  << std::endl;
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

}
