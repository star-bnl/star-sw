#include <iostream>
#include <cmath>
#include <string>

#include "AvalancheMicroscopic.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

// Numerical prefactors
double AvalancheMicroscopic::c1 = SpeedOfLight * sqrt(2. / ElectronMass);
double AvalancheMicroscopic::c2 = c1 * c1 / 4.;

AvalancheMicroscopic::AvalancheMicroscopic() :
  sensor(0), 
  nPhotons(0), nElectrons(0), nIons(0),  
  histEnergy(0), hasEnergyHistogram(false),
  histDistance(0), hasDistanceHistogram(false), distanceOption('z'),
  histSecondary(0), hasSecondaryHistogram(false),
  useSignal(false), useDriftLines(false), usePhotons(false),
  deltaCut(0.), gammaCut(0.),
  nCollSkip(100),
  hasUserHandleAttachment(false),
  hasUserHandleInelastic(false),
  hasUserHandleIonisation(false),
  userHandleAttachment(0), userHandleInelastic(0),
  userHandleIonisation(0),
  debug(false) {

}

void 
AvalancheMicroscopic::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << "AvalancheMicroscopic::SetSensor:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }
  sensor = s;
}

void 
AvalancheMicroscopic::EnableEnergyHistogramming(TH1F* histo) {

  if (histo == 0) {
    std::cerr << "AvalancheMicroscopic::EnableEnergyHistogramming:" 
              << std::endl;
    std::cerr << "    Histogram is not defined." << std::endl;
    return;
  }
  
  histEnergy = histo;
  hasEnergyHistogram = true;
  
}

void 
AvalancheMicroscopic::DisableEnergyHistogramming() {

  hasEnergyHistogram = false;
  
}

void 
AvalancheMicroscopic::EnableDistanceHistogramming(TH1F* histo, const char opt) {

  if (histo == 0) {
    std::cerr << "AvalancheMicroscopic::EnableDistanceHistogramming:" 
              << std::endl;
    std::cerr << "    Histogram is not defined." << std::endl;
    return;
  }
  
  histDistance = histo;
  hasDistanceHistogram = true;

  if (opt == 'x' || opt == 'y' || opt == 'z' || opt == 'r') {
    distanceOption = opt;
  } else {
    std::cerr << "AvalancheMicroscopic::EnableDistanceHistogramming:";
    std::cerr << "    Unknown option " << opt << "." << std::endl;
    std::cerr << "    Valid options are x, y, z, r." << std::endl;
    std::cerr << "    Using default value (r)." << std::endl;
    distanceOption = 'r';
  }
  
}

void 
AvalancheMicroscopic::DisableDistanceHistogramming() {

  hasDistanceHistogram = false;
  
}

void 
AvalancheMicroscopic::EnableSecondaryEnergyHistogramming(TH1F* histo) {

  if (histo == 0) {
    std::cerr << "AvalancheMicroscopic::EnableSecondaryEnergyHistogramming:" 
              << std::endl;
    std::cerr << "    Histogram is not defined." << std::endl;
    return;
  }
  
  histSecondary = histo;
  hasSecondaryHistogram = true;
  
}

void 
AvalancheMicroscopic::DisableSecondaryEnergyHistogramming() {

  hasSecondaryHistogram = false;
  
}

void 
AvalancheMicroscopic::SetCollisionSteps(const int n) {

  if (n <= 0) {
    std::cerr << "AvalancheMicroscopic::SetCollisionSteps:" << std::endl;
    std::cerr << "    Number of collisions to be skipped set to" 
              << " default value (100)." << std::endl;
    nCollSkip = 100;
    return;
  }
  
  nCollSkip = n;
  
}

void 
AvalancheMicroscopic::GetEndpoint(const int i, 
  double& x0, double& y0, double& z0, double& t0, double& e0,
  double& x1, double& y1, double& z1, double& t1, double& e1,
  int& status) const {
  
  if (i < 0 || i >= nEndpoints) {
    std::cerr << "AvalancheMicroscopic::GetEndpoint:" << std::endl;
    std::cerr << "    Endpoint " << i << " does not exist." << std::endl;
    return;
  }

  x0 = endpoints[i].x0; y0 = endpoints[i].y0; z0 = endpoints[i].z0;
  t0 = endpoints[i].t0; e0 = endpoints[i].e0;
  x1 = endpoints[i].x;  y1 = endpoints[i].y;  z1 = endpoints[i].z;
  t1 = endpoints[i].t;  e1 = endpoints[i].energy;  
  status = -1;  
  if (e1 < 0) {
    // Electron stopped because of attachment
    e1 = -e1;
    status = -7;
  }

}

int 
AvalancheMicroscopic::GetNumberOfDriftLinePoints(const int i) const {

  if (i < 0 || i >= nEndpoints) {
    std::cerr << "AvalancheMicroscopic::GetNumberOfDriftLinePoints:" << std::endl;
    std::cerr << "    Endpoint " << i << " does not exist." << std::endl;
    return 0;
  }
  
  if (!useDriftLines) return 2;

  return endpoints[i].driftLine.size() + 2;

}

void 
AvalancheMicroscopic::GetDriftLinePoint(
  double& x, double& y, double& z, double &t,
  const int ip, const int iel) const {
  
  if (iel < 0 || iel >= nEndpoints) {
    std::cerr << "AvalancheMicroscopic::GetDriftLinePoint:" << std::endl;
    std::cerr << "    Endpoint " << iel << " does not exist." << std::endl;
    return;
  }

  if (ip <= 0) {
    x = endpoints[iel].x0;
    y = endpoints[iel].y0;
    z = endpoints[iel].z0;
    t = endpoints[iel].t0;
    return;
  }

  const int np = endpoints[iel].driftLine.size();
  if (ip > np) {
    x = endpoints[iel].x;
    y = endpoints[iel].y;
    z = endpoints[iel].z;
    t = endpoints[iel].t;
    return;
  }

  x = endpoints[iel].driftLine[ip - 1].x;
  y = endpoints[iel].driftLine[ip - 1].y;
  z = endpoints[iel].driftLine[ip - 1].z;
  t = endpoints[iel].driftLine[ip - 1].t;

}

void 
AvalancheMicroscopic::GetPhoton(const int i, double& e,
  double& x0, double& y0, double& z0, double& t0,
  double& x1, double& y1, double& z1, double& t1,
  int& status) const {
 
  if (i < 0 || i >= nPhotons) {
    std::cerr << "AvalancheMicroscopic::GetPhoton:" << std::endl;
    std::cerr << "    Photon " << i << " does not exist." << std::endl;
    return;
  }

  x0 = photons[i].x0; x1 = photons[i].x1;
  y0 = photons[i].y0; y1 = photons[i].y1;
  z0 = photons[i].z0; z1 = photons[i].z1;
  t0 = photons[i].t0; t1 = photons[i].t1;
  status = photons[i].status;
  e = photons[i].energy;


}

void 
AvalancheMicroscopic::SetUserHandleAttachment(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleAttachment = f;
  hasUserHandleAttachment = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleAttachment() {
  
  userHandleAttachment = 0;
  hasUserHandleAttachment = false;
  
}

void 
AvalancheMicroscopic::SetUserHandleInelastic(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleInelastic = f;
  hasUserHandleInelastic = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleInelastic() {
  
  userHandleInelastic = 0;
  hasUserHandleInelastic = false;
  
}

void 
AvalancheMicroscopic::SetUserHandleIonisation(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleIonisation = f;
  hasUserHandleIonisation = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleIonisation() {
  
  userHandleIonisation = 0;
  hasUserHandleIonisation = false;
  
}

bool 
AvalancheMicroscopic::AvalancheElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0) {
  
  // Make sure that the sensor is defined
  if (sensor == 0) {
    std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }

  // Make sure that the starting point is inside a medium
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cerr << "    No medium at initial position." << std::endl;
    return false;
  }
  
  // Make sure that the medium is "driftable" and microscopic
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cerr << "    Medium at initial position does not provide " 
              << " microscopic tracking data." << std::endl;
    return false;
  }
  
  if (debug) {
    std::cout << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cout << "    Starting to drift in medium " 
              << medium->GetName() << "." << std::endl;
  }
  
  // Get the id number of the drift medium
  int id = medium->GetId();    
   
  // Clear the stack
  stack.clear();
  endpoints.clear();
  photons.clear();
  
  // Reset the particle counters
  nPhotons = 0;
  nElectrons = 1;
  nIons = 0;
  nEndpoints = 0;
  
  // Check if the initial energy is higher than the transport cut
  if (e0 <= deltaCut) {
    std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cerr << "    Initial electron energy (" << e0 << " eV)"
              << " is lower than the transport threshold" 
              << " (" << deltaCut << " eV)." << std::endl;
    return false;
  }
    
  // Null-collision rate
  double fLim = medium->GetElectronNullCollisionRate();
  if (fLim <= 0.) {
    std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
    std::cerr << "    Got null-collision rate <= 0." << std::endl;
    return false;
  }
  // Real collision rate
  double fReal;
   
  // Count collisions between updates
  int nCollTemp = 0;  
  
  // Electric field
  double ex, ey, ez;
  int status;
         
  // Current position, direction and energy
  double x, y, z, t;
  double dx, dy, dz, d;
  double energy;
  // Timestep (squared)
  double dt, dt2;
  // Direction and energy after a step
  double newDx, newDy, newDz, newEnergy;
  
  // Collision type (elastic, ionisation, attachment, inelastic)
  int cstype;
  // Cross-section term
  int level;
  // Scattering angles
  double phi, cphi, sphi;
  double ctheta, stheta;
  double arg;

  // Secondary electron energy
  double esec;
  
  // Random number
  double r;
  // Prefactors
  double a, b;
    
  // Add the first electron
  electron newElectron;
  newElectron.x0 = x0;  newElectron.y0 = y0;  newElectron.z0 = z0;  
  newElectron.x  = x0;  newElectron.y  = y0;  newElectron.z = z0;  
  newElectron.t0 = t0;  newElectron.t  = t0;
  newElectron.dx = dx0; newElectron.dy = dy0; newElectron.dz = dz0;
  newElectron.e0 = Max(e0, Small); newElectron.energy = newElectron.e0;
  if (hasDistanceHistogram) {
    newElectron.xLast = x0; newElectron.yLast = y0; newElectron.zLast = z0;
  }  
  newElectron.driftLine.clear();
  stack.push_back(newElectron);

  // Check the given initial direction
  d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (fabs(d) < Small) {
    // Direction has zero norm, draw a random direction
    phi = TwoPi * RndmUniform();
    ctheta = 1. - 2. * RndmUniform();
    stheta = sqrt(1. - ctheta * ctheta);
    stack[0].dx = cos(phi) * stheta;
    stack[0].dy = sin(phi) * stheta;
    stack[0].dz = ctheta;
  } else if (fabs(d - 1.) > Small) {
    // Normalise direction to 1
    stack[0].dx /= d; stack[0].dy /= d; stack[0].dz /= d;
  }

  // Status flag
  bool ok = true;
  // Stack size
  int nSize = 1;
  // Index of the electron in the stack
  int iEl;
  while (1) {
    nSize = stack.size();
    if (nSize <= 0) break;
    // Loop over all electrons in the avalanche
    for (iEl = nSize; iEl--;) {
      // Get the electron from the stack
      x = stack[iEl].x; y = stack[iEl].y; z = stack[iEl].z;
      energy = stack[iEl].energy; t = stack[iEl].t;      
      dx = stack[iEl].dx; dy = stack[iEl].dy; dz = stack[iEl].dz;

      ok = true;
      nCollTemp = 0;

      while (1) {

        if (energy < deltaCut) {
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = energy;
          stack[iEl].dx = dx; stack[iEl].dy = dy; stack[iEl].dz = dz;
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }

        if (hasEnergyHistogram) {
          histEnergy->Fill(energy);
        }

        // Get the local electric field and medium
        sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
        // Sign change
        ex = -ex; ey = -ey; ez = -ez;
        
        if (status != 0) {
          // Electron has left all drift media
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = energy;
          stack[iEl].dx = dx; stack[iEl].dy = dy; stack[iEl].dz = dz;          
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }
        
        if (medium->GetId() != id) {
          // Medium has changed
          if (!medium->IsMicroscopic()) {
            // Electron has left the microscopic drift medium
            stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
            stack[iEl].t = t; stack[iEl].energy = energy;
            stack[iEl].dx = dx; stack[iEl].dy = dy; stack[iEl].dz = dz;
            endpoints.push_back(stack[iEl]);
            stack.erase(stack.begin() + iEl);
            ok = false;
            break;
          }
          id = medium->GetId();
          // Update the null-collision rate
          fLim = medium->GetElectronNullCollisionRate();
          if (fLim <= 0.) {
            std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
            std::cerr << "    Got null-collision rate <= 0." << std::endl;
            return false;
          }          
        }
        
        a = c1 * (dx * ex + dy * ey + dz * ez) * sqrt(energy);
        b = c2 * (ex * ex + ey * ey + ez * ez);

        // Determine the timestep
        dt = 0.;
        while (1) {
          // Determine the flight time
          r = RndmUniformPos();
          dt += - log(r) / fLim;
          // Update the energy
          newEnergy = Max(energy + (a + b * dt) * dt, Small);
          // Get the real collision rate at the updated energy
          fReal = medium->GetElectronCollisionRate(newEnergy);
          if (fReal > fLim) {
            // Real collision rate is higher than null-collision rate
            dt += log(r) / fLim;
            // Increase the null collision rate and try again
            std::cerr << "AvalancheMicroscopic::AvalancheElectron:" << std::endl;
            std::cerr << "    Increasing the null-collision rate by 5%." 
                      << std::endl;
            fLim *= 1.05;
            continue;
          }
          // Check for real or null collision
          if (RndmUniform() > fReal / fLim) continue;
          break;
        }
      
        ++nCollTemp;

        // Update the directions (at instant before collision)
        dt2 = dt * dt;
        a = sqrt(energy / newEnergy);
        b = 0.5 * c1 * dt / sqrt(newEnergy);
        newDx = dx * a + ex * b; 
        newDy = dy * a + ey * b; 
        newDz = dz * a + ez * b;
        // Update the position
        a = c1 * dt * sqrt(energy);
        b = dt2 * c2;            
        x += dx * a + ex * b;
        y += dy * a + ey * b;
        z += dz * a + ez * b;
        t += dt;
      
        // Verify the new position
        if (!sensor->IsInArea(x, y, z)) {
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = newEnergy;
          stack[iEl].dx = dx; stack[iEl].dy = dy; stack[iEl].dz = dz;          
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }
        
        // Get the collision type and parameters
        medium->GetElectronCollision(newEnergy, cstype, level, energy, ctheta, 
                                     d, esec);
        
        switch (cstype) {
          // Elastic collision
          case 0:
            break;
          // Ionising collision
          case 1:
            if (hasUserHandleIonisation) {
              userHandleIonisation(x, y, z, t, cstype, level, medium);
            }
            if (hasDistanceHistogram) {
              switch (distanceOption) {
                case 'x':
                  histDistance->Fill(stack[iEl].xLast - x);
                  break;
                case 'y':
                  histDistance->Fill(stack[iEl].yLast - y);
                  break;
                case 'z':
                  histDistance->Fill(stack[iEl].zLast - z);
                  break;
                case 'r':
                  const double rion = pow(stack[iEl].xLast - x, 2) + 
                                      pow(stack[iEl].yLast - y, 2) + 
                                      pow(stack[iEl].zLast - z, 2);
                  histDistance->Fill(sqrt(rion));
                  break;
              }
              stack[iEl].xLast = x; stack[iEl].yLast = y; stack[iEl].zLast = z;  
            }
            if (hasSecondaryHistogram) histSecondary->Fill(esec);
            // Randomise secondary electron direction
            phi = TwoPi * RndmUniform();
            double ctheta0 = 1. - 2. * RndmUniform();
            double stheta0 = sqrt(1. - ctheta0 * ctheta0);
            // Add the secondary electron to the stack
            newElectron = stack[iEl];
            newElectron.x0 = x; newElectron.x = x;
            newElectron.y0 = y; newElectron.y = y;
            newElectron.z0 = z; newElectron.z = z;
            newElectron.t0 = t; newElectron.t = t; 
            newElectron.energy = Max(esec, Small);
            newElectron.e0 = newElectron.energy;
            newElectron.dx = cos(phi) * stheta0;
            newElectron.dy = sin(phi) * stheta0;
            newElectron.dz = ctheta0;
            newElectron.driftLine.clear();
            stack.push_back(newElectron);
            // Increment the electron and ion counters         
            ++nElectrons; ++nIons;
            break;          
          // Attachment
          case 2:          
            if (hasUserHandleAttachment) {
              userHandleAttachment(x, y, z, t, cstype, level, medium);
            }
            // Decrement the electron counter
            --nElectrons;
            stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z; 
            stack[iEl].t = t; stack[iEl].energy = - energy;
            endpoints.push_back(stack[iEl]);
            stack.erase(stack.begin() + iEl);
            ok = false;
            break;
          // Inelastic collision
          case 3:
            if (hasUserHandleInelastic) {
              userHandleInelastic(x, y, z, t, cstype, level, medium);
            }          
            break;
          // Excitation
          case 4:
            if (hasUserHandleInelastic) {
              userHandleInelastic(x, y, z, t, cstype, level, medium);
            }
            if (esec < 0.) {
              esec = -esec;
              // Radiative de-excitation
              if (usePhotons && esec > gammaCut) {
                TransportPhoton(x, y, z, t + d, esec);
              }
            } else if (esec > 0.) {
              // Penning ionisation     
              // Randomise secondary electron direction
              phi = TwoPi * RndmUniform();
              double ctheta0 = 1. - 2. * RndmUniform();
              double stheta0 = sqrt(1. - ctheta0 * ctheta0);
              // Add the secondary electron to the stack
              newElectron = stack[iEl];
              newElectron.x0 = x; newElectron.x = x;
              newElectron.y0 = y; newElectron.y = y;
              newElectron.z0 = z; newElectron.z = z;
              newElectron.t0 = t + d; newElectron.t = t + d; 
              newElectron.energy = Max(esec, Small);
              newElectron.e0 = newElectron.energy;
              newElectron.dx = cos(phi) * stheta0;
              newElectron.dy = sin(phi) * stheta0;
              newElectron.dz = ctheta0;
              newElectron.driftLine.clear();
              stack.push_back(newElectron);
              // Increment the electron and ion counters         
              ++nElectrons; ++nIons;
            }
            break; 
          // Super-elastic collision
          case 5:
            break;
          default:
            std::cerr << "AvalancheMicroscopic::AvalancheElectron:" 
                      << std::endl;
            std::cerr << "    Unknown collision type." << std::endl;
            ok = false;
            break;
        }

        if (!ok) break;

        newDz = Min(newDz, 1.);       
        arg = sqrt(newDx * newDx + newDy * newDy);
        stheta = sqrt(1. - ctheta * ctheta);
        phi = TwoPi * RndmUniform();
        sphi = sin(phi); cphi = cos(phi);

        if (arg == 0.) {
          dz = ctheta;
          dx = cphi * stheta;
          dy = sphi * stheta;
        } else {
          a = stheta / arg;
          dz = newDz * ctheta + arg * stheta * sphi;
          dy = newDy * ctheta + a * (newDx * cphi - newDy * newDz * sphi);
          dx = newDx * ctheta - a * (newDy * cphi + newDx * newDz * sphi);
        }

        // Continue with the next electron in the stack?
        if (nCollTemp > nCollSkip) break;

      }
      
      if (!ok) continue;
      
      // Normalise the direction vector
      d = sqrt(dx * dx + dy * dy + dz * dz);
      dx = dx / d; dy = dy / d; dz = dz / d;
      // Update the stack
      stack[iEl].energy = energy; stack[iEl].t = t;
      stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
      stack[iEl].dx = dx; stack[iEl].dy = dy; stack[iEl].dz = dz;
      // Add a new point to the drift line (if enabled)
      if (useDriftLines) {
        point newPoint;
        newPoint.x = x; newPoint.y = y; newPoint.z = z; newPoint.t = t;
        stack[iEl].driftLine.push_back(newPoint);
      }
    }
  }
  nEndpoints = endpoints.size();
  
  return true;
    
}

void
AvalancheMicroscopic::TransportPhoton(const double x0, const double y0, 
                const double z0, const double t0, const double e0) {

  // Make sure that the sensor is defined
  if (sensor == 0) {
    std::cerr << "AvalancheMicroscopic::TransportPhoton:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }

  // Make sure that the starting point is inside a medium
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << "AvalancheMicroscopic::TransportPhoton:" << std::endl;
    std::cerr << "    No medium at initial position." << std::endl;
    return;
  }
  
  // Make sure that the medium is "driftable" and microscopic
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << "AvalancheMicroscopic::TransportPhoton:" << std::endl;
    std::cerr << "    Medium at initial position does not provide " 
              << " microscopic tracking data." << std::endl;
    return;
  }
  
  if (debug) {
    std::cout << "AvalancheMicroscopic::TransportPhoton:" << std::endl;
    std::cout << "    Starting photon transport in medium " 
              << medium->GetName() << "." << std::endl;
  }
  
  // Get the id number of the drift medium
  int id = medium->GetId();
  // Get the density of the drift medium
  double rho = medium->GetNumberDensity();

  // Position 
  double x = x0, y = y0, z = z0;
  double t = t0;
  // Initial direction (randomised)
  double ctheta = 1. - 2 * RndmUniform();
  double stheta = sqrt(1. - ctheta * ctheta);
  double phi = TwoPi * RndmUniform();
  double dx = cos(phi) * stheta;
  double dy = sin(phi) * stheta;
  double dz = ctheta;
  // Energy 
  double e = e0;
  // Photon collision rate
  double f = 0.;
  // Timestep
  double dt = 0.;

  int type, level;
  double e1, s, esec;

  f = medium->GetPhotonCollisionRate(e);
  if (f <= 0.) return;

  dt = - log(RndmUniformPos()) / f;
  t += dt;
  dt *= SpeedOfLight;
  x += dt * dx; y += dt * dy; z += dt * dz;

  // Check if the photon is still inside a medium
  if (!sensor->GetMedium(x, y, z, medium)) { 
    photon newPhoton;
    newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
    newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
    newPhoton.energy = e0;
    photons.push_back(newPhoton);
    ++nPhotons;
    return;
  }

  if (!medium->GetPhotonCollision(e, type, level, e1, ctheta, s, esec)) return;
 
  if (type == 1) {
    // Ionisation
    // Randomise secondary electron direction
    phi = TwoPi * RndmUniform();
    ctheta = 1. - 2. * RndmUniform();
    stheta = sqrt(1. - ctheta * ctheta);
    // Add the secondary electron to the stack
    electron newElectron;
    newElectron.x0 = x; newElectron.x = x;
    newElectron.y0 = y; newElectron.y = y;
    newElectron.z0 = z; newElectron.z = z;
    newElectron.t0 = t; newElectron.t = t; 
    newElectron.energy = Max(esec, Small);
    newElectron.e0 = newElectron.energy;
    newElectron.dx = cos(phi) * stheta;
    newElectron.dy = sin(phi) * stheta;
    newElectron.dz = ctheta;
    newElectron.driftLine.clear();
    stack.push_back(newElectron);
    // Increment the electron and ion counters         
    ++nElectrons; ++nIons;
  }

  photon newPhoton;
  newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
  newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
  newPhoton.energy = e0;
  photons.push_back(newPhoton);
  ++nPhotons;

}

}
