#include <iostream>
#include <cmath>
#include <string>

#include "AvalancheMicroscopic.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

AvalancheMicroscopic::AvalancheMicroscopic() :
  sensor(0), 
  nPhotons(0), nElectrons(0), nIons(0), nEndpoints(0),
  usePlotting(false), viewer(0), 
  histEnergy(0), hasEnergyHistogram(false),
  histDistance(0), hasDistanceHistogram(false), distanceOption('z'),
  histSecondary(0), hasSecondaryHistogram(false),
  useSignal(false), useInducedCharge(false),
  useDriftLines(false), usePhotons(false), useBandStructure(false),
  useNullCollisionSteps(false), useBfield(false),
  rb11(1.), rb12(0.), rb13(0.), rb21(0.), rb22(1.), rb23(0.),
  rb31(0.), rb32(0.), rb33(1.), rx22(1.), rx23(0.), rx32(0.), rx33(1.),
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
AvalancheMicroscopic::EnablePlotting(DriftView* view) {

  if (view == 0) {
    std::cerr << "AvalancheMicroscopic::EnablePlotting:" << std::endl;
    std::cerr << "    Viewer is not defined." << std::endl;
    return;
  }
  
  viewer = view;
  usePlotting = true;
  if (!useDriftLines) {
    std::cout << "AvalancheMicroscopic::EnablePlotting:" << std::endl;
    std::cout << "    Enabling storage of drift line." << std::endl;
    EnableDriftLines();
  }

}

void
AvalancheMicroscopic::DisablePlotting() {
 
  viewer = 0;
  usePlotting = false;

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
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = t1 = t1 = e1 = 0.;
    status = 0;
    return;
  }

  x0 = endpoints[i].x0; y0 = endpoints[i].y0; z0 = endpoints[i].z0;
  t0 = endpoints[i].t0; e0 = endpoints[i].e0;
  x1 = endpoints[i].x;  y1 = endpoints[i].y;  z1 = endpoints[i].z;
  t1 = endpoints[i].t;  e1 = endpoints[i].energy;  
  status = endpoints[i].status; 

}

int 
AvalancheMicroscopic::GetNumberOfDriftLinePoints(const int i) const {

  if (i < 0 || i >= nEndpoints) {
    std::cerr << "AvalancheMicroscopic::GetNumberOfDriftLinePoints:" 
              << std::endl;
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
    x = endpoints[iel].x0; y = endpoints[iel].y0; z = endpoints[iel].z0;
    t = endpoints[iel].t0;
    return;
  }

  const int np = endpoints[iel].driftLine.size();
  if (ip > np) {
    x = endpoints[iel].x; y = endpoints[iel].y; z = endpoints[iel].z;
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
AvalancheMicroscopic::DriftElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0) {

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, false);

}

bool 
AvalancheMicroscopic::AvalancheElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0) {

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, true);

}

bool 
AvalancheMicroscopic::TransportElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0,
    const bool aval) {
  
  // Make sure that the sensor is defined
  if (sensor == 0) {
    std::cerr << "AvalancheMicroscopic::TransportElectron:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return false;
  }

  // Make sure that the starting point is inside a medium
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << "AvalancheMicroscopic::TransportElectron:" << std::endl;
    std::cerr << "    No medium at initial position." << std::endl;
    return false;
  }
  
  // Make sure that the medium is "driftable" and microscopic
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << "AvalancheMicroscopic::TransportElectron:" << std::endl;
    std::cerr << "    Medium at initial position does not provide " 
              << " microscopic tracking data." << std::endl;
    return false;
  }
  
  if (debug) {
    std::cout << "AvalancheMicroscopic::TransportElectron:" << std::endl;
    std::cout << "    Starting to drift in medium " 
              << medium->GetName() << "." << std::endl;
  }
  
  // Get the id number of the drift medium
  int id = medium->GetId();    
  
  // Numerical prefactors in equation of motion
  const double c1 = SpeedOfLight * sqrt(2. / ElectronMass);
  const double c2 = c1 * c1 / 4.;


  // Clear the lists of electrons and photons
  stack.clear(); endpoints.clear(); photons.clear();

  // Temporary stack of photons produced in the de-excitation cascade
  std::vector<double> stackPhotonsTime;   stackPhotonsTime.clear();
  std::vector<double> stackPhotonsEnergy; stackPhotonsEnergy.clear();

  // Reset the particle counters
  nPhotons = 0; nElectrons = 1; nIons = 0; nEndpoints = 0;
  
  // Null-collision rate
  double fLim = medium->GetElectronNullCollisionRate();
  if (fLim <= 0.) {
    std::cerr << "AvalancheMicroscopic::TransportElectron:" << std::endl;
    std::cerr << "    Got null-collision rate <= 0." << std::endl;
    return false;
  }
  // Null-collision flag
  bool isNullCollision = false;
  // Real collision rate
  double fReal;
   
  // Count number of collisions between updates
  int nCollTemp = 0;  
  
  // Electric and magnetic field
  double ex = 0., ey = 0., ez = 0., emag = 0.;
  double bx = 0., by = 0., bz = 0., bmag = 0.;
  int status = 0;

  double cbtheta = 1., sbtheta = 0.;
  double cwt = 1., swt = 0.;
  double wb = 0.;

  bool bOk = true;

  // Current position, direction, velocity and energy
  double x, y, z, t;
  double kx, ky, kz, k;
  double vx, vy, vz, v;
  double dx, dy, dz, d;
  double energy;
  // Conduction band
  int band = 0;
  
  // Timestep
  double dt;
  // Direction and energy after a step
  double newKx = 0., newKy = 0., newKz = 0., newEnergy = 0.;
  
  // Collision type (elastic, ionisation, attachment, inelastic)
  int cstype;
  // Cross-section term
  int level;
  // Scattering angles
  double phi, cphi, sphi;
  double ctheta, stheta;
  double ctheta0, stheta0;
  double arg;

  // Secondary electron energy
  double esec;
  
  // Random number
  double r;
  // Numerical factors
  double a1 = 0., a2 = 0., a3 = 0., a4 = 0.;
    
  // Add the first electron
  electron newElectron;
  newElectron.status = 0;
  newElectron.x0 = x0;  newElectron.y0 = y0;  newElectron.z0 = z0;  
  newElectron.x  = x0;  newElectron.y  = y0;  newElectron.z = z0;  
  newElectron.t0 = t0;  newElectron.t  = t0;
  newElectron.kx = dx0; newElectron.ky = dy0; newElectron.kz = dz0;
  newElectron.e0 = Max(e0, Small); newElectron.energy = newElectron.e0;
  newElectron.band = band;
  if (hasDistanceHistogram) {
    newElectron.xLast = x0; newElectron.yLast = y0; newElectron.zLast = z0;
  }  
  newElectron.driftLine.clear();
  stack.push_back(newElectron);

  // Check the given initial direction
  k = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (useBandStructure) {
    medium->GetElectronMomentum(Max(e0, Small), kx, ky, kz, band);
    stack[0].kx = kx;
    stack[0].ky = ky;
    stack[0].kz = kz;
  } else if (fabs(k) < Small) {
    // Direction has zero norm, draw a random direction
    phi = TwoPi * RndmUniform();
    ctheta = 1. - 2. * RndmUniform();
    stheta = sqrt(1. - ctheta * ctheta);
    stack[0].kx = cos(phi) * stheta;
    stack[0].ky = sin(phi) * stheta;
    stack[0].kz = ctheta;
  } else {
    // Normalise direction to 1
    stack[0].kx /= k; stack[0].ky /= k; stack[0].kz /= k;
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
      energy = stack[iEl].energy; band = stack[iEl].band; t = stack[iEl].t; 
      kx = stack[iEl].kx; ky = stack[iEl].ky; kz = stack[iEl].kz;

      ok = true;
      nCollTemp = 0;

      // Get the local electric field and medium
      sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Sign change
      ex = -ex; ey = -ey; ez = -ez;
      
      if (debug) {
        std::cout << "AvalancheMicroscopic::TransportElectron:" << std::endl;
        std::cout << "    Drifting electron " << iEl << "." << std::endl;
        std::cout << "    Field at " << x << ", " << y << ", " << z << ": "
                  << ex << ", " << ey << ", " << ez << std::endl;
        std::cout << "    Status: " << status << std::endl;
        std::cout << "    Medium: " << medium->GetName() << std::endl;
      }

      if (status != 0) {
        // Electron is not inside a drift medium
        stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
        stack[iEl].t = t; stack[iEl].energy = energy; stack[iEl].band = band;
        stack[iEl].kx = kx; stack[iEl].ky = ky; stack[iEl].kz = kz;
        stack[iEl].status = -1;
        endpoints.push_back(stack[iEl]);
        stack.erase(stack.begin() + iEl);
        if (debug) {
          std::cout << "AvalancheMicroscopic::TransportElectron:" << std::endl;
          std::cout << "    Electron left the drift medium." << std::endl;
          std::cout << "    At " << x << ", " << ", " << y << ", " << z 
                    << std::endl;
        }
        continue;
      }

      // If activated, get the local magnetic field
      if (useBfield) {
        sensor->MagneticField(x, y, z, bx, by, bz, status);
        bx = -bx; by = -by; bz = -bz;
        // Make sure that neither E nor B are zero
        bmag = sqrt(bx * bx + by * by + bz * bz);
        emag = sqrt(ex * ex + ey * ey + ez * ez);
        if (bmag > Small && emag > Small) bOk = true;
        else bOk = false;
      }

      while (1) {

        isNullCollision = false;

        if (energy < deltaCut) {
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = energy; stack[iEl].band = band;
          stack[iEl].kx = kx; stack[iEl].ky = ky; stack[iEl].kz = kz;
          stack[iEl].status = -16;
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }

        if (hasEnergyHistogram) histEnergy->Fill(energy);

        if (medium->GetId() != id) {
          // Medium has changed
          if (!medium->IsMicroscopic()) {
            // Electron has left the microscopic drift medium
            stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
            stack[iEl].t = t; stack[iEl].energy = energy;
            stack[iEl].kx = kx; stack[iEl].ky = ky; stack[iEl].kz = kz;
            stack[iEl].status = -5;
            endpoints.push_back(stack[iEl]);
            stack.erase(stack.begin() + iEl);
            ok = false;
            break;
          }
          id = medium->GetId();
          // Update the null-collision rate
          fLim = medium->GetElectronNullCollisionRate();
          if (fLim <= 0.) {
            std::cerr << "AvalancheMicroscopic::TransportElectron:" 
                      << std::endl;
            std::cerr << "    Got null-collision rate <= 0." << std::endl;
            return false;
          }          
        }

        if (useBfield && bOk) {
          // Calculate the cyclotron frequency
          wb = OmegaCyclotronOverB * bmag;
          // Calculate the angle between E and B vector
          cbtheta = (ex * bx + ey * by + ez * bz) / (emag * bmag);
          sbtheta = sqrt(1. - cbtheta * cbtheta);
          // Calculate the electric field in the rotated system
          ex = emag * cbtheta; ey = 0.; ez = emag * sbtheta / wb;
          
          // Rotate the direction vector into the local coordinate system
          ComputeRotationMatrix(bx, by, bz, bmag, ex, ey, ez);
          RotateGlobal2Local(kx, ky, kz);
 
          // Calculate the velocity vector in the local frame
          v = c1 * sqrt(energy);
          vx = v * kx; vy = v * ky; vz = v * kz;
          
          a1 = vx * ex;
          a2 = c2 * ex * ex;
          a3 = ez * (2 * c2 * ez - vy);
          a4 = ez * vz;
        } else if (useBandStructure) {
          energy = medium->GetElectronEnergy(kx, ky, kz, vx, vy, vz);
        } else {
          // Calculate the velocity vector
          v = c1 * sqrt(energy);
          vx = v * kx; vy = v * ky; vz = v * kz;
          
          a1 = vx * ex + vy * ey + vz * ez;
          a2 = c2 * (ex * ex + ey * ey + ez * ez);
        }

        // Determine the timestep
        dt = 0.;
        while (1) {
          // Determine the flight time
          r = RndmUniformPos();
          dt += - log(r) / fLim;
          // Update the energy
          if (useBfield && bOk) {
            cwt = cos(wb * dt); swt = sqrt(1. - cwt * cwt);
            newEnergy = Max(energy + (a1 + a2 * dt) * dt + 
                            a3 * (1. - cwt) + a4 * swt, Small);
          } else if (useBandStructure) {
            newEnergy = Max(
                            medium->GetElectronEnergy(kx + ex * dt,
                                                      ky + ey * dt,
                                                      kz + ez * dt, 
                                                      dx, dy, dz, band),
                            Small);
          } else {
            newEnergy = Max(energy + (a1 + a2 * dt) * dt, Small);
          }
          // Get the real collision rate at the updated energy
          fReal = medium->GetElectronCollisionRate(newEnergy, band);
          if (fReal > fLim) {
            // Real collision rate is higher than null-collision rate
            dt += log(r) / fLim;
            // Increase the null collision rate and try again
            std::cerr << "AvalancheMicroscopic::TransportElectron:"
                      << std::endl;
            std::cerr << "    Increasing the null-collision rate by 5%." 
                      << std::endl;
            fLim *= 1.05;
            continue;
          }
          // Check for real or null collision
          if (RndmUniform() <= fReal / fLim) break;
          if (useNullCollisionSteps) {
            isNullCollision = true;
            break;
          }
        }
      
        ++nCollTemp;        

        // Update the directions (at instant before collision)
        // and calculate the proposed new position
        if (useBfield && bOk) {
          // Calculate the new velocity
          a1 = 2. * c2 * ez;
          a2 = (vy - a1);
          a3 = vx;
          a4 = vz;
          vx += 2. * c2 * ex * dt;
          vy = a2 * cwt + vz * swt + a1;
          vz = vz * cwt - a2 * swt;
          // Rotate back to the lab frame
          RotateLocal2Global(vx, vy, vz);
          v = sqrt(vx * vx + vy * vy + vz * vz);
          newKx = vx / v; newKy = vy / v; newKz = vz / v;
          
          // Calculate the step in coordinate space
          vx = a3 + c2 * ex * dt;
          vy = (a2 * swt + a4 * (1. - cwt)) / (wb * dt) + a1;
          vz = (a4 * swt - a2 * (1. - cwt)) / (wb * dt);
          // Rotate back to the lab frame
          RotateLocal2Global(vx, vy, vz);
        } else if (useBandStructure) {
          // Update the wave-vector
          newKx = kx + ex * dt;
          newKy = ky + ey * dt;
          newKz = kz + ez * dt;
          // Average velocity over the step
          vx = 0.5 * (vx + dx);
          vy = 0.5 * (vy + dy);
          vz = 0.5 * (vz + dz);
        } else {
          // Update the direction
          a1 = sqrt(energy / newEnergy);
          a2 = 0.5 * c1 * dt / sqrt(newEnergy);
          newKx = kx * a1 + ex * a2; 
          newKy = ky * a1 + ey * a2; 
          newKz = kz * a1 + ez * a2;

          // Calculate the step in coordinate space
          a1 = c1 * sqrt(energy);
          a2 = dt * c2; 
          vx = kx * a1 + ex * a2;
          vy = ky * a1 + ey * a2;
          vz = kz * a1 + ez * a2;
        }
 
        // Get the local electric field and medium at the proposed new position
        sensor->ElectricField(x + vx * dt, y + vy * dt, z + vz * dt, 
                              ex, ey, ez, medium, status);
        // Sign change
        ex = -ex; ey = -ey; ez = -ez;
 
        // Check if the electron is still inside a drift medium
        if (status != 0) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = energy;
          dx = vx * dt; dy = vy * dt; dz = vz * dt;
          d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d; dy /= d; dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx; yM = y + d * dy; zM = z + d * dz; 
            // Check if the mid-point is inside the drift medium
            sensor->ElectricField(xM, yM, zM, ex, ey, ez, medium, status);
            if (status == 0) {
              x = xM; y = yM; z = zM; t += dt;
            } 
          }
          // Place the electron OUTSIDE the drift medium
          x += d * dx; y += d * dy; z += d * dz; 
          if (useSignal) sensor->AddSignal(-1, stack[iEl].t, t - stack[iEl].t, 
                                           0.5 * (x - stack[iEl].x), 
                                           0.5 * (y - stack[iEl].y),
                                           0.5 * (z - stack[iEl].z), 
                                           vx, vy, vz);
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t;
          stack[iEl].kx = newKx; stack[iEl].ky = newKy; stack[iEl].kz = newKz;
          stack[iEl].status = -1;
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }

        // Check if the new position is inside the bounding box
        if (!sensor->IsInArea(x + vx * dt, y + vy * dt, z + vz * dt)) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t; stack[iEl].energy = energy;
          dx = vx * dt, dy = vy * dt, dz = vz * dt;
          d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d; dy /= d; dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx; yM = y + d * dy; zM = z + d * dz; 
            // Check if the mid-point is inside the drift area
            if (sensor->IsInArea(xM, yM, zM)) {
              x = xM; y = yM; z = zM; t += dt;
            }
          }
          // Place the electron OUTSIDE the drift area
          x += d * dx; y += d * dy; z += d * dz;

          if (useSignal) sensor->AddSignal(-1, stack[iEl].t, t - stack[iEl].t, 
                                           0.5 * (x - stack[iEl].x), 
                                           0.5 * (y - stack[iEl].y),
                                           0.5 * (z - stack[iEl].z), 
                                           vx, vy, vz);
          stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
          stack[iEl].t = t;
          stack[iEl].kx = newKx; stack[iEl].ky = newKy; stack[iEl].kz = newKz;
          stack[iEl].status = -1;
          endpoints.push_back(stack[iEl]);
          stack.erase(stack.begin() + iEl);
          ok = false;
          break;
        }

        // If activated, calculate the induced signal
        if (useSignal) sensor->AddSignal(-1, t, dt, 
                                         x + 0.5 * vx * dt, 
                                         y + 0.5 * vy * dt,
                                         z + 0.5 * vy * dt, vx, vy, vz);
        // Update the coordinates
        x += vx * dt; y += vy * dt; z += vz * dt; t += dt;

        // If activated, get the magnetic field at the new location
        if (useBfield) {
          sensor->MagneticField(x, y, z, bx, by, bz, status);
          bx = -bx; by = -by; bz = -bz;
          // Make sure that neither E nor B are zero
          bmag = sqrt(bx * bx + by * by + bz * bz);
          emag = sqrt(ex * ex + ey * ey + ez * ez);
          if (bmag > Small && emag > Small) bOk = true;
          else bOk = false;
        }

        if (isNullCollision) {
          energy = newEnergy;
          kx = newKx; ky = newKy; kz = newKz;
          continue;
        }
        
        // Get the collision type and parameters
        medium->GetElectronCollision(newEnergy, cstype, level, energy, ctheta, 
                                     k, esec, band);

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
            // Add the secondary electron to the stack
            newElectron = stack[iEl];
            newElectron.x0 = x; newElectron.x = x;
            newElectron.y0 = y; newElectron.y = y;
            newElectron.z0 = z; newElectron.z = z;
            newElectron.t0 = t; newElectron.t = t;
            newElectron.energy = Max(esec, Small);
            newElectron.e0 = newElectron.energy;
            if (useBandStructure) {
              medium->GetElectronMomentum(Max(esec, Small), 
                                          newElectron.kx, newElectron.ky,
                                          newElectron.kz, band);
            } else {
              // Randomise secondary electron direction
              phi = TwoPi * RndmUniform();
              ctheta0 = 1. - 2. * RndmUniform();
              stheta0 = sqrt(1. - ctheta0 * ctheta0);
              newElectron.kx = cos(phi) * stheta0;
              newElectron.ky = sin(phi) * stheta0;
              newElectron.kz = ctheta0;
              newElectron.driftLine.clear();
            }
            if (aval) stack.push_back(newElectron);
            // Increment the electron and ion counters
            ++nElectrons; ++nIons;
            if (debug) {
              std::cout << "AvalancheMicroscopic::TransportElectron:\n";
              std::cout << "    Ionisation." << std::endl;
              std::cout << "    At " << x << ", " << y << ", " << z 
                        << std::endl;
            }
            break;
          // Attachment
          case 2:
            if (hasUserHandleAttachment) {
              userHandleAttachment(x, y, z, t, cstype, level, medium);
            }
            // Decrement the electron counter
            --nElectrons;
            stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
            stack[iEl].t = t; stack[iEl].energy = energy;
            stack[iEl].status = -7;
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
            if (esec < 0. && k < 0.) {
              // Detailed modelling of de-excitation cascade
              double tDxc = 0.;
              int typeDxc = 0;
              stackPhotonsTime.clear(); stackPhotonsEnergy.clear();
              const int nDxc = medium->GetNumberOfDeexcitationProducts();
              for (int j = nDxc; j--;) {
                if (medium->GetDeexcitationProduct(j, tDxc, typeDxc, esec)) {
                  if (typeDxc == -1) {
                    // Additional electron (Penning ionisation)
                    phi = TwoPi * RndmUniform();
                    ctheta0 = 1. - 2 * RndmUniform();
                    stheta0 = sqrt(1. - ctheta0 * ctheta0);
                    // Add the secondary electron to the stack
                    newElectron = stack[iEl];
                    newElectron.x0 = x; newElectron.x = x;
                    newElectron.y0 = y; newElectron.y = y;
                    newElectron.z0 = z; newElectron.z = z;
                    newElectron.t0 = t + tDxc; newElectron.t = t + tDxc;
                    newElectron.energy = Max(esec, Small);
                    newElectron.e0 = newElectron.energy;
                    newElectron.kx = cos(phi) * stheta0;
                    newElectron.ky = sin(phi) * stheta0;
                    newElectron.kz = ctheta0;
                    newElectron.driftLine.clear();
                    if (aval) stack.push_back(newElectron);
                    // Increment the electron and ion counters
                    ++nElectrons; ++nIons;
                  } else if (typeDxc == 1 && usePhotons && esec > gammaCut) {
                    // Radiative de-excitation
                    stackPhotonsTime.push_back(t + tDxc);
                    stackPhotonsEnergy.push_back(esec);
                  }
                  // Transport the photons (if any)
                  const int nSizePhotons = stackPhotonsTime.size();
                  for (int k = nSizePhotons; k--;) {
                    if (aval) TransportPhoton(x, y, z, 
                                    stackPhotonsTime[k], 
                                    stackPhotonsEnergy[k]);
                  }
                }
              }
            } else if (esec > 0.) {
              // Penning ionisation
              // Randomise secondary electron direction
              phi = TwoPi * RndmUniform();
              ctheta0 = 1. - 2. * RndmUniform();
              stheta0 = sqrt(1. - ctheta0 * ctheta0);
              // Add the secondary electron to the stack
              newElectron = stack[iEl];
              newElectron.x0 = x; newElectron.x = x;
              newElectron.y0 = y; newElectron.y = y;
              newElectron.z0 = z; newElectron.z = z;
              newElectron.t0 = t + k; newElectron.t = t + k; 
              newElectron.energy = Max(esec, Small);
              newElectron.e0 = newElectron.energy;
              newElectron.kx = cos(phi) * stheta0;
              newElectron.ky = sin(phi) * stheta0;
              newElectron.kz = ctheta0;
              newElectron.driftLine.clear();
              if (aval) stack.push_back(newElectron);
              // Increment the electron and ion counters
              ++nElectrons; ++nIons;
            }
            break;
          // Super-elastic collision
          case 5:
            break;
          case 10:
            // Acoustic intravalley phonon
            break;
          case 11:
          case 12:
          case 13:
          case 14:
            // Intervalley phonons
            break;
          case 15:
            // Impurity scattering
            break;
          default:
            std::cerr << "AvalancheMicroscopic::TransportElectron:" 
                      << std::endl;
            std::cerr << "    Unknown collision type." << std::endl;
            ok = false;
            break;
        }

        if (!ok) break;
        
        if (useBandStructure) {
          medium->GetElectronMomentum(energy, kx, ky, kz, band);
          continue;
        }

        newKz = Min(newKz, 1.); 
        arg = sqrt(newKx * newKx + newKy * newKy);
        stheta = sqrt(1. - ctheta * ctheta);
        phi = TwoPi * RndmUniform();
        sphi = sin(phi); cphi = cos(phi);

        if (arg == 0.) {
          kz = ctheta;
          kx = cphi * stheta;
          ky = sphi * stheta;
        } else {
          a1 = stheta / arg;
          kz = newKz * ctheta + arg * stheta * sphi;
          ky = newKy * ctheta + a1 * (newKx * cphi - newKy * newKz * sphi);
          kx = newKx * ctheta - a1 * (newKy * cphi + newKx * newKz * sphi);
        }

        // Continue with the next electron in the stack?
        if (nCollTemp > nCollSkip) break;

      }
      
      if (!ok) continue;
      
      if (!useBandStructure) {
        // Normalise the direction vector
        k = sqrt(kx * kx + ky * ky + kz * kz);
        kx /= k; ky /= k; kz /= k;
      }
      // Update the stack
      stack[iEl].energy = energy; stack[iEl].t = t;
      stack[iEl].x = x; stack[iEl].y = y; stack[iEl].z = z;
      stack[iEl].kx = kx; stack[iEl].ky = ky; stack[iEl].kz = kz;
      // Add a new point to the drift line (if enabled)
      if (useDriftLines) {
        point newPoint;
        newPoint.x = x; newPoint.y = y; newPoint.z = z; newPoint.t = t;
        stack[iEl].driftLine.push_back(newPoint);
      }
    }
  }
  nEndpoints = endpoints.size();

  if (useInducedCharge) {
    for (int i = nEndpoints; i--;) {
      sensor->AddInducedCharge(-1, 
                           endpoints[i].x0, endpoints[i].y0, endpoints[i].z0,
                           endpoints[i].x,  endpoints[i].y,  endpoints[i].z);
    }
  }

  if (usePlotting) {
    for (int i = nEndpoints; i--;) {
      const int np = GetNumberOfDriftLinePoints(i);
      viewer->NewElectronDriftLine(np);
      for (int j = np; j--;) {
        GetDriftLinePoint(x, y, z, t, j, i);
        viewer->SetPoint(j, x, y, z);
      }
    }
    for (int i = nPhotons; i--;) {
      viewer->NewPhotonTrack(photons[i].x0, photons[i].y0, photons[i].z0,
                             photons[i].x1, photons[i].y1, photons[i].z1);
    } 
  }
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
  if (!sensor->GetMedium(x, y, z, medium) || medium->GetId() != id) {
    // Try to terminate the photon track close to the boundary
    // by means of iterative bisection
    dx *= dt; dy *= dt; dz *= dt;
    x -= dx; y -= dy; z -= dz;
    double delta = sqrt(dx * dx + dy * dy + dz * dz);
    if (delta > 0) {
      dx /= delta; dy /= delta; dz /= delta;
    }
    // Mid-point
    double xM = x, yM = y, zM = z;
    while (delta > BoundaryDistance) {
      delta *= 0.5;
      dt *= 0.5;
      xM = x + delta * dx; yM = y + delta * dy; zM = z + delta * dz; 
      // Check if the mid-point is inside the drift medium
      if (sensor->GetMedium(xM, yM, zM, medium) && medium->GetId() == id) {
        x = xM; y = yM; z = zM; t += dt;
      }
    }
    photon newPhoton;
    newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
    newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
    newPhoton.energy = e0;
    newPhoton.status = -1;
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
    newElectron.kx = cos(phi) * stheta;
    newElectron.ky = sin(phi) * stheta;
    newElectron.kz = ctheta;
    newElectron.driftLine.clear();
    stack.push_back(newElectron);
    // Increment the electron and ion counters         
    ++nElectrons; ++nIons;
  } else if (type == 3) {
    // Excitation
    double tDxc = 0.;
    int typeDxc = 0;
    std::vector<double> stackPhotonsTime;   stackPhotonsTime.clear();
    std::vector<double> stackPhotonsEnergy; stackPhotonsEnergy.clear();
    const int nDxc = medium->GetNumberOfDeexcitationProducts();
    for (int j = nDxc; j--;) {
      if (!medium->GetDeexcitationProduct(j, tDxc, typeDxc, esec)) continue;
      if (typeDxc == -1) {
        // Ionisation
        phi = TwoPi * RndmUniform();
        ctheta = 1. - 2 * RndmUniform();
        stheta = sqrt(1. - ctheta * ctheta);
        // Add the electron to the stack
        electron newElectron;
        newElectron.x0 = x; newElectron.x = x;
        newElectron.y0 = y; newElectron.y = y;
        newElectron.z0 = z; newElectron.z = z;
        newElectron.t0 = t + tDxc; newElectron.t = t + tDxc; 
        newElectron.energy = Max(esec, Small);
        newElectron.e0 = newElectron.energy;
        newElectron.kx = cos(phi) * stheta;
        newElectron.ky = sin(phi) * stheta;
        newElectron.kz = ctheta;
        newElectron.driftLine.clear();
        stack.push_back(newElectron);
        // Increment the electron and ion counters         
        ++nElectrons; ++nIons;
      } else if (typeDxc == 1 && usePhotons && esec > gammaCut) {
        // Radiative de-excitation
        stackPhotonsTime.push_back(t + tDxc);
        stackPhotonsEnergy.push_back(esec);
      }
    }
    // Transport the photons (if any)
    const int nSizePhotons = stackPhotonsTime.size();
    for (int k = nSizePhotons; k--;) {
      TransportPhoton(x, y, z, stackPhotonsTime[k], stackPhotonsEnergy[k]);
    }
  }

  photon newPhoton;
  newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
  newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
  newPhoton.energy = e0;
  newPhoton.energy = -2;
  photons.push_back(newPhoton);
  ++nPhotons;

}

void 
AvalancheMicroscopic::ComputeRotationMatrix(
    const double bx, const double by, const double bz, const double bmag, 
    const double ex, const double ey, const double ez) {

  // Adopting the Magboltz convention, the stepping is performed
  // in a coordinate system with the B field along the x axis
  // and the electric field at an angle btheta in the x-z plane

  // Calculate the first rotation matrix (to align B with x axis)
  const double bt = by * by + bz * bz;
  if (bt < Small) {
    rb11 = rb22 = rb33 = 1.;
    rb12 = rb13 = rb21 = rb23 = rb31 = rb32 = 0.;
  } else {
    rb11 = bx / bmag; 
    rb22 = rb11 + (1. - rb11) * bz * bz / bt;
    rb33 = rb11 + (1. - rb11) * by * by / bt;
    rb12 = by / bmag; rb21 = -rb12;
    rb13 = bz / bmag; rb31 = -rb13;
    rb23 = rb32 = (1. - rb11) * by * bz / bt;
  }
  // Calculate the second rotation matrix (rotation around x axis)
  const double fy = rb21 * ex + rb22 * ey + rb23 * ez;
  const double fz = rb31 * ex + rb32 * ey + rb33 * ez;
  const double ft = sqrt(fy * fy + fz * fz);
  if (ft < Small) {
    rx22 = rx33 = 1.;
    rx23 = rx32 = 0.;
  } else {
    rx22 = rx33 = fz / ft;
    rx23 = - fy / ft; rx32 = -rx23;
  }

}

void
AvalancheMicroscopic::RotateGlobal2Local(double& dx, double& dy, double& dz) {

  const double dx1 = rb11 * dx + rb12 * dy + rb13 * dz;
  const double dy1 = rb21 * dx + rb22 * dy + rb23 * dz;
  const double dz1 = rb31 * dx + rb32 * dy + rb33 * dz;
  
  dx = dx1;
  dy = rx22 * dy1 + rx23 * dz1;
  dz = rx32 * dy1 + rx33 * dz1;

}

void
AvalancheMicroscopic::RotateLocal2Global(double& dx, double& dy, double& dz) {

  const double dx1 = dx;
  const double dy1 =  rx22 * dy - rx23 * dz;
  const double dz1 = -rx23 * dy + rx33 * dz;
  
  dx =  rb11 * dx1 - rb12 * dy1 - rb13 * dz1;
  dy = -rb21 * dx1 + rb22 * dy1 + rb23 * dz1;
  dz = -rb31 * dx1 + rb32 * dy1 + rb33 * dz1;

}

}
