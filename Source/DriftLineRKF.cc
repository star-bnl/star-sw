#include <iostream>
#include <cmath>

#include "DriftLineRKF.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

DriftLineRKF::DriftLineRKF() :
  sensor(0), medium(0), 
  maxStepSize(1.e8), intAccuracy(1.e-8), 
  maxSteps(1000), 
  usePlotting(false), viewer(0), 
  debug(false), verbose(false) {
  
  className = "DriftLineRKF";
  path.clear();

}


void 
DriftLineRKF::SetSensor(Sensor* s) {
    
  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  sensor = s;

}

void
DriftLineRKF::EnablePlotting(ViewDrift* view) {

  if (view == 0) {
    std::cerr << className << "::EnablePlotting:\n";
    std::cerr << "    Viewer pointer is null.\n";
    return;
  }
  
  usePlotting = true;
  viewer = view;

}

void
DriftLineRKF::DisablePlotting() {

  viewer = 0;
  usePlotting = false;

}

void
DriftLineRKF::DriftElectron(const double x0, const double y0, const double z0,
                            const double t0) {
                            
  double meanTime = 0.; double rmsTime = 0.; 
  DriftLine(x0, y0, z0, t0, meanTime, rmsTime, "electron");

}

void
DriftLineRKF::DriftHole(const double x0, const double y0, const double z0,
                        const double t0) {
                            
  double meanTime = 0.; double rmsTime = 0.; 
  DriftLine(x0, y0, z0, t0, meanTime, rmsTime, "hole");

}

void
DriftLineRKF::DriftIon(const double x0, const double y0, const double z0,
                       const double t0) {
                            
  double meanTime = 0.; double rmsTime = 0.; 
  DriftLine(x0, y0, z0, t0, meanTime, rmsTime, "ion");

}

void 
DriftLineRKF::DriftLine(double x0, double y0, double z0, double t0,
                        double& meanTime, double& rmsTime, std::string particleType) {

  if (usePlotting) {
    viewer->NewElectronDriftLine(1, iLine, x0, y0, z0);
  }

  // Check if the sensor is defined
  if (sensor == 0) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }
  
  // Check to make sure initial position is in a 
  // valid location ie. non zero field, 
  // in a drift medium.
  
  // Get field values
  double ex, ey, ez; 
  double bx, by, bz;
  int status;
  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if (status != 0) {
    std::cerr << className << "::DriftLine:\n";
    std::cerr << "    No valid field at initial position.\n";
    return;
  }

  // Numerical constants for RKF integration
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

  // Current position
  double r[3] = {x0, y0, z0};
  // Estimate for next step
  double r1[3] = {0., 0., 0.};
  // Initial velocity
  double v0[3] = {0., 0., 0.};
  // Velocities at mid-points
  double v1[3] = {0., 0., 0.};
  double v2[3] = {0., 0., 0.};
  double v3[3] = {0., 0., 0.};
  // Position where particle has crossed the trap radius of a wire
  double rc[3] = {0., 0., 0.}; 

  // Final velocity estimates
  double phi1[3], phi2[3];

  // Initialize particle velocity
  if (particleType == "electron") {
    if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v0[0], v0[1], v0[2])) {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    }
  } else if (particleType == "hole") {
    if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, v0[0], v0[1], v0[2])) {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    }
  } else if (particleType == "ion") {
    if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, v0[0], v0[1], v0[2])) {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    }
  }
  double vTot = sqrt(v0[0] * v0[0] + v0[1] * v0[1] + v0[2] * v0[2]);

  // Time step and previous time step
  double dt = intAccuracy / vTot;
  double pdt = 0.;
  
  // Count the number of steps
  int counter = 0;

  // Continue with the next step of drift if true
  bool keepGoing = true;

  path.clear();
  while (counter <= maxSteps && keepGoing) {
    step tempStep;
    path.push_back(tempStep);
    path[counter].xi = r[0];
    path[counter].yi = r[1];
    path[counter].zi = r[2];
    if (counter == 0) path[counter].ti = t0;
    else path[counter].ti = path[counter - 1].tf;

    // First estimate of new drift velocity
    r1[0] = r[0] + dt * b10 * v0[0];
    r1[1] = r[1] + dt * b10 * v0[1];
    r1[2] = r[2] + dt * b10 * v0[2];
    sensor->MagneticField(r1[0], r1[1], r1[2], bx, by, bz, status);
    sensor->ElectricField(r1[0], r1[1], r1[2], ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Drift line crossed wire. Abandoning.\n";
        path[counter].status = "Crossed Wire.";
        break;
      } else if (sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)) {
        DriftToWire(r1[0], r1[1], r1[2], particleType);
        break;
      } else {
        if (particleType == "electron") {
          if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v1[0], v1[1], v1[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "hole") {
          if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, v1[0], v1[1], v1[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "ion") {
          if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, v1[0], v1[1], v1[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        }
      }
    } else {
      EndDriftLine(particleType);
      break;
    }
    
    // Second estimate of new drift velocity
    r1[0] = r[0] + dt * (b20 * v0[0] + b21 * v1[0]);
    r1[1] = r[1] + dt * (b20 * v0[1] + b21 * v1[1]);
    r1[2] = r[2] + dt * (b20 * v0[2] + b21 * v1[2]);
    sensor->MagneticField(r1[0], r1[1], r1[2], bx, by, bz, status);
    sensor->ElectricField(r1[0], r1[1], r1[2], ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Drift line crossed wire. Abandoning.\n";
        path[counter].status = "Crossed Wire.";
        break;
      } else if (sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)) {
        DriftToWire(r1[0], r1[1], r1[2], particleType);
        break;
      } else {
        if (particleType == "electron") {
          if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v2[0], v2[1], v2[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "hole") {
          if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, v2[0], v2[1], v2[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "ion") {
          if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, v2[0], v2[1], v2[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        }
      }     
    } else {
      EndDriftLine(particleType);
      break;
    }
    
    // Third estimate of new drift velocity
    r1[0] = r[0] + dt * (b30 * v0[0] + b31 * v1[0] + b32 * v2[0]);
    r1[1] = r[1] + dt * (b30 * v0[1] + b31 * v1[1] + b32 * v2[1]);
    r1[2] = r[2] + dt * (b30 * v0[2] + b31 * v1[2] + b32 * v2[2]);   
    sensor->MagneticField(r1[0], r1[1], r1[2], bx, by, bz, status);
    sensor->ElectricField(r1[0], r1[1], r1[2], ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Drift line crossed wire. Abandoning.\n";
        path[counter].status = "Crossed Wire.";
        break;
      } else if(sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)){
        DriftToWire(r1[0], r1[1], r1[2], particleType);
        break;
      } else {
        if (particleType == "electron") {
          if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v3[0], v3[1], v3[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "hole") {
          if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, v3[0], v3[1], v3[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        } else if (particleType == "ion") {
          if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, v3[0], v3[1], v3[2])) {
            std::cerr << className << "::DriftLine:\n";
            std::cerr << "    Failed to retrieve drift velocity.\n";
            return;
          }
        }
      }
    } else {
      EndDriftLine(particleType);
      break;
    }
    // Calculate estimates of velocity over step
    phi1[0] = c10 * v0[0] + c11 * v1[0] + c12 * v2[0];
    phi1[1] = c10 * v0[1] + c11 * v1[1] + c12 * v2[1];
    phi1[2] = c10 * v0[2] + c11 * v1[2] + c12 * v2[2];
    
    phi2[0] = c20 * v0[0] + c22 * v2[0] + c23 * v3[0];
    phi2[1] = c20 * v0[1] + c22 * v2[1] + c23 * v3[1];
    phi2[2] = c20 * v0[2] + c22 * v2[2] + c23 * v3[2];
    // Check step length is valid
    double stepLength = sqrt(phi1[0] * phi1[0] + phi1[1] * phi1[1] + phi1[2] * phi1[2]);
    if (stepLength <= 0.0) {
      std::cerr << className << "::DriftLine:\n" 
                << "    Step length zero. Abandoning drift.\n";
      keepGoing = false;
    } else if (dt * stepLength > maxStepSize) {
      if (debug) {
        std::cout << className << "::DriftLine:\n" 
                  << "    Step length too long. Reducing time step.\n";
      }
      dt = 0.5 * maxStepSize / stepLength;
    } else {
      if (debug) {
        std::cout << className << "::DriftLine:\n" 
                  << "    Step good.\n";
      }
    }
    pdt = dt;
    // Update position
    r[0] += dt * phi1[0];
    r[1] += dt * phi1[1];
    r[2] += dt * phi1[2];

    path[counter].xf = r[0];
    path[counter].yf = r[1];
    path[counter].zf = r[2];
    path[counter].tf = path[counter].ti + dt;
 
    sensor->ElectricField(r[0], r[1], r[2], ex, ey, ez, medium, status);
    if (status != 0) {
      if (debug) std::cout << "    Outside bounds!\n";
      EndDriftLine(particleType);
      break;
    }
   
    // Adjust step size depending on accuracy
    if (phi1[0] != phi2[0] || phi1[1] != phi2[1] || phi1[2] != phi2[2]) {
      if (debug) {
        std::cout << className << "::DriftLine:\n" 
                  << "    Adapting step size.\n";
      }
      dt = sqrt(dt * intAccuracy / 
                (fabs(phi1[0] - phi2[0]) + fabs(phi1[1] - phi2[1]) + fabs(phi1[2] - phi2[2])));
    } else {
      if (debug) {
        std::cout << className << "::DriftLine:\n" 
                  << "    Increasing step size.\n";
      }
      dt *= 2.;
    }
    // Make sure that dt is different from zero; 
    // this should always be ok.
    if (dt <= 0.) {
      std::cerr << className << "::DriftLine:\n";
      std::cerr << "    Step size is zero (program bug).\n";
      std::cerr << "    The calculation is abandoned.\n";
      return;
    }
   
    // Prevent step size growing to fast
    if (dt > 10. * pdt) {
      dt = 10. * pdt;
    }

    // Stop in case dt tends to become too small.
    if (dt * (fabs(phi1[0]) + fabs(phi1[1]) + fabs(phi1[2])) < intAccuracy) {
      if (debug) {
        std::cerr << className << "::DriftLine:\n";
        std::cerr << "    Step size has become smaller than int. accuracy.\n";
        std::cerr << "    The calculation is abandoned.\n";
        return;
      }
    }

    // Update velocity
    v0[0] = v3[0];
    v0[1] = v3[1];
    v0[2] = v3[2];
    
    if (keepGoing && counter <= maxSteps) {
      path[counter].status = "alive";
    } else if (counter > maxSteps) {
       path[counter].status = "maxStep";
    } else {
       path[counter].status = "Abandoned";
    }
    // Increase counter (default counter max = 1000)
    counter++;
  }
  const int nSteps = path.size();
  // If the user specifies output step history
  if (verbose) {
    std::cout << "    Step #    time    Xi    Yi    Zi    dt    Status\n";
    for (int i = 0; i < nSteps; ++i) {
      std::cout.precision(8);
      std::cout << i << "    " << path[i].ti << "    "
                << path[i].xi << "    "
                << path[i].yi << "    "
                << path[i].zi << "    "
                << fabs(path[i].tf - path[i].ti) << "    "
                << path[i].status << "\n";
    }
    std::cout << path.size() - 1 << "    " << path.back().tf << "    "
              << path.back().xf << "    "
              << path.back().yf << "    "
              << path.back().zf << "    "
              << " ---     END"; 
  }
  for (int i = 0; i < nSteps; ++i) {
    if (usePlotting) {
      viewer->AddDriftLinePoint(iLine, 
                                path[i].xi, 
                                path[i].yi, 
                                path[i].zi);
    }
  }
  if (usePlotting) {
    viewer->AddDriftLinePoint(iLine, 
                              path.back().xf,
                              path.back().yf, 
                              path.back().zf);
  }
  
  // This should be done with in the while loop
  // calculating the rmsTime and setting meanTime
  for (int i = 0; i < nSteps; ++i) {
    rmsTime += IntegrateDiffusion(path[i].xi, path[i].yi, path[i].zi, 
                                  path[i].xf, path[i].yf, path[i].zf,
                                  particleType);
  }
  
  rmsTime = sqrt(rmsTime);
  meanTime = path.back().tf;

}

void
DriftLineRKF::DriftToWire(double x0, double y0, double z0, 
                          const std::string particleType) {

  if (debug) {
    std::cout << className << "::DriftToWire:\n";
    std::cout << "    Particle trapped by wire at: "
              << x0 << ", " << y0 << ", " << z0
              << " (r = " << sqrt(x0 * x0 + y0 * y0 + z0 * z0) << ")\n";
    std::cout << "    by wire located at (" << xWire << ", " 
              << yWire << ") with physical radius " 
              << rWire << " cm.\n";
  }
 
  bool lastStep = false;
  double timeToDrift = 0.;

  // Check to make sure initial position has non-zero field
  double ex, ey, ez;
  double bx, by, bz;
  int status;
  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if (status != 0) {
    std::cerr << className << "::DriftToWire:\n";
    std::cerr << "    Zero field at initial position.\n";
    std::cerr << "    Abandoning drift to wire.\n";
    path.back().status = "Zero field. Abandoned.";
    return;
  }
  
  // Estimate time to wire
  double vx0 = 0.;
  double vz0 = 0.;
  double vy0 = 0.;
  if (particleType == "electron") {
    if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::DriftToWire:\n";
      std::cerr << "    Unable to retrieve drift velocity.\n";
      return;
    }
  } else if (particleType == "hole") {
    if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::DriftToWire:\n";
      std::cerr << "    Unable to retrieve drift velocity.\n";
      return;
    }
  } else if (particleType == "ion") {
    if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::DriftToWire:\n";
      std::cerr << "    Unable to retrieve drift velocity.\n";
      return;
    }
  }    

  double speed0 = sqrt(vx0 * vx0 + vy0 * vy0 + vz0 * vz0);
  double dist2wire = sqrt(pow(xWire - x0, 2) + pow(yWire - y0, 2)) - rWire;
  double tCrude = dist2wire / speed0;

  // Check if tCrude is too small
  if (tCrude < 1.e-6) {
    path.back().xf = x0;
    path.back().yf = y0;
    path.back().zf = z0;
    path.back().tf = path.back().ti + tCrude;
    path.back().status = "Distance to wire too small.";
    return;
  }

  while (!lastStep) {
   
    // Estimate where the drift-line will end up
    double x1 = x0 + tCrude * vx0;
    double y1 = y0 + tCrude * vy0;
    double z1 = z0 + tCrude * vz0;
    if (debug) {
      std::cout << "    Step to wire: " << x1 << ", " << y1 << ", " << z1 << "\n";
    }
   
    // Check to make sure step is in a good location
    dist2wire = sqrt(pow(xWire - x1, 2) + pow(yWire - y1, 2)) - rWire;
    if (dist2wire < 0.) {
     
      if (debug) {
        std::cout << className << "::DriftToWire:\n";
        std::cout << "    Drift line inside wire. This may be the last step.\n";
      }
     
      lastStep = true;
   
      // Move the end point outside the wire.
      while (dist2wire < 0.) {
        tCrude *= 0.9999;
        x1 = x0 + tCrude * vx0;
        y1 = y0 + tCrude * vy0;
        z1 = z0 + tCrude * vz0;
        dist2wire = sqrt(pow(xWire - x1, 2) + pow(yWire - y1, 2)) - rWire;
      }
    }
   
    sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
    if (status != 0) {
      std::cerr << className << "::DriftToWire:\n";
      std::cerr << "    Zero field at step location (" 
                << x1 << ", " << y1 << ", " << z1 << "). Abandoning.\n";
      std::cerr << "    Status returned: " << status << ".\n";
      path.back().status = "Zero field. Abandoned.";
      return;
    }

    // Now calculate the drift velocity at this end point
    double vx1 = 0., vy1 = 0., vz1 = 0.;
    if (particleType == "electron") {
      if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        return;
      }
    } else if (particleType == "hole") {
      if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        return;
      }
    } else if (particleType == "ion") {
      if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        return;
      }
    }

    double speed1 = sqrt(vx1 * vx1 + vy1 * vy1 + vz1 * vz1);
   
    // Calculate a mid point between (x0, y0) and (x1, y1)
    double xm = 0.5 * (x0 + x1);
    double ym = 0.5 * (y0 + y1);
    double zm = 0.5 * (z0 + z1);

    // Check mid point location and find velocity
    sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
    sensor->ElectricField(xm, ym, zm, ex, ey, ez, medium, status);
    if (status != 0) {
      std::cerr << className << "::DriftToWire:\n";
      std::cerr << "    Zero field at step location (" 
                << xm << ", " << ym << ", " << zm << "). Abandoning.\n";
      path.back().status = "Zero field. Abandoned.";
      return;
    }
   
    // Now calculate the drift velocity at this mid point
    double vxm = 0., vym = 0., vzm = 0.;
    if (particleType == "electron") {
      if(!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)){
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
        return;
      }
    } else if (particleType == "hole") {
      if(!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)){
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
        return;
      }
    } else if (particleType == "ion") {
      if(!medium->IonVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)){
        std::cerr << className << "::DriftToWire:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
        return;
      }
    }
    
    double speedm = sqrt(vxm * vxm + vym * vym + vzm * vzm);
   
    // Compare the first and second order estimates
    double stepLength = sqrt(pow(x0 - x1, 2) + pow(y0 - y1, 2));
    if (stepLength * fabs(1. / speed0 - 2. / speedm + 1. / speed1) / 3. < 
        1.e-4 * (1. + path.back().ti)) {
      // Accuracy is good enough
      timeToDrift += stepLength * (1. / speed0 + 4. / speedm + 1. / speed1) / 6.;
      path.back().xf = x1;
      path.back().yf = y1;
      path.back().zf = z1;
      // Proceed to the next step
      x0 = x1;
      y0 = y1;
      z0 = z1;
      vx0 = vx1;
      vy0 = vy1;
      vz0 = vz1;
    } else {
      // Accuracy was not good enough so half the step time
      tCrude *= 0.5;
      lastStep = false;
    }
  }
  path.back().status = "Drifted to wire.";
  path.back().tf = path.back().ti + timeToDrift;

}

  void DriftLineRKF::GetEndPoint(double& xend, double& yend, double& zend, double& tend,
				 std::string& stat) const {
    const int nSteps = path.size();
    xend = path[nSteps-1].xi;
    yend = path[nSteps-1].yi;
    zend = path[nSteps-1].zi;
    tend = path[nSteps-1].ti;
    stat = path[nSteps-1].status;
  }
  
double 
DriftLineRKF::IntegrateDiffusion(const double x, const double y, const double z,
                                 const double xe, const double ye, const double ze,
                                 const std::string particleType) {

  if (debug) { 
    std::cout << "-----------------------------------------\n";
    std::cout << "Integrating diffusion over: ";
    std::cout << x  << ", " << y  << ", " << z  << " to " 
	      << xe << ", " << ye << ", " << ze <<"\n";
  }

  // Used to determine when the last integration step has been taken
  bool lastStep = false;

  // Store the total diffusion
  double dLrms = 0.;

  // Check to make sure initial position has non-zero field
  double ex, ey, ez;
  double bx, by, bz;
  int status;
  sensor->MagneticField(x, y, z, bx, by, bz, status);
  sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  if(status != 0) {
    std::cerr << className << "::IntegrateDiffusion:\n";
    std::cerr << "    Zero field at initial position. Abandoning.\n";
    return 0.;
  }
  
  // Determine drift velocity at init point 
  double vx0 = 0., vy0 = 0., vz0 = 0.;
  if (particleType == "electron") {
    if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
      path.back().status = "Abandoned";
    }
  } else if (particleType == "hole") {
    if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
      path.back().status = "Abandoned";
    }
  } else if (particleType == "ion") {
    if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
      path.back().status = "Abandoned";
    }  
  }
  double speed0 = sqrt(vx0 * vx0 + vy0 * vy0 + vz0 * vz0);
  
  // Determine diffusion at init point
  double dL0 = 0.;
  double dT0 = 0.;
  if (particleType == "electron") {
    if (!medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dL0, dT0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve diffusion.\n";
      return 0.;
    }
  } else if (particleType == "hole") {
    if (!medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dL0, dT0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve diffusion.\n";
      return 0.;
    }
  } else if (particleType == "ion") {
    if (!medium->IonDiffusion(ex, ey, ez, bx, by, bz, dL0, dT0)) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Unable to retrieve diffusion.\n";
      return 0.;  
    }
  }

  // Determine the initial step length
  double stepLength = sqrt(pow(x - xe, 2) + pow(y - ye, 2) + pow(z - ze, 2));
  if (debug) std::cout << "Step Length = " << stepLength <<"\n";
  // Check to see if initial step size is too small
  if (stepLength <= 1.e-6) {
    if (debug) {
      std::cout << className << "::IntegrateDiffusion:\n";
      std::cout << "    Initial step size too small.\n";
      std::cout << "    Using constant diffusion over step.\n";
    }
    return pow(dL0 / speed0, 2) * stepLength; 
  }
 
  double x0 = x;
  double y0 = y;
  double z0 = z;

  double x1 = xe;
  double y1 = ye;
  double z1 = ze;

  bool keepGoing = true;
  int stepCounter = 0;
  while (keepGoing) {
    stepCounter++;
    if (lastStep) keepGoing = false;
   
    sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
    sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
    if (status != 0) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Zero field at step location (" 
                << x1 << ", " << y1 << ", " << z1 << "). Abandoning.\n";
      std::cerr << "    Status returned: " << status << ".\n";
      return 0.;
    }
 
    // Determine drift velocity at init point 
    double vx1 = 0.;
    double vy1 = 0.;
    double vz1 = 0.;
    if (particleType == "electron") {
      if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }
    } else if (particleType == "hole") {
      if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }
    } else if (particleType == "ion") {
      if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }  
    }
    double speed1 = sqrt(vx1 * vx1 + vy1 * vy1 + vz1 * vz1);
   
    // Now calculate the diffusion at this end point
    double dL1 = 0.;
    double dT1 = 0.;
    if (particleType == "electron") {
      if (!medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dL1, dT1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;
      }
    } else if (particleType == "hole") {
      if (!medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dL1, dT1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;
      }
    } else if (particleType == "ion") {
      if (!medium->IonDiffusion(ex, ey, ez, bx, by, bz, dL1, dT1)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;  
      }
    }
   
    // Calculate a mid point between (x0, y0) and (x1, y1)
    double xm = 0.5 * (x0 + x1);
    double ym = 0.5 * (y0 + y1);
    double zm = 0.5 * (z0 + z1);
    // Check mid point location
    sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
    sensor->ElectricField(xm, ym, zm, ex, ey, ez, medium, status);
    if (status != 0) {
      std::cerr << className << "::IntegrateDiffusion:\n";
      std::cerr << "    Zero field at step location (" 
                << xm << ", " << ym << ", " << zm << "). Abandoning.\n";
      return 0.;
    }
  
    // Determine drift velocity at mid point 
    double vxm = 0.;
    double vym = 0.;
    double vzm = 0.;
    if (particleType == "electron") {
      if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }
    } else if (particleType == "hole") {
      if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }
    } else if (particleType == "ion") {
      if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve drift velocity. Abandoning.\n";
        path.back().status = "Abandoned";
      }  
    }
    double speedm = sqrt(vxm * vxm + vym * vym + vzm * vzm); 
    
    // Now calculate the diffusion at this mid point
    double dLm = 0.;
    double dTm = 0.;
    if (particleType == "electron") {
      if (!medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dLm, dTm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;
      }
    } else if (particleType == "hole") {
      if (!medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dLm, dTm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;
      }
    } else if (particleType == "ion") {
      if (!medium->IonDiffusion(ex, ey, ez, bx, by, bz, dLm, dTm)) {
        std::cerr << className << "::IntegrateDiffusion:\n";
        std::cerr << "    Unable to retrieve diffusion.\n";
        return 0.;  
      }
    }

    // Compare the trapezoidal estimate with the Simpsons
    double diffIntAcc = 1.e-3;
    const double sigma0 = pow(dL0 / speed0, 2);
    const double sigma1 = pow(dL1 / speed1, 2);
    const double sigmam = pow(dLm / speedm, 2); 
    const double simpson = stepLength *(sigma0 + 4. * sigmam + sigma1) / 6.;
    const double trapez = stepLength * (sigma0 + sigma1) / 2.;
    if (fabs(trapez - simpson) * sqrt(2. * stepLength / (sigma0 + sigma1)) / 6. < diffIntAcc) {
      // Accuracy is good enough
      dLrms += simpson;
      // Proceed to the next step
      x0 = x1;
      y0 = y1;
      z0 = z1;
      dL0 = dL1;
      dT0 = dT1;

      if (x0 == xe && y0 == ye && z0 == ze) {
        keepGoing = false;
        //std::cout<<"Reached end of step.\n";
        break;
      }
     
      double xn = xe - x0;
      double yn = ye - y0;
      double zn = ze - z0;

      double norm = sqrt(xn * xn + yn * yn + zn * zn);
      if (norm < 1.e-6) {
        if (debug) {
          std::cout << className << "::IntegrateDiffusion:\n"
                    << "    Step too small. Using constant diffusion over step.\n";
        }
        dLrms += pow(dL0 / speed0, 2) * stepLength;
        break;
      }
      xn /= norm;
      yn /= norm;
      zn /= norm;      

      x1 += stepLength * xn;
      y1 += stepLength * yn;
      z1 += stepLength * zn;

      const double dist2wire = sqrt(pow(xWire - x1, 2) + pow(yWire - y1, 2)) - rWire;
      if (dist2wire < rWire) {
        std::cout << "Inside Wire.\n";
        break;
      }
    } else {
      // Accuracy was not good enough so half the step time
      x1 = xm;
      y1 = ym;
      z1 = zm;
      dL1 = dLm;
      dT1 = dTm;
      stepLength = sqrt(pow(x0 - x1, 2) + pow(y0 - y1, 2) + pow(z0 - z1, 2));
    }
    //getchar()
  }
  const double totalStep = sqrt(pow(x - xe, 2) + pow(y - ye, 2) + pow(z - ze, 2));
  std::cout << "DLrms = " << dLrms << " Acquired over " << totalStep << " [cm] in  " << stepCounter << " steps.\n";
  return dLrms;

}  

void 
DriftLineRKF::EndDriftLine(const std::string particleType) {

  // These will store the original position for use later in time calculation
  double xp = path.back().xi;
  double yp = path.back().yi;
  double zp = path.back().zi;
  double x0 = xp, y0 = yp, z0 = zp;
  
  double bx, by, bz;
  double ex, ey, ez;  
  int status;
  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if (status != 0) {
    std::cerr << className << "::EndDriftLine:\n";
    std::cerr << "    No valid field at initial point.\n";
    std::cerr << "    Program bug!\n";
    return;
  }
  double vx, vy, vz;
  if (particleType == "electron") {
    if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz)) {
      std::cerr << className << "::EndDriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    
    }
  } else if (particleType == "hole") {
    if (!medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz)) {
      std::cerr << className << "::EndDriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    
    }
  } else if (particleType == "ion") {
    if (!medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz)) {
      std::cerr << className << "::EndDriftLine:\n";
      std::cerr << "    Failed to retrieve drift velocity.\n";
      return;
    
    }
  }
  double speed = sqrt(vx * vx + vy * vy + vz * vz);

  // x1, y1, z1 to store beginning of previous step for now.
  double x1 = xp, y1 = yp, z1 = zp;
  if (path.size() > 1) {
    x1 = path[path.size() - 2].xi;
    y1 = path[path.size() - 2].yi;
    z1 = path[path.size() - 2].zi;
  }

  // TODO: Do something for single point case. 
  double lastStepLength = sqrt(pow(fabs(x1 - x0), 2) +
                               pow(fabs(y1 - y0), 2) +
                               pow(fabs(z1 - z0), 2));
 
  x1 = x0;
  y1 = y0;
  z1 = z0;

  // Add steps sizes equal to the last step size until you leave the volume
  // steps added in same direction as previous step
  bool keepGoing = true;
  while (keepGoing) {
    sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
    // TODO: Check also if inside the drift area.
    if (status != 0) {
      keepGoing = false;
      break;
    }
    x1 += lastStepLength * vx / speed;
    y1 += lastStepLength * vy / speed;
    z1 += lastStepLength * vz / speed;
  }

  double x, y, z;
  for (int i = 0; i < 100; ++i) {
    x = x0 + 0.5 * (x1 - x0);
    y = y0 + 0.5 * (y1 - y0);
    z = z0 + 0.5 * (z1 - z0);
    sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
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
  
  // Add final step to path
  path.back().xf = x;
  path.back().yf = y;
  path.back().zf = z;
  path.back().tf = path.back().ti + fabs(sqrt(pow((x - xp), 2) + 
                                              pow((y - yp), 2) + 
                                              pow((z - zp), 2))) / speed; 
  path.back().status = "left volume";

}

}

