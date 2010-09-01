#include <iostream>
#include <math.h>

#include "DriftLineRKF.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

DriftLineRKF::DriftLineRKF() :
  sensor(0), medium(0),
  minErr(0.), maxErr(0.01), timeStep(0.1),
  usePlotting(false), viewer(0), debug(false) {
   
}

void 
DriftLineRKF::SetErrorBounds(const double min, const double max) {

  if (min > max) {
    std::cerr << "DriftLineRKF::SetErrorBounds:\n";
    std::cerr << "    Min error bound greater then max error bound.\n";
    std::cerr << "    Flipping values.\n";
    maxErr = min;
    minErr = max;
  } else if (min == max) {
    std::cerr << "DriftLineRKF::SetErrorBounds:\n";
    std::cerr << "    Min error bound equals max error bound.\n";
  } else {
    minErr = min;
    maxErr = max;
  }

}

void 
DriftLineRKF::SetSensor(Sensor* s) {
    
  if (s == 0) {
    std::cerr << "DriftLineRKF::SetSensor:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  sensor = s;

}

void
DriftLineRKF::SetTimeStep(const double dt) {

  if (dt <= 0.) {
    std::cerr << "DriftLineRKF::SetTimeStep:\n";
    std::cerr << "    Time step must be greater than zero.\n";
    return;
  }

  timeStep = dt;

}

void
DriftLineRKF::EnablePlotting(ViewDrift* view) {

  if (view == 0) {
    std::cerr << "DriftLineRKF::EnablePlotting:\n";
    std::cerr << "    Viewer is not defined.\n";
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

bool
DriftLineRKF::EquationOfMotion(
      const double x0, const double y0, const double z0, 
      const double vx, const double vy, const double vz, const double t, 
      double& kx, double& ky, double& kz, const double qom) {

  const double x = x0 + t * vx;
  const double y = y0 + t * vy;
  const double z = z0 + t * vz;
    
  double ex, ey, ez; 
  double bx, by, bz;
  int status;

  sensor->MagneticField(x, y, z, bx, by, bz, status);
  sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);

  if (debug) {
    std::cout << "    B-field:\t(" << bx << ", " << by << ", " << bz << ")\n";
    std::cout << "    E-field:\t(" << ex << ", " << ey << ", " << ez << ")\n";
  }

  kx = qom * ((vy * bz - vz * by) + ex);
  ky = qom * ((vz * bx - vx * bz) + ey);
  kz = qom * ((vx * by - vy * bx) + ez);

  if (status != 0) {
    if (debug) {
      std::cerr << "DriftLineRKF::EquationOfMotion:" << std::endl;
      std::cerr << "    Error receiving field.\n";
      std::cerr << "    Status:\t" << status << "\n";
    }
    return false;
  }

  return true;

}

void 
DriftLineRKF::DriftLine(double x0, double y0, double z0, double t0,
                        double e0, double dx, double dy, double dz,
                        std::string particleType) {

  // Check if the sensor is defined
  if (sensor == 0) {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  // Dependent and independent variables of ODE
  double r[3] = {x0, y0, z0};
  double v[3] = {dx, dy, dz};
  double vAdd[3] = {0., 0., 0.};
  double t = t0;

  // Normalize the direction
  double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d <= Small) {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    Initial direction is zero.\n";
    return;
  }
  v[0] /= d; v[1] /= d; v[2] /= d;

  double qom;
  // Calculate charge-mass ratio and velocity
  if (particleType.compare("e-") == 0) {
    qom = -SpeedOfLight * SpeedOfLight / ElectronMass;
    d = SpeedOfLight * sqrt(2. * e0 / ElectronMass);
  } else if (particleType.compare("e+") == 0){
    qom =  SpeedOfLight * SpeedOfLight / ElectronMass;
    d = SpeedOfLight * sqrt(2. * e0 / ElectronMass);
  } else if (particleType.compare("p+") == 0) {
    qom =  SpeedOfLight * SpeedOfLight / ProtonMass;
    d = SpeedOfLight * sqrt(2. * e0 / ProtonMass);
  } else if (particleType.compare("p-") == 0) {
    qom = -SpeedOfLight * SpeedOfLight / ProtonMass;
    d = SpeedOfLight * sqrt(2. * e0 / ProtonMass);
  } else if (particleType.compare("u+") == 0) {
    qom =  SpeedOfLight * SpeedOfLight / MuonMass;
    d = SpeedOfLight * sqrt(2. * e0 / MuonMass);
  } else if (particleType.compare("u-") == 0) {
    qom = -SpeedOfLight * SpeedOfLight / MuonMass;
    d = SpeedOfLight * sqrt(2. * e0 / MuonMass);
  } else {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    Error setting charge to mass ratio.\n";
    std::cerr << "    Unknown particle type. Using electron as default.\n"; 
    qom = -SpeedOfLight * SpeedOfLight / ElectronMass;
    d = SpeedOfLight * sqrt(2. * e0 / ElectronMass);
  }
  v[0] *= d; v[1] *= d; v[2] *= d;

  // RKF coefficients
  double k1[3], k2[3], k3[3], k4[3], k5[3], k6[3];

  // Time step and previous time step
  double dt = timeStep;
  double pdt = 0.;
  double temp = 0.;

  // 4th order current and previous approx.
  double prev4th[3] = {v[0], v[1], v[2]};
  double cur4th[3] = {0., 0., 0.};
  // 5th order current and previous approx.
  double prev5th[3] = {v[0], v[1], v[2]};
  double cur5th[3] = {0., 0., 0.};
  // Store calculation of error
  double error;

  if (debug) {
    std::cout << "DriftLineRKF::DriftLine:" << std::endl;
    std::cout << "    Initial position:\t(" 
              << r[0] << ", " << r[1] << ", " << r[2] << ")\n";
    std::cout << "    Initial velocity:\t(" 
              << v[0] << ", " << v[1] << ", " <<v[2] << ")\n";
  }

  int nPoints = 0;
  if (usePlotting) {
    viewer->NewElectronDriftLine(1, iLine);
    viewer->SetPoint(iLine, nPoints, r[0], r[1], r[2]);
    ++nPoints;
  }
 
  int counter = 0;
  bool keepGoing = true;
  bool accuracyGood = false;

  while (keepGoing) {
    counter = 0;
    accuracyGood = false;
    while (!accuracyGood) {
      if (debug) getchar();
      // 0th order calculation
      EquationOfMotion(r[0], r[1], r[2], v[0], v[1], v[2], 
                       0., k1[0], k1[1], k1[2], qom);
      k1[0] *= dt; k1[1] *= dt; k1[2] *= dt;

      // 1st order calculation
      vAdd[0] = v[0] + k1[0] / 4.;
      vAdd[1] = v[1] + k1[1] / 4.;
      vAdd[2] = v[2] + k1[2] / 4.;
      EquationOfMotion(r[0], r[1], r[2], vAdd[0], vAdd[1], vAdd[2], 
                       dt / 4., k2[0], k2[1], k2[2], qom);
      k2[0] *= dt; k2[1] *= dt; k2[2] *= dt;

      // 2nd order calculation
      vAdd[0] = v[0] + 3. * k1[0] / 32. + 9. * k2[0] / 32.;
      vAdd[1] = v[1] + 3. * k1[1] / 32. + 9. * k2[1] / 32.;
      vAdd[2] = v[2] + 3. * k1[2] / 32. + 9. * k2[2] / 32.;
      EquationOfMotion(r[0], r[1], r[2], vAdd[0], vAdd[1], vAdd[2], 
                       3. * dt / 8., 
                       k3[0], k3[1], k3[2], qom);
      k3[0] *= dt; k3[1] *= dt; k3[2] *= dt;

      // 3rd order calculation
      vAdd[0] = v[0] + (1932. * k1[0] - 7200. * k2[0] + 7296. * k3[0]) / 2197.;
      vAdd[1] = v[1] + (1932. * k1[1] - 7200. * k2[1] + 7296. * k3[1]) / 2197.;
      vAdd[2] = v[2] + (1932. * k1[2] - 7200. * k2[2] + 7296. * k3[2]) / 2197.;
      EquationOfMotion(r[0], r[1], r[2], vAdd[0], vAdd[1], vAdd[2],
                       12. * dt / 13., 
                       k4[0], k4[1], k4[2], qom);
      k4[0] *= dt; k4[1] *= dt; k4[2] *= dt;

      // 4th order calculation
      vAdd[0] = v[0] +  439. * k1[0] / 216. -   8. * k2[0] + 
                       3680. * k3[0] / 513. - 845. * k4[0] / 4104.;
      vAdd[1] = v[1] +  439. * k1[1] / 216. -   8. * k2[1] +
                       3680. * k3[1] / 513. - 845. * k4[1] / 4104.;
      vAdd[2] = v[2] +  439. * k1[2] / 216. -   8. * k2[2] +
                       3680. * k3[2] / 513. - 845. * k4[2] / 4104.;
      EquationOfMotion(r[0], r[1], r[2], vAdd[0], vAdd[1], vAdd[2], 
                       dt, k5[0], k5[1], k5[2], qom);
      k5[0] *= dt; k5[1] *= dt; k5[2] *= dt;

      // 5th order calculation
      vAdd[0] = v[0] - 8. * k1[0] / 27.   +    2. * k2[0] - 
                    3544. * k3[0] / 2565. + 1859. * k4[0] / 4104. - 
                      11. * k5[0] / 40.;
      vAdd[1] = v[1] - 8. * k1[1] / 27.   +    2. * k2[1] - 
                    3544. * k3[1] / 2565. + 1859. * k4[1] / 4104. - 
                      11. * k5[1] / 40.;
      vAdd[2] = v[2] - 8. * k1[2] / 27.   +    2. * k2[2] - 
                    3544. * k3[2] / 2565. + 1859. * k4[2] / 4104. - 
                      11. * k5[2] / 40.;
      EquationOfMotion(r[0], r[1], r[2], vAdd[0], vAdd[1], vAdd[2], 
                       dt / 2., k6[0], k6[1], k6[2], qom);
      k6[0] *= dt; k6[1] *= dt; k6[2] *= dt;

      // Calculate 4th and 5th order approximations
      cur4th[0] = prev4th[0] + 25. * k1[0] / 216. + 1408. * k3[0] / 2565. + 
                             2197. * k4[0] / 4104. - k5[0] / 5.;
      cur4th[1] = prev4th[1] + 25. * k1[1] / 216. + 1408. * k3[1] / 2565. + 
                             2197. * k4[1] / 4104. - k5[1] / 5.;
      cur4th[2] = prev4th[2] + 25. * k1[2] / 216. + 1408. * k3[2] / 2565. + 
                             2197. * k4[2] / 4104. - k5[2] / 5.;

      cur5th[0] = prev5th[0] + 16. * k1[0] / 135. + 6656. * k3[0] / 12825. + 
                            28561. * k4[0] / 56430. -  9. * k5[0] / 50. + 
                                2. * k6[0] / 55.;
      cur5th[1] = prev5th[1] + 16. * k1[1] / 135. + 6656. * k3[1] / 12825. + 
                            28561. * k4[1] / 56430. -  9. * k5[1] / 50. + 
                                2. * k6[1] / 55.;
      cur5th[2] = prev5th[2] + 16. * k1[2] / 135. + 6656. * k3[2] / 12825. + 
                            28561. * k4[2] / 56430. -  9. * k5[2] / 50. + 
                                2. * k6[2] / 55.;

      // Calculate error
      error = sqrt(pow(cur4th[0] - cur5th[0], 2) + 
                   pow(cur4th[1] - cur5th[1], 2) + 
                   pow(cur4th[2] - cur5th[2], 2));
      if(debug) {
        std::cout << "    Iteration " << counter + 1 << "\n";
        std::cout << "      Time step:\t" << dt <<" ns\n";
        std::cout << "      4th order estimate: " 
                  << cur4th[0] << "\t" << cur4th[1] << "\t" 
                  << cur4th[2] << "\n";
        std::cout << "      5th order estimate: "
                  << cur5th[0] << "\t" << cur5th[1] << "\t" 
                  << cur5th[2] << "\n";
        std::cout << "      Error:\t" << error <<"\n";
        std::cout << "      Bounds:\t" << minErr <<" -> " << maxErr <<"\n";
      }

      // Check if estimate is within error bounds
      if (error == 0.0) {
        accuracyGood = true;
        dt *= 2.;
        if (debug) std::cout << "    Error is 0. Increase time step.\n";
      } else if (error < minErr) {
        accuracyGood = true;
        if (pdt == dt * 2) {
          temp = (dt + pdt) / 2.;
          pdt = dt;
          dt = temp;
        } else {
          pdt = dt;
          dt *= 2.;
        }
        if (debug) std::cout << "    Error too small. Increase time step.\n";
      } else if (error > maxErr) {
        accuracyGood = false;
        if (pdt == dt / 2.) {
          temp = (dt + pdt) / 2.;
          pdt = dt;
          dt = temp;
        } else {
          pdt = dt;
          dt /= 2.;
        }
        if (debug) std::cout << "    Error too large. Reduce time step.\n";
      } else {
        accuracyGood = true;
        if (debug) std::cout << "    Error within bounds.\n";
      }

      ++counter;
      if (counter >= 1000) {
        accuracyGood = true;
        keepGoing = false;
        std::cerr << "DriftLineRKF::DriftLine:\n";
        std::cerr << "    Could not find step size suitable "
                  << " for error bounds.\n";
        std::cerr << "    Try increasing/decreasing the allowed error.\n"; 
        return;
      }
    }

    // Update the velocities and coordinates   
    v[0] = cur5th[0];
    v[1] = cur5th[1];
    v[2] = cur5th[2];
	    
    r[0] += v[0] * dt;
    r[1] += v[1] * dt;
    r[2] += v[2] * dt;

    t += dt;
    
    if (debug) {
      std::cout << "    New velocity:\t("
                << v[0] << ", " << v[1] << ", " << v[2] << ") = "
                << sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]) << "\n"; 
      std::cout << "    New position:\t("
                << r[0] << ", " << r[1] <<", " << r[2] << ")\n";
    }

    // Make sure the new point is inside the geometry
    if (!sensor->IsInArea(r[0], r[1], r[2])) { 
      if (debug) std::cout << "    Outside bounds!\n";
      keepGoing = false;
      break;
    }
   
    if (usePlotting) {
      viewer->SetPoint(iLine, nPoints, r[0], r[1], r[2]);
      ++nPoints;
    }

    prev4th[0] = cur4th[0]; prev5th[0] = cur5th[0];
    prev4th[1] = cur4th[1]; prev5th[1] = cur5th[1];
    prev4th[2] = cur4th[2]; prev5th[2] = cur5th[2];

  }
    
}
  
}

