#include <iostream>
#include <fstream>
#include <cmath>

#include "Sensor.hh"
#include "FundamentalConstants.hh"
#include "Plotting.hh"

namespace Garfield {

double Sensor::signalConversion = ElementaryCharge * 1.e9;

Sensor::Sensor() :
  nComponents(0), lastComponent(-1), 
  nElectrodes(0),
  nTimeBins(200),
  tStart(0.), tStep(10.),
  nEvents(0),
  xMin(0.), yMin(0.), zMin(0.),
  xMax(0.), yMax(0.), zMax(0),
  hasUserArea(false),
  xMinUser(0.), yMinUser(0.), zMinUser(0.), 
  xMaxUser(0.), yMaxUser(0.), zMaxUser(0.),
  debug(false) {
    
  components.clear();
  electrodes.clear();
  
}

void 
Sensor::ElectricField(const double x, const double y, const double z, 
                      double& ex, double& ey, double& ez, double& v, 
                      Medium*& medium, int& status) {
  
  ex = ey = ez = v = 0.;
  status = -10;
  medium = 0;
  double fx, fy, fz, p;
  Medium* med = 0;
  int stat;
  // Add up electric field contributions from all components
  for (int i = nComponents; i--;) {
    components[i].comp->ElectricField(x, y, z, fx, fy, fz, p, med, stat);
    if (status != 0) {
      status = stat;
      medium = med;
    }
    ex += fx; ey += fy; ez += fz;
    v += p;
  }

}

void
Sensor::ElectricField(const double x, const double y, const double z, 
                      double& ex, double& ey, double& ez, 
                      Medium*& medium, int& status) {
  
  ex = ey = ez = 0.; 
  status = -10;
  medium = 0;
  double fx, fy, fz;
  Medium* med = 0;
  int stat;
  // Add up electric field contributions from all components
  for (int i = nComponents; i--;) {
    components[i].comp->ElectricField(x, y, z, fx, fy, fz, med, stat);
    if (status != 0) {
      status = stat;
      medium = med;
    }
    ex += fx; ey += fy; ez += fz;
  }
}

void 
Sensor::MagneticField(const double x, const double y, const double z, 
                      double& bx, double& by, double& bz, int& status) {

  bx = by = bz = 0.;
  double fx, fy, fz;
  // Add up contributions
  for (int i = nComponents; i--;) {
    components[i].comp->MagneticField(x, y, z, fx, fy, fz, status);
    if (status != 0) continue;
    bx += fx; by += fy; bz += fz;
  } 

}

bool 
Sensor::GetMedium(const double x, const double y, const double z,
                  Medium*& m) {

  // Check if we are still in the same component as in the previous call
  if (lastComponent < 0) return false;

  if (components[lastComponent].comp->GetMedium(x, y, z, m)) {
    return true;
  }

  for (int i = nComponents; i--;) {
    if (components[i].comp->GetMedium(x, y, z, m)) {
      lastComponent = i;
      return true;
    }
  }

  return false;

}

bool 
Sensor::SetArea() {

  if (!GetBoundingBox(xMinUser, yMinUser, zMinUser, 
                      xMaxUser, yMaxUser, zMaxUser)) {
    std::cerr << "Sensor::SetArea:" << std::endl;
    std::cerr << "    Bounding box is not known." << std::endl;
    return false;
  }
  
  std::cout << "Sensor::SetArea:" << std::endl;
  std::cout << "    " << xMinUser << " < x [cm] < " << xMaxUser << std::endl;
  std::cout << "    " << yMinUser << " < y [cm] < " << yMaxUser << std::endl;
  std::cout << "    " << zMinUser << " < z [cm] < " << zMaxUser << std::endl;
  return true;

}

bool 
Sensor::SetArea(const double xmin, const double ymin, const double zmin,
                const double xmax, const double ymax, const double zmax) {

  if (fabs(xmax - xmin) < Small || 
      fabs(ymax - ymin) < Small || 
      fabs(zmax - zmin) < Small) {
    std::cerr << "AvalancheMicroscopic::SetArea:" << std::endl;
    std::cerr << "    Invalid range." << std::endl;
    return false;
  }

  xMinUser = xmin; yMinUser = ymin; zMinUser = zmin;
  xMaxUser = xmax; yMaxUser = ymax; zMaxUser = zmax;
  
  if (xmin > xmax) {
    xMinUser = xmax;
    xMaxUser = xmin;
  }
  if (ymin > ymax) {
    yMinUser = ymax;
    yMaxUser = ymin;
  }
  if (zmin > zmax) {
    zMinUser = zmax;
    zMaxUser = zmin;
  }
  hasUserArea = true;
  return true;

}


bool 
Sensor::GetArea(double& xmin, double& ymin, double& zmin,
                double& xmax, double& ymax, double& zmax) {
               
  if (hasUserArea) {
    xmin = xMinUser; ymin = yMinUser; zmin = zMinUser;
    xmax = xMaxUser; ymax = yMaxUser; zmax = zMaxUser;
    return true;
  }
    
  std::cerr << "Sensor::GetArea:" << std::endl;
  std::cerr << "    User area bounds are not yet defined." << std::endl;
  xmin = ymin = zmin = 0.;
  xmax = ymax = zmax = 0.;
  return false;
    
}

bool 
Sensor::IsInArea(const double x, const double y, const double z) {
 
  if (!hasUserArea) {
    if (!SetArea()) {
      std::cerr << "Sensor::IsInArea:" << std::endl;
      std::cerr << "    User area is not known." << std::endl;
      return false;
    }
    hasUserArea = true;
  }
  
  if (x >= xMinUser && x <= xMaxUser &&
	  y >= yMinUser && y <= yMaxUser &&
	  z >= zMinUser && z <= zMaxUser) {
    if (debug) {
      std::cout << "Sensor::IsInArea: " << std::endl;
      std::cout << "    (" << x << ", " << y << ", " << z << ") "
                << " is inside." << std::endl;
    }
    return true;
  } 
    
  if (debug) {
    std::cout << "Sensor::IsInArea: " << std::endl;
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is outside." << std::endl;
  }

  return false;

}

void
Sensor::AddComponent(ComponentBase* comp) {

  if (comp == 0) {
    std::cerr << "Sensor::AddComponent:" << std::endl;
    std::cerr << "    Component is not defined." << std::endl;
    return;
  }

  component newComponent;
  newComponent.comp = comp;
  components.push_back(newComponent);
  ++nComponents; 
  if (nComponents == 1) lastComponent = 0; 

}

void
Sensor::AddElectrode(ComponentBase* comp, std::string label) {

  if (comp == 0) {
    std::cerr << "Sensor::AddElectrode:" << std::endl;
    std::cerr << "    Component is not defined." << std::endl;
    return;
  }

  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) {
      std::cout << "Sensor::AddElectrode:" << std::endl;
      std::cout << "    Warning: An electrode with label " 
                << label << " exists already." << std::endl;
      std::cout << "    Weighting fields will be summed up." << std::endl;
      break;
    }
  }
  
  electrode newElectrode;
  newElectrode.comp = comp;
  newElectrode.label = label;
  electrodes.push_back(newElectrode);
  ++nElectrodes;
  electrodes[nElectrodes - 1].signal.resize(nTimeBins);
  std::cout << "Sensor::AddElectrode:" << std::endl;
  std::cout << "    Added readout electrode " << label << "." << std::endl;
  std::cout << "    All signals are reset." << std::endl;
  ClearSignal();

}

void 
Sensor::Clear() {

  components.clear();
  nComponents = 0;
  lastComponent = -1;
  electrodes.clear();
  nElectrodes = 0;
  nTimeBins = 200;
  tStart = 0.;
  tStep = 10.;  
  nEvents = 0;
  hasUserArea = false;

}

bool 
Sensor::GetVoltageRange(double& vmin, double& vmax) {

  // We don't know the range yet
  bool set = false;
  // Loop over the fields
  double umin, umax;
  for (int i = 0; i < nComponents; ++i) {
    if (!components[i].comp->GetVoltageRange(umin, umax)) continue;
    if (set) {
      if (umin < vmin) vmin = umin;
      if (umax > vmax) vmax = umax;
    } else {
      vmin = umin;
      vmax = umax;
      set = true;
    }
  }
  
  // Warn if we still don't know the range
  if (!set) {
    std::cerr << "Sensor::GetVoltageRange:" << std::endl;
    std::cerr << "    Sensor voltage range not known." << std::endl;
    vmin = vmax = 0.;
    return false;
  }  

  // Debugging
  if (debug) {
    std::cout << "Sensor::GetVoltageRange:" << std::endl;
    std::cout << "    Voltage range " << vmin 
              << " < V < " << vmax << "." << std::endl;
  }
  
  return true;

}

void
Sensor::ClearSignal() {

  for (int i = nElectrodes; i--;) {
    electrodes[i].charge = 0.;
    for (int j = nTimeBins; j--;) electrodes[i].signal[j] = 0.;
  }
  nEvents = 0;

}

void 
Sensor::AddSignal(const int q, const double t, const double dt,
                  const double x,  const double y,  const double z,
                  const double vx, const double vy, const double vz) {
  
  // Get the time bin
  const int bin = int((t - tStart) / tStep);
  // Check if the starting time is outside the range 
  if (bin < 0 || bin >= nTimeBins) return;
  if (dt <= 0.) return;
  if (nEvents <= 0) ++nEvents;
  
  double wx = 0., wy = 0., wz = 0.;
  double cur, delta;
  if (debug) {
    std::cout << "Sensor::AddSignal:" << std::endl;
    std::cout << "    Time: " << t << std::endl;
    std::cout << "    Step: " << dt << std::endl;
    std::cout << "    Charge: " << q << std::endl;
    std::cout << "    Velocity: (" 
              << vx << ", " << vy << ", " << vz << ")" << std::endl;
  }
  for (int i = nElectrodes; i--;) {
    // Calculate the weighting field for this electrode
    electrodes[i].comp->WeightingField(x, y, z, wx, wy, wz, 
                                       electrodes[i].label);
    // Calculate the induced current
    cur = q * (wx * vx + wy * vy + wz * vz);
    if (debug) {
      std::cout << "    Electrode " << electrodes[i].label << ":" << std::endl;
      std::cout << "      Weighting field: (" 
                << wx << ", " << wy << ", " << wz << ")" << std::endl;
      std::cout << "      Induced charge: " << cur * dt << std::endl;
    }
    delta = tStart + (bin + 1) * tStep - t;    
    // Check if the provided timestep extends over more than one time bin
    if (dt > delta) {
      electrodes[i].signal[bin] += cur * delta; 
      delta = dt - delta;
      int j = 1;
      while (delta > tStep && bin + j < nTimeBins) {
        electrodes[i].signal[bin + j] += cur * tStep;
        delta -= tStep;
        ++j;
      }
      if (bin + j < nTimeBins) electrodes[i].signal[bin + j] += cur * delta;
    } else {
      electrodes[i].signal[bin] += cur * dt;
    }
  }

}

void
Sensor::AddInducedCharge(const int q, 
                         const double x0, const double y0, const double z0,
                         const double x1, const double y1, const double z1) {

  if (debug) std::cout << "Sensor::AddInducedCharge:" << std::endl;
  double w0 = 0., w1 = 0.;
  for (int i = nElectrodes; i--;) {
    // Calculate the weighting potential for the starting point
    w0 = electrodes[i].comp->WeightingPotential(x0, y0, z0, 
                                                electrodes[i].label);
    // Calculate the weighting potential for the end point
    w1 = electrodes[i].comp->WeightingPotential(x1, y1, z1,
                                                electrodes[i].label);
    electrodes[i].charge += q * (w1 - w0);
    if (debug) {
      std::cout << "    Electrode " << electrodes[i].label << ":" << std::endl;
      std::cout << "      Weighting potential at (" 
                << x0 << ", " << y0 << ", " << z0 << "): " << w0 << std::endl;
      std::cout << "      Weighting potential at ("
                << x1 << ", " << y1 << ", " << z1 << "): " << w1 << std::endl;
      std::cout << "      Induced charge: " 
                << electrodes[i].charge << std::endl;
    }
  }

}

void 
Sensor::SetTimeWindow(const double tstart, const double tstep, 
                      const int nsteps) {

  tStart = tstart;
  if (tstep <= 0.) {
    std::cerr << "Sensor::SetTimeWindow:" << std::endl;
    std::cerr << "    Starting time out of range." << std::endl;
  } else {
    tStep = tstep;
  }
  
  if (nsteps <= 0) {
    std::cerr << "Sensor::SetTimeWindow:" << std::endl;
    std::cerr << "    Number of time bins out of range." << std::endl;
  } else {
    nTimeBins = nsteps;
  }
  
  if (debug) {
    std::cout << "Sensor::SetTimeWindow:" << std::endl;
    std::cout << "    " << tStart << " < t [ns] < " 
              << tStart + nTimeBins * tStep << std::endl;
    std::cout << "    Step size: " << tStep << " ns" << std::endl;
  }
 
  std::cout << "Sensor::SetTimeWindow:" << std::endl;
  std::cout << "    Resetting all signals." << std::endl; 
  for (int i = nElectrodes; i--;) {
    electrodes[i].signal.clear();
    electrodes[i].signal.resize(nTimeBins);
  }
  nEvents = 0;

}

double 
Sensor::GetSignal(const std::string label, const int bin) {

  if (nEvents <= 0) return 0.;
  if (bin < 0 || bin >= nTimeBins) return 0.;
  double sig = 0.;
  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) sig += electrodes[i].signal[bin];
  }
  if (debug) {
    std::cout << "Sensor::GetSignal:" << std::endl;
    std::cout << "    Electrode: " << label << std::endl;
    std::cout << "    Bin: " << bin << std::endl;
    std::cout << "    Signal: " << sig / tStep;
  }
  return signalConversion * sig / (nEvents * tStep);

}

double
Sensor::GetInducedCharge(const std::string label) {

  if (nEvents <= 0) return 0.;
  double charge = 0.;
  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) charge += electrodes[i].charge;
  }
  if (debug) {
    std::cout << "Sensor::GetInducedCharge:" << std::endl;
    std::cout << "    Electrode: " << label << std::endl;
    std::cout << "    Charge: " << charge / tStep;
  }

  return charge / nEvents;

}

void 
Sensor::PlotSignal(const std::string label) {

  plottingEngine->PlotSignal(this, label);

}

bool 
Sensor::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                       double& xmax, double& ymax, double& zmax) {

  // We don't know the range yet
  bool set = false;
  // Loop over the fields
  double x0, y0, z0, x1, y1, z1;
  for (int i = nComponents; i--;) {
    if (!components[i].comp->GetBoundingBox(x0, y0, z0, x1, y1, z1)) continue;
    if (set) {
      if (x0 < xmin) xmin = x0;
      if (y0 < ymin) ymin = y0;
      if (z0 < zmin) zmin = z0;
      if (x1 > xmax) xmax = x1;
      if (y1 > ymax) ymax = y1;
      if (z1 > zmax) zmax = z1;
    } else {
      xmin = x0; ymin = y0; zmin = z0;
      xmax = x1; ymax = y1; zmax = z1;
      set = true;
    }
  }

  // Warn if we still don't know the range
  if (!set) {
    std::cerr << "Sensor::GetBoundingBox:" << std::endl;
    std::cerr << "    Sensor bounding box not known." << std::endl;
    xmin = 0.; ymin = 0.; zmin = 0.;
    xmax = 0.; ymax = 0.; zmax = 0.;
    return false;
  } 
  
  if (debug) {
    std::cout << "Sensor::GetBoundingBox:" << std::endl;
    std::cout << "    " << xmin << " < x [cm] < " << xmax << std::endl;
    std::cout << "    " << ymin << " < y [cm] < " << ymax << std::endl;
    std::cout << "    " << zmin << " < z [cm] < " << zmax << std::endl;
  }
  return true;
  
}
  
}
