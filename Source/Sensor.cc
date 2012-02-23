#include <iostream>
#include <fstream>
#include <cmath>

#include "Sensor.hh"
#include "GarfieldConstants.hh"
#include "Plotting.hh"
#include "Numerics.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

double Sensor::signalConversion = ElementaryCharge;

Sensor::Sensor() :
  nComponents(0), lastComponent(-1), 
  nElectrodes(0),
  nTimeBins(200), tStart(0.), tStep(10.),
  nEvents(0),
  hasTransferFunction(false), fTransfer(0),
  hasNoiseFunction(false), fNoise(0),
  nThresholdCrossings(0),
  xMin(0.), yMin(0.), zMin(0.),
  xMax(0.), yMax(0.), zMax(0),
  hasUserArea(false),
  xMinUser(0.), yMinUser(0.), zMinUser(0.), 
  xMaxUser(0.), yMaxUser(0.), zMaxUser(0.),
  debug(false) {
  
  className = "Sensor";
  
  components.clear();
  electrodes.clear();
  thresholdCrossings.clear();
  
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
  // Add up electric field contributions from all components.
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
  // Add up electric field contributions from all components.
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
  // Add up contributions.
  for (int i = nComponents; i--;) {
    components[i].comp->MagneticField(x, y, z, fx, fy, fz, status);
    if (status != 0) continue;
    bx += fx; by += fy; bz += fz;
  } 

}

void 
Sensor::WeightingField(const double x, const double y, const double z, 
                       double& wx, double& wy, double& wz, 
                       const std::string label) {
  
  wx = wy = wz = 0.;
  double fx = 0., fy = 0., fz = 0.;
  // Add up field contributions from all components.
  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) {
      fx = fy = fz = 0.;
      electrodes[i].comp->WeightingField(x, y, z, fx, fy, fz, label);
    }
    wx += fx; wy += fy; wz += fz;
  }

}

bool 
Sensor::GetMedium(const double x, const double y, const double z,
                  Medium*& m) {

  m = 0;

  // Make sure there is at least one component.
  if (lastComponent < 0) return false;

  // Check if we are still in the same component as in the previous call.
  if (components[lastComponent].comp->GetMedium(x, y, z, m)) {
    // Cross-check that the medium is defined.
    if (m) return true;
  }

  for (int i = nComponents; i--;) {
    if (components[i].comp->GetMedium(x, y, z, m)) {
      // Cross-check that the medium is defined.
      if (m) {
        lastComponent = i;
        return true;
      }
    }
  }
  return false;

}

bool 
Sensor::SetArea() {

  if (!GetBoundingBox(xMinUser, yMinUser, zMinUser, 
                      xMaxUser, yMaxUser, zMaxUser)) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Bounding box is not known.\n";
    return false;
  }
  
  std::cout << className << "::SetArea:\n";
  std::cout << "    " << xMinUser << " < x [cm] < " << xMaxUser << "\n";
  std::cout << "    " << yMinUser << " < y [cm] < " << yMaxUser << "\n";
  std::cout << "    " << zMinUser << " < z [cm] < " << zMaxUser << "\n";
  if (std::isinf(xMinUser) || std::isinf(xMaxUser)) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  if (std::isinf(yMinUser) || std::isinf(yMaxUser)) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  if (std::isinf(zMinUser) || std::isinf(zMaxUser)) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  hasUserArea = true;
  return true;

}

bool 
Sensor::SetArea(const double xmin, const double ymin, const double zmin,
                const double xmax, const double ymax, const double zmax) {

  if (fabs(xmax - xmin) < Small || 
      fabs(ymax - ymin) < Small || 
      fabs(zmax - zmin) < Small) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Invalid range.\n";
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
  
  // User area bounds are not (yet) defined.
  // Get the bounding box of the sensor. 
  if (!SetArea()) return false;
  
  xmin = xMinUser; ymin = yMinUser; zmin = zMinUser;
  xmax = xMaxUser; ymax = yMaxUser; zmax = zMaxUser;

  return true;
    
}

bool 
Sensor::IsInArea(const double x, const double y, const double z) {
 
  if (!hasUserArea) {
    if (!SetArea()) {
      std::cerr << className << "::IsInArea:\n";
      std::cerr << "    User area cannot be established.\n";
      return false;
    }
    hasUserArea = true;
  }
  
  if (x >= xMinUser && x <= xMaxUser &&
      y >= yMinUser && y <= yMaxUser &&
      z >= zMinUser && z <= zMaxUser) {
    return true;
  } 
    
  if (debug) {
    std::cout << className << "::IsInArea:\n" << std::endl;
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is outside.\n";
  }
  return false;

}


bool
Sensor::IsWireCrossed(const double x0, const double y0, const double z0,
                      const double x1, const double y1, const double z1,
                      double& xc, double& yc, double& zc) {

  for (int i = nComponents; i--;) {
    if (components[i].comp->IsWireCrossed(x0, y0, z0, x1, y1, z1, 
                                          xc, yc, zc)) {
      return true;
    }
  }
  return false;

}

bool
Sensor::IsInTrapRadius(double x0, double y0, double z0, 
                       double& xw, double& yw, double& rw){

  for (int i = nComponents; i--;) {
    if (components[i].comp->IsInTrapRadius(x0, y0, z0, xw, yw, rw)) { 
      return true;
    }
  }
  return false;

}

void
Sensor::AddComponent(ComponentBase* comp) {

  if (!comp) {
    std::cerr << className << "::AddComponent:\n";
    std::cerr << "    Component pointer is null.\n";
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

  if (!comp) {
    std::cerr << className << "::AddElectrode:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) {
      std::cout << className << "::AddElectrode:\n";
      std::cout << "    Warning: An electrode with label \"" 
                << label << "\" exists already.\n";
      std::cout << "    Weighting fields will be summed up.\n";
      break;
    }
  }
  
  electrode newElectrode;
  newElectrode.comp = comp;
  newElectrode.label = label;
  electrodes.push_back(newElectrode);
  ++nElectrodes;
  electrodes[nElectrodes - 1].signal.resize(nTimeBins);
  electrodes[nElectrodes - 1].electronsignal.resize(nTimeBins);
  electrodes[nElectrodes - 1].ionsignal.resize(nTimeBins);
  std::cout << className << "::AddElectrode:\n";
  std::cout << "    Added readout electrode \"" << label << "\".\n";
  std::cout << "    All signals are reset.\n";
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

  // We don't know the range yet.
  bool set = false;
  // Loop over the components.
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
  
  // Warn if we still don't know the range.
  if (!set) {
    std::cerr << className << "::GetVoltageRange:\n";
    std::cerr << "    Sensor voltage range not known.\n";
    vmin = vmax = 0.;
    return false;
  }  

  if (debug) {
    std::cout << className << "::GetVoltageRange:\n";
    std::cout << "    Voltage range " << vmin 
              << " < V < " << vmax << ".\n";
  }
  return true;

}

void
Sensor::ClearSignal() {

  for (int i = nElectrodes; i--;) {
    electrodes[i].charge = 0.;
    for (int j = nTimeBins; j--;){
      electrodes[i].signal[j] = 0.;
      electrodes[i].electronsignal[j] = 0.;
      electrodes[i].ionsignal[j]      = 0.;
    }
  }
  nEvents = 0;

}

void 
Sensor::AddSignal(const int q, const double t, const double dt,
                  const double x,  const double y,  const double z,
                  const double vx, const double vy, const double vz) {
 
  // Get the time bin.
  if (t < tStart || dt <= 0.) {
    if (debug) {
      std::cerr << className << "::AddSignal:\n";
      if (t < tStart) std::cerr << "    Time " << t << " out of range.\n";
      if (dt <= 0.) std::cerr << "    Time step < 0.\n";
    }
    return;
  }
  const int bin = int((t - tStart) / tStep);
  // Check if the starting time is outside the range 
  if (bin < 0 || bin >= nTimeBins) {
    if (debug) {
      std::cerr << className << "::AddSignal:\n";
      std::cerr << "    Bin " << bin << " out of range.\n";
    }
    return;
  }
  if (nEvents <= 0) nEvents = 1;
  
  double wx = 0., wy = 0., wz = 0.;
  double cur, delta;
  if (debug) {
    std::cout << className << "::AddSignal:\n";
    std::cout << "    Time: " << t << "\n";
    std::cout << "    Step: " << dt << "\n";
    std::cout << "    Charge: " << q << "\n";
    std::cout << "    Velocity: (" 
              << vx << ", " << vy << ", " << vz << ")\n";
  }
  for (int i = nElectrodes; i--;) {
    // Calculate the weighting field for this electrode
    electrodes[i].comp->WeightingField(x, y, z, wx, wy, wz, 
                                       electrodes[i].label);
    // Calculate the induced current
    cur = -q * (wx * vx + wy * vy + wz * vz);
    if (debug) {
      std::cout << "    Electrode " << electrodes[i].label << ":\n";
      std::cout << "      Weighting field: (" 
                << wx << ", " << wy << ", " << wz << ")\n";
      std::cout << "      Induced charge: " << cur * dt << "\n";
    }
    delta = tStart + (bin + 1) * tStep - t;    
    // Check if the provided timestep extends over more than one time bin
    if (dt > delta) {
      electrodes[i].signal[bin] += cur * delta; 
      if (q < 0) {
        electrodes[i].electronsignal[bin] += cur * delta;
      } else {
        electrodes[i].ionsignal[bin] += cur * delta;
      }
      delta = dt - delta;
      int j = 1;
      while (delta > tStep && bin + j < nTimeBins) {
        electrodes[i].signal[bin + j] += cur * tStep;
        if (q < 0) {
          electrodes[i].electronsignal[bin + j] += cur * tStep;
        } else {
          electrodes[i].ionsignal[bin + j] += cur * tStep;
        }
        delta -= tStep;
        ++j;
      }
      if (bin + j < nTimeBins)
      {
        electrodes[i].signal[bin + j] += cur * delta;
        if (q < 0) {
          electrodes[i].electronsignal[bin + j] += cur * delta;
        } else {
          electrodes[i].ionsignal[bin + j] += cur * delta;
        }
      }
    } else {
      electrodes[i].signal[bin] += cur * dt;
      if (q < 0) {
        electrodes[i].electronsignal[bin] += cur * dt;
      } else {
        electrodes[i].ionsignal[bin] += cur * dt;
      }
    }
  }

}

void
Sensor::AddInducedCharge(const int q, 
                         const double x0, const double y0, const double z0,
                         const double x1, const double y1, const double z1) {

  if (debug) std::cout << className << "::AddInducedCharge:\n";
  double w0 = 0., w1 = 0.;
  for (int i = nElectrodes; i--;) {
    // Calculate the weighting potential at the starting point.
    w0 = electrodes[i].comp->WeightingPotential(x0, y0, z0, 
                                                electrodes[i].label);
    // Calculate the weighting potential at the end point.
    w1 = electrodes[i].comp->WeightingPotential(x1, y1, z1,
                                                electrodes[i].label);
    electrodes[i].charge += q * (w1 - w0);
    if (debug) {
      std::cout << "    Electrode " << electrodes[i].label << ":\n";
      std::cout << "      Weighting potential at (" 
                << x0 << ", " << y0 << ", " << z0 << "): " << w0 << "\n";
      std::cout << "      Weighting potential at ("
                << x1 << ", " << y1 << ", " << z1 << "): " << w1 << "\n";
      std::cout << "      Induced charge: " 
                << electrodes[i].charge << "\n";
    }
  }

}

void 
Sensor::SetTimeWindow(const double tstart, const double tstep, 
                      const int nsteps) {

  tStart = tstart;
  if (tstep <= 0.) {
    std::cerr << className << "::SetTimeWindow:\n";
    std::cerr << "    Starting time out of range.\n";
  } else {
    tStep = tstep;
  }
  
  if (nsteps <= 0) {
    std::cerr << className << "::SetTimeWindow:\n";
    std::cerr << "    Number of time bins out of range.\n";
  } else {
    nTimeBins = nsteps;
  }
  
  if (debug) {
    std::cout << className << "::SetTimeWindow:\n";
    std::cout << "    " << tStart << " < t [ns] < " 
              << tStart + nTimeBins * tStep << "\n";
    std::cout << "    Step size: " << tStep << " ns\n";
  }
 
  std::cout << className << "::SetTimeWindow:\n";
  std::cout << "    Resetting all signals.\n"; 
  for (int i = nElectrodes; i--;) {
    electrodes[i].signal.clear();
    electrodes[i].signal.resize(nTimeBins);
    electrodes[i].electronsignal.clear();
    electrodes[i].electronsignal.resize(nTimeBins);
    electrodes[i].ionsignal.clear();
    electrodes[i].ionsignal.resize(nTimeBins);
  }
  nEvents = 0;

}

double
Sensor::GetElectronSignal(const std::string label, const int bin) {

  if (nEvents <= 0) return 0.;
  if (bin<0 || bin >= nTimeBins) return 0.;
  double sig = 0.;
  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) sig += electrodes[i].electronsignal[bin];
  }
  if (debug) {
    std::cout << className << "::GetElectronSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    ElectronSignal: " << sig / tStep << "\n";
  }
  return signalConversion * sig / (nEvents * tStep);
}  

double
Sensor::GetIonSignal(const std::string label, const int bin) {

  if (nEvents <= 0) return 0.;
  if (bin<0 || bin >= nTimeBins) return 0.;
  double sig = 0.;
  for (int i = nElectrodes; i--;) {
    if (electrodes[i].label == label) sig += electrodes[i].ionsignal[bin];
  }
  if (debug) {
    std::cout << className << "::GetIonSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    IonSignal: " << sig / tStep << "\n";
  }
  return signalConversion * sig / (nEvents * tStep);
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
    std::cout << className << "::GetSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    Signal: " << sig / tStep << "\n";
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
    std::cout << className << "::GetInducedCharge:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Charge: " << charge / tStep << "\n";
  }

  return charge / nEvents;

}

void
Sensor::SetTransferFunction(double (*f)(double t)) {

  if (f == 0) {
    std::cerr << className << "::SetTransferFunction:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  fTransfer = f;
  hasTransferFunction = true;
  
}
  
bool
Sensor::ConvoluteSignal() {

  if (!hasTransferFunction) {
    std::cerr << className << "::ConvoluteSignal:\n";
    std::cerr << "    No transfer function available.\n";
    return false;
  }
  if (nEvents <= 0) {
    std::cerr << className << "::ConvoluteSignal:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }
  
  // Set the range where the transfer function is valid.
  double cnvMin = 0.;
  double cnvMax = 1.e10;

  std::vector<double> cnvTab;
  cnvTab.resize(2 * nTimeBins);
  int iOffset = nTimeBins;  
  // Evaluate the transfer function.
  for (int i = 0; i < nTimeBins; ++i) {
    // Negative time part.
    double t = -i * tStep;
    if (t < cnvMin || t > cnvMax) {
      cnvTab[iOffset - i] = 0.;
    } else {
      cnvTab[iOffset - i] = fTransfer(t);
    }
    if (i < 1) continue;
    // Positive time part.
    t = i * tStep;
    if (t < cnvMin || t > cnvMax) {
      cnvTab[iOffset + i] = 0.;
    } else {
      cnvTab[iOffset + i] = fTransfer(t);
    }
  }
  
  std::vector<double> tmpSignal;
  tmpSignal.resize(nTimeBins);
  // Loop over all electrodes.
  for (int i = 0; i < nElectrodes; ++i) {
    for (int j = 0; j < nTimeBins; ++j) {
      tmpSignal[j] = 0.;
      for (int k = 0; k < nTimeBins; ++k) {
        tmpSignal[j] += tStep * cnvTab[iOffset + j - k] * 
                        electrodes[i].signal[k];
      }
    }
    for (int j = 0; j < nTimeBins; ++j) {
      electrodes[i].signal[j] = tmpSignal[j];
    }
  }
  return true;
  
}

bool
Sensor::IntegrateSignal() {

  if (nEvents <= 0) {
    std::cerr << className << "::IntegrateSignal:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }
  
  for (int i = 0; i < nElectrodes; ++i) {
    for (int j = 0; j < nTimeBins; ++j) {
      electrodes[i].signal[j] *= tStep;
      if (j > 0) {
        electrodes[i].signal[j] += electrodes[i].signal[j - 1];
      }
    }
  }
  return true;
  
}


void
Sensor::SetNoiseFunction(double (*f)(double t)) {

  if (f == 0) {
    std::cerr << className << "::SetNoiseFunction:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  fNoise = f;
  hasNoiseFunction = true;
  
}

void
Sensor::AddNoise() {

  if (!hasNoiseFunction) {
    std::cerr << className << "::AddNoise:\n";
    std::cerr << "    Noise function is not defined.\n";
    return;
  }
  if (nEvents <= 0) nEvents = 1;
  
  for (int i = nElectrodes; i--;) {
    for (int j = nTimeBins; j--;) {
      const double t = tStart + (j + 0.5) * tStep;
      electrodes[i].signal[j] += fNoise(t);
      // Adding noise to both channels might be wrong,
      // maybe an extended option
      // where to add noise would be an idea?
      electrodes[i].electronsignal[j] += fNoise(t);
      electrodes[i].ionsignal[j] += fNoise(t);
    }
  }
  
}

bool
Sensor::ComputeThresholdCrossings(const double thr, const std::string label, int& n) {

  // Reset the list of threshold crossings.
  thresholdCrossings.clear();
  nThresholdCrossings = n = 0;
  thresholdLevel = thr;

  // Set the interpolation order.
  int iOrder = 1;
  
  if (nEvents <= 0) {
    std::cerr << className << "::ComputeThresholdCrossings:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }
  
  // Compute the total signal.
  std::vector<double> signal;
  signal.resize(nTimeBins);
  for (int i = nTimeBins; i--;) signal[i] = 0.;
  // Loop over the electrodes.
  bool foundLabel = false;
  for (int j = nElectrodes; j--;) {
    if (electrodes[j].label == label) {
      foundLabel = true;
      for (int i = nTimeBins; i--;) {
        signal[i] += electrodes[j].signal[i];
      }
    }
  }
  if (!foundLabel) {
    std::cerr << className << "::ComputeThresholdCrossings:\n";
    std::cerr << "    Electrode " << label << " not found.\n";
    return false;
  }
  for (int i = nTimeBins; i--;) {
    signal[i] *= signalConversion / (nEvents * tStep);
  }
  
  // Establish the range.
  double vMin = signal[0];
  double vMax = signal[0];
  for (int i = nTimeBins; i--;) {
    if (signal[i] < vMin) vMin = signal[i];
    if (signal[i] > vMax) vMax = signal[i];
  }
  if (thr < vMin && thr > vMax) {
    if (debug) {
      std::cout << className << "::ComputeThresholdCrossings:\n";
      std::cout << "    Threshold outside the range [" 
                << vMin << ", " << vMax << "]\n";
    }
    return true;
  }

  // Check for rising edges.
  bool rise = true;
  bool fall = false;

  while (rise || fall) {
    if (debug) {
      if (rise) {
        std::cout << className << "::ComputeThresholdCrossings:\n";
        std::cout << "    Hunting for rising edges.\n";
      } else if (fall) {
        std::cout << className << "::ComputeThresholdCrossings:\n";
        std::cout << "    Hunting for falling edges.\n";
      }
    }
    // Initialise the vectors.
    std::vector<double> times;
    std::vector<double> values;
    times.clear();
    values.clear();
    times.push_back(tStart);
    values.push_back(signal[0]);
    int nValues = 1;
    // Scan the signal.
    for (int i = 1; i < nTimeBins; ++i) {
      // Compute the vector element.
      const double tNew = tStart + i * tStep;
      const double vNew = signal[i];
      // If still increasing or decreasing, add to the vector.
      if ((rise && vNew > values.back()) ||
          (fall && vNew < values.back())) {
        times.push_back(tNew);
        values.push_back(vNew);
        ++nValues;
      // Otherwise see whether we crossed the threshold level.
      } else if ((values[0] - thr) * (thr - values.back()) >= 0. && 
                 nValues > 1 && 
                 ((rise && values.back() > values[0]) ||
                  (fall && values.back() < values[0] ))) {      
        // Compute the crossing time.
        double tcr = Numerics::Divdif(times, values, nValues, thr, iOrder);
        thresholdCrossing newCrossing;
        newCrossing.time = tcr;
        newCrossing.rise = rise;
        thresholdCrossings.push_back(newCrossing);
        ++nThresholdCrossings;
        times.clear();
        values.clear();
        times.push_back(tNew);
        values.push_back(vNew);
        nValues = 1;
      } else {
        // No crossing, simply reset the vector.
        times.clear();
        values.clear();
        times.push_back(tNew);
        values.push_back(vNew);
        nValues = 1;
      }
    }
    // Check the final vector.
    if ((values[0] - thr) * (thr - values.back()) >= 0. && 
         nValues > 1 && 
         ((rise && values.back() > values[0]) ||
          (fall && values.back() < values[0]))) {
      double tcr = Numerics::Divdif(times, values, nValues, thr, iOrder);
      thresholdCrossing newCrossing;
      newCrossing.time = tcr;
      newCrossing.rise = rise;
      thresholdCrossings.push_back(newCrossing);
      ++nThresholdCrossings;
    }
    if (rise) {
      rise = false;
      fall = true;
    } else if (fall) {
      rise = fall = false;
    }
  }
  n = nThresholdCrossings;
  
  if (debug) {
    std::cout << className << "::ComputeThresholdCrossings:\n";
    std::cout << "    Found " << nThresholdCrossings << " crossings.\n";
    if (nThresholdCrossings > 0) {
      std::cout << "      Time  [ns]    Direction\n";
    }
    for (int i = 0; i < nThresholdCrossings; ++i) {
      std::cout << "      " << thresholdCrossings[i].time << "      ";
      if (thresholdCrossings[i].rise) {
        std::cout << "rising\n";
      } else {
        std::cout << "falling\n";
      }
    }
  }
  // Seems to have worked.
  return true;

}

bool
Sensor::GetThresholdCrossing(const int i, double& time, double& level, bool& rise) {
  
  level = thresholdLevel;
  
  if (i < 0 || i >= nThresholdCrossings) {
    std::cerr << className << "::GetThresholdCrossing:\n";
    std::cerr << "    Index (" << i << ") out of range.\n";
    time = tStart + nTimeBins * tStep;
    return false;
  }
  
  time = thresholdCrossings[i].time;
  rise = thresholdCrossings[i].rise;
  return true;
  
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
    std::cerr << className << "::GetBoundingBox:\n";
    std::cerr << "    Sensor bounding box not known.\n";
    xmin = 0.; ymin = 0.; zmin = 0.;
    xmax = 0.; ymax = 0.; zmax = 0.;
    return false;
  } 
  
  if (debug) {
    std::cout << className << "::GetBoundingBox:\n";
    std::cout << "    " << xmin << " < x [cm] < " << xmax << "\n";
    std::cout << "    " << ymin << " < y [cm] < " << ymax << "\n";
    std::cout << "    " << zmin << " < z [cm] < " << zmax << "\n";
  }
  return true;
  
}
  
}
