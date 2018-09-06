#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>

#include "Sensor.hh"
#include "GarfieldConstants.hh"
#include "Plotting.hh"
#include "Numerics.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

double Sensor::m_signalConversion = ElementaryCharge;

Sensor::Sensor() {}

ComponentBase* Sensor::GetComponent(const unsigned int i) {

  if (i >= m_components.size()) {
    std::cerr << m_className << "::GetComponent: Index out of range.\n";
	  return nullptr;	
	};
	return m_components[i];
}

void Sensor::ElectricField(const double x, const double y, const double z,
                           double& ex, double& ey, double& ez, double& v,
                           Medium*& medium, int& status) {

  ex = ey = ez = v = 0.;
  status = -10;
  medium = nullptr;
  double fx, fy, fz, p;
  Medium* med = nullptr;
  int stat;
  // Add up electric field contributions from all components.
  for (auto component : m_components) {
    component->ElectricField(x, y, z, fx, fy, fz, p, med, stat);
    if (status != 0) {
      status = stat;
      medium = med;
    }
    if (stat == 0) {
      ex += fx;
      ey += fy;
      ez += fz;
      v += p;
    }
  }
}

void Sensor::ElectricField(const double x, const double y, const double z,
                           double& ex, double& ey, double& ez, Medium*& medium,
                           int& status) {

  ex = ey = ez = 0.;
  status = -10;
  medium = nullptr;
  double fx, fy, fz;
  Medium* med = nullptr;
  int stat;
  // Add up electric field contributions from all components.
  for (auto component : m_components) {
    component->ElectricField(x, y, z, fx, fy, fz, med, stat);
    if (status != 0) {
      status = stat;
      medium = med;
    }
    if (stat == 0) {
      ex += fx;
      ey += fy;
      ez += fz;
    }
  }
}

void Sensor::MagneticField(const double x, const double y, const double z,
                           double& bx, double& by, double& bz, int& status) {

  bx = by = bz = 0.;
  double fx, fy, fz;
  // Add up contributions.
  for (auto component : m_components) {
    component->MagneticField(x, y, z, fx, fy, fz, status);
    if (status != 0) continue;
    bx += fx;
    by += fy;
    bz += fz;
  }
}

void Sensor::WeightingField(const double x, const double y, const double z,
                            double& wx, double& wy, double& wz,
                            const std::string& label) {

  wx = wy = wz = 0.;
  double fx = 0., fy = 0., fz = 0.;
  // Add up field contributions from all components.
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) {
      fx = fy = fz = 0.;
      electrode.comp->WeightingField(x, y, z, fx, fy, fz, label);
      wx += fx;
      wy += fy;
      wz += fz;
    }
  }
}

double Sensor::WeightingPotential(const double x, const double y,
                                  const double z, const std::string& label) {

  double v = 0.;
  // Add up contributions from all components.
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) {
      v += electrode.comp->WeightingPotential(x, y, z, label);
    }
  }
  return v;
}

bool Sensor::GetMedium(const double x, const double y, const double z,
                       Medium*& m) {

  m = nullptr;

  // Make sure there is at least one component.
  if (m_components.empty()) return false;

  // Check if we are still in the same component as in the previous call.
  if (m_lastComponent) {
    m = m_lastComponent->GetMedium(x, y, z);
    if (m) return true;
  }

  for (auto component : m_components) {
    m = component->GetMedium(x, y, z);
    if (m) {
      m_lastComponent = component;
      return true;
    }
  }
  return false;
}

bool Sensor::SetArea() {

  if (!GetBoundingBox(m_xMinUser, m_yMinUser, m_zMinUser, m_xMaxUser,
                      m_yMaxUser, m_zMaxUser)) {
    std::cerr << m_className << "::SetArea: Bounding box is not known.\n";
    return false;
  }

  std::cout << m_className << "::SetArea:\n";
  std::cout << "    " << m_xMinUser << " < x [cm] < " << m_xMaxUser << "\n";
  std::cout << "    " << m_yMinUser << " < y [cm] < " << m_yMaxUser << "\n";
  std::cout << "    " << m_zMinUser << " < z [cm] < " << m_zMaxUser << "\n";
  if (std::isinf(m_xMinUser) || std::isinf(m_xMaxUser)) {
    std::cerr << m_className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  if (std::isinf(m_yMinUser) || std::isinf(m_yMaxUser)) {
    std::cerr << m_className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  if (std::isinf(m_zMinUser) || std::isinf(m_zMaxUser)) {
    std::cerr << m_className << "::SetArea:\n";
    std::cerr << "    Warning: infinite x-range\n";
  }
  m_hasUserArea = true;
  return true;
}

bool Sensor::SetArea(const double xmin, const double ymin, const double zmin,
                     const double xmax, const double ymax, const double zmax) {

  if (fabs(xmax - xmin) < Small || fabs(ymax - ymin) < Small ||
      fabs(zmax - zmin) < Small) {
    std::cerr << m_className << "::SetArea:\n";
    std::cerr << "    Invalid range.\n";
    return false;
  }

  m_xMinUser = xmin;
  m_yMinUser = ymin;
  m_zMinUser = zmin;
  m_xMaxUser = xmax;
  m_yMaxUser = ymax;
  m_zMaxUser = zmax;

  if (xmin > xmax) {
    m_xMinUser = xmax;
    m_xMaxUser = xmin;
  }
  if (ymin > ymax) {
    m_yMinUser = ymax;
    m_yMaxUser = ymin;
  }
  if (zmin > zmax) {
    m_zMinUser = zmax;
    m_zMaxUser = zmin;
  }
  m_hasUserArea = true;
  return true;
}

bool Sensor::GetArea(double& xmin, double& ymin, double& zmin, double& xmax,
                     double& ymax, double& zmax) {

  if (m_hasUserArea) {
    xmin = m_xMinUser;
    ymin = m_yMinUser;
    zmin = m_zMinUser;
    xmax = m_xMaxUser;
    ymax = m_yMaxUser;
    zmax = m_zMaxUser;
    return true;
  }

  // User area bounds are not (yet) defined.
  // Get the bounding box of the sensor.
  if (!SetArea()) return false;

  xmin = m_xMinUser;
  ymin = m_yMinUser;
  zmin = m_zMinUser;
  xmax = m_xMaxUser;
  ymax = m_yMaxUser;
  zmax = m_zMaxUser;

  return true;
}

bool Sensor::IsInArea(const double x, const double y, const double z) {

  if (!m_hasUserArea) {
    if (!SetArea()) {
      std::cerr << m_className << "::IsInArea:\n";
      std::cerr << "    User area cannot be established.\n";
      return false;
    }
    m_hasUserArea = true;
  }

  if (x >= m_xMinUser && x <= m_xMaxUser && y >= m_yMinUser &&
      y <= m_yMaxUser && z >= m_zMinUser && z <= m_zMaxUser) {
    return true;
  }

  if (m_debug) {
    std::cout << m_className << "::IsInArea:\n" << std::endl;
    std::cout << "    (" << x << ", " << y << ", " << z << ") "
              << " is outside.\n";
  }
  return false;
}

bool Sensor::IsWireCrossed(const double x0, const double y0, const double z0,
                           const double x1, const double y1, const double z1,
                           double& xc, double& yc, double& zc) {

  for (auto component : m_components) {
    if (component->IsWireCrossed(x0, y0, z0, x1, y1, z1, xc, yc, zc)) {
      return true;
    }
  }
  return false;
}

bool Sensor::IsInTrapRadius(const double q0, const double x0, 
                            const double y0, double z0, double& xw,
                            double& yw, double& rw) {

  for (auto component : m_components) {
    if (component->IsInTrapRadius(q0, x0, y0, z0, xw, yw, rw)) return true;
  }
  return false;
}

void Sensor::AddComponent(ComponentBase* comp) {

  if (!comp) {
    std::cerr << m_className << "::AddComponent: Null pointer.\n";
    return;
  }

  m_components.push_back(comp);
}

void Sensor::AddElectrode(ComponentBase* comp, const std::string& label) {

  if (!comp) {
    std::cerr << m_className << "::AddElectrode: Null pointer.\n";
    return;
  }
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) {
      std::cout << m_className << "::AddElectrode:\n"
                << "    Warning: An electrode with label \"" << label
                << "\" exists already. Weighting fields will be summed up.\n";
      break;
    }
  }

  Electrode electrode;
  electrode.comp = comp;
  electrode.label = label;
  electrode.signal.resize(m_nTimeBins);
  electrode.electronsignal.resize(m_nTimeBins);
  electrode.ionsignal.resize(m_nTimeBins);
  m_electrodes.push_back(std::move(electrode));
  std::cout << m_className << "::AddElectrode:\n"
            << "    Added readout electrode \"" << label << "\".\n"
            << "    All signals are reset.\n";
  ClearSignal();
}

void Sensor::Clear() {

  m_components.clear();
  m_lastComponent = nullptr;
  m_electrodes.clear();
  m_nTimeBins = 200;
  m_tStart = 0.;
  m_tStep = 10.;
  m_nEvents = 0;
  m_hasUserArea = false;
}

bool Sensor::GetVoltageRange(double& vmin, double& vmax) {

  // We don't know the range yet.
  bool set = false;
  // Loop over the components.
  for (auto component : m_components) {
    double umin = 0., umax = 0.;
    if (!component->GetVoltageRange(umin, umax)) continue;
    if (set) {
      vmin = std::min(umin, vmin);
      vmax = std::max(umax, vmax);
    } else {
      vmin = umin;
      vmax = umax;
      set = true;
    }
  }

  // Warn if we still don't know the range.
  if (!set) {
    std::cerr << m_className << "::GetVoltageRange:\n"
              << "    Sensor voltage range not known.\n";
    vmin = vmax = 0.;
    return false;
  }

  if (m_debug) {
    std::cout << m_className << "::GetVoltageRange:\n";
    std::cout << "    Voltage range " << vmin << " < V < " << vmax << ".\n";
  }
  return true;
}

void Sensor::ClearSignal() {

  for (auto& electrode : m_electrodes) {
    electrode.charge = 0.;
    electrode.signal.assign(m_nTimeBins, 0.);
    electrode.electronsignal.assign(m_nTimeBins, 0.);
    electrode.ionsignal.assign(m_nTimeBins, 0.);
  }
  m_nEvents = 0;
}

void Sensor::AddSignal(const double q, const double t, const double dt,
                       const double x, const double y, const double z,
                       const double vx, const double vy, const double vz) {
  // Get the time bin.
  if (t < m_tStart || dt <= 0.) {
    if (m_debug) {
      std::cerr << m_className << "::AddSignal:\n";
      if (t < m_tStart) std::cerr << "    Time " << t << " out of range.\n";
      if (dt <= 0.) std::cerr << "    Time step < 0.\n";
    }
    return;
  }
  const int bin = int((t - m_tStart) / m_tStep);
  // Check if the starting time is outside the range
  if (bin < 0 || bin >= (int)m_nTimeBins) {
    if (m_debug) {
      std::cerr << m_className << "::AddSignal:\n";
      std::cerr << "    Bin " << bin << " out of range.\n";
    }
    return;
  }
  if (m_nEvents <= 0) m_nEvents = 1;

  double wx = 0., wy = 0., wz = 0.;
  if (m_debug) {
    std::cout << m_className << "::AddSignal:\n";
    std::cout << "    Time: " << t << "\n";
    std::cout << "    Step: " << dt << "\n";
    std::cout << "    Charge: " << q << "\n";
    std::cout << "    Velocity: (" << vx << ", " << vy << ", " << vz << ")\n";
  }
  for (auto& electrode : m_electrodes) {
    // Calculate the weighting field for this electrode
    electrode.comp->WeightingField(x, y, z, wx, wy, wz, electrode.label);
    // Calculate the induced current
    const double cur = -q * (wx * vx + wy * vy + wz * vz);
    if (m_debug) {
      std::cout << "    Electrode " << electrode.label << ":\n";
      std::cout << "      Weighting field: (" << wx << ", " << wy << ", " << wz
                << ")\n";
      std::cout << "      Induced charge: " << cur* dt << "\n";
    }
    double delta = m_tStart + (bin + 1) * m_tStep - t;
    // Check if the provided timestep extends over more than one time bin
    if (dt > delta) {
      electrode.signal[bin] += cur * delta;
      if (q < 0) {
        electrode.electronsignal[bin] += cur * delta;
      } else {
        electrode.ionsignal[bin] += cur * delta;
      }
      delta = dt - delta;
      unsigned int j = 1;
      while (delta > m_tStep && bin + j < m_nTimeBins) {
        electrode.signal[bin + j] += cur * m_tStep;
        if (q < 0) {
          electrode.electronsignal[bin + j] += cur * m_tStep;
        } else {
          electrode.ionsignal[bin + j] += cur * m_tStep;
        }
        delta -= m_tStep;
        ++j;
      }
      if (bin + j < m_nTimeBins) {
        electrode.signal[bin + j] += cur * delta;
        if (q < 0) {
          electrode.electronsignal[bin + j] += cur * delta;
        } else {
          electrode.ionsignal[bin + j] += cur * delta;
        }
      }
    } else {
      electrode.signal[bin] += cur * dt;
      if (q < 0) {
        electrode.electronsignal[bin] += cur * dt;
      } else {
        electrode.ionsignal[bin] += cur * dt;
      }
    }
  }
}

void Sensor::AddInducedCharge(const double q, const double x0, const double y0,
                              const double z0, const double x1, const double y1,
                              const double z1) {

  if (m_debug) std::cout << m_className << "::AddInducedCharge:\n";
  for (auto& electrode : m_electrodes) {
    // Calculate the weighting potential at the starting point.
    auto cmp = electrode.comp;
    const double w0 = cmp->WeightingPotential(x0, y0, z0, electrode.label);
    // Calculate the weighting potential at the end point.
    const double w1 = cmp->WeightingPotential(x1, y1, z1, electrode.label);
    electrode.charge += q * (w1 - w0);
    if (m_debug) {
      std::cout << "    Electrode " << electrode.label << ":\n";
      std::cout << "      Weighting potential at (" << x0 << ", " << y0 << ", "
                << z0 << "): " << w0 << "\n";
      std::cout << "      Weighting potential at (" << x1 << ", " << y1 << ", "
                << z1 << "): " << w1 << "\n";
      std::cout << "      Induced charge: " << electrode.charge << "\n";
    }
  }
}

void Sensor::SetTimeWindow(const double tstart, const double tstep,
                           const unsigned int nsteps) {

  m_tStart = tstart;
  if (tstep <= 0.) {
    std::cerr << m_className << "::SetTimeWindow:\n";
    std::cerr << "    Starting time out of range.\n";
  } else {
    m_tStep = tstep;
  }

  if (nsteps == 0) {
    std::cerr << m_className << "::SetTimeWindow:\n";
    std::cerr << "    Number of time bins out of range.\n";
  } else {
    m_nTimeBins = nsteps;
  }

  if (m_debug) {
    std::cout << m_className << "::SetTimeWindow:\n";
    std::cout << "    " << m_tStart << " < t [ns] < "
              << m_tStart + m_nTimeBins* m_tStep << "\n";
    std::cout << "    Step size: " << m_tStep << " ns\n";
  }

  std::cout << m_className << "::SetTimeWindow:\n";
  std::cout << "    Resetting all signals.\n";
  for (auto& electrode : m_electrodes) {
    electrode.signal.assign(m_nTimeBins, 0.);
    electrode.electronsignal.assign(m_nTimeBins, 0.);
    electrode.ionsignal.assign(m_nTimeBins, 0.);
  }
  m_nEvents = 0;
}

double Sensor::GetElectronSignal(const std::string& label, 
                                 const unsigned int bin) {

  if (m_nEvents == 0) return 0.;
  if (bin >= m_nTimeBins) return 0.;
  double sig = 0.;
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) sig += electrode.electronsignal[bin];
  }
  if (m_debug) {
    std::cout << m_className << "::GetElectronSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    ElectronSignal: " << sig / m_tStep << "\n";
  }
  return m_signalConversion * sig / (m_nEvents * m_tStep);
}

double Sensor::GetIonSignal(const std::string& label, const unsigned int bin) {

  if (m_nEvents == 0) return 0.;
  if (bin >= m_nTimeBins) return 0.;
  double sig = 0.;
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) sig += electrode.ionsignal[bin];
  }
  if (m_debug) {
    std::cout << m_className << "::GetIonSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    IonSignal: " << sig / m_tStep << "\n";
  }
  return m_signalConversion * sig / (m_nEvents * m_tStep);
}

double Sensor::GetSignal(const std::string& label, const unsigned int bin) {

  if (m_nEvents == 0) return 0.;
  if (bin >= m_nTimeBins) return 0.;
  double sig = 0.;
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) sig += electrode.signal[bin];
  }
  if (m_debug) {
    std::cout << m_className << "::GetSignal:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Bin: " << bin << "\n";
    std::cout << "    Signal: " << sig / m_tStep << "\n";
  }
  return m_signalConversion * sig / (m_nEvents * m_tStep);
}

double Sensor::GetInducedCharge(const std::string& label) {

  if (m_nEvents == 0) return 0.;
  double charge = 0.;
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) charge += electrode.charge;
  }
  if (m_debug) {
    std::cout << m_className << "::GetInducedCharge:\n";
    std::cout << "    Electrode: " << label << "\n";
    std::cout << "    Charge: " << charge / m_tStep << "\n";
  }

  return charge / m_nEvents;
}

void Sensor::SetTransferFunction(double (*f)(double t)) {

  if (!f) {
    std::cerr << m_className << "::SetTransferFunction:\n    Null pointer.\n";
    return;
  }
  m_fTransfer = f;
  m_hasTransferFunction = true;
  m_transferFunctionTimes.clear();
  m_transferFunctionValues.clear();
}

void Sensor::SetTransferFunction(const std::vector<double>& times,
                                 const std::vector<double>& values) {

  if (times.empty() || values.empty()) {
    std::cerr << m_className << "::SetTransferFunction:\n";
    std::cerr << "    Time and value vectors must not be empty.\n";
    return;
  } else if (times.size() != values.size()) {
    std::cerr << m_className << "::SetTransferFunction:\n";
    std::cerr << "    Time and value vectors must have same size.\n";
    return;
  }
  m_transferFunctionTimes = times;
  m_transferFunctionValues = values;
  m_fTransfer = nullptr;
  m_hasTransferFunction = true;
}

double Sensor::InterpolateTransferFunctionTable(double t) {

  if (m_transferFunctionTimes.empty() || m_transferFunctionValues.empty()) {
    return 0.;
  }
  // Don't extrapolate beyond the range defined in the table.
  if (t < m_transferFunctionTimes.front() ||
      t > m_transferFunctionTimes.back()) {
    return 0.;
  }
  // Find the proper interval in the table.
  const auto begin = m_transferFunctionTimes.cbegin();
  const auto it1 = std::upper_bound(begin, m_transferFunctionTimes.cend(), t);
  if (it1 == begin) return m_transferFunctionValues.front();
  const auto it0 = std::prev(it1);
  const auto f0 = m_transferFunctionValues[it0 - begin];
  const auto f1 = m_transferFunctionValues[it1 - begin];

  // Linear interpolation.
  return f0 + (t - *it0) * (f1 - f0) / (*it1 - *it0);
}

double Sensor::GetTransferFunction(const double t) {

  if (!m_hasTransferFunction) return 0.;
  if (m_fTransfer) return m_fTransfer(t);
  return InterpolateTransferFunctionTable(t);
}

bool Sensor::ConvoluteSignal() {

  if (!m_hasTransferFunction) {
    std::cerr << m_className << "::ConvoluteSignal:\n";
    std::cerr << "    No transfer function available.\n";
    return false;
  }
  if (m_nEvents == 0) {
    std::cerr << m_className << "::ConvoluteSignal:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }

  // Set the range where the transfer function is valid.
  double cnvMin = 0.;
  double cnvMax = 1.e10;

  std::vector<double> cnvTab(2 * m_nTimeBins, 0.);
  int iOffset = m_nTimeBins;
  // Evaluate the transfer function.
  for (unsigned int i = 0; i < m_nTimeBins; ++i) {
    // Negative time part.
    double t = -i * m_tStep;
    if (t < cnvMin || t > cnvMax) {
      cnvTab[iOffset - i] = 0.;
    } else if (m_fTransfer) {
      cnvTab[iOffset - i] = m_fTransfer(t);
    } else {
      cnvTab[iOffset - i] = InterpolateTransferFunctionTable(t);
    }
    if (i < 1) continue;
    // Positive time part.
    t = i * m_tStep;
    if (t < cnvMin || t > cnvMax) {
      cnvTab[iOffset + i] = 0.;
    } else if (m_fTransfer) {
      cnvTab[iOffset + i] = m_fTransfer(t);
    } else {
      cnvTab[iOffset + i] = InterpolateTransferFunctionTable(t);
    }
  }

  std::vector<double> tmpSignal(m_nTimeBins, 0.);
  // Loop over all electrodes.
  for (auto& electrode : m_electrodes) {
    for (unsigned int j = 0; j < m_nTimeBins; ++j) {
      tmpSignal[j] = 0.;
      for (unsigned int k = 0; k < m_nTimeBins; ++k) {
        tmpSignal[j] += m_tStep * cnvTab[iOffset + j - k] * electrode.signal[k];
      }
    }
    electrode.signal.swap(tmpSignal);
  }
  return true;
}

bool Sensor::IntegrateSignal() {

  if (m_nEvents == 0) {
    std::cerr << m_className << "::IntegrateSignal:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }

  for (auto& electrode : m_electrodes) {
    for (unsigned int j = 0; j < m_nTimeBins; ++j) {
      electrode.signal[j] *= m_tStep;
      electrode.electronsignal[j] *= m_tStep;
      electrode.ionsignal[j] *= m_tStep;
      if (j > 0) {
        electrode.signal[j] += electrode.signal[j - 1];
        electrode.electronsignal[j] += electrode.electronsignal[j - 1];
        electrode.ionsignal[j] += electrode.ionsignal[j - 1];
      }
    }
  }
  return true;
}

void Sensor::SetNoiseFunction(double (*f)(double t)) {

  if (f == 0) {
    std::cerr << m_className << "::SetNoiseFunction: Null pointer.\n";
    return;
  }
  m_fNoise = f;
  m_hasNoiseFunction = true;
}

void Sensor::AddNoise(const bool total, const bool electron, const bool ion) {

  if (!m_hasNoiseFunction) {
    std::cerr << m_className << "::AddNoise:\n";
    std::cerr << "    Noise function is not defined.\n";
    return;
  }
  if (m_nEvents == 0) m_nEvents = 1;

  for (auto& electrode : m_electrodes) {
    double t = m_tStart + 0.5 * m_tStep;
    for (unsigned int j = 0; j < m_nTimeBins; ++j) {
      const double noise = m_fNoise(t);
      if (total) electrode.signal[j] += noise;
      if (electron) electrode.electronsignal[j] += noise;
      if (ion) electrode.ionsignal[j] += noise;
      t += m_tStep;
    }
  }
}

bool Sensor::ComputeThresholdCrossings(const double thr,
                                       const std::string& label, int& n) {

  // Reset the list of threshold crossings.
  m_thresholdCrossings.clear();
  m_thresholdLevel = thr;

  // Set the interpolation order.
  int iOrder = 1;

  if (m_nEvents == 0) {
    std::cerr << m_className << "::ComputeThresholdCrossings:\n";
    std::cerr << "    No signals present.\n";
    return false;
  }

  // Compute the total signal.
  std::vector<double> signal(m_nTimeBins, 0.);
  // Loop over the electrodes.
  bool foundLabel = false;
  for (const auto& electrode : m_electrodes) {
    if (electrode.label == label) {
      foundLabel = true;
      for (unsigned int i = 0; i < m_nTimeBins; ++i) {
        signal[i] += electrode.signal[i];
      }
    }
  }
  if (!foundLabel) {
    std::cerr << m_className << "::ComputeThresholdCrossings:\n";
    std::cerr << "    Electrode " << label << " not found.\n";
    return false;
  }
  const double scale = m_signalConversion / (m_nEvents * m_tStep);
  for (unsigned int i = 0; i < m_nTimeBins; ++i) signal[i] *= scale;

  // Establish the range.
  double vMin = signal[0];
  double vMax = signal[0];
  for (unsigned int i = 0; i < m_nTimeBins; ++i) {
    if (signal[i] < vMin) vMin = signal[i];
    if (signal[i] > vMax) vMax = signal[i];
  }
  if (thr < vMin && thr > vMax) {
    if (m_debug) {
      std::cout << m_className << "::ComputeThresholdCrossings:\n";
      std::cout << "    Threshold outside the range [" << vMin << ", " << vMax
                << "]\n";
    }
    return true;
  }

  // Check for rising edges.
  bool rise = true;
  bool fall = false;

  while (rise || fall) {
    if (m_debug) {
      if (rise) {
        std::cout << m_className << "::ComputeThresholdCrossings:\n";
        std::cout << "    Hunting for rising edges.\n";
      } else if (fall) {
        std::cout << m_className << "::ComputeThresholdCrossings:\n";
        std::cout << "    Hunting for falling edges.\n";
      }
    }
    // Initialise the vectors.
    std::vector<double> times;
    std::vector<double> values;
    times.push_back(m_tStart);
    values.push_back(signal[0]);
    int nValues = 1;
    // Scan the signal.
    for (unsigned int i = 1; i < m_nTimeBins; ++i) {
      // Compute the vector element.
      const double tNew = m_tStart + i * m_tStep;
      const double vNew = signal[i];
      // If still increasing or decreasing, add to the vector.
      if ((rise && vNew > values.back()) || (fall && vNew < values.back())) {
        times.push_back(tNew);
        values.push_back(vNew);
        ++nValues;
        // Otherwise see whether we crossed the threshold level.
      } else if ((values[0] - thr) * (thr - values.back()) >= 0. &&
                 nValues > 1 && ((rise && values.back() > values[0]) ||
                                 (fall && values.back() < values[0]))) {
        // Compute the crossing time.
        double tcr = Numerics::Divdif(times, values, nValues, thr, iOrder);
        ThresholdCrossing newCrossing;
        newCrossing.time = tcr;
        newCrossing.rise = rise;
        m_thresholdCrossings.push_back(std::move(newCrossing));
        times.clear();
        values.clear();
        times.emplace_back(tNew);
        values.emplace_back(vNew);
        nValues = 1;
      } else {
        // No crossing, simply reset the vector.
        times.clear();
        values.clear();
        times.emplace_back(tNew);
        values.emplace_back(vNew);
        nValues = 1;
      }
    }
    // Check the final vector.
    if ((values[0] - thr) * (thr - values.back()) >= 0. && nValues > 1 &&
        ((rise && values.back() > values[0]) ||
         (fall && values.back() < values[0]))) {
      double tcr = Numerics::Divdif(times, values, nValues, thr, iOrder);
      ThresholdCrossing newCrossing;
      newCrossing.time = tcr;
      newCrossing.rise = rise;
      m_thresholdCrossings.push_back(std::move(newCrossing));
    }
    if (rise) {
      rise = false;
      fall = true;
    } else if (fall) {
      rise = fall = false;
    }
  }
  n = m_thresholdCrossings.size();

  if (m_debug) {
    std::cout << m_className << "::ComputeThresholdCrossings:\n";
    std::cout << "    Found " << n << " crossings.\n";
    if (n > 0) std::cout << "      Time  [ns]    Direction\n";
    for (int i = 0; i < n; ++i) {
      std::cout << "      " << m_thresholdCrossings[i].time << "      ";
      if (m_thresholdCrossings[i].rise) {
        std::cout << "rising\n";
      } else {
        std::cout << "falling\n";
      }
    }
  }
  // Seems to have worked.
  return true;
}

bool Sensor::GetThresholdCrossing(const unsigned int i, double& time, 
                                  double& level, bool& rise) const {

  level = m_thresholdLevel;

  if (i >= m_thresholdCrossings.size()) {
    std::cerr << m_className << "::GetThresholdCrossing:\n";
    std::cerr << "    Index (" << i << ") out of range.\n";
    time = m_tStart + m_nTimeBins * m_tStep;
    return false;
  }

  time = m_thresholdCrossings[i].time;
  rise = m_thresholdCrossings[i].rise;
  return true;
}

bool Sensor::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                            double& xmax, double& ymax, double& zmax) {

  // We don't know the range yet
  bool set = false;
  // Loop over the fields
  double x0, y0, z0, x1, y1, z1;
  for (auto component : m_components) {
    if (!component->GetBoundingBox(x0, y0, z0, x1, y1, z1)) continue;
    if (set) {
      if (x0 < xmin) xmin = x0;
      if (y0 < ymin) ymin = y0;
      if (z0 < zmin) zmin = z0;
      if (x1 > xmax) xmax = x1;
      if (y1 > ymax) ymax = y1;
      if (z1 > zmax) zmax = z1;
    } else {
      xmin = x0;
      ymin = y0;
      zmin = z0;
      xmax = x1;
      ymax = y1;
      zmax = z1;
      set = true;
    }
  }

  // Warn if we still don't know the range
  if (!set) {
    std::cerr << m_className << "::GetBoundingBox:\n";
    std::cerr << "    Sensor bounding box not known.\n";
    xmin = ymin = zmin = 0.;
    xmax = ymax = zmax = 0.;
    return false;
  }

  if (m_debug) {
    std::cout << m_className << "::GetBoundingBox:\n";
    std::cout << "    " << xmin << " < x [cm] < " << xmax << "\n";
    std::cout << "    " << ymin << " < y [cm] < " << ymax << "\n";
    std::cout << "    " << zmin << " < z [cm] < " << zmax << "\n";
  }
  return true;
}
}
