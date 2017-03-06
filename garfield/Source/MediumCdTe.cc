#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <vector>

#include "MediumCdTe.hh"
#include "Random.hh"
#include "GarfieldConstants.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

MediumCdTe::MediumCdTe()
    : Medium(),
      // m_bandGap(1.44),
      m_eMobility(1.1e-6),
      m_hMobility(0.1e-6),
      m_eSatVel(1.02e-2),
      m_hSatVel(0.72e-2),
      m_eHallFactor(1.15),
      m_hHallFactor(0.7),
      m_eTrapCs(1.e-15),
      m_hTrapCs(1.e-15),
      m_eTrapDensity(1.e13),
      m_hTrapDensity(1.e13),
      m_eTrapTime(0.),
      m_hTrapTime(0.),
      m_trappingModel(0),
      m_hasUserMobility(false),
      m_hasUserSaturationVelocity(false),
      m_opticalDataFile("OpticalData_CdTe.txt") {

  m_className = "MediumCdTe";
  m_name = "CdTe";

  SetTemperature(300.);
  SetDielectricConstant(11.);
  SetAtomicNumber(48.52);
  SetAtomicWeight(240.01);
  SetMassDensity(5.85);

  EnableDrift();
  EnablePrimaryIonisation();
  m_microscopic = false;

  m_w = 4.43;
  m_fano = 0.1;
}

void MediumCdTe::GetComponent(const unsigned int i, 
                              std::string& label, double& f) {

  if (i == 0) {
    label = "Cd";
    f = 0.5;
  } else if (i == 1) {
    label = "Te";
    f = 0.5;
  } else {
    std::cerr << m_className << "::GetComponent:\n    Index out of range.\n";
  }
}

void MediumCdTe::SetTrapCrossSection(const double ecs, const double hcs) {

  if (ecs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n"
              << "    Capture cross-section [cm2] must positive.\n";
  } else {
    m_eTrapCs = ecs;
  }

  if (hcs < 0.) {
    std::cerr << m_className << "::SetTrapCrossSection:\n"
              << "    Capture cross-section [cm2] must be positive.n";
  } else {
    m_hTrapCs = hcs;
  }

  m_trappingModel = 0;
  m_isChanged = true;
}

void MediumCdTe::SetTrapDensity(const double n) {

  if (n < 0.) {
    std::cerr << m_className << "::SetTrapDensity:\n"
              << "    Trap density [cm-3] must be greater than zero.\n";
  } else {
    m_eTrapDensity = n;
    m_hTrapDensity = n;
  }

  m_trappingModel = 0;
  m_isChanged = true;
}

void MediumCdTe::SetTrappingTime(const double etau, const double htau) {

  if (etau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n"
              << "    Trapping time [ns-1] must be greater than zero.\n";
  } else {
    m_eTrapTime = etau;
  }

  if (htau <= 0.) {
    std::cerr << m_className << "::SetTrappingTime:\n"
              << "    Trapping time [ns-1] must be greater than zero.\n";
  } else {
    m_hTrapTime = htau;
  }

  m_trappingModel = 1;
  m_isChanged = true;
}

bool MediumCdTe::ElectronVelocity(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz, double& vx,
                                  double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (m_hasElectronVelocityE) {
    // Interpolation in user table.
    return Medium::ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  const double mu = -m_eMobility;
  const double b2 = bx * bx + by * by + bz * bz;
  if (b2 < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = m_eHallFactor * mu;
    const double muH2 = muH * muH;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + muH2 * b2;
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH2 * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH2 * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH2 * bz * eb) / nom;
  }
  return true;
}

bool MediumCdTe::ElectronTownsend(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz,
                                  double& alpha) {

  alpha = 0.;
  if (!tabElectronTownsend.empty()) {
    // Interpolation in user table.
    return Medium::ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool MediumCdTe::ElectronAttachment(const double ex, const double ey,
                                    const double ez, const double bx,
                                    const double by, const double bz,
                                    double& eta) {

  eta = 0.;
  if (m_hasElectronAttachment) {
    // Interpolation in user table.
    return Medium::ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
  }

  switch (m_trappingModel) {
    case 0:
      eta = m_eTrapCs * m_eTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = m_eTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << m_className << "::ElectronAttachment:\n"
                << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }

  return true;
}

bool MediumCdTe::HoleVelocity(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (m_hasHoleVelocityE) {
    // Interpolation in user table.
    return Medium::HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  }
  // Calculate the mobility
  const double mu = m_hMobility;
  const double b2 = bx * bx + by * by + bz * bz;
  if (b2 < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    // Hall mobility
    const double muH = m_hHallFactor * mu;
    const double muH2 = muH * muH;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + muH2 * b2;
    // Compute the drift velocity using the Langevin equation.
    vx = mu * (ex + muH * (ey * bz - ez * by) + muH2 * bx * eb) / nom;
    vy = mu * (ey + muH * (ez * bx - ex * bz) + muH2 * by * eb) / nom;
    vz = mu * (ez + muH * (ex * by - ey * bx) + muH2 * bz * eb) / nom;
  }
  return true;
}

bool MediumCdTe::HoleTownsend(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& alpha) {

  alpha = 0.;
  if (m_hasHoleTownsend) {
    // Interpolation in user table.
    return Medium::HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
  }
  return false;
}

bool MediumCdTe::HoleAttachment(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& eta) {

  eta = 0.;
  if (m_hasHoleAttachment) {
    // Interpolation in user table.
    return Medium::HoleAttachment(ex, ey, ez, bx, by, bz, eta);
  }
  switch (m_trappingModel) {
    case 0:
      eta = m_hTrapCs * m_hTrapDensity;
      break;
    case 1:
      double vx, vy, vz;
      HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
      eta = m_hTrapTime * sqrt(vx * vx + vy * vy + vz * vz);
      if (eta > 0.) eta = 1. / eta;
      break;
    default:
      std::cerr << m_className << "::HoleAttachment:\n"
                << "    Unknown model activated. Program bug!\n";
      return false;
      break;
  }
  return true;
}

void MediumCdTe::SetLowFieldMobility(const double mue, const double muh) {

  if (mue <= 0. || muh <= 0.) {
    std::cerr << m_className << "::SetLowFieldMobility:\n"
              << "    Mobility must be greater than zero.\n";
    return;
  }

  m_eMobility = mue;
  m_hMobility = muh;
  m_hasUserMobility = true;
  m_isChanged = true;
}

void MediumCdTe::SetSaturationVelocity(const double vsate, const double vsath) {

  if (vsate <= 0. || vsath <= 0.) {
    std::cout << m_className << "::SetSaturationVelocity:\n"
              << "    Restoring default values.\n";
    m_hasUserSaturationVelocity = false;
  } else {
    m_eSatVel = vsate;
    m_hSatVel = vsath;
    m_hasUserSaturationVelocity = true;
  }
  m_isChanged = true;
}

bool MediumCdTe::GetOpticalDataRange(double& emin, double& emax, 
                                     const unsigned int i) {

  if (i != 0) {
    std::cerr << m_className << "::GetOpticalDataRange:\n"
              << "    Medium has only one component.\n";
  }

  // Make sure the optical data table has been loaded.
  if (m_opticalDataTable.empty()) {
    if (!LoadOpticalData(m_opticalDataFile)) {
      std::cerr << m_className << "::GetOpticalDataRange:\n"
                << "    Optical data table could not be loaded.\n";
      return false;
    }
  }

  emin = m_opticalDataTable[0].energy;
  emax = m_opticalDataTable.back().energy;
  if (m_debug) {
    std::cout << m_className << "::GetOpticalDataRange:\n    "
              << emin << " < E [eV] < " << emax << "\n";
  }
  return true;
}

bool MediumCdTe::GetDielectricFunction(const double e, double& eps1,
                                       double& eps2, const unsigned int i) {

  if (i != 0) {
    std::cerr << m_className << "::GetDielectricFunction:\n"
              << "    Medium has only one component.\n";
    return false;
  }

  // Make sure the optical data table has been loaded.
  if (m_opticalDataTable.empty()) {
    if (!LoadOpticalData(m_opticalDataFile)) {
      std::cerr << m_className << "::GetDielectricFunction:\n"
                << "    Optical data table could not be loaded.\n";
      return false;
    }
  }

  // Make sure the requested energy is within the range of the table.
  const double emin = m_opticalDataTable[0].energy;
  const double emax = m_opticalDataTable.back().energy;
  if (e < emin || e > emax) {
    std::cerr << m_className << "::GetDielectricFunction:\n"
              << "    Requested energy (" << e << " eV)"
              << " is outside the range of the optical data table.\n"
              << "    " << emin << " < E [eV] < " << emax << "\n";
    eps1 = eps2 = 0.;
    return false;
  }

  // Locate the requested energy in the table.
  int iLo = 0;
  int iUp = m_opticalDataTable.size() - 1;
  int iM;
  while (iUp - iLo > 1) {
    iM = (iUp + iLo) >> 1;
    if (e >= m_opticalDataTable[iM].energy) {
      iLo = iM;
    } else {
      iUp = iM;
    }
  }

  // Interpolate the real part of dielectric function.
  // Use linear interpolation if one of the values is negative,
  // Otherwise use log-log interpolation.
  const double logX0 = log(m_opticalDataTable[iLo].energy);
  const double logX1 = log(m_opticalDataTable[iUp].energy);
  const double logX = log(e);
  if (m_opticalDataTable[iLo].eps1 <= 0. || 
      m_opticalDataTable[iUp].eps1 <= 0.) {
    eps1 = m_opticalDataTable[iLo].eps1 +
           (e - m_opticalDataTable[iLo].energy) *
           (m_opticalDataTable[iUp].eps1 - m_opticalDataTable[iLo].eps1) /
           (m_opticalDataTable[iUp].energy - m_opticalDataTable[iLo].energy);
  } else {
    const double logY0 = log(m_opticalDataTable[iLo].eps1);
    const double logY1 = log(m_opticalDataTable[iUp].eps1);
    eps1 = logY0 + (logX - logX0) * (logY1 - logY0) / (logX1 - logX0);
    eps1 = exp(eps1);
  }

  // Interpolate the imaginary part of dielectric function,
  // using log-log interpolation.
  const double logY0 = log(m_opticalDataTable[iLo].eps2);
  const double logY1 = log(m_opticalDataTable[iUp].eps2);
  eps2 = logY0 + (log(e) - logX0) * (logY1 - logY0) / (logX1 - logX0);
  eps2 = exp(eps2);
  return true;
}

bool MediumCdTe::LoadOpticalData(const std::string& filename) {

  // Get the path to the data directory.
  char* pPath = getenv("GARFIELD_HOME");
  if (pPath == 0) {
    std::cerr << m_className << "::LoadOpticalData:\n"
              << "    Environment variable GARFIELD_HOME is not set.\n";
    return false;
  }
  const std::string filepath = std::string(pPath) + "/Data/" + filename;

  // Open the file.
  std::ifstream infile;
  infile.open(filepath.c_str(), std::ios::in);
  // Make sure the file could actually be opened.
  if (!infile) {
    std::cerr << m_className << "::LoadOpticalData:\n"
              << "    Error opening file " << filename << ".\n";
    return false;
  }

  // Clear the optical data table.
  m_opticalDataTable.clear();

  double lastEnergy = -1.;
  double energy, eps1, eps2, loss;
  opticalData data;
  // Read the file line by line.
  std::string line;
  std::istringstream dataStream;
  int i = 0;
  while (!infile.eof()) {
    ++i;
    // Read the next line.
    std::getline(infile, line);
    // Strip white space from the beginning of the line.
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments.
    if (line[0] == '#' || line[0] == '*' || (line[0] == '/' && line[1] == '/'))
      continue;
    // Extract the values.
    dataStream.str(line);
    dataStream >> energy >> eps1 >> eps2 >> loss;
    if (dataStream.eof()) break;
    // Check if the data has been read correctly.
    if (infile.fail()) {
      std::cerr << m_className << "::LoadOpticalData:\n    Error reading file "
                << filename << " (line " << i << ").\n";
      return false;
    }
    // Reset the stringstream.
    dataStream.str("");
    dataStream.clear();
    // Make sure the values make sense.
    // The table has to be in ascending order
    //  with respect to the photon energy.
    if (energy <= lastEnergy) {
      std::cerr << m_className << "::LoadOpticalData:\n    Table is not in "
                << "monotonically increasing order (line " << i << ").\n    "
                << lastEnergy << "  " << energy << "  " 
                << eps1 << "  " << eps2 << "\n";
      return false;
    }
    // The imaginary part of the dielectric function has to be positive.
    if (eps2 < 0.) {
      std::cerr << m_className << "::LoadOpticalData:\n    Negative value "
                << "of the loss function (line " << i << ").\n";
      return false;
    }
    // Ignore negative photon energies.
    if (energy <= 0.) continue;
    // Add the values to the list.
    data.energy = energy;
    data.eps1 = eps1;
    data.eps2 = eps2;
    m_opticalDataTable.push_back(data);
    lastEnergy = energy;
  }

  const int nEntries = m_opticalDataTable.size();
  if (nEntries <= 0) {
    std::cerr << m_className << "::LoadOpticalData:\n    Importing data from "
              << filepath << "failed.\n    No valid data found.\n";
    return false;
  }

  if (m_debug) {
    std::cout << m_className << "::LoadOpticalData:\n    Read " << nEntries
              << " values from file " << filepath << "\n";
  }
  return true;
}
}
