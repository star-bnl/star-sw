#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>

#include "Sensor.hh"
#include "TrackPAI.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackPAI::TrackPAI() : Track() {

  m_className = "TrackPAI";
}

bool TrackPAI::NewTrack(const double x0, const double y0, const double z0,
                        const double t0, const double dx0, const double dy0,
                        const double dz0) {

  m_ready = false;

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::NewTrack: Sensor is not defined.\n";
    return false;
  }

  // Get the medium at this location and check if it is "ionisable".
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::NewTrack: No medium at initial position.\n";
    return false;
  }
  if (!medium->IsIonisable()) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Medium at initial position is not ionisable.\n";
    return false;
  }

  if (medium->GetName() != m_mediumName ||
      medium->GetNumberDensity() != m_mediumDensity) {
    m_isChanged = true;
    if (!SetupMedium(medium)) {
      std::cerr << m_className << "::NewTrack:\n    Properties of medium "
                << medium->GetName() << " are not available.\n";
      return false;
    }
    m_mediumName = medium->GetName();
    m_mediumDensity = medium->GetNumberDensity();
  }

  m_ready = true;

  if (m_isChanged) {
    if (!SetupCrossSectionTable()) {
      std::cerr << m_className << "::NewTrack:\n"
                << "    Calculation of ionisation cross-section failed.\n";
      m_ready = false;
      return false;
    }
    m_isChanged = false;
  }

  m_x = x0;
  m_y = y0;
  m_z = z0;
  m_t = t0;
  const double d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (d < Small) {
    if (m_debug) {
      std::cout << m_className << "::NewTrack:\n"
                << "    Direction vector has zero norm.\n"
                << "    Initial direction is randomized.\n";
    }
    RndmDirection(m_dx, m_dy, m_dz);
  } else {
    // Normalize the direction vector.
    m_dx = dx0 / d;
    m_dy = dy0 / d;
    m_dz = dz0 / d;
  }
  return true;
}

bool TrackPAI::GetCluster(double& xcls, double& ycls, double& zcls,
                          double& tcls, int& ncls, double& edep,
                          double& extra) {

  ncls = 0;
  edep = extra = 0.;

  // Clear the stack.
  m_electrons.clear();
  m_holes.clear();

  if (!m_ready) {
    std::cerr << m_className << "::GetCluster:\n";
    std::cerr << "    Track not initialized. Call NewTrack first.\n";
    return false;
  }

  if (m_isChanged) {
    if (SetupCrossSectionTable()) {
      m_isChanged = false;
    } else {
      std::cerr << m_className << "::GetCluster:\n";
      std::cerr << "    Calculation of ionisation cross-section failed.\n";
      return false;
    }
  }

  // Draw a step length and propagate the particle.
  const double d = -m_imfp * log(RndmUniformPos());
  m_x += d * m_dx;
  m_y += d * m_dy;
  m_z += d * m_dz;
  m_t += d / m_speed;

  // Check the medium at this location.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(m_x, m_y, m_z, medium)) {
    m_ready = false;
    return false;
  }
  if (medium->GetName() != m_mediumName ||
      medium->GetNumberDensity() != m_mediumDensity || !medium->IsIonisable()) {
    m_ready = false;
    return false;
  }

  // Check if the particle is still inside the drift area.
  if (!m_sensor->IsInArea(m_x, m_y, m_z)) {
    m_ready = false;
    return false;
  }

  xcls = m_x;
  ycls = m_y;
  zcls = m_z;
  tcls = m_t;

  // Sample the energy deposition.
  double f = 0.;
  edep = SampleEnergyDeposit(RndmUniform(), f);
  // Update the particle energy.
  m_e -= edep;

  // Number of electron/hole (or electron/ion pairs) produced.
  ncls = 1;

  if (m_debug) {
    std::cout << m_className << "::GetCluster:\n";
    std::cout << "   Fraction of Rutherford scattering: " << f << "\n";
  }
  return true;
}

double TrackPAI::SampleEnergyDeposit(const double u, double& f) const {

  if (u > m_cdf.back()) { 
    // Use the free-electron differential cross-section.
    f = 1.;
    return SampleAsymptoticCs(u);
  }

  if (u <= m_cdf[0]) return m_energies[0];
  if (u >= 1.) return m_energies.back();

  // Find the energy loss by interpolation
  // from the cumulative distribution table.
  const auto begin = m_cdf.cbegin();
  const auto it1 = std::upper_bound(begin, m_cdf.cend(), u);
  if (it1 == m_cdf.cbegin()) return m_energies[0]; 
  const auto it0 = std::prev(it1);
  const double c0 = *it0;
  const double c1 = *it1;
  const double e0 = m_energies[it0 - begin];
  const double e1 = m_energies[it1 - begin];
  const double r0 = m_rutherford[it0 - begin];
  const double r1 = m_rutherford[it1 - begin]; 
  if (e0 < 100.) {
    const double edep = e0 + (u - c0) * (e1 - e0) / (c1 - c0);
    f = r0 + (edep - e0) * (r1 - r0) / (e1 - e0);
    return edep;
  }
  const double loge0 = log(e0);
  const double loge1 = log(e1);
  const double logc0 = log(c0);
  const double logc1 = log(c1);
  double edep = loge0 + (log(u) - logc0) * (loge1 - loge0) / (logc1 - logc0);
  f = r0 + (log(edep) - loge0) * (r1 - r0) / (loge1 - loge0);
  edep = exp(edep);
  return edep;
}

bool TrackPAI::SetupMedium(Medium* medium) {

  // Make sure that the medium is defined.
  if (!medium) {
    std::cerr << m_className << "::SetupMedium: Null pointer.\n";
    return false;
  }

  // Get the density and effective Z.
  m_electronDensity = medium->GetNumberDensity() * medium->GetAtomicNumber();
  if (m_electronDensity <= 0.) {
    std::cerr << m_className << "::SetupMedium:\n"
              << "    Unphysical electron density ("
              << m_electronDensity << ")\n";
    return false;
  }

  // Get the dielectric function.
  double emin, emax;
  if (!medium->GetOpticalDataRange(emin, emax)) {
    std::cerr << m_className << "::SetupMedium:\n";
    std::cerr << "    Could not load optical data for medium " << m_mediumName
              << ".\n";
    return false;
  }

  // Make sure the minimum energy is positive.
  if (emin < Small) emin = Small;

  // Reset the arrays.
  m_energies.clear();
  m_opticalDataTable.clear();
  opticalData newEpsilon;

  // Use logarithmically spaced energy steps.
  const double r = pow(emax / emin, 1. / double(m_nSteps));
  double eps1, eps2;

  double eC = 0.5 * emin * (1. + r);
  for (int i = 0; i < m_nSteps; ++i) {
    medium->GetDielectricFunction(eC, eps1, eps2);
    newEpsilon.eps1 = eps1;
    newEpsilon.eps2 = eps2;
    m_opticalDataTable.push_back(newEpsilon);
    m_energies.push_back(eC);
    eC *= r;
  }

  // Compute the integral of loss function times energy.
  m_opticalDataTable[0].integral = 0.;
  double integral = 0.;
  double f1 = m_energies[0] * LossFunction(m_opticalDataTable[0].eps1, 
                                           m_opticalDataTable[0].eps2);
  double f2 = f1;
  for (int i = 1; i < m_nSteps; ++i) {
    f2 = m_energies[i] *
         LossFunction(m_opticalDataTable[i].eps1, m_opticalDataTable[i].eps2);
    const double eM = 0.5 * (m_energies[i - 1] + m_energies[i]);
    medium->GetDielectricFunction(eM, eps1, eps2);
    const double fM = eM * LossFunction(eps1, eps2);
    // Simpson's rule
    integral += (f1 + 4 * fM + f2) * (m_energies[i] - m_energies[i - 1]) / 6.;
    m_opticalDataTable[i].integral = integral;
    f1 = f2;
  }

  // Check the consistency of the optical data by means of the TRK sum rule
  const double trk = 2 * Pi2 * FineStructureConstant * pow(HbarC, 3) *
                     m_electronDensity / ElectronMass;
  if (fabs(integral - trk) > 0.2 * trk) {
    std::cerr << m_className << "::SetupMedium:\n";
    std::cerr << "    Deviation from Thomas-Reiche-Kuhn sum rule by > 20%.\n";
    std::cerr << "    Optical data are probably incomplete or erroneous!\n";
  }

  return true;
}

double TrackPAI::GetClusterDensity() {

  if (!m_ready) {
    std::cerr << m_className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (m_isChanged) {
    if (SetupCrossSectionTable()) {
      m_isChanged = false;
    } else {
      std::cerr << m_className << "::GetClusterDensity:\n";
      std::cerr << "    Ionisation cross-section could not be calculated.\n";
      return 0.;
    }
  }

  return 1. / m_imfp;
}

double TrackPAI::GetStoppingPower() {

  if (!m_ready) {
    std::cerr << m_className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialised.\n";
    return 0.;
  }

  if (m_isChanged) {
    if (SetupCrossSectionTable()) {
      m_isChanged = false;
    } else {
      std::cerr << m_className << "::GetStoppingPower:\n";
      std::cerr << "    Ionisation cross-section could not be calculated.\n";
      return 0.;
    }
  }

  return m_dedx;
}

bool TrackPAI::SetupCrossSectionTable() {

  if (!m_ready) {
    std::cerr << m_className << "::SetupCrossSectionTable:\n"
              << "    Medium not set up.\n";
    return false;
  }

  const double c1 = 2. * Pi2 * FineStructureConstant * pow(HbarC, 3) *
                    m_electronDensity / ElectronMass;
  const double c2 = m_q * m_q * FineStructureConstant / (m_beta2 * Pi * HbarC);

  // Get the max. allowed energy transfer.
  m_emax = ComputeMaxTransfer();

  std::ofstream outfile;
  if (m_debug) outfile.open("dcs.txt", std::ios::out);

  // Compute the differential cross-section.
  std::vector<double> dcs;
  m_rutherford.clear();

  for (int i = 0; i < m_nSteps; ++i) {
    // Define shorthand variables for photon energy and dielectric function.
    const double egamma = m_energies[i];
    const double eps1 = m_opticalDataTable[i].eps1;
    const double eps2 = m_opticalDataTable[i].eps2;
    const double integral = m_opticalDataTable[i].integral;

    // First, calculate the distant-collision terms.
    double dcsLog = 0., dcsDensity = 0., dcsCher = 0.;
    if (eps2 > 0.) {
      // Normal case (loss function > 0).
      const double lf = LossFunction(eps1, eps2); 
      // Non-relativistic logarithmic term.
      dcsLog = lf * log(2 * ElectronMass * m_beta2 / egamma);
      // Relativistic logarithmic term (density effect)
      const double u = 1. - m_beta2 * eps1;
      const double v = m_beta2 * eps2;
      dcsDensity = -0.5 * lf * log(u * u + v * v);
      // "Cerenkov" term
      dcsCher =
          (m_beta2 - eps1 / (eps1 * eps1 + eps2 * eps2)) * (HalfPi - atan(u / v));
    } else if (eps1 > 1. / m_beta2) {
      // Imaginary part is zero, only the Cerenkov term contributes.
      dcsCher = Pi * (m_beta2 - 1. / eps1);
    }

    // Calculate the close-collision term (quasi-free scattering)
    double dcsRuth = 0.;
    double f = 0.;
    if (egamma > 0. && integral > 0.) {
      dcsRuth = integral / (egamma * egamma);
      f = dcsRuth / (dcsLog + dcsDensity + dcsCher);
    }
    m_rutherford.push_back(f);
    dcs.push_back(dcsLog + dcsDensity + dcsCher + dcsRuth);
    // If requested, write the cross-section terms to file.
    if (m_debug) {
      outfile << egamma << "  " << eps1 << "  " << eps2 << "  " << dcsLog* c2
              << "  " << dcsDensity* c2 << "  " << dcsCher* c2 << "  "
              << dcsRuth* c2 << "\n";
    }
  }
  if (m_debug) outfile.close();

  // Compute the cumulative distribution,
  // total cross-section and stopping power.
  m_cdf.clear();
  m_cdf.push_back(0.);
  m_dedx = 0.;
  double cs = 0.;
  for (int i = 1; i < m_nSteps; ++i) {
    const double e0 = m_energies[i - 1];
    const double e1 = m_energies[i];
    const double de = e1 - e0;
    const double dcs0 = dcs[i - 1];
    const double dcs1 = dcs[i];
    cs += 0.5 * (dcs0 + dcs1) * de;
    m_cdf.push_back(cs);
    m_dedx += 0.5 * (dcs0 * e0 + dcs1 * e1) * de;
  }

  // Add the contribution of high energy transfers to the stopping power
  // and the total cross-section
  const double elim = m_energies.back();
  if (elim < m_emax) {
    cs += c1 * ComputeCsTail(elim, m_emax);
    m_dedx += c1 * ComputeDeDxTail(elim, m_emax);
  } else {
    std::cerr << m_className << "::SetupCrossSectionTable:\n";
    std::cerr << "    Max. energy transfer lower than optical data range.\n";
  }

  if (cs <= 0.) {
    std::cerr << m_className << "::SetupCrossSectionTable:\n";
    std::cerr << "    Total cross-section <= 0.\n";
    return false;
  }

  // Normalise the cumulative distribution.
  for (int i = m_nSteps; i--;) m_cdf[i] /= cs;

  cs *= c2;
  m_dedx *= c2;

  // Compute the inelastic mean free path
  m_imfp = 1. / cs;

  return true;
}

double TrackPAI::ComputeMaxTransfer() const {

  if (m_isElectron) {
    // Max. transfer for electrons is half the kinetic energy.
    return 0.5 * (m_energy - m_mass);
  }

  // Other particles.
  const double bg2 = m_beta2 / (1. - m_beta2);
  const double mass2 = m_mass * m_mass;

  return 2. * mass2 * ElectronMass * bg2 /
         (mass2 + ElectronMass * ElectronMass + 2. * m_energy * ElectronMass);
}

double TrackPAI::ComputeCsTail(const double emin, const double emax) {

  if (m_isElectron) {
    // Electrons
    const double ek = m_energy - m_mass;
    return 1. / emin - 1. / emax - 2 * emin / (ek * ek) -
           emin * emin / ((ek - emin) * ek * ek);
  } else if (m_mass == ElectronMass) {
    // Positrons
    const double ek = m_energy - m_mass;
    return 1. / emin - 1. / emax + 3 * (emax - emin) / (ek * ek) -
           (emax - emin) * (ek * (emax + emin) +
                            (emin * emin + emin * emax + emax * emax) / 3.) /
               pow(ek, 4) -
           (2. / ek) * log(emax / emin);
  }

  switch (m_spin) {
    case 0:
      // Spin 0
      return 1. / emin - 1. / emax - m_beta2 * log(emax / emin) / emax;
      break;
    case 1:
      // Spin 1/2
      return 1. / emin - 1. / emax - m_beta2 * log(emax / emin) / emax +
             (emax - emin) / (2 * m_energy * m_energy);
      break;
    case 2: {
      // Spin 1
      const double e2 = 2 * m_energy * m_energy;
      const double ec = m_mass * m_mass / ElectronMass;
      const double a = 1. / (3 * ec);
      const double b = (emax - emin);
      return 1. / emin - 1. / emax + a * b * (emin + e2 + emax) / e2 -
             m_beta2 * a * b / emax + (a - m_beta2 / emax) * log(emax / emin);
      break;
    }
    default:
      break;
  }
  // Rutherford type cross-section
  return 1. / emin - 1. / emax;
}

double TrackPAI::ComputeDeDxTail(const double emin, const double emax) {

  if (m_isElectron) {
    const double ek = m_energy - m_mass;
    return -log(emin * (ek - emin) / (ek * ek)) +
           (1. / (8 * (emin - ek) * ek * ek)) *
               (-4 * pow(emin, 3) + 4 * emin * emin * ek +
                emin * ek * ek * (17. - 16. * CLog2) +
                pow(ek, 3) * (-9. + 16. * CLog2));
  } else if (m_mass == ElectronMass) {
    // Positron
    const double ek = m_energy - m_mass;
    return log(ek / emin) -
           (ek - emin) * (ek - emin) *
               (3. * emin * emin - 2. * emin * ek + 11. * ek * ek) /
               (12. * pow(ek, 4));
  }

  switch (m_spin) {
    case 0:
      return log(emax / emin) - m_beta2 * (emax - emin) / emax;
      break;
    case 1:
      // Spin 1/2
      return log(emax / emin) - m_beta2 * (emax - emin) / emax +
             (emax * emax - emin * emin) / (2 * m_energy * m_energy);
      break;
    case 2: {
      // Spin 1
      double e2 = m_energy * m_energy;
      double ec = m_mass * m_mass / ElectronMass;
      return log(emax / emin) + (pow(emax, 3) - pow(emin, 3)) / (9. * e2 * ec) +
             (emax * emax - emin * emin) / (6. * e2) + (emax - emin) * 
             (2. - (1. + emin / emax + 6 * ec / emax) * m_beta2) / (6. * ec);
      break;
    }
    default:
      break;
  }

  // Rutherford cross-section
  return log(emax / emin);
}

double TrackPAI::SampleAsymptoticCs(double u) const {

  const double emin = m_energies.back();
  // Rescale the random number
  u = (u - m_cdf.back()) / (1. - m_cdf.back());

  if (m_isElectron) {
    return SampleAsymptoticCsElectron(emin, u);
  } else if (m_mass == ElectronMass) {
    return SampleAsymptoticCsPositron(emin, u);
  }

  switch (m_spin) {
    case 0:
      // Spin 0
      return SampleAsymptoticCsSpinZero(emin, u);
      break;
    case 1:
      // Spin 1/2
      return SampleAsymptoticCsSpinHalf(emin, u);
      break;
    case 2:
      // Spin 1
      return SampleAsymptoticCsSpinOne(emin, u);
      break;
    default:
      break;
  }
  // Rutherford cross-section (analytic inversion)
  return emin * m_emax / (u * emin + (1. - u) * m_emax);
}

double TrackPAI::SampleAsymptoticCsSpinZero(const double emin, double u) const {

  const double a = emin / m_emax;
  const double b = m_beta2 * a;
  u *= (1. - a + b * log(a));
  double eLow = emin, eUp = m_emax;
  while (eUp - eLow > 1.) {
    const double eM = 0.5 * (eUp + eLow);
    if (u >= 1. - emin / eM - b * log(eM / emin)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }

  return 0.5 * (eLow + eUp);
}

double TrackPAI::SampleAsymptoticCsSpinHalf(const double emin, double u) const {

  const double a = emin / m_emax;
  const double b = m_beta2 * a;
  const double c = emin / (2. * m_energy * m_energy);
  u *= 1. - a + b * log(a) + (m_emax - emin) * c;
  double eLow = emin, eUp = m_emax;
  while (eUp - eLow > 1.) {
    const double eM = 0.5 * (eUp + eLow);
    if (u >= 1. - emin / eM - b * log(eM / emin) + (eM - emin) * c) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }

  return 0.5 * (eLow + eUp);
}

double TrackPAI::SampleAsymptoticCsSpinOne(const double emin, double u) const {

  const double e2 = 2 * m_energy * m_energy;
  const double ec = m_mass * m_mass / ElectronMass;
  const double a = 2 * ec / e2 - m_beta2 / m_emax;
  const double b = 1.5 * ec / emin;
  const double c = 1. - 1.5 * ec * m_beta2 / m_emax;
  u *= (m_emax - emin) * (0.5 * (emin + m_emax) / e2 + a + b / m_emax) +
       c * log(m_emax / emin);
  double eLow = emin, eUp = m_emax;
  while (eUp - eLow > 1.) {
    const double eM = 0.5 * (eUp + eLow);
    if (u >=
        (eM - emin) * ((emin + eM) / e2 + a + b / eM) + c * log(eM / emin)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }

  return 0.5 * (eLow + eUp);
}

double TrackPAI::SampleAsymptoticCsElectron(const double emin, double u) const {

  const double ek = m_energy - m_mass;
  const double ek2 = ek * ek;
  const double a = ek / (emin * (ek - emin));
  const double norm = 1. / emin - 0.5 / ek - emin * emin / ((ek - emin) * ek2) -
                      2. * emin / ek2;
  u *= norm;
  double eLow = emin, eUp = m_emax;
  while (eUp - eLow > 1.) {
    const double eM = 0.5 * (eUp + eLow);
    if (u >= a - 1. / eM + (eM - emin) / ek2 + 1. / (ek - eM)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }
  return 0.5 * (eLow + eUp);
}

double TrackPAI::SampleAsymptoticCsPositron(const double emin, double u) const {

  const double ek = m_energy - m_mass;
  const double ek2 = ek * ek;
  const double ek3 = ek2 * ek;
  const double ek4 = 3 * ek3 * ek;
  const double emin2 = emin * emin;
  const double a = 1. / emin;
  const double b = 3. / ek2;
  const double c = 2. / ek;
  u *= 1. / emin - 1. / m_emax + 3 * (m_emax - emin) / ek2 -
       (m_emax - emin) * (m_emax + emin) / ek3 +
       (m_emax - emin) * (emin * emin + emin * m_emax + m_emax * m_emax) / ek4 -
       (2. / ek) * log(m_emax / emin);
  double eLow = emin, eUp = m_emax;
  while (eUp - eLow > 1.) {
    const double eM = 0.5 * (eUp + eLow);
    const double eM2 = eM * eM;
    if (u >= a - 1. / eM + b * (eM - emin) - (eM2 - emin2) / ek3 +
                 (eM - emin) * (emin2 + emin * eM + eM2) / ek4 -
                 c * log(eM / emin)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }

  return 0.5 * (eLow + eUp);
}

}
