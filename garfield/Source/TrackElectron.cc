#include <iostream>
#include <cmath>

#include "Sensor.hh"
#include "TrackElectron.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackElectron::TrackElectron()
    : m_ready(false),
      m_x(0.),
      m_y(0.),
      m_z(0.),
      m_t(0.),
      m_dx(0.),
      m_dy(0),
      m_dz(1.),
      m_mediumName(""),
      m_mediumDensity(0.),
      m_mfp(0.) {

  m_className = "TrackElectron";

  // Setup the particle properties.
  m_q = -1;
  m_spin = 1;
  m_mass = ElectronMass;
  m_isElectron = true;
  SetBetaGamma(3.);
  m_particleName = "electron";

}

void TrackElectron::SetParticle(std::string particle) {

  if (particle != "electron" && particle != "e" && particle != "e-") {
    std::cerr << m_className << "::SetParticle:\n";
    std::cerr << "    Only electrons can be transported.\n";
  }
}

bool TrackElectron::NewTrack(const double x0, const double y0, const double z0,
                             const double t0, const double dx0,
                             const double dy0, const double dz0) {

  m_ready = false;

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  // Get the medium at this location and check if it is "ionisable".
  Medium* medium = NULL; 
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  }
  if (!medium->IsIonisable()) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    return false;
  }

  // Check if the medium is a gas.
  if (!medium->IsGas()) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    Medium at initial position is not a gas.\n";
    return false;
  }

  if (!SetupGas(medium)) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    Properties of medium " << medium->GetName()
              << " are not available.\n";
    return false;
  }

  if (!UpdateCrossSection()) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    Cross-sections could not be calculated.\n";
    return false;
  }

  m_mediumName = medium->GetName();

  m_x = x0;
  m_y = y0;
  m_z = z0;
  m_t = t0;
  const double dd = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (dd < Small) {
    if (m_debug) {
      std::cout << m_className << "::NewTrack:\n";
      std::cout << "    Direction vector has zero norm.\n";
      std::cout << "    Initial direction is randomized.\n";
    }
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();
    m_dx = cos(phi) * stheta;
    m_dy = sin(phi) * stheta;
    m_dz = ctheta;
  } else {
    // Normalize the direction vector.
    m_dx = dx0 / dd;
    m_dy = dy0 / dd;
    m_dz = dz0 / dd;
  }

  m_ready = true;
  return true;
}

bool TrackElectron::GetCluster(double& xcls, double& ycls, double& zcls,
                               double& tcls, int& ncls, double& edep,
                               double& extra) {

  edep = extra = 0.;
  ncls = 0;

  m_electrons.clear();

  if (!m_ready) {
    std::cerr << m_className << "::GetCluster:\n"
              << "    Track not initialized. Call NewTrack first.\n";
    return false;
  }

  // Draw a step length and propagate the electron.
  const double d = -m_mfp * log(RndmUniformPos());
  m_x += d * m_dx;
  m_y += d * m_dy;
  m_z += d * m_dz;
  m_t += d / (sqrt(m_beta2) * SpeedOfLight);

  if (!m_sensor->IsInArea(m_x, m_y, m_z)) {
    m_ready = false;
    return false;
  }

  Medium* medium = NULL;
  if (!m_sensor->GetMedium(m_x, m_y, m_z, medium)) {
    m_ready = false;
    return false;
  }

  if (medium->GetName() != m_mediumName ||
      medium->GetNumberDensity() != m_mediumDensity || !medium->IsIonisable()) {
    m_ready = false;
    return false;
  }

  xcls = m_x;
  ycls = m_y;
  zcls = m_z;
  tcls = m_t;
  const double r = RndmUniform();
  int iComponent = 0;
  const int nComponents = m_components.size();
  for (int i = 0; i < nComponents; ++i) {
    if (r <= RndmUniform()) {
      iComponent = i;
      break;
    }
  }

  // Sample secondary electron energy according to
  // Opal-Beaty-Peterson splitting function.
  const double e0 = ElectronMass * (sqrt(1. / (1. - m_beta2)) - 1.);
  double esec = m_components[iComponent].wSplit *
                tan(RndmUniform() * atan((e0 - m_components[iComponent].ethr) /
                                         (2. * m_components[iComponent].wSplit)));
  esec = m_components[iComponent].wSplit *
         pow(esec / m_components[iComponent].wSplit, 0.9524);
  m_electrons.resize(1);
  m_electrons[0].energy = esec;
  m_electrons[0].x = xcls;
  m_electrons[0].y = ycls;
  m_electrons[0].z = zcls;

  ncls = 1;
  edep = esec;

  return true;
}

double TrackElectron::GetClusterDensity() {

  if (!m_ready) {
    std::cerr << m_className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (m_mfp <= 0.) {
    std::cerr << m_className << "::GetClusterDensity:\n";
    std::cerr << "    Mean free path is not available.\n";
    return 0.;
  }

  return 1. / m_mfp;
}

double TrackElectron::GetStoppingPower() {

  if (!m_ready) {
    std::cerr << m_className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialised.\n";
    return 0.;
  }

  const double prefactor = 4 * Pi * pow(HbarC / ElectronMass, 2);
  const double lnBg2 = log(m_beta2 / (1. - m_beta2));

  double dedx = 0.;
  // Primary energy
  const double e0 = ElectronMass * (sqrt(1. / (1. - m_beta2)) - 1.);
  const int nComponents = m_components.size();
  for (int i = nComponents; i--;) {
    // Calculate the mean number of clusters per cm.
    const double cmean =
        m_mediumDensity * m_components[i].fraction * (prefactor / m_beta2) *
        (m_components[i].m2Ion * (lnBg2 - m_beta2) + m_components[i].cIon);
    const double ew = (e0 - m_components[i].ethr) / (2 * m_components[i].wSplit);
    // Calculate the mean secondary electron energy.
    const double emean =
        (m_components[i].wSplit / (2 * atan(ew))) * log(1. + ew * ew);
    dedx += cmean * emean;
  }

  return dedx;
}

bool TrackElectron::SetupGas(Medium* gas) {

  m_components.clear();

  if (!gas) {
    std::cerr << m_className << "::SetupGas:\n";
    std::cerr << "     Medium is not defined.\n";
    return false;
  }

  m_mediumDensity = gas->GetNumberDensity();
  const int nComponents = gas->GetNumberOfComponents();
  if (nComponents <= 0) {
    std::cerr << m_className << "::SetupGas:\n";
    std::cerr << "    Medium composition is not defined.\n";
    return false;
  }
  m_components.resize(nComponents);

  // Density correction parameters from
  //   R. M. Sternheimer, M. J. Berger, S. M. Seltzer,
  //   Atomic Data and Nuclear Data Tables 30 (1984), 261-271
  bool ok = true;
  for (int i = nComponents; i--;) {
    std::string gasname = "";
    double frac = 0.;
    gas->GetComponent(i, gasname, frac);
    m_components[i].fraction = frac;
    m_components[i].p = 0.;
    if (gasname == "CF4") {
      m_components[i].m2Ion = 7.2;
      m_components[i].cIon = 93.;
      m_components[i].x0Dens = 1.;
      m_components[i].x1Dens = 0.;
      m_components[i].cDens = 0.;
      m_components[i].aDens = 0.;
      m_components[i].mDens = 0.;
      m_components[i].ethr = 15.9;
      m_components[i].wSplit = 19.5;
    } else if (gasname == "Ar") {
      m_components[i].m2Ion = 3.593;
      m_components[i].cIon = 39.7;
      m_components[i].x0Dens = 1.7635;
      m_components[i].x1Dens = 4.4855;
      m_components[i].cDens = 11.9480;
      m_components[i].aDens = 0.19714;
      m_components[i].mDens = 2.9618;
      m_components[i].ethr = 15.75961;
      m_components[i].wSplit = 15.;
    } else if (gasname == "He") {
      m_components[i].m2Ion = 0.489;
      m_components[i].cIon = 5.5;
      m_components[i].x0Dens = 2.2017;
      m_components[i].x1Dens = 3.6122;
      m_components[i].cDens = 11.1393;
      m_components[i].aDens = 0.13443;
      m_components[i].mDens = 5.8347;
      m_components[i].ethr = 24.58739;
      m_components[i].wSplit = 10.5;
    } else if (gasname == "He-3") {
      m_components[i].m2Ion = 0.489;
      m_components[i].cIon = 5.5;
      m_components[i].x0Dens = 2.2017;
      m_components[i].x1Dens = 3.6122;
      m_components[i].cDens = 11.1393;
      m_components[i].aDens = 0.13443;
      m_components[i].mDens = 5.8347;
      m_components[i].ethr = 24.58739;
      m_components[i].wSplit = 10.5;
    } else if (gasname == "Ne") {
      m_components[i].m2Ion = 1.69;
      m_components[i].cIon = 17.8;
      m_components[i].x0Dens = 2.0735;
      m_components[i].x1Dens = 4.6421;
      m_components[i].cDens = 11.9041;
      m_components[i].aDens = 0.08064;
      m_components[i].mDens = 3.5771;
      m_components[i].ethr = 21.56454;
      m_components[i].wSplit = 19.5;
    } else if (gasname == "Kr") {
      m_components[i].m2Ion = 5.5;
      m_components[i].cIon = 56.9;
      m_components[i].x0Dens = 1.7158;
      m_components[i].x1Dens = 5.0748;
      m_components[i].cDens = 12.5115;
      m_components[i].aDens = 0.07446;
      m_components[i].mDens = 3.4051;
      m_components[i].ethr = 13.996;
      m_components[i].wSplit = 21.;
    } else if (gasname == "Xe") {
      m_components[i].m2Ion = 8.04;
      m_components[i].cIon = 75.25;
      m_components[i].x0Dens = 1.5630;
      m_components[i].x1Dens = 4.7371;
      m_components[i].cDens = 12.7281;
      m_components[i].aDens = 0.23314;
      m_components[i].mDens = 2.7414;
      m_components[i].ethr = 12.129843;
      m_components[i].wSplit = 23.7;
    } else if (gasname == "CH4") {
      m_components[i].m2Ion = 3.75;
      m_components[i].cIon = 42.5;
      m_components[i].x0Dens = 1.6263;
      m_components[i].x1Dens = 3.9716;
      m_components[i].cDens = 9.5243;
      m_components[i].aDens = 0.09253;
      m_components[i].mDens = 3.6257;
      m_components[i].ethr = 12.65;
      m_components[i].wSplit = 8.;
    } else if (gasname == "iC4H10") {
      m_components[i].m2Ion = 15.5;
      m_components[i].cIon = 160.;
      m_components[i].x0Dens = 1.3788;
      m_components[i].x1Dens = 3.7524;
      m_components[i].cDens = 8.5633;
      m_components[i].aDens = 0.10852;
      m_components[i].mDens = 3.4884;
      m_components[i].ethr = 10.67;
      m_components[i].wSplit = 7.;
    } else if (gasname == "CO2") {
      m_components[i].m2Ion = 5.6;
      m_components[i].cIon = 57.91;
      m_components[i].x0Dens = 1.6294;
      m_components[i].x1Dens = 4.1825;
      m_components[i].aDens = 0.11768;
      m_components[i].mDens = 3.3227;
      m_components[i].ethr = 13.777;
      m_components[i].wSplit = 13.;
    } else if (gasname == "N2") {
      m_components[i].m2Ion = 3.35;
      m_components[i].cIon = 38.1;
      m_components[i].x0Dens = 1.7378;
      m_components[i].x1Dens = 4.1323;
      m_components[i].cDens = 10.5400;
      m_components[i].aDens = 0.15349;
      m_components[i].mDens = 3.2125;
      m_components[i].ethr = 15.581;
      m_components[i].wSplit = 13.8;
    } else {
      std::cerr << m_className << "::SetupGas:\n";
      std::cerr << "    Cross-section for " << gasname
                << " is not available.\n";
      ok = false;
    }
  }

  if (!ok) {
    m_components.clear();
  }

  return true;
}

bool TrackElectron::UpdateCrossSection() {

  const double prefactor = 4 * Pi * pow(HbarC / ElectronMass, 2);
  const double lnBg2 = log(m_beta2 / (1. - m_beta2));
  // Parameter X in the Sternheimer fit formula
  const double eta = m_mediumDensity / LoschmidtNumber;
  const double x = 0.5 * (lnBg2 + log(eta)) / log(10.);
  double csSum = 0.;
  const int nComponents = m_components.size();
  for (int i = nComponents; i--;) {
    double delta = 0.;
    if (m_components[i].x0Dens < m_components[i].x1Dens &&
        x >= m_components[i].x0Dens) {
      delta = 2 * log(10.) * x - m_components[i].cDens;
      if (x < m_components[i].x1Dens) {
        delta += m_components[i].aDens *
                 pow(m_components[i].x1Dens - x, m_components[i].mDens);
      }
    }
    const double cs = (m_components[i].fraction * prefactor / m_beta2) *
                      (m_components[i].m2Ion * (lnBg2 - m_beta2 - delta) + 
                       m_components[i].cIon);
    m_components[i].p = cs;
    csSum += cs;
  }

  if (csSum <= 0.) {
    std::cerr << m_className << "::UpdateCrossSection:\n";
    std::cerr << "    Total cross-section <= 0.\n";
    return false;
  }

  m_mfp = 1. / (csSum * m_mediumDensity);

  for (int i = 0; i < nComponents; ++i) {
    m_components[i].p /= csSum;
    if (i > 0) m_components[i].p += m_components[i - 1].p;
  }

  return true;
}
}
