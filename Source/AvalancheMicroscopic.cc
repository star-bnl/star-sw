#include <iostream>
#include <cmath>
#include <string>
#include <functional>
#include <algorithm>

#include "AvalancheMicroscopic.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

AvalancheMicroscopic::AvalancheMicroscopic()
    : m_sensor(NULL),
      m_nElectrons(0),
      m_nHoles(0),
      m_nIons(0),
      m_usePlotting(false),
      m_viewer(NULL),
      m_plotExcitations(true),
      m_plotIonisations(true),
      m_plotAttachments(true),
      m_histElectronEnergy(NULL),
      m_histHoleEnergy(NULL),
      m_histDistance(NULL),
      m_distanceOption('r'),
      m_histSecondary(NULL),
      m_useSignal(false),
      m_useInducedCharge(false),
      m_useDriftLines(false),
      m_usePhotons(false),
      m_useBandStructureDefault(true),
      m_useNullCollisionSteps(false),
      m_useBfield(false),
      m_rb11(1.),
      m_rb12(0.),
      m_rb13(0.),
      m_rb21(0.),
      m_rb22(1.),
      m_rb23(0.),
      m_rb31(0.),
      m_rb32(0.),
      m_rb33(1.),
      m_rx22(1.),
      m_rx23(0.),
      m_rx32(0.),
      m_rx33(1.),
      m_deltaCut(0.),
      m_gammaCut(0.),
      m_sizeCut(0),
      m_nCollSkip(100),
      m_hasTimeWindow(false),
      m_tMin(0.),
      m_tMax(0.),
      m_hasUserHandleStep(false),
      m_hasUserHandleAttachment(false),
      m_hasUserHandleInelastic(false),
      m_hasUserHandleIonisation(false),
      m_userHandleStep(0),
      m_userHandleAttachment(0),
      m_userHandleInelastic(0),
      m_userHandleIonisation(0),
      m_debug(false) {

  m_className = "AvalancheMicroscopic";

  m_stack.reserve(10000);
  m_endpointsElectrons.reserve(10000);
  m_endpointsHoles.reserve(10000);
  m_photons.reserve(1000);

}

void AvalancheMicroscopic::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n    Null pointer.\n";
    return;
  }
  m_sensor = s;
}

void AvalancheMicroscopic::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n    Null pointer.\n";
    return;
  }

  m_viewer = view;
  m_usePlotting = true;
  if (!m_useDriftLines) {
    std::cout << m_className << "::EnablePlotting:\n";
    std::cout << "    Enabling storage of drift line.\n";
    EnableDriftLines();
  }
}

void AvalancheMicroscopic::DisablePlotting() {

  m_viewer = NULL;
  m_usePlotting = false;
}

void AvalancheMicroscopic::EnableElectronEnergyHistogramming(TH1* histo) {

  if (!histo) {
    std::cerr << m_className << "::EnableElectronEnergyHistogramming:\n"
              << "    Null pointer.\n";
    return;
  }

  m_histElectronEnergy = histo;
}

void AvalancheMicroscopic::EnableHoleEnergyHistogramming(TH1* histo) {

  if (!histo) {
    std::cerr << m_className << "::EnableHoleEnergyHistogramming:\n"
              << "    Null pointer.\n";
    return;
  }

  m_histHoleEnergy = histo;
}

void AvalancheMicroscopic::SetDistanceHistogram(TH1* histo, const char opt) {

  if (!histo) {
    std::cerr << m_className << "::SetDistanceHistogram:\n    Null pointer.\n";
    return;
  }

  m_histDistance = histo;

  if (opt == 'x' || opt == 'y' || opt == 'z' || opt == 'r') {
    m_distanceOption = opt;
  } else {
    std::cerr << m_className << "::SetDistanceHistogram:";
    std::cerr << "    Unknown option " << opt << ".\n";
    std::cerr << "    Valid options are x, y, z, r.\n";
    std::cerr << "    Using default value (r).\n";
    m_distanceOption = 'r';
  }

  if (m_distanceHistogramType.empty()) {
    std::cout << m_className << "::SetDistanceHistogram:\n";
    std::cout << "    Don't forget to call EnableDistanceHistogramming.\n";
  }
}

void AvalancheMicroscopic::EnableDistanceHistogramming(const int type) {

  // Check if this type of collision is already registered
  // for histogramming.
  const unsigned int nDistanceHistogramTypes = m_distanceHistogramType.size();
  if (nDistanceHistogramTypes > 0) {
    for (unsigned int i = 0; i < nDistanceHistogramTypes; ++i) {
      if (m_distanceHistogramType[i] != type) continue;
      std::cout << m_className << "::EnableDistanceHistogramming:\n";
      std::cout << "    Collision type " << type
                << " is already being histogrammed.\n";
      return;
    }
  }

  m_distanceHistogramType.push_back(type);
  std::cout << m_className << "::EnableDistanceHistogramming:\n";
  std::cout << "    Histogramming of collision type " << type << " enabled.\n";
  if (!m_histDistance) {
    std::cout << "    Don't forget to set the histogram.\n";
  }
}

void AvalancheMicroscopic::DisableDistanceHistogramming(const int type) {

  if (m_distanceHistogramType.empty()) {
    std::cerr << m_className << "::DisableDistanceHistogramming:\n";
    std::cerr << "    Collision type " << type << " is not histogrammed.\n";
    return;
  }
  const unsigned int nDistanceHistogramTypes = m_distanceHistogramType.size();
  for (unsigned int i = 0; i < nDistanceHistogramTypes; ++i) {
    if (m_distanceHistogramType[i] != type) continue;
    m_distanceHistogramType.erase(m_distanceHistogramType.begin() + i);
    std::cout << "    Histogramming of collision type " << type
              << " disabled.\n";
    return;
  }

  std::cerr << m_className << "::DisableDistanceHistogramming:\n";
  std::cerr << "    Collision type " << type << " is not histogrammed.\n";
}

void AvalancheMicroscopic::DisableDistanceHistogramming() {

  m_histDistance = NULL;
  m_distanceHistogramType.clear();
}

void AvalancheMicroscopic::EnableSecondaryEnergyHistogramming(TH1* histo) {

  if (!histo) {
    std::cerr << m_className << "::EnableSecondaryEnergyHistogramming:\n"
              << "    Null pointer.\n";
    return;
  }

  m_histSecondary = histo;
}

void AvalancheMicroscopic::SetTimeWindow(const double t0, const double t1) {

  if (fabs(t1 - t0) < Small) {
    std::cerr << m_className << "::SetTimeWindow:\n";
    std::cerr << "    Time interval must be greater than zero.\n";
    return;
  }

  m_tMin = std::min(t0, t1);
  m_tMax = std::max(t0, t1);
  m_hasTimeWindow = true;
}

void AvalancheMicroscopic::UnsetTimeWindow() { m_hasTimeWindow = false; }

void AvalancheMicroscopic::GetElectronEndpoint(const unsigned int i, double& x0,
                                               double& y0, double& z0,
                                               double& t0, double& e0,
                                               double& x1, double& y1,
                                               double& z1, double& t1,
                                               double& e1, int& status) const {

  if (i >= m_endpointsElectrons.size()) {
    std::cerr << m_className << "::GetElectronEndpoint:\n";
    std::cerr << "    Endpoint index " << i << " out of range.\n";
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = z1 = t1 = e1 = 0.;
    status = 0;
    return;
  }

  x0 = m_endpointsElectrons[i].x0;
  y0 = m_endpointsElectrons[i].y0;
  z0 = m_endpointsElectrons[i].z0;
  t0 = m_endpointsElectrons[i].t0;
  e0 = m_endpointsElectrons[i].e0;
  x1 = m_endpointsElectrons[i].x;
  y1 = m_endpointsElectrons[i].y;
  z1 = m_endpointsElectrons[i].z;
  t1 = m_endpointsElectrons[i].t;
  e1 = m_endpointsElectrons[i].energy;
  status = m_endpointsElectrons[i].status;
}

void AvalancheMicroscopic::GetElectronEndpoint(
    const unsigned int i, double& x0, double& y0, double& z0, double& t0, double& e0,
    double& x1, double& y1, double& z1, double& t1, double& e1, double& dx1,
    double& dy1, double& dz1, int& status) const {

  if (i >= m_endpointsElectrons.size()) {
    std::cerr << m_className << "::GetElectronEndpoint:\n";
    std::cerr << "    Endpoint index " << i << " out of range.\n";
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = z1 = t1 = e1 = 0.;
    dx1 = dy1 = dz1 = 0.;
    status = 0;
    return;
  }

  x0 = m_endpointsElectrons[i].x0;
  y0 = m_endpointsElectrons[i].y0;
  z0 = m_endpointsElectrons[i].z0;
  t0 = m_endpointsElectrons[i].t0;
  e0 = m_endpointsElectrons[i].e0;
  x1 = m_endpointsElectrons[i].x;
  y1 = m_endpointsElectrons[i].y;
  z1 = m_endpointsElectrons[i].z;
  t1 = m_endpointsElectrons[i].t;
  e1 = m_endpointsElectrons[i].energy;
  dx1 = m_endpointsElectrons[i].kx;
  dy1 = m_endpointsElectrons[i].ky;
  dz1 = m_endpointsElectrons[i].kz;
  status = m_endpointsElectrons[i].status;
}

void AvalancheMicroscopic::GetHoleEndpoint(const unsigned int i, double& x0, double& y0,
                                           double& z0, double& t0, double& e0,
                                           double& x1, double& y1, double& z1,
                                           double& t1, double& e1,
                                           int& status) const {

  if (i >= m_endpointsHoles.size()) {
    std::cerr << m_className << "::GetHoleEndpoint:\n";
    std::cerr << "    Endpoint index " << i << " out of range.\n";
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = z1 = t1 = e1 = 0.;
    status = 0;
    return;
  }

  x0 = m_endpointsHoles[i].x0;
  y0 = m_endpointsHoles[i].y0;
  z0 = m_endpointsHoles[i].z0;
  t0 = m_endpointsHoles[i].t0;
  e0 = m_endpointsHoles[i].e0;
  x1 = m_endpointsHoles[i].x;
  y1 = m_endpointsHoles[i].y;
  z1 = m_endpointsHoles[i].z;
  t1 = m_endpointsHoles[i].t;
  e1 = m_endpointsHoles[i].energy;
  status = m_endpointsHoles[i].status;
}

unsigned int AvalancheMicroscopic::GetNumberOfElectronDriftLinePoints(const unsigned int i)
    const {

  if (i >= m_endpointsElectrons.size()) {
    std::cerr << m_className << "::GetNumberOfElectronDriftLinePoints:\n";
    std::cerr << "    Endpoint index (" << i << ") out of range.\n";
    return 0;
  }

  if (!m_useDriftLines) return 2;

  return m_endpointsElectrons[i].driftLine.size() + 2;
}

unsigned int AvalancheMicroscopic::GetNumberOfHoleDriftLinePoints(const unsigned int i) const {

  if (i >= m_endpointsHoles.size()) {
    std::cerr << m_className << "::GetNumberOfHoleDriftLinePoints:\n";
    std::cerr << "    Endpoint index (" << i << ") out of range.\n";
    return 0;
  }

  if (!m_useDriftLines) return 2;

  return m_endpointsHoles[i].driftLine.size() + 2;
}

void AvalancheMicroscopic::GetElectronDriftLinePoint(double& x, double& y,
                                                     double& z, double& t,
                                                     const int ip,
                                                     const unsigned int iel) const {

  if (iel >= m_endpointsElectrons.size()) {
    std::cerr << m_className << "::GetElectronDriftLinePoint:\n";
    std::cerr << "    Endpoint index (" << iel << ") out of range.\n";
    return;
  }

  if (ip <= 0) {
    x = m_endpointsElectrons[iel].x0;
    y = m_endpointsElectrons[iel].y0;
    z = m_endpointsElectrons[iel].z0;
    t = m_endpointsElectrons[iel].t0;
    return;
  }

  const int np = m_endpointsElectrons[iel].driftLine.size();
  if (ip > np) {
    x = m_endpointsElectrons[iel].x;
    y = m_endpointsElectrons[iel].y;
    z = m_endpointsElectrons[iel].z;
    t = m_endpointsElectrons[iel].t;
    return;
  }

  x = m_endpointsElectrons[iel].driftLine[ip - 1].x;
  y = m_endpointsElectrons[iel].driftLine[ip - 1].y;
  z = m_endpointsElectrons[iel].driftLine[ip - 1].z;
  t = m_endpointsElectrons[iel].driftLine[ip - 1].t;
}

void AvalancheMicroscopic::GetHoleDriftLinePoint(double& x, double& y,
                                                 double& z, double& t,
                                                 const int ip,
                                                 const unsigned int ih) const {

  if (ih >= m_endpointsHoles.size()) {
    std::cerr << m_className << "::GetHoleDriftLinePoint:\n";
    std::cerr << "    Endpoint index (" << ih << ") out of range.\n";
    return;
  }

  if (ip <= 0) {
    x = m_endpointsHoles[ih].x0;
    y = m_endpointsHoles[ih].y0;
    z = m_endpointsHoles[ih].z0;
    t = m_endpointsHoles[ih].t0;
    return;
  }

  const int np = m_endpointsHoles[ih].driftLine.size();
  if (ip > np) {
    x = m_endpointsHoles[ih].x;
    y = m_endpointsHoles[ih].y;
    z = m_endpointsHoles[ih].z;
    t = m_endpointsHoles[ih].t;
    return;
  }

  x = m_endpointsHoles[ih].driftLine[ip - 1].x;
  y = m_endpointsHoles[ih].driftLine[ip - 1].y;
  z = m_endpointsHoles[ih].driftLine[ip - 1].z;
  t = m_endpointsHoles[ih].driftLine[ip - 1].t;
}

void AvalancheMicroscopic::GetPhoton(const unsigned int i, double& e, double& x0,
                                     double& y0, double& z0, double& t0,
                                     double& x1, double& y1, double& z1,
                                     double& t1, int& status) const {

  if (i >= m_photons.size()) {
    std::cerr << m_className << "::GetPhoton:\n    Index out of range.\n";
    return;
  }

  x0 = m_photons[i].x0;
  x1 = m_photons[i].x1;
  y0 = m_photons[i].y0;
  y1 = m_photons[i].y1;
  z0 = m_photons[i].z0;
  z1 = m_photons[i].z1;
  t0 = m_photons[i].t0;
  t1 = m_photons[i].t1;
  status = m_photons[i].status;
  e = m_photons[i].energy;
}

void AvalancheMicroscopic::SetUserHandleStep(
    void (*f)(double x, double y, double z, double t, double e, double dx,
              double dy, double dz, bool hole)) {

  if (!f) {
    std::cerr << m_className << "::SetUserHandleStep:\n";
    std::cerr << "    Function pointer is a null pointer.\n";
    return;
  }
  m_userHandleStep = f;
  m_hasUserHandleStep = true;
}

void AvalancheMicroscopic::UnsetUserHandleStep() {

  m_userHandleStep = 0;
  m_hasUserHandleStep = false;
}

void AvalancheMicroscopic::SetUserHandleAttachment(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleAttachment = f;
  m_hasUserHandleAttachment = true;
}

void AvalancheMicroscopic::UnsetUserHandleAttachment() {

  m_userHandleAttachment = 0;
  m_hasUserHandleAttachment = false;
}

void AvalancheMicroscopic::SetUserHandleInelastic(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleInelastic = f;
  m_hasUserHandleInelastic = true;
}

void AvalancheMicroscopic::UnsetUserHandleInelastic() {

  m_userHandleInelastic = 0;
  m_hasUserHandleInelastic = false;
}

void AvalancheMicroscopic::SetUserHandleIonisation(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleIonisation = f;
  m_hasUserHandleIonisation = true;
}

void AvalancheMicroscopic::UnsetUserHandleIonisation() {

  m_userHandleIonisation = 0;
  m_hasUserHandleIonisation = false;
}

bool AvalancheMicroscopic::DriftElectron(const double x0, const double y0,
                                         const double z0, const double t0,
                                         const double e0, const double dx0,
                                         const double dy0, const double dz0) {

  // Clear the list of electrons and photons.
  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_photons.clear();

  // Reset the particle counters.
  m_nElectrons = m_nHoles = m_nIons = 0;

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, false, false);
}

bool AvalancheMicroscopic::AvalancheElectron(const double x0, const double y0,
                                             const double z0, const double t0,
                                             const double e0, const double dx0,
                                             const double dy0,
                                             const double dz0) {

  // Clear the list of electrons, holes and photons.
  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_photons.clear();

  // Reset the particle counters.
  m_nElectrons = m_nHoles = m_nIons = 0;

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, true, false);
}

bool AvalancheMicroscopic::TransportElectron(const double x0, const double y0,
                                             const double z0, const double t0,
                                             const double e0, const double dx0,
                                             const double dy0, const double dz0,
                                             const bool aval, bool hole) {

  // Make sure that the sensor is defined.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportElectron:\n"
              << "    Sensor is not defined.\n";
    return false;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium = NULL;
  if (!m_sensor->GetMedium(x0, y0, z0, medium) || !medium) {
    std::cerr << m_className << "::TransportElectron:\n"
              << "    No medium at initial position.\n";
    return false;
  }

  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << m_className << "::TransportElectron:\n";
    std::cerr << "    Medium at initial position does not provide "
              << " microscopic tracking data.\n";
    return false;
  }

  // If the medium is a semiconductor, we may use "band structure" stepping.
  bool useBandStructure = medium->IsSemiconductor() && m_useBandStructureDefault;
  if (m_debug) {
    std::cout << m_className << "::TransportElectron:\n";
    std::cout << "    Starting to drift in medium " << medium->GetName()
              << ".\n";
  }

  // Get the id number of the drift medium.
  int id = medium->GetId();

  // Numerical prefactors in equation of motion
  const double c1 = SpeedOfLight * sqrt(2. / ElectronMass);
  const double c2 = c1 * c1 / 4.;

  // Temporary stack of photons produced in the de-excitation cascade.
  std::vector<double> stackPhotonsTime;
  std::vector<double> stackPhotonsEnergy;

  // Electric and magnetic field
  double ex = 0., ey = 0., ez = 0., emag = 0.;
  double bx = 0., by = 0., bz = 0., bmag = 0.;
  int status = 0;
  // Cyclotron frequency
  double cwt = 1., swt = 0.;
  double wb = 0.;
  // Flag indicating if magnetic field is usable
  bool bOk = true;

  // Current position, direction, velocity and energy
  double x = x0, y = y0, z = z0, t = t0;
  double kx = dx0, ky = dy0, kz = dz0;
  double vx = dx0, vy = dy0, vz = dz0;
  double energy = e0;
  // Index of the conduction band (irrelevant for gases)
  int band = -1;

  // Timestep
  double dt = 0.;
  // Direction, velocity and energy after a step
  double newKx = 0., newKy = 0., newKz = 0.;
  double newVx = 0., newVy = 0., newVz = 0.;
  double newEnergy = 0.;
  // Collision type (elastic, ionisation, attachment, inelastic, ...)
  int cstype;
  // Cross-section term
  int level;

  // Number of secondaries
  int nion = 0, ndxc = 0;

  // Random number
  double r;
  // Numerical factors
  double a1 = 0., a2 = 0., a3 = 0., a4 = 0.;

  // Clear the stack.
  m_stack.clear();
  // Add the initial electron to the stack.
  electron newElectron;
  newElectron.status = 0;
  if (hole) {
    newElectron.hole = true;
  } else {
    newElectron.hole = false;
  }
  newElectron.x0 = x0;
  newElectron.x = x0;
  newElectron.y0 = y0;
  newElectron.y = y0;
  newElectron.z0 = z0;
  newElectron.z = z0;
  newElectron.t0 = t0;
  newElectron.t = t0;
  newElectron.kx = dx0;
  newElectron.ky = dy0;
  newElectron.kz = dz0;
  newElectron.e0 = std::max(e0, Small);
  newElectron.energy = newElectron.e0;
  newElectron.band = band;
  // Previous coordinates for distance histogramming.
  newElectron.xLast = x0;
  newElectron.yLast = y0;
  newElectron.zLast = z0;
  newElectron.driftLine.reserve(1000);
  m_stack.push_back(newElectron);
  if (hole) {
    ++m_nHoles;
  } else {
    ++m_nElectrons;
  }

  if (useBandStructure) {
    // With band structure, (kx, ky, kz) represents the momentum.
    // No normalization in this case.
    medium->GetElectronMomentum(std::max(e0, Small), kx, ky, kz, band);
    m_stack[0].kx = kx;
    m_stack[0].ky = ky;
    m_stack[0].kz = kz;
    m_stack[0].band = band;
  } else {
    m_stack[0].band = 0;
    band = 0;
    // Check the given initial direction.
    const double k = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
    if (fabs(k) < Small) {
      // Direction has zero norm, draw a random direction.
      const double phi = TwoPi * RndmUniform();
      const double ctheta = 2 * RndmUniform() - 1.;
      const double stheta = sqrt(1. - ctheta * ctheta);
      m_stack[0].kx = cos(phi) * stheta;
      m_stack[0].ky = sin(phi) * stheta;
      m_stack[0].kz = ctheta;
    } else {
      // Normalise the direction to 1.
      m_stack[0].kx /= k;
      m_stack[0].ky /= k;
      m_stack[0].kz /= k;
    }
  }

  // Get the null-collision rate.
  double fLim = medium->GetElectronNullCollisionRate(band);
  if (fLim <= 0.) {
    std::cerr << m_className << "::TransportElectron:\n";
    std::cerr << "    Got null-collision rate <= 0.\n";
    return false;
  }
  double fInv = 1. / fLim;

  // Status flag
  bool ok = true;
  while (true) {
    // Remove all inactive items from the stack.
    m_stack.erase(std::remove_if(m_stack.begin(), m_stack.end(), IsInactive), m_stack.end()); 
    // If the list of electrons/holes is exhausted, we're done.
    if (m_stack.empty()) break;
    // Loop over all electrons/holes in the avalanche.
    const std::vector<electron>::const_reverse_iterator rend = m_stack.rend();
    std::vector<electron>::reverse_iterator it;
    for (it = m_stack.rbegin(); it != rend; ++it) {
      // Get an electron/hole from the stack.
      x = (*it).x;
      y = (*it).y;
      z = (*it).z;
      t = (*it).t;
      energy = (*it).energy;
      band = (*it).band;
      kx = (*it).kx;
      ky = (*it).ky;
      kz = (*it).kz;
      hole = (*it).hole;

      ok = true;

      // Count number of collisions between updates.
      unsigned int nCollTemp = 0;

      // Get the local electric field and medium.
      m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Sign change for electrons.
      if (!hole) {
        ex = -ex;
        ey = -ey;
        ez = -ez;
      }

      if (m_debug) {
        const std::string eh = hole ? "hole" : "electron";
        std::cout << m_className << "::TransportElectron:\n    Drifting " << eh
                  << it - m_stack.rbegin() << ".\n";
        std::cout << "    Field [V/cm] at (" << x << ", " << y << ", " << z
                  << "): " << ex << ", " << ey << ", " << ez << "\n";
        std::cout << "    Status: " << status << "\n";
        std::cout << "    Medium: " << medium->GetName() << "\n";
      }

      if (status != 0) {
        // Electron/hole is not inside a drift medium.
        (*it).x = x;
        (*it).y = y;
        (*it).z = z;
        (*it).t = t;
        (*it).energy = energy;
        (*it).band = band;
        (*it).kx = kx;
        (*it).ky = ky;
        (*it).kz = kz;
        (*it).status = StatusLeftDriftMedium;
        AddToEndPoints(*it, hole);
        if (m_debug) {
          const std::string eh = hole ? "Hole" : "Electron";
          std::cout << m_className << "::TransportElectron:\n    " << eh 
                      << " left the drift medium at " 
                      << x << ", " << y << ", " << z << "\n";
        }
        continue;
      }

      // If switched on, get the local magnetic field.
      if (m_useBfield) {
        m_sensor->MagneticField(x, y, z, bx, by, bz, status);
        if (hole) {
          bx *= Tesla2Internal;
          by *= Tesla2Internal;
          bz *= Tesla2Internal;
        } else {
          bx *= -Tesla2Internal;
          by *= -Tesla2Internal;
          bz *= -Tesla2Internal;
        }
        // Make sure that neither E nor B are zero.
        bmag = sqrt(bx * bx + by * by + bz * bz);
        emag = sqrt(ex * ex + ey * ey + ez * ez);
        bOk = (bmag > Small && emag > Small);
      }

      // Trace the electron/hole.
      while (1) {

        bool isNullCollision = false;

        // Make sure the electron energy exceeds the transport cut.
        if (energy < m_deltaCut) {
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).energy = energy;
          (*it).band = band;
          (*it).kx = kx;
          (*it).ky = ky;
          (*it).kz = kz;
          (*it).status = StatusBelowTransportCut;
          AddToEndPoints(*it, hole);
          if (m_debug) {
            std::cout << m_className << "::TransportElectron:\n";
            std::cout << "    Kinetic energy (" << energy << ")"
                      << " below transport cut.\n";
          }
          ok = false;
          break;
        }

        // Fill the energy distribution histogram.
        if (hole && m_histHoleEnergy) {
          m_histHoleEnergy->Fill(energy);
        } else if (!hole && m_histElectronEnergy) {
          m_histElectronEnergy->Fill(energy);
        }

        // Check if the electrons is within the specified time window.
        if (m_hasTimeWindow && (t < m_tMin || t > m_tMax)) {
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).energy = energy;
          (*it).band = band;
          (*it).kx = kx;
          (*it).ky = ky;
          (*it).kz = kz;
          (*it).status = StatusOutsideTimeWindow;
          AddToEndPoints(*it, hole);
          if (m_debug) {
            const std::string eh = hole ? "Hole" : "Electron";
            std::cout << m_className << "::TransportElectron:\n    " << eh 
                        << " left the time window.\n";
          }
          ok = false;
          break;
        }

        if (medium->GetId() != id) {
          // Medium has changed.
          if (!medium->IsMicroscopic()) {
            // Electron/hole has left the microscopic drift medium.
            (*it).x = x;
            (*it).y = y;
            (*it).z = z;
            (*it).t = t;
            (*it).energy = energy;
            (*it).band = band;
            (*it).kx = kx;
            (*it).ky = ky;
            (*it).kz = kz;
            (*it).status = StatusLeftDriftMedium;
            AddToEndPoints(*it, hole);
            ok = false;
            if (m_debug) {
              std::cout << m_className << "::TransportElectron:\n";
              std::cout << "    Medium at " << x << ", " << y << ", " << z
                        << " does not have microscopic data.\n";
            }
            break;
          }
          id = medium->GetId();
          useBandStructure = (medium->IsSemiconductor() && m_useBandStructureDefault);
          // Update the null-collision rate.
          fLim = medium->GetElectronNullCollisionRate(band);
          if (fLim <= 0.) {
            std::cerr << m_className << "::TransportElectron:\n";
            std::cerr << "    Got null-collision rate <= 0.\n";
            return false;
          }
          fInv = 1. / fLim;
        }

        if (m_useBfield && bOk) {
          // Calculate the cyclotron frequency.
          wb = OmegaCyclotronOverB * bmag;
          // Rotate the direction vector into the local coordinate system.
          ComputeRotationMatrix(bx, by, bz, bmag, ex, ey, ez);
          RotateGlobal2Local(kx, ky, kz);
          // Calculate the electric field in the rotated system.
          RotateGlobal2Local(ex, ey, ez);
          // Calculate the velocity vector in the local frame.
          const double v = c1 * sqrt(energy);
          vx = v * kx;
          vy = v * ky;
          vz = v * kz;
          a1 = vx * ex;
          a2 = c2 * ex * ex;
          a3 = ez / bmag - vy;
          a4 = (ez / wb);
        } else if (useBandStructure) {
          energy = medium->GetElectronEnergy(kx, ky, kz, vx, vy, vz, band);
        } else {
          // No band structure, no magnetic field.
          // Calculate the velocity vector.
          const double v = c1 * sqrt(energy);
          vx = v * kx;
          vy = v * ky;
          vz = v * kz;

          a1 = vx * ex + vy * ey + vz * ez;
          a2 = c2 * (ex * ex + ey * ey + ez * ez);
        }

        if (m_hasUserHandleStep) {
          m_userHandleStep(x, y, z, t, energy, kx, ky, kz, hole);
        }

        // Determine the timestep.
        dt = 0.;
        while (1) {
          // Sample the flight time.
          r = RndmUniformPos();
          dt += -log(r) * fInv;
          // Calculate the energy after the proposed step.
          if (m_useBfield && bOk) {
            cwt = cos(wb * dt);
            swt = sin(wb * dt);
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt +
                                     a4 * (a3 * (1. - cwt) + vz * swt),
                                 Small);
          } else if (useBandStructure) {
            newEnergy = std::max(
                medium->GetElectronEnergy(
                    kx + ex * dt * SpeedOfLight, ky + ey * dt * SpeedOfLight,
                    kz + ez * dt * SpeedOfLight, newVx, newVy, newVz, band),
                Small);
          } else {
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt, Small);
          }
          // Get the real collision rate at the updated energy.
          double fReal = medium->GetElectronCollisionRate(newEnergy, band);
          if (fReal <= 0.) {
            std::cerr << m_className << "::TransportElectron:\n";
            std::cerr << "    Got collision rate <= 0.\n";
            std::cerr << "    At " << newEnergy << " eV (band " << band
                      << ").\n";
            return false;
          }
          if (fReal > fLim) {
            // Real collision rate is higher than null-collision rate.
            dt += log(r) * fInv;
            // Increase the null collision rate and try again.
            std::cerr << m_className << "::TransportElectron:\n";
            std::cerr << "    Increasing null-collision rate by 5%.\n";
            if (useBandStructure) std::cerr << "    Band " << band << "\n";
            fLim *= 1.05;
            fInv = 1. / fLim;
            continue;
          }
          // Check for real or null collision.
          if (RndmUniform() <= fReal * fInv) break;
          if (m_useNullCollisionSteps) {
            isNullCollision = true;
            break;
          }
        }
        if (!ok) break;

        // Increase the collision counter.
        ++nCollTemp;

        // Update the directions (at instant before collision)
        // and calculate the proposed new position.
        if (m_useBfield && bOk) {
          // Calculate the new velocity.
          newVx = vx + 2. * c2 * ex * dt;
          newVy = vz * swt - a3 * cwt + ez / bmag;
          newVz = vz * cwt + a3 * swt;
          // Normalise and rotate back to the lab frame.
          const double v = sqrt(newVx * newVx + newVy * newVy + newVz * newVz);
          newKx = newVx / v;
          newKy = newVy / v;
          newKz = newVz / v;
          RotateLocal2Global(newKx, newKy, newKz);
          // Calculate the step in coordinate space.
          vx += c2 * ex * dt;
          ky = (vz * (1. - cwt) - a3 * swt) / (wb * dt) + ez / bmag;
          kz = (vz * swt + a3 * (1. - cwt)) / (wb * dt);
          vy = ky;
          vz = kz;
          // Rotate back to the lab frame.
          RotateLocal2Global(vx, vy, vz);
        } else if (useBandStructure) {
          // Update the wave-vector.
          newKx = kx + ex * dt * SpeedOfLight;
          newKy = ky + ey * dt * SpeedOfLight;
          newKz = kz + ez * dt * SpeedOfLight;
          // Average velocity over the step.
          vx = 0.5 * (vx + newVx);
          vy = 0.5 * (vy + newVy);
          vz = 0.5 * (vz + newVz);
        } else {
          // Update the direction.
          a1 = sqrt(energy / newEnergy);
          a2 = 0.5 * c1 * dt / sqrt(newEnergy);
          newKx = kx * a1 + ex * a2;
          newKy = ky * a1 + ey * a2;
          newKz = kz * a1 + ez * a2;

          // Calculate the step in coordinate space.
          a1 = c1 * sqrt(energy);
          a2 = dt * c2;
          vx = kx * a1 + ex * a2;
          vy = ky * a1 + ey * a2;
          vz = kz * a1 + ez * a2;
        }

        // Get the electric field and medium at the proposed new position.
        m_sensor->ElectricField(x + vx * dt, y + vy * dt, z + vz * dt, ex, ey, ez,
                              medium, status);
        if (!hole) {
          ex = -ex;
          ey = -ey;
          ez = -ez;
        }

        // Check if the electron is still inside a drift medium.
        if (status != 0) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection.
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).energy = energy;
          double dx = vx * dt, dy = vy * dt, dz = vz * dt;
          double d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d;
            dy /= d;
            dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx;
            yM = y + d * dy;
            zM = z + d * dz;
            // Check if the mid-point is inside the drift medium.
            m_sensor->ElectricField(xM, yM, zM, ex, ey, ez, medium, status);
            if (status == 0) {
              x = xM;
              y = yM;
              z = zM;
              t += dt;
            }
          }
          // Place the endpoint OUTSIDE the drift medium
          x += d * dx;
          y += d * dy;
          z += d * dz;
          if (m_useSignal) {
            if (hole) {
              m_sensor->AddSignal(
                  +1, (*it).t, t - (*it).t, 0.5 * (x + (*it).x),
                  0.5 * (y + (*it).y), 0.5 * (z + (*it).z), vx, vy, vz);
            } else {
              m_sensor->AddSignal(
                  -1, (*it).t, t - (*it).t, 0.5 * (x + (*it).x),
                  0.5 * (y + (*it).y), 0.5 * (z + (*it).z), vx, vy, vz);
            }
          }
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).kx = newKx;
          (*it).ky = newKy;
          (*it).kz = newKz;
          (*it).status = StatusLeftDriftMedium;
          AddToEndPoints(*it, hole);
          ok = false;
          if (m_debug) {
            const std::string eh = hole ? "Hole" : "Electron";
            std::cout << m_className << "::TransportElectron:\n    " << eh
                      << " left the drift medium at " 
                      << x << ", " << y << ", " << z << "\n";
          }
          break;
        }

        // Check if the new position is inside the user area.
        if (!m_sensor->IsInArea(x + vx * dt, y + vy * dt, z + vz * dt)) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection.
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).energy = energy;
          double dx = vx * dt, dy = vy * dt, dz = vz * dt;
          double d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d;
            dy /= d;
            dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx;
            yM = y + d * dy;
            zM = z + d * dz;
            // Check if the mid-point is inside the drift area.
            if (m_sensor->IsInArea(xM, yM, zM)) {
              x = xM;
              y = yM;
              z = zM;
              t += dt;
            }
          }
          // Place the endpoint OUTSIDE the drift area.
          x += d * dx;
          y += d * dy;
          z += d * dz;

          // If switched on, calculate the induced signal over this step.
          if (m_useSignal) {
            const int q = hole ? 1 : -1;
            m_sensor->AddSignal(q, (*it).t, t - (*it).t, 
                                0.5 * (x + (*it).x),
                                0.5 * (y + (*it).y), 
                                0.5 * (z + (*it).z), vx, vy, vz);
          }
          (*it).x = x;
          (*it).y = y;
          (*it).z = z;
          (*it).t = t;
          (*it).kx = newKx;
          (*it).ky = newKy;
          (*it).kz = newKz;
          (*it).status = StatusLeftDriftArea;
          AddToEndPoints(*it, hole);
          ok = false;
          if (m_debug) {
            const std::string eh = hole ? "Hole" : "Electron";
            std::cout << m_className << "::TransportElectron:\n    " << eh 
                      << " left the drift area at " 
                      << x << ", " << y << ", " << z << "\n";
          }
          break;
        }

        // Check if the electron/hole has crossed a wire.
        double xCross = x, yCross = y, zCross = z;
        if (m_sensor->IsWireCrossed(x, y, z, x + vx * dt, y + vy * dt,
                                  z + vz * dt, xCross, yCross, zCross)) {
          // If switched on, calculated the induced signal over this step.
          if (m_useSignal) {
            dt = sqrt(pow(xCross - x, 2) + pow(yCross - y, 2) +
                      pow(zCross - z, 2)) /
                 sqrt(vx * vx + vy * vy + vz * vz);

            const int q = hole ? 1 : -1;
            m_sensor->AddSignal(q, t, dt, 
                                0.5 * (x + xCross), 0.5 * (y + yCross), 
                                0.5 * (z + zCross), vx, vy, vz);
          }
          (*it).x = xCross;
          (*it).y = yCross;
          (*it).z = zCross;
          (*it).t = t + dt;
          (*it).kx = newKx;
          (*it).ky = newKy;
          (*it).kz = newKz;
          (*it).status = StatusLeftDriftMedium;
          AddToEndPoints(*it, hole);
          ok = false;
          if (m_debug) {
            std::cout << m_className << "::TransportElectron:\n";
            std::cout << "    Electron/hole hit a wire.\n";
            std::cout << "    At " << x << ", " << y << "," << z << "\n";
          }
          break;
        }

        // If switched on, calculate the induced signal.
        if (m_useSignal) {
          const int q = hole ? 1 : -1;
          m_sensor->AddSignal(q, t, dt, x + 0.5 * vx * dt, y + 0.5 * vy * dt,
                              z + 0.5 * vz * dt, vx, vy, vz);
        }

        // Update the coordinates.
        x += vx * dt;
        y += vy * dt;
        z += vz * dt;
        t += dt;

        // If switched on, get the magnetic field at the new location.
        if (m_useBfield) {
          m_sensor->MagneticField(x, y, z, bx, by, bz, status);
          bx *= (hole ? Tesla2Internal : -Tesla2Internal);
          by *= (hole ? Tesla2Internal : -Tesla2Internal);
          bz *= (hole ? Tesla2Internal : -Tesla2Internal);
          // Make sure that neither E nor B are zero.
          bmag = sqrt(bx * bx + by * by + bz * bz);
          emag = sqrt(ex * ex + ey * ey + ez * ez);
          if (bmag > Small && emag > Small)
            bOk = true;
          else
            bOk = false;
        }

        if (isNullCollision) {
          energy = newEnergy;
          kx = newKx;
          ky = newKy;
          kz = newKz;
          continue;
        }

        // Get the collision type and parameters.
        medium->GetElectronCollision(newEnergy, cstype, level, energy, newKx,
                                     newKy, newKz, nion, ndxc, band);

        // If activated, histogram the distance with respect to the
        // last collision.
        if (m_histDistance && !m_distanceHistogramType.empty()) {
          const int nDistanceHistogramTypes = m_distanceHistogramType.size();
          for (int iType = nDistanceHistogramTypes; iType--;) {
            if (m_distanceHistogramType[iType] != cstype) continue;
            if (m_debug) {
              std::cout << m_className << "::TransportElectron:\n";
              std::cout << "    Collision type: " << cstype << "\n";
              std::cout << "    Fill distance histogram.\n";
              getchar();
            }
            switch (m_distanceOption) {
              case 'x':
                m_histDistance->Fill((*it).xLast - x);
                break;
              case 'y':
                m_histDistance->Fill((*it).yLast - y);
                break;
              case 'z':
                m_histDistance->Fill((*it).zLast - z);
                break;
              case 'r':
                const double r2 = pow((*it).xLast - x, 2) +
                                  pow((*it).yLast - y, 2) +
                                  pow((*it).zLast - z, 2);
                m_histDistance->Fill(sqrt(r2));
                break;
            }
            (*it).xLast = x;
            (*it).yLast = y;
            (*it).zLast = z;
            break;
          }
        }

        switch (cstype) {
          // Elastic collision
          case ElectronCollisionTypeElastic:
            break;
          // Ionising collision
          case ElectronCollisionTypeIonisation:
            if (m_usePlotting && m_plotIonisations) {
              m_viewer->AddIonisationMarker(x, y, z);
            }
            if (m_hasUserHandleIonisation) {
              m_userHandleIonisation(x, y, z, t, cstype, level, medium);
            }
            for (int j = nion; j--;) {
              int itype;
              double esec;
              medium->GetIonisationProduct(j, itype, esec);
              if (itype == IonProdTypeElectron) {
                esec = std::max(esec, Small);
                if (m_histSecondary) m_histSecondary->Fill(esec);
                // Add the secondary electron to the stack.
                newElectron = (*it);
                newElectron.hole = false;
                newElectron.x0 = x;
                newElectron.x = x;
                newElectron.y0 = y;
                newElectron.y = y;
                newElectron.z0 = z;
                newElectron.z = z;
                newElectron.t0 = t;
                newElectron.t = t;
                newElectron.energy = esec;
                newElectron.e0 = newElectron.energy;
                if (useBandStructure) {
                  newElectron.band = -1;
                  medium->GetElectronMomentum(esec, newElectron.kx,
                                              newElectron.ky, newElectron.kz,
                                              newElectron.band);
                } else {
                  // Randomise the secondary electron direction.
                  const double phi = TwoPi * RndmUniform();
                  const double ctheta = 2 * RndmUniform() - 1.;
                  const double stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                }
                newElectron.status = 0;
                newElectron.driftLine.clear();
                if (aval && (m_sizeCut == 0 || m_stack.size() < m_sizeCut)) {
                  m_stack.push_back(newElectron);
                }
                // Increment the electron counter.
                ++m_nElectrons;
              } else if (itype == IonProdTypeHole) {
                esec = std::max(esec, Small);
                // Add the secondary hole to the stack.
                newElectron = (*it);
                newElectron.hole = true;
                newElectron.x0 = x;
                newElectron.x = x;
                newElectron.y0 = y;
                newElectron.y = y;
                newElectron.z0 = z;
                newElectron.z = z;
                newElectron.t0 = t;
                newElectron.t = t;
                newElectron.energy = esec;
                newElectron.e0 = newElectron.energy;
                if (useBandStructure) {
                  newElectron.band = -1;
                  medium->GetElectronMomentum(esec, newElectron.kx,
                                              newElectron.ky, newElectron.kz,
                                              newElectron.band);
                } else {
                  // Randomise the secondary hole direction.
                  const double phi = TwoPi * RndmUniform();
                  const double ctheta = 2 * RndmUniform() - 1.;
                  const double stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                }
                newElectron.status = 0;
                newElectron.driftLine.clear();
                if (aval && (m_sizeCut == 0 || m_stack.size() < m_sizeCut)) {
                  m_stack.push_back(newElectron);
                }
                // Increment the hole counter.
                ++m_nHoles;
              } else if (itype == IonProdTypeIon) {
                ++m_nIons;
              }
            }
            if (m_debug) {
              std::cout << m_className << "::TransportElectron:\n";
              std::cout << "    Ionisation.\n";
              std::cout << "    At " << x << "," << y << "," << z << "\n";
            }
            break;
          // Attachment
          case ElectronCollisionTypeAttachment:
            if (m_usePlotting && m_plotAttachments) {
              m_viewer->AddAttachmentMarker(x, y, z);
            }
            if (m_hasUserHandleAttachment) {
              m_userHandleAttachment(x, y, z, t, cstype, level, medium);
            }
            (*it).x = x;
            (*it).y = y;
            (*it).z = z;
            (*it).t = t;
            (*it).energy = energy;
            (*it).status = StatusAttached;
            if (hole) {
              m_endpointsHoles.push_back(*it);
              --m_nHoles;
            } else {
              m_endpointsElectrons.push_back(*it);
              --m_nElectrons;
            }
            ok = false;
            break;
          // Inelastic collision
          case ElectronCollisionTypeInelastic:
            if (m_hasUserHandleInelastic) {
              m_userHandleInelastic(x, y, z, t, cstype, level, medium);
            }
            break;
          // Excitation
          case ElectronCollisionTypeExcitation:
            if (m_usePlotting && m_plotExcitations) {
              m_viewer->AddExcitationMarker(x, y, z);
            }
            if (m_hasUserHandleInelastic) {
              m_userHandleInelastic(x, y, z, t, cstype, level, medium);
            }
            if (ndxc > 0) {
              // Get the electrons and photons produced in the
              // deexcitation cascade.
              double tDxc = 0., sDxc = 0., eDxc = 0.;
              int typeDxc = 0;
              stackPhotonsTime.clear();
              stackPhotonsEnergy.clear();
              for (int j = ndxc; j--;) {
                if (!medium->GetDeexcitationProduct(j, tDxc, sDxc, typeDxc,
                                                    eDxc)) {
                  std::cerr << m_className << "::TransportElectron:\n";
                  std::cerr << "    Cannot retrieve deexcitation product " << j
                            << "/" << ndxc << ".\n";
                  break;
                }

                if (typeDxc == DxcProdTypeElectron) {
                  if (!aval || (m_sizeCut > 0 && m_stack.size() >= m_sizeCut))
                    continue;
                  // Penning ionisation
                  newElectron = (*it);
                  double xDxc = x, yDxc = y, zDxc = z;
                  if (sDxc > Small) {
                    // Randomise the point of creation
                    double phiDxc = TwoPi * RndmUniform();
                    double cthetaDxc = 1. - 2 * RndmUniform();
                    double sthetaDxc = sqrt(1. - cthetaDxc * cthetaDxc);
                    xDxc += sDxc * cos(phiDxc) * sthetaDxc;
                    yDxc += sDxc * sin(phiDxc) * sthetaDxc;
                    zDxc += sDxc * cthetaDxc;
                  }
                  // Get the electric field and medium at this location.
                  Medium* dxcMedium = 0;
                  double fx = 0., fy = 0., fz = 0.;
                  m_sensor->ElectricField(xDxc, yDxc, zDxc, fx, fy, fz, dxcMedium,
                                        status);
                  // Check if this location is inside a drift medium.
                  if (status != 0) continue;
                  // Check if this location is inside the drift area.
                  if (!m_sensor->IsInArea(xDxc, yDxc, zDxc)) continue;
                  // Make sure we haven't jumped across a wire.
                  if (m_sensor->IsWireCrossed(x, y, z, xDxc, yDxc, zDxc, xCross,
                                            yCross, zCross)) {
                    continue;
                  }
                  newElectron.x0 = xDxc;
                  newElectron.x = xDxc;
                  newElectron.y0 = yDxc;
                  newElectron.y = yDxc;
                  newElectron.z0 = zDxc;
                  newElectron.z = zDxc;
                  newElectron.t0 = t + tDxc;
                  newElectron.t = t + tDxc;
                  newElectron.energy = std::max(eDxc, Small);
                  newElectron.e0 = newElectron.energy;
                  // Randomise the initial direction.
                  const double phi = TwoPi * RndmUniform();
                  const double ctheta = 2 * RndmUniform() - 1.;
                  const double stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                  newElectron.status = 0;
                  newElectron.driftLine.clear();
                  // Add the electron to the list.
                  m_stack.push_back(newElectron);
                  // Increment the electron and ion counters.
                  ++m_nElectrons;
                  ++m_nIons;
                } else if (typeDxc == DxcProdTypePhoton && m_usePhotons &&
                           eDxc > m_gammaCut) {
                  // Radiative de-excitation
                  stackPhotonsTime.push_back(t + tDxc);
                  stackPhotonsEnergy.push_back(eDxc);
                }
              }

              // Transport the photons (if any)
              const int nSizePhotons = stackPhotonsTime.size();
              for (int j = nSizePhotons; j--;) {
                if (aval) {
                  TransportPhoton(x, y, z, stackPhotonsTime[j],
                                  stackPhotonsEnergy[j]);
                }
              }
            }
            break;
          // Super-elastic collision
          case ElectronCollisionTypeSuperelastic:
            break;
          // Acoustic phonon scattering (intravalley)
          case ElectronCollisionTypeAcousticPhonon:
            break;
          // Optical phonon scattering (intravalley)
          case ElectronCollisionTypeOpticalPhonon:
            break;
          // Intervalley scattering (phonon assisted)
          case ElectronCollisionTypeIntervalleyG:
          case ElectronCollisionTypeIntervalleyF:
          case ElectronCollisionTypeInterbandXL:
          case ElectronCollisionTypeInterbandXG:
          case ElectronCollisionTypeInterbandLG:
            break;
          // Coulomb scattering
          case ElectronCollisionTypeImpurity:
            break;
          default:
            std::cerr << m_className << "::TransportElectron:\n";
            std::cerr << "    Unknown collision type.\n";
            ok = false;
            break;
        }

        // Continue with the next electron/hole?
        if (!ok || nCollTemp > m_nCollSkip ||
            cstype == ElectronCollisionTypeIonisation ||
            (m_plotExcitations && cstype == ElectronCollisionTypeExcitation) ||
            (m_plotAttachments && cstype == ElectronCollisionTypeAttachment)) {
          break;
        }
        kx = newKx;
        ky = newKy;
        kz = newKz;
      }

      if (!ok) continue;

      if (!useBandStructure) {
        // Normalise the direction vector.
        const double k = sqrt(kx * kx + ky * ky + kz * kz);
        kx /= k;
        ky /= k;
        kz /= k;
      }
      // Update the stack.
      (*it).energy = energy;
      (*it).t = t;
      (*it).x = x;
      (*it).y = y;
      (*it).z = z;
      (*it).kx = kx;
      (*it).ky = ky;
      (*it).kz = kz;
      // Add a new point to the drift line (if enabled).
      if (m_useDriftLines) {
        point newPoint;
        newPoint.x = x;
        newPoint.y = y;
        newPoint.z = z;
        newPoint.t = t;
        (*it).driftLine.push_back(newPoint);
      }
    }
  }

  // Calculate the induced charge.
  if (m_useInducedCharge) {
    const unsigned int nElectronEndpoints = m_endpointsElectrons.size();
    for (unsigned int i = 0; i < nElectronEndpoints; ++i) {
      const electron& p = m_endpointsElectrons[i];
      m_sensor->AddInducedCharge(-1, p.x0, p.y0, p.z0, p.x, p.y, p.z);
    }
    const unsigned int nHoleEndpoints = m_endpointsHoles.size();
    for (unsigned int i = 0; i < nHoleEndpoints; ++i) {
      const electron& p = m_endpointsHoles[i];
      m_sensor->AddInducedCharge(+1, p.x0, p.y0, p.z0, p.x, p.y, p.z);
    }
  }

  // Plot the drift paths and photon tracks.
  if (m_usePlotting) {
    // Electrons
    const unsigned int nElectronEndpoints = m_endpointsElectrons.size();
    for (unsigned int i = 0; i < nElectronEndpoints; ++i) {
      const int np = GetNumberOfElectronDriftLinePoints(i);
      int jL;
      if (np <= 0) continue;
      const electron& p = m_endpointsElectrons[i];
      m_viewer->NewElectronDriftLine(np, jL, p.x0, p.y0, p.z0);
      for (int jP = np; jP--;) {
        GetElectronDriftLinePoint(x, y, z, t, jP, i);
        m_viewer->SetDriftLinePoint(jL, jP, x, y, z);
      }
    }
    // Holes
    const unsigned int nHoleEndpoints = m_endpointsHoles.size();
    for (unsigned int i = 0; i < nHoleEndpoints; ++i) {
      const int np = GetNumberOfHoleDriftLinePoints(i);
      int jL;
      if (np <= 0) continue;
      const electron& p = m_endpointsHoles[i];
      m_viewer->NewHoleDriftLine(np, jL, p.x0, p.y0, p.z0);
      for (int jP = np; jP--;) {
        GetHoleDriftLinePoint(x, y, z, t, jP, i);
        m_viewer->SetDriftLinePoint(jL, jP, x, y, z);
      }
    }
    // Photons
    const unsigned int nPhotons = m_photons.size();
    for (unsigned int i = 0; i < nPhotons; ++i) {
      const photon& p = m_photons[i];
      m_viewer->NewPhotonTrack(p.x0, p.y0, p.z0, p.x1, p.y1, p.z1);
    }
  }
  return true;
}

void AvalancheMicroscopic::TransportPhoton(const double x0, const double y0,
                                           const double z0, const double t0,
                                           const double e0) {

  // Make sure that the sensor is defined.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  }

  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    Medium at initial position does not provide "
              << " microscopic tracking data.\n";
    return;
  }

  if (m_debug) {
    std::cout << m_className << "::TransportPhoton:\n";
    std::cout << "    Starting photon transport in medium " << medium->GetName()
              << ".\n";
  }

  // Get the id number of the drift medium.
  int id = medium->GetId();

  // Position
  double x = x0, y = y0, z = z0;
  double t = t0;
  // Initial direction (randomised)
  double ctheta = 2 * RndmUniform() - 1.;
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
  double e1;
  int nsec = 0;
  double esec = 0.;

  f = medium->GetPhotonCollisionRate(e);
  if (f <= 0.) return;

  dt = -log(RndmUniformPos()) / f;
  t += dt;
  dt *= SpeedOfLight;
  x += dt * dx;
  y += dt * dy;
  z += dt * dz;

  // Check if the photon is still inside a medium.
  if (!m_sensor->GetMedium(x, y, z, medium) || medium->GetId() != id) {
    // Try to terminate the photon track close to the boundary
    // by means of iterative bisection.
    dx *= dt;
    dy *= dt;
    dz *= dt;
    x -= dx;
    y -= dy;
    z -= dz;
    double delta = sqrt(dx * dx + dy * dy + dz * dz);
    if (delta > 0) {
      dx /= delta;
      dy /= delta;
      dz /= delta;
    }
    // Mid-point
    double xM = x, yM = y, zM = z;
    while (delta > BoundaryDistance) {
      delta *= 0.5;
      dt *= 0.5;
      xM = x + delta * dx;
      yM = y + delta * dy;
      zM = z + delta * dz;
      // Check if the mid-point is inside the drift medium.
      if (m_sensor->GetMedium(xM, yM, zM, medium) && medium->GetId() == id) {
        x = xM;
        y = yM;
        z = zM;
        t += dt;
      }
    }
    photon newPhoton;
    newPhoton.x0 = x0;
    newPhoton.y0 = y0;
    newPhoton.z0 = z0;
    newPhoton.x1 = x;
    newPhoton.y1 = y;
    newPhoton.z1 = z;
    newPhoton.energy = e0;
    newPhoton.status = StatusLeftDriftMedium;
    m_photons.push_back(newPhoton);
    return;
  }

  if (!medium->GetPhotonCollision(e, type, level, e1, ctheta, nsec, esec))
    return;

  if (type == PhotonCollisionTypeIonisation) {
    // Randomise secondary electron direction.
    phi = TwoPi * RndmUniform();
    ctheta = 2 * RndmUniform() - 1.;
    stheta = sqrt(1. - ctheta * ctheta);
    // Add the secondary electron to the stack.
    electron newElectron;
    newElectron.hole = false;
    newElectron.x0 = x;
    newElectron.x = x;
    newElectron.y0 = y;
    newElectron.y = y;
    newElectron.z0 = z;
    newElectron.z = z;
    newElectron.t0 = t;
    newElectron.t = t;
    newElectron.energy = std::max(esec, Small);
    newElectron.e0 = newElectron.energy;
    newElectron.kx = cos(phi) * stheta;
    newElectron.ky = sin(phi) * stheta;
    newElectron.kz = ctheta;
    newElectron.status = 0;
    newElectron.driftLine.clear();
    if (m_sizeCut == 0 || m_stack.size() < m_sizeCut)
      m_stack.push_back(newElectron);
    // Increment the electron and ion counters.
    ++m_nElectrons;
    ++m_nIons;
  } else if (type == PhotonCollisionTypeExcitation) {
    double tDxc = 0.;
    double sDxc = 0.;
    int typeDxc = 0;
    std::vector<double> stackPhotonsTime;
    stackPhotonsTime.clear();
    std::vector<double> stackPhotonsEnergy;
    stackPhotonsEnergy.clear();
    for (int j = nsec; j--;) {
      if (!medium->GetDeexcitationProduct(j, tDxc, sDxc, typeDxc, esec))
        continue;
      if (typeDxc == DxcProdTypeElectron) {
        // Ionisation
        phi = TwoPi * RndmUniform();
        ctheta = 2 * RndmUniform() - 1.;
        stheta = sqrt(1. - ctheta * ctheta);
        // Add the electron to the stack.
        electron newElectron;
        newElectron.hole = false;
        newElectron.x0 = x;
        newElectron.x = x;
        newElectron.y0 = y;
        newElectron.y = y;
        newElectron.z0 = z;
        newElectron.z = z;
        newElectron.t0 = t + tDxc;
        newElectron.t = t + tDxc;
        newElectron.energy = std::max(esec, Small);
        newElectron.e0 = newElectron.energy;
        newElectron.kx = cos(phi) * stheta;
        newElectron.ky = sin(phi) * stheta;
        newElectron.kz = ctheta;
        newElectron.status = 0;
        newElectron.driftLine.clear();
        m_stack.push_back(newElectron);
        // Increment the electron and ion counters.
        ++m_nElectrons;
        ++m_nIons;
      } else if (typeDxc == DxcProdTypePhoton && m_usePhotons &&
                 esec > m_gammaCut) {
        // Radiative de-excitation
        stackPhotonsTime.push_back(t + tDxc);
        stackPhotonsEnergy.push_back(esec);
      }
    }
    // Transport the photons (if any).
    const int nSizePhotons = stackPhotonsTime.size();
    for (int k = nSizePhotons; k--;) {
      TransportPhoton(x, y, z, stackPhotonsTime[k], stackPhotonsEnergy[k]);
    }
  }

  photon newPhoton;
  newPhoton.x0 = x0;
  newPhoton.y0 = y0;
  newPhoton.z0 = z0;
  newPhoton.x1 = x;
  newPhoton.y1 = y;
  newPhoton.z1 = z;
  newPhoton.energy = e0;
  newPhoton.status = -2;
  m_photons.push_back(newPhoton);
}

void AvalancheMicroscopic::ComputeRotationMatrix(
    const double bx, const double by, const double bz, const double bmag,
    const double ex, const double ey, const double ez) {

  // Adopting the Magboltz convention, the stepping is performed
  // in a coordinate system with the B field along the x axis
  // and the electric field at an angle btheta in the x-z plane.

  // Calculate the first rotation matrix (to align B with x axis).
  const double bt = by * by + bz * bz;
  if (bt < Small) {
    // B field is already along axis.
    m_rb11 = m_rb22 = m_rb33 = 1.;
    m_rb12 = m_rb13 = m_rb21 = m_rb23 = m_rb31 = m_rb32 = 0.;
  } else {
    const double btInv = 1. / bt;
    m_rb11 = bx / bmag;
    m_rb12 = by / bmag;
    m_rb21 = -m_rb12;
    m_rb13 = bz / bmag;
    m_rb31 = -m_rb13;
    m_rb22 = (m_rb11 * by * by + bz * bz) * btInv;
    m_rb33 = (m_rb11 * bz * bz + by * by) * btInv;
    m_rb23 = m_rb32 = (m_rb11 - 1.) * by * bz * btInv;
  }
  // Calculate the second rotation matrix (rotation around x axis).
  const double fy = m_rb21 * ex + m_rb22 * ey + m_rb23 * ez;
  const double fz = m_rb31 * ex + m_rb32 * ey + m_rb33 * ez;
  const double ft = sqrt(fy * fy + fz * fz);
  if (ft < Small) {
    // E and B field are parallel.
    m_rx22 = m_rx33 = 1.;
    m_rx23 = m_rx32 = 0.;
  } else {
    m_rx22 = m_rx33 = fz / ft;
    m_rx23 = -fy / ft;
    m_rx32 = -m_rx23;
  }
}

void AvalancheMicroscopic::RotateGlobal2Local(double& dx, double& dy,
                                              double& dz) const {

  const double dx1 = m_rb11 * dx + m_rb12 * dy + m_rb13 * dz;
  const double dy1 = m_rb21 * dx + m_rb22 * dy + m_rb23 * dz;
  const double dz1 = m_rb31 * dx + m_rb32 * dy + m_rb33 * dz;

  dx = dx1;
  dy = m_rx22 * dy1 + m_rx23 * dz1;
  dz = m_rx32 * dy1 + m_rx33 * dz1;
}

void AvalancheMicroscopic::RotateLocal2Global(double& dx, double& dy,
                                              double& dz) const {

  const double dx1 = dx;
  const double dy1 = m_rx22 * dy + m_rx32 * dz;
  const double dz1 = m_rx23 * dy + m_rx33 * dz;

  dx = m_rb11 * dx1 + m_rb21 * dy1 + m_rb31 * dz1;
  dy = m_rb12 * dx1 + m_rb22 * dy1 + m_rb32 * dz1;
  dz = m_rb13 * dx1 + m_rb23 * dy1 + m_rb33 * dz1;
}
}
