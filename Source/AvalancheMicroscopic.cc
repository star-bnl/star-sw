#include <iostream>
#include <cmath>
#include <string>
#include <functional>
#include <algorithm>

#include "AvalancheMicroscopic.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace {

void PrintStatus(const std::string& hdr, const std::string& status,
                 const double x, const double y, const double z,
                 const bool hole) {

  const std::string eh = hole ? "Hole " : "Electron ";
  std::cout << hdr << eh << status << " at " << x << ", " << y << ", " << z << "\n";
}

}

namespace Garfield {

AvalancheMicroscopic::AvalancheMicroscopic() {

  m_endpointsElectrons.reserve(10000);
  m_endpointsHoles.reserve(10000);
  m_photons.reserve(1000);
}

void AvalancheMicroscopic::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor: Null pointer.\n";
    return;
  }
  m_sensor = s;
}

void AvalancheMicroscopic::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting: Null pointer.\n";
    return;
  }

  m_viewer = view;
  m_usePlotting = true;
  if (!m_useDriftLines) {
    std::cout << m_className << "::EnablePlotting:\n"
              << "    Enabling storage of drift line.\n";
    EnableDriftLines();
  }
}

void AvalancheMicroscopic::DisablePlotting() {

  m_viewer = nullptr;
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
    std::cerr << m_className << "::SetDistanceHistogram: Null pointer.\n";
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

  m_histDistance = nullptr;
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
    std::cerr << m_className << "::GetPhoton: Index out of range.\n";
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
    std::cerr << m_className << "::SetUserHandleStep: Null pointer.\n";
    return;
  }
  m_userHandleStep = f;
  m_hasUserHandleStep = true;
}

void AvalancheMicroscopic::UnsetUserHandleStep() {

  m_userHandleStep = nullptr;
  m_hasUserHandleStep = false;
}

void AvalancheMicroscopic::SetUserHandleAttachment(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleAttachment = f;
  m_hasUserHandleAttachment = true;
}

void AvalancheMicroscopic::UnsetUserHandleAttachment() {

  m_userHandleAttachment = nullptr;
  m_hasUserHandleAttachment = false;
}

void AvalancheMicroscopic::SetUserHandleInelastic(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleInelastic = f;
  m_hasUserHandleInelastic = true;
}

void AvalancheMicroscopic::UnsetUserHandleInelastic() {

  m_userHandleInelastic = nullptr;
  m_hasUserHandleInelastic = false;
}

void AvalancheMicroscopic::SetUserHandleIonisation(void (*f)(
    double x, double y, double z, double t, int type, int level, Medium* m)) {

  m_userHandleIonisation = f;
  m_hasUserHandleIonisation = true;
}

void AvalancheMicroscopic::UnsetUserHandleIonisation() {

  m_userHandleIonisation = nullptr;
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
                                             double e0, const double dx0,
                                             const double dy0, const double dz0,
                                             const bool aval, bool hole0) {

  const std::string hdr = m_className + "::TransportElectron: ";
  // Make sure that the sensor is defined.
  if (!m_sensor) {
    std::cerr << hdr << "Sensor is not defined.\n";
    return false;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x0, y0, z0, medium) || !medium) {
    std::cerr << hdr << "No medium at initial position.\n";
    return false;
  }

  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << hdr << "Medium does not have cross-section data.\n";
    return false;
  }

  // If the medium is a semiconductor, we may use "band structure" stepping.
  bool useBandStructure = medium->IsSemiconductor() && m_useBandStructureDefault;
  if (m_debug) {
    std::cout << hdr << "Start drifting in medium " 
              << medium->GetName() << ".\n";
  }

  // Get the id number of the drift medium.
  int id = medium->GetId();

  // Numerical prefactors in equation of motion
  const double c1 = SpeedOfLight * sqrt(2. / ElectronMass);
  const double c2 = c1 * c1 / 4.;

  // Electric and magnetic field
  double ex = 0., ey = 0., ez = 0.;
  double bx = 0., by = 0., bz = 0., bmag = 0.;
  int status = 0;
  // Cyclotron frequency
  double cwt = 1., swt = 0.;
  double wb = 0.;
  // Flag indicating if magnetic field is usable
  bool bOk = true;

  // Direction, velocity and energy after a step
  double newKx = 0., newKy = 0., newKz = 0.;
  double newVx = 0., newVy = 0., newVz = 0.;
  double newEnergy = 0.;

  // Numerical factors
  double a1 = 0., a2 = 0., a3 = 0., a4 = 0.;

  // Make sure the initial energy is positive.
  e0 = std::max(e0, Small);

  std::vector<Electron> stackOld;
  std::vector<Electron> stackNew;
  stackOld.reserve(10000);
  stackNew.reserve(1000);
  std::vector<std::pair<double, double> > stackPhotons;
  std::vector<std::pair<int, double> > secondaries;

  // Put the initial electron on the stack.
  if (useBandStructure) {
    // With band structure, (kx, ky, kz) represents the momentum.
    // No normalization in this case.
    int band = -1;
    double kx = 0., ky = 0., kz = 0.;
    medium->GetElectronMomentum(e0, kx, ky, kz, band);
    AddToStack(x0, y0, z0, t0, e0, kx, ky, kz, band, hole0, stackOld);
  } else {
    double kx = dx0;
    double ky = dy0;
    double kz = dz0;
    // Check the given initial direction.
    const double k = sqrt(kx * kx + ky * ky + kz * kz);
    if (fabs(k) < Small) {
      // Direction has zero norm, draw a random direction.
      RndmDirection(kx, ky, kz);
    } else {
      // Normalise the direction to 1.
      kx /= k;
      ky /= k;
      kz /= k;
    }
    AddToStack(x0, y0, z0, t0, e0, kx, ky, kz, 0, hole0, stackOld);
  }
  if (hole0) {
    ++m_nHoles;
  } else {
    ++m_nElectrons;
  }

  // Get the null-collision rate.
  double fLim = medium->GetElectronNullCollisionRate(stackOld.front().band);
  if (fLim <= 0.) {
    std::cerr << hdr << "Got null-collision rate <= 0.\n";
    return false;
  }
  double fInv = 1. / fLim;

  while (true) {
    // Remove all inactive items from the stack.
    stackOld.erase(std::remove_if(stackOld.begin(), stackOld.end(), IsInactive), 
                   stackOld.end());
    // Add the electrons produced in the last iteration.
    if (aval && m_sizeCut > 0) {
      // If needed, reduce the number of electrons to add. 
      if (stackOld.size() > m_sizeCut) {
        stackNew.clear();
      } else if (stackOld.size() + stackNew.size() > m_sizeCut) {
        stackNew.resize(m_sizeCut - stackOld.size()); 
      }
    }
    stackOld.insert(stackOld.end(), 
                    std::make_move_iterator(stackNew.begin()), 
                    std::make_move_iterator(stackNew.end())); 
    stackNew.clear();
    // If the list of electrons/holes is exhausted, we're done.
    if (stackOld.empty()) break;
    // Loop over all electrons/holes in the avalanche.
    for (auto it = stackOld.begin(), end = stackOld.end(); it != end; ++it) {
      // Get an electron/hole from the stack.
      double x = (*it).x;
      double y = (*it).y;
      double z = (*it).z;
      double t = (*it).t;
      double energy = (*it).energy;
      int band = (*it).band;
      double kx = (*it).kx;
      double ky = (*it).ky;
      double kz = (*it).kz;
      bool hole = (*it).hole;

      bool ok = true;

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
        const std::string eh = hole ? "hole " : "electron ";
        std::cout << hdr << "\n    Drifting " << eh << it - stackOld.begin() 
                  << ".\n    Field [V/cm] at (" << x << ", " << y << ", " << z
                  << "): " << ex << ", " << ey << ", " << ez << "\n    Status: "
                  << status << "\n";
        if (medium) std::cout << "    Medium: " << medium->GetName() << "\n";
      }

      if (status != 0) {
        // Electron is not inside a drift medium.
        Update(it, x, y, z, t, energy, kx, ky, kz, band);
        (*it).status = StatusLeftDriftMedium;
        AddToEndPoints(*it, hole);
        if (m_debug) PrintStatus(hdr, "left the drift medium", x, y, z, hole);
        continue;
      }

      // If switched on, get the local magnetic field.
      if (m_useBfield) {
        m_sensor->MagneticField(x, y, z, bx, by, bz, status);
        const double scale = hole ? Tesla2Internal : -Tesla2Internal;
        bx *= scale;
        by *= scale;
        bz *= scale;
        // Make sure that neither E nor B are zero.
        bmag = sqrt(bx * bx + by * by + bz * bz);
        const double emag2 = ex * ex + ey * ey + ez * ez;
        bOk = (bmag > Small && emag2 > Small);
      }

      // Trace the electron/hole.
      while (1) {

        bool isNullCollision = false;

        // Make sure the electron energy exceeds the transport cut.
        if (energy < m_deltaCut) {
          Update(it, x, y, z, t, energy, kx, ky, kz, band);
          (*it).status = StatusBelowTransportCut;
          AddToEndPoints(*it, hole);
          if (m_debug) {
            std::cout << hdr << "Kinetic energy (" << energy 
                      << ") below transport cut.\n";
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
          Update(it, x, y, z, t, energy, kx, ky, kz, band);
          (*it).status = StatusOutsideTimeWindow;
          AddToEndPoints(*it, hole);
          if (m_debug) PrintStatus(hdr, "left the time window", x, y, z, hole);
          ok = false;
          break;
        }

        if (medium->GetId() != id) {
          // Medium has changed.
          if (!medium->IsMicroscopic()) {
            // Electron/hole has left the microscopic drift medium.
            Update(it, x, y, z, t, energy, kx, ky, kz, band);
            (*it).status = StatusLeftDriftMedium;
            AddToEndPoints(*it, hole);
            ok = false;
            if (m_debug) {
              std::cout << hdr << "\n    Medium at " 
                        << x << ", " << y << ", " << z
                        << " does not have cross-section data.\n";
            }
            break;
          }
          id = medium->GetId();
          useBandStructure = (medium->IsSemiconductor() && m_useBandStructureDefault);
          // Update the null-collision rate.
          fLim = medium->GetElectronNullCollisionRate(band);
          if (fLim <= 0.) {
            std::cerr << hdr << "Got null-collision rate <= 0.\n";
            return false;
          }
          fInv = 1. / fLim;
        }

        double vx = 0., vy = 0., vz = 0.;
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
        double dt = 0.;
        while (1) {
          // Sample the flight time.
          const double r = RndmUniformPos();
          dt += -log(r) * fInv;
          // Calculate the energy after the proposed step.
          if (m_useBfield && bOk) {
            cwt = cos(wb * dt);
            swt = sin(wb * dt);
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt +
                                     a4 * (a3 * (1. - cwt) + vz * swt),
                                 Small);
          } else if (useBandStructure) {
            const double cdt = dt * SpeedOfLight;
            newEnergy = std::max(
                medium->GetElectronEnergy(kx + ex * cdt, ky + ey * cdt,
                    kz + ez * cdt, newVx, newVy, newVz, band),
                Small);
          } else {
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt, Small);
          }
          // Get the real collision rate at the updated energy.
          double fReal = medium->GetElectronCollisionRate(newEnergy, band);
          if (fReal <= 0.) {
            std::cerr << hdr << "Got collision rate <= 0 at " << newEnergy
                      << " eV (band " << band << ").\n";
            return false;
          }
          if (fReal > fLim) {
            // Real collision rate is higher than null-collision rate.
            dt += log(r) * fInv;
            // Increase the null collision rate and try again.
            std::cerr << hdr << "Increasing null-collision rate by 5%.\n";
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

        double x1 = x + vx * dt;
        double y1 = y + vy * dt;
        double z1 = z + vz * dt;
        double t1 = t + dt; 
        // Get the electric field and medium at the proposed new position.
        m_sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
        if (!hole) {
          ex = -ex;
          ey = -ey;
          ez = -ez;
        }

        // Check if the electron is still inside a drift medium/the drift area.
        if (status != 0 || !m_sensor->IsInArea(x1, y1, z1)) {
          // Try to terminate the drift line close to the boundary (endpoint 
          // outside the drift medium/drift area) using iterative bisection.
          Terminate(x, y, z, t, x1, y1, z1, t1); 
          if (m_useSignal) {
            const int q = hole ? 1 : -1;
            m_sensor->AddSignal(q, t, t1 - t,
                                0.5 * (x + x1), 0.5 * (y + y1), 
                                0.5 * (z + z1), vx, vy, vz);
          }
          Update(it, x1, y1, z1, t1, energy, newKx, newKy, newKz, band);
          if (status != 0) {
            (*it).status = StatusLeftDriftMedium;
            if (m_debug) PrintStatus(hdr, "left the drift medium", x1, y1, z1, hole);
          } else {
            (*it).status = StatusLeftDriftArea;
            if (m_debug) PrintStatus(hdr, "left the drift area", x1, y1, z1, hole);
          } 
          AddToEndPoints(*it, hole);
          ok = false;
          break;
        }

        // Check if the electron/hole has crossed a wire.
        double xc = x, yc = y, zc = z;
        if (m_sensor->IsWireCrossed(x, y, z, x1, y1, z1, xc, yc, zc)) {
          // If switched on, calculated the induced signal over this step.
          if (m_useSignal) {
            const double dx = xc - x;
            const double dy = yc - y;
            const double dz = zc - z;
            dt = sqrt(dx * dx + dy * dy + dz * dz) /
                 sqrt(vx * vx + vy * vy + vz * vz);
            const int q = hole ? 1 : -1;
            m_sensor->AddSignal(q, t, dt, 0.5 * (x + xc), 0.5 * (y + yc),
                                0.5 * (z + zc), vx, vy, vz);
          }
          Update(it, xc, yc, zc, t + dt, energy, newKx, newKy, newKz, band);
          (*it).status = StatusLeftDriftMedium;
          AddToEndPoints(*it, hole);
          ok = false;
          if (m_debug) PrintStatus(hdr, "hit a wire", x, y, z, hole);
          break;
        }

        // If switched on, calculate the induced signal.
        if (m_useSignal) {
          const int q = hole ? 1 : -1;
          m_sensor->AddSignal(q, t, dt, 0.5 * (x + x1), 0.5 * (y + y1),
                              0.5 * (z + z1), vx, vy, vz);
        }

        // Update the coordinates.
        x = x1;
        y = y1;
        z = z1;
        t = t1;

        // If switched on, get the magnetic field at the new location.
        if (m_useBfield) {
          m_sensor->MagneticField(x, y, z, bx, by, bz, status);
          const double scale = hole ? Tesla2Internal : -Tesla2Internal;
          bx *= scale;
          by *= scale;
          bz *= scale;
          // Make sure that neither E nor B are zero.
          bmag = sqrt(bx * bx + by * by + bz * bz);
          const double emag2 = ex * ex + ey * ey + ez * ez;
          bOk = (bmag > Small && emag2 > Small);
        }

        if (isNullCollision) {
          energy = newEnergy;
          kx = newKx;
          ky = newKy;
          kz = newKz;
          continue;
        }

        // Get the collision type and parameters.
        int cstype = 0;
        int level = 0;
        int ndxc = 0;
        medium->GetElectronCollision(newEnergy, cstype, level, energy, newKx,
                                     newKy, newKz, secondaries, ndxc, band);
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
            for (const auto& secondary : secondaries) {
              if (secondary.first == IonProdTypeElectron) {
                const double esec = std::max(secondary.second, Small);
                if (m_histSecondary) m_histSecondary->Fill(esec);
                // Increment the electron counter.
                ++m_nElectrons;
                if (!aval) continue;
                // Add the secondary electron to the stack.
                if (useBandStructure) {
                  double kxs = 0., kys = 0., kzs = 0.;
                  int bs = -1;
                  medium->GetElectronMomentum(esec, kxs, kys, kzs, bs);
                  AddToStack(x, y, z, t, esec, kxs, kys, kzs, bs, false, stackNew);
                } else {
                  AddToStack(x, y, z, t, esec, false, stackNew);
                }
              } else if (secondary.first == IonProdTypeHole) {
                const double esec = std::max(secondary.second, Small);
                // Increment the hole counter.
                ++m_nHoles;
                if (!aval) continue;
                // Add the secondary hole to the stack.
                if (useBandStructure) {
                  double kxs = 0., kys = 0., kzs = 0.;
                  int bs = -1;
                  medium->GetElectronMomentum(esec, kxs, kys, kzs, bs);
                  AddToStack(x, y, z, t, esec, kxs, kys, kzs, bs, true, stackNew);
                } else {
                  AddToStack(x, y, z, t, esec, true, stackNew);
                }
              } else if (secondary.first == IonProdTypeIon) {
                ++m_nIons;
              }
            }
            secondaries.clear();
            if (m_debug) PrintStatus(hdr, "ionised", x, y, z, hole);
            break;
          // Attachment
          case ElectronCollisionTypeAttachment:
            if (m_usePlotting && m_plotAttachments) {
              m_viewer->AddAttachmentMarker(x, y, z);
            }
            if (m_hasUserHandleAttachment) {
              m_userHandleAttachment(x, y, z, t, cstype, level, medium);
            }
            // TODO: check kx or newKx!
            Update(it, x, y, z, t, energy, newKx, newKy, newKz, band);
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
            if (ndxc <= 0) break;
            // Get the electrons/photons produced in the deexcitation cascade.
            stackPhotons.clear();
            for (int j = ndxc; j--;) {
              double tdx = 0., sdx = 0., edx = 0.;
              int typedx = 0;
              if (!medium->GetDeexcitationProduct(j, tdx, sdx, typedx, edx)) {
                std::cerr << hdr << "Cannot retrieve deexcitation product "
                          << j << "/" << ndxc << ".\n";
                break;
              }

              if (typedx == DxcProdTypeElectron) {
                // Penning ionisation
                double xp = x, yp = y, zp = z;
                if (sdx > Small) {
                  // Randomise the point of creation.
                  double dxp = 0., dyp = 0., dzp = 0.;
                  RndmDirection(dxp, dyp, dzp);
                  xp += sdx * dxp;
                  yp += sdx * dyp;
                  zp += sdx * dzp;
                }
                // Get the electric field and medium at this location.
                Medium* med = nullptr;
                double fx = 0., fy = 0., fz = 0.;
                m_sensor->ElectricField(xp, yp, zp, fx, fy, fz, med, status);
                // Check if this location is inside a drift medium/area.
                if (status != 0 || !m_sensor->IsInArea(xp, yp, zp)) continue;
                // Increment the electron and ion counters.
                ++m_nElectrons;
                ++m_nIons;
                // Make sure we haven't jumped across a wire.
                if (m_sensor->IsWireCrossed(x, y, z, xp, yp, zp, xc, yc, zc)) {
                  continue;
                }
                if (!aval) continue;
                // Add the Penning electron to the list.
                AddToStack(xp, yp, zp, t + tdx, std::max(edx, Small), false, stackNew);
              } else if (typedx == DxcProdTypePhoton && m_usePhotons &&
                         edx > m_gammaCut) {
                // Radiative de-excitation
                stackPhotons.emplace_back(std::make_pair(t + tdx, edx));
              }
            }

            // Transport the photons (if any)
            if (aval) {
              const int nSizePhotons = stackPhotons.size();
              for (int j = nSizePhotons; j--;) {
                TransportPhoton(x, y, z, stackPhotons[j].first, stackPhotons[j].second, stackNew);
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
            std::cerr << hdr << "Unknown collision type.\n";
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
      Update(it, x, y, z, t, energy, kx, ky, kz, band);
      // Add a new point to the drift line (if enabled).
      if (m_useDriftLines) {
        point newPoint;
        newPoint.x = x;
        newPoint.y = y;
        newPoint.z = z;
        newPoint.t = t;
        (*it).driftLine.push_back(std::move(newPoint));
      }
    }
  }

  // Calculate the induced charge.
  if (m_useInducedCharge) {
    const unsigned int nElectronEndpoints = m_endpointsElectrons.size();
    for (unsigned int i = 0; i < nElectronEndpoints; ++i) {
      const Electron& p = m_endpointsElectrons[i];
      m_sensor->AddInducedCharge(-1, p.x0, p.y0, p.z0, p.x, p.y, p.z);
    }
    const unsigned int nHoleEndpoints = m_endpointsHoles.size();
    for (unsigned int i = 0; i < nHoleEndpoints; ++i) {
      const Electron& p = m_endpointsHoles[i];
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
      const Electron& p = m_endpointsElectrons[i];
      m_viewer->NewElectronDriftLine(np, jL, p.x0, p.y0, p.z0);
      for (int jP = np; jP--;) {
        double x = 0., y = 0., z = 0., t = 0.;
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
      const Electron& p = m_endpointsHoles[i];
      m_viewer->NewHoleDriftLine(np, jL, p.x0, p.y0, p.z0);
      for (int jP = np; jP--;) {
        double x = 0., y = 0., z = 0., t = 0.;
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
                                           const double e0, 
                                           std::vector<Electron>& stack) {

  // Make sure that the sensor is defined.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportPhoton: Sensor is not defined.\n";
    return;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportPhoton:\n"
              << "    No medium at initial position.\n";
    return;
  }

  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << m_className << "::TransportPhoton:\n"
              << "    Medium at initial position does not provide "
              << " microscopic tracking data.\n";
    return;
  }

  // Get the id number of the drift medium.
  int id = medium->GetId();

  // Position
  double x = x0, y = y0, z = z0;
  double t = t0;
  // Initial direction (randomised).
  double dx = 0., dy = 0., dz = 0.;
  RndmDirection(dx, dy, dz);
  // Energy
  double e = e0;


  // Photon collision rate
  double f = medium->GetPhotonCollisionRate(e);
  if (f <= 0.) return;
  // Timestep
  double dt = -log(RndmUniformPos()) / f;
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
    m_photons.push_back(std::move(newPhoton));
    return;
  }

  int type, level;
  double e1;
  double ctheta = 0.;
  int nsec = 0;
  double esec = 0.;
  if (!medium->GetPhotonCollision(e, type, level, e1, ctheta, nsec, esec))
    return;

  if (type == PhotonCollisionTypeIonisation) {
    // Add the secondary electron (random direction) to the stack.
    if (m_sizeCut == 0 || stack.size() < m_sizeCut) {
      AddToStack(x, y, z, t, std::max(esec, Small), false, stack); 
    }
    // Increment the electron and ion counters.
    ++m_nElectrons;
    ++m_nIons;
  } else if (type == PhotonCollisionTypeExcitation) {
    double tdx = 0.;
    double sdx = 0.;
    int typedx = 0;
    std::vector<double> tPhotons;
    std::vector<double> ePhotons;
    for (int j = nsec; j--;) {
      if (!medium->GetDeexcitationProduct(j, tdx, sdx, typedx, esec))
        continue;
      if (typedx == DxcProdTypeElectron) {
        // Ionisation. 
        AddToStack(x, y, z, t + tdx, std::max(esec, Small), false, stack);
        // Increment the electron and ion counters.
        ++m_nElectrons;
        ++m_nIons;
      } else if (typedx == DxcProdTypePhoton && m_usePhotons &&
                 esec > m_gammaCut) {
        // Radiative de-excitation
        tPhotons.push_back(t + tdx);
        ePhotons.push_back(esec);
      }
    }
    // Transport the photons (if any).
    const int nSizePhotons = tPhotons.size();
    for (int k = nSizePhotons; k--;) {
      TransportPhoton(x, y, z, tPhotons[k], ePhotons[k], stack);
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
  m_photons.push_back(std::move(newPhoton));
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

void AvalancheMicroscopic::Update(std::vector<Electron>::iterator it, 
                                  const double x, const double y, 
                                  const double z, const double t,
                                  const double energy, 
                                  const double kx, const double ky,
                                  const double kz, const int band) {

  (*it).x = x;
  (*it).y = y;
  (*it).z = z;
  (*it).t = t;
  (*it).energy = energy;
  (*it).kx = kx;
  (*it).ky = ky;
  (*it).kz = kz;
  (*it).band = band;
}

void AvalancheMicroscopic::AddToStack(const double x, const double y,
                                      const double z, const double t,
                                      const double energy, 
                                      const bool hole,
                                      std::vector<Electron>& container) const {

  // Randomise the direction.
  double dx = 0., dy = 0., dz = 1.;
  RndmDirection(dx, dy, dz);
  AddToStack(x, y, z, t, energy, dx, dy, dz, 0, hole, container); 
}

void AvalancheMicroscopic::AddToStack(const double x, const double y,
                                      const double z, const double t,
                                      const double energy, 
                                      const double dx, const double dy,
                                      const double dz, const int band,
                                      const bool hole,
                                      std::vector<Electron>& container) const {

  Electron electron;
  electron.status = 0;
  electron.hole = hole;
  electron.x0 = x;
  electron.y0 = y;
  electron.z0 = z;
  electron.t0 = t;
  electron.e0 = energy;
  electron.x = x;
  electron.y = y;
  electron.z = z;
  electron.t = t;
  electron.energy = energy;
  electron.kx = dx;
  electron.ky = dy;
  electron.kz = dz;
  electron.band = band;
  // Previous coordinates for distance histogramming.
  electron.xLast = x;
  electron.yLast = y;
  electron.zLast = z;
  electron.driftLine.reserve(1000);
  container.push_back(std::move(electron));
}

void AvalancheMicroscopic::Terminate(double x0, double y0, double z0, double t0,
                                     double& x1, double& y1,
                                     double& z1, double& t1) {

  const double dx = x1 - x0;
  const double dy = y1 - y0;
  const double dz = z1 - z0;
  double d = sqrt(dx * dx + dy * dy + dz * dz);
  while (d > BoundaryDistance) {
    d *= 0.5;
    const double xm = 0.5 * (x0 + x1);
    const double ym = 0.5 * (y0 + y1);
    const double zm = 0.5 * (z0 + z1);
    const double tm = 0.5 * (t0 + t1);
    // Check if the mid-point is inside the drift medium.
    double ex = 0., ey = 0., ez = 0.;
    Medium* medium = nullptr;
    int status = 0;
    m_sensor->ElectricField(xm, ym, zm, ex, ey, ez, medium, status);
    if (status == 0 && m_sensor->IsInArea(xm, ym, zm)) {
      x0 = xm;
      y0 = ym;
      z0 = zm;
      t0 = tm;
    } else {
      x1 = xm;
      y1 = ym;
      z1 = zm;
      t1 = tm;
    }
  }
}

}
