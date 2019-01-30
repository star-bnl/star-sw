#include <iostream>

#include "wcpplib/matter/GasLib.h"
#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

#include "heed++/code/ElElasticScat.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedCondElectron.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/HeedMatterDef.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/PhotoAbsCSLib.h"

#include "HeedChamber.hh"
#include "HeedFieldMap.h"

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

#include "TrackHeed.hh"

namespace {

void ClearBank(std::vector<Heed::gparticle*>& bank) {

  for (auto particle : bank) if (particle) delete particle;
  bank.clear();
}

}

// Global functions and variables required by Heed
namespace Heed {

// Particle id number for book-keeping
long last_particle_number;

}

// Actual class implementation

namespace Garfield {

TrackHeed::TrackHeed() : Track() {

  m_className = "TrackHeed";
  m_conductionElectrons.reserve(1000);
  m_conductionIons.reserve(1000);
}

TrackHeed::~TrackHeed() {}

bool TrackHeed::NewTrack(const double x0, const double y0, const double z0,
                         const double t0, const double dx0, const double dy0,
                         const double dz0) {

  m_hasActiveTrack = false;
  m_ready = false;

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::NewTrack: Sensor is not defined.\n";
    return false;
  }

  bool update = false;
  if (!UpdateBoundingBox(update)) return false;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    No medium at initial position.\n";
    return false;
  } else if (!medium->IsIonisable()) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Medium at initial position is not ionisable.\n";
    return false;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != m_mediumName ||
      fabs(medium->GetMassDensity() - m_mediumDensity) > 1.e-9) {
    m_isChanged = true;
  }

  // If medium, particle or bounding box have changed,
  // update the cross-sections.
  if (m_isChanged) {
    if (!Setup(medium)) return false;
    m_isChanged = false;
    m_mediumName = medium->GetName();
    m_mediumDensity = medium->GetMassDensity();
  }

  ClearParticleBank();

  m_deltaElectrons.clear();
  m_conductionElectrons.clear();
  m_conductionIons.clear();

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d < Small) {
    if (m_debug) {
      std::cout << m_className << "::NewTrack:\n"
                << "    Direction vector has zero norm.\n"
                << "    Initial direction is randomized.\n";
    }
    // Null vector. Sample the direction isotropically.
    RndmDirection(dx, dy, dz);
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  Heed::vec velocity(dx, dy, dz);
  velocity = velocity * Heed::CLHEP::c_light * GetBeta();

  if (m_debug) {
    std::cout << m_className << "::NewTrack:\n    Track starts at ("
              << x0 << ", " << y0 << ", " << z0 << ") at time " << t0 << "\n"
              << "    Direction: (" << dx << ", " << dy << ", " << dz << ")\n";
  }

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  Heed::point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);

  // Setup the particle.
  Heed::particle_def* particleType = &Heed::muon_minus_def;
  if (m_particleName == "e-") {
    particleType = &Heed::electron_def;
  } else if (m_particleName == "e+") {
    particleType = &Heed::positron_def;
  } else if (m_particleName == "mu-") {
    particleType = &Heed::muon_minus_def;
  } else if (m_particleName == "mu+") {
    particleType = &Heed::muon_plus_def;
  } else if (m_particleName == "pi-") {
    particleType = &Heed::pi_minus_meson_def;
  } else if (m_particleName == "pi+") {
    particleType = &Heed::pi_plus_meson_def;
  } else if (m_particleName == "K-") {
    particleType = &Heed::K_minus_meson_def;
  } else if (m_particleName == "K+") {
    particleType = &Heed::K_plus_meson_def;
  } else if (m_particleName == "p") {
    particleType = &Heed::proton_def;
  } else if (m_particleName == "pbar") {
    particleType = &Heed::anti_proton_def;
  } else if (m_particleName == "d") {
    particleType = &Heed::deuteron_def;
  } else if (m_particleName == "alpha") {
    particleType = &Heed::alpha_particle_def;
  } else if (m_particleName == "exotic") {
    // User defined particle
    Heed::user_particle_def.set_mass(m_mass * 1.e-6);
    Heed::user_particle_def.set_charge(m_q);
    particleType = &Heed::user_particle_def;
  } else {
    // Not a predefined particle, use muon definition.
    if (m_q > 0.) {
      particleType = &Heed::muon_minus_def;
    } else {
      particleType = &Heed::muon_plus_def;
    }
  }

  Heed::HeedParticle particle(m_chamber.get(), p0, velocity, t0,
                              particleType, &m_fieldMap);
  // Set the step limits.
  particle.set_step_limits(m_maxStep * Heed::CLHEP::cm,
                           m_radStraight * Heed::CLHEP::cm,
                           m_stepAngleStraight * Heed::CLHEP::rad,
                           m_stepAngleCurved * Heed::CLHEP::rad);
  // Transport the particle.
  particle.fly(m_particleBank);
  m_bankIterator = m_particleBank.begin();
  m_hasActiveTrack = true;
  m_ready = true;

  // Plot the new track.
  if (m_usePlotting) PlotNewTrack(x0, y0, z0);
  return true;
}

double TrackHeed::GetClusterDensity() {

  if (!m_ready) {
    std::cerr << m_className << "::GetClusterDensity:\n"
              << "    Track has not been initialized.\n";
    return 0.;
  }

  if (!m_transferCs) {
    std::cerr << m_className << "::GetClusterDensity:\n"
              << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return m_transferCs->quanC;
}

double TrackHeed::GetStoppingPower() {

  if (!m_ready) {
    std::cerr << m_className << "::GetStoppingPower:\n"
              << "    Track has not been initialized.\n";
    return 0.;
  }

  if (!m_transferCs) {
    std::cerr << m_className << "::GetStoppingPower:\n"
              << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return m_transferCs->meanC1 * 1.e6;
}

bool TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls,
                           double& tcls, int& n, double& e, double& extra) {

  int ni = 0;
  return GetCluster(xcls, ycls, zcls, tcls, n, ni, e, extra);
}

bool TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls,
                           double& tcls, int& ne, int& ni, double& e, 
                           double& extra) {

  // Initialise and reset.
  xcls = ycls = zcls = tcls = 0.;
  extra = 0.;
  ne = ni = 0;
  e = 0.;

  m_deltaElectrons.clear();
  m_conductionElectrons.clear();
  m_conductionIons.clear();

  // Make sure NewTrack has been called successfully.
  if (!m_ready) {
    std::cerr << m_className << "::GetCluster:\n"
              << "    Track has not been initialized. Call NewTrack first.\n";
    return false;
  }

  if (m_particleBank.empty()) return false;

  std::vector<Heed::gparticle*>::const_iterator end = m_particleBank.end();
  if (m_bankIterator == end) {
    std::cerr << m_className << "::GetCluster:\n"
              << "    There are no more clusters on this track.\n";
    return false;
  }

  // Look for the next cluster (i. e. virtual photon) in the list.
  Heed::HeedPhoton* virtualPhoton = nullptr;
  for (; m_bankIterator != end; ++m_bankIterator) {
    // Convert the particle to a (virtual) photon.
    virtualPhoton = dynamic_cast<Heed::HeedPhoton*>(*m_bankIterator);
    if (!virtualPhoton) {
      std::cerr << m_className << "::GetCluster:\n"
                << "    Particle is not a virtual photon. Program bug!\n";
      // Try the next element.
      continue;
    }

    // Get the location of the interaction (convert from mm to cm
    // and shift with respect to bounding box center).
    xcls = virtualPhoton->position().x * 0.1 + m_cX;
    ycls = virtualPhoton->position().y * 0.1 + m_cY;
    zcls = virtualPhoton->position().z * 0.1 + m_cZ;
    tcls = virtualPhoton->time();
    // Skip clusters outside the drift area or outside the active medium.
    if (!IsInside(xcls, ycls, zcls)) continue;
    // Add the first ion (at the position of the cluster).
    m_conductionIons.emplace_back(
        Heed::HeedCondElectron(xcls, ycls, zcls, tcls));
    ++m_bankIterator;
    break;
  }

  // Stop if we did not find a virtual photon.
  if (m_bankIterator == end || !virtualPhoton) return false;
  // Plot the cluster, if requested.
  if (m_usePlotting) PlotCluster(xcls, ycls, zcls);

  std::vector<Heed::gparticle*> secondaries;
  // Transport the virtual photon.
  virtualPhoton->fly(secondaries);
  // Get the transferred energy (convert from MeV to eV).
  e = virtualPhoton->m_energy * 1.e6;

  while (!secondaries.empty()) {
    std::vector<Heed::gparticle*> newSecondaries;
    // Loop over the secondaries.
    for (auto secondary : secondaries) {
      // Check if it is a delta electron.
      auto delta = dynamic_cast<Heed::HeedDeltaElectron*>(secondary);
      if (delta) {
        extra += delta->kinetic_energy() * 1.e6;
        const double x = delta->position().x * 0.1 + m_cX;
        const double y = delta->position().y * 0.1 + m_cY;
        const double z = delta->position().z * 0.1 + m_cZ;
        if (!IsInside(x, y, z)) continue;
        if (m_doDeltaTransport) {
          // Transport the delta electron.
          delta->fly(newSecondaries);
          // Add the conduction electrons and ions to the list.
          m_conductionElectrons.insert(m_conductionElectrons.end(), 
                                       delta->conduction_electrons.begin(),
                                       delta->conduction_electrons.end());
          m_conductionIons.insert(m_conductionIons.end(), 
                                  delta->conduction_ions.begin(),
                                  delta->conduction_ions.end());
        } else {
          // Add the delta electron to the list, for later use.
          deltaElectron newDeltaElectron;
          newDeltaElectron.x = delta->position().x * 0.1 + m_cX;
          newDeltaElectron.y = delta->position().y * 0.1 + m_cY;
          newDeltaElectron.z = delta->position().z * 0.1 + m_cZ;
          newDeltaElectron.t = delta->time();
          newDeltaElectron.e = delta->kinetic_energy() * 1.e6;
          newDeltaElectron.dx = delta->direction().x;
          newDeltaElectron.dy = delta->direction().y;
          newDeltaElectron.dz = delta->direction().z;
          m_deltaElectrons.push_back(std::move(newDeltaElectron));
        }
        continue;
      } 
      // Check if it is a real photon.
      auto photon = dynamic_cast<Heed::HeedPhoton*>(secondary);
      if (!photon) {
        std::cerr << m_className << "::GetCluster:\n"
                  << "    Particle is neither an electron nor a photon.\n";
      }
      extra += photon->m_energy * 1.e6;
      const double x = photon->position().x * 0.1 + m_cX;
      const double y = photon->position().y * 0.1 + m_cY;
      const double z = photon->position().z * 0.1 + m_cZ;
      if (!IsInside(x, y, z)) continue;
      // Transport the photon.
      if (m_usePhotonReabsorption) photon->fly(newSecondaries);
    }
    for (auto secondary : secondaries) if (secondary) delete secondary;
    secondaries.clear();
    secondaries.swap(newSecondaries);
  }
  // Get the total number of electrons produced in this step.
  ne = m_doDeltaTransport ? m_conductionElectrons.size() : m_deltaElectrons.size();
  ni = m_conductionIons.size();
  return true;
}

bool TrackHeed::GetElectron(const unsigned int i, 
                            double& x, double& y, double& z, double& t,
                            double& e, double& dx, double& dy, double& dz) {

  // Make sure NewTrack has successfully been called.
  if (!m_ready) {
    std::cerr << m_className << "::GetElectron:\n"
              << "    Track has not been initialized. Call NewTrack first.\n";
    return false;
  }

  if (m_doDeltaTransport) {
    // Make sure an electron with this number exists.
    if (i >= m_conductionElectrons.size()) {
      std::cerr << m_className << "::GetElectron: Index out of range.\n";
      return false;
    }

    x = m_conductionElectrons[i].x * 0.1 + m_cX;
    y = m_conductionElectrons[i].y * 0.1 + m_cY;
    z = m_conductionElectrons[i].z * 0.1 + m_cZ;
    t = m_conductionElectrons[i].time;
    e = 0.;
    dx = dy = dz = 0.;

  } else {
    // Make sure a delta electron with this number exists.
    if (i >= m_deltaElectrons.size()) {
      std::cerr << m_className << "::GetElectron:\n"
                << "    Delta electron number out of range.\n";
      return false;
    }

    x = m_deltaElectrons[i].x;
    y = m_deltaElectrons[i].y;
    z = m_deltaElectrons[i].z;
    t = m_deltaElectrons[i].t;
    e = m_deltaElectrons[i].e;
    dx = m_deltaElectrons[i].dx;
    dy = m_deltaElectrons[i].dy;
    dz = m_deltaElectrons[i].dz;
  }
  return true;
}

bool TrackHeed::GetIon(const unsigned int i,
                       double& x, double& y, double& z, double& t) const {

  // Make sure a "conduction" ion with this number exists.
  if (i >= m_conductionIons.size()) {
    std::cerr << m_className << "::GetIon: Index out of range.\n";
    return false;
  }

  x = m_conductionIons[i].x * 0.1 + m_cX;
  y = m_conductionIons[i].y * 0.1 + m_cY;
  z = m_conductionIons[i].z * 0.1 + m_cZ;
  t = m_conductionIons[i].time;
  return true;
}

void TrackHeed::TransportDeltaElectron(const double x0, const double y0,
                                       const double z0, const double t0,
                                       const double e0, const double dx0,
                                       const double dy0, const double dz0,
                                       int& nel) {
  int ni = 0;
  return TransportDeltaElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, nel, ni);
}

void TrackHeed::TransportDeltaElectron(const double x0, const double y0,
                                       const double z0, const double t0,
                                       const double e0, const double dx0,
                                       const double dy0, const double dz0,
                                       int& nel, int& ni) {

  nel = 0;
  ni = 0;

  // Check if delta electron transport was disabled.
  if (!m_doDeltaTransport) {
    std::cerr << m_className << "::TransportDeltaElectron:\n"
              << "    Delta electron transport has been switched off.\n";
    return;
  }


  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportDeltaElectron:\n"
              << "    Sensor is not defined.\n";
    m_ready = false;
    return;
  }

  bool update = false;
  if (!UpdateBoundingBox(update)) return;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportDeltaElectron:\n"
              << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportDeltaElectron:\n"
              << "    Medium at initial position is not ionisable.\n";
    m_ready = false;
    return;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != m_mediumName ||
      fabs(medium->GetMassDensity() - m_mediumDensity) > 1.e-9) {
    m_isChanged = true;
    update = true;
    m_ready = false;
    m_hasActiveTrack = false;
  }

  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    m_ready = true;
    m_mediumName = medium->GetName();
    m_mediumDensity = medium->GetMassDensity();
  }

  m_deltaElectrons.clear();
  m_conductionElectrons.clear();
  m_conductionIons.clear();

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  Heed::point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);

  // Make sure the kinetic energy is positive.
  if (e0 <= 0.) {
    // Just create a conduction electron on the spot.
    m_conductionElectrons.emplace_back(Heed::HeedCondElectron(p0, t0));
    nel = 1;
    return;
  }

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d <= 0.) {
    // Null vector. Sample the direction isotropically.
    RndmDirection(dx, dy, dz);
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  Heed::vec velocity(dx, dy, dz);

  // Calculate the speed for the given kinetic energy.
  const double gamma = 1. + e0 / ElectronMass;
  const double beta = sqrt(1. - 1. / (gamma * gamma));
  double speed = Heed::CLHEP::c_light * beta;
  velocity = velocity * speed;

  // Transport the electron.
  std::vector<Heed::gparticle*> secondaries;
  Heed::HeedDeltaElectron delta(m_chamber.get(), p0, velocity, t0, 0, &m_fieldMap);
  delta.fly(secondaries);
  ClearBank(secondaries);

  m_conductionElectrons.swap(delta.conduction_electrons);
  m_conductionIons.swap(delta.conduction_ions);
  nel = m_conductionElectrons.size();
  ni = m_conductionIons.size();
}

void TrackHeed::TransportPhoton(const double x0, const double y0,
                                const double z0, const double t0,
                                const double e0, const double dx0,
                                const double dy0, const double dz0, int& nel) {
  int ni = 0;
  TransportPhoton(x0, y0, z0, t0, e0, dx0, dy0, dz0, nel, ni);
}

void TrackHeed::TransportPhoton(const double x0, const double y0,
                                const double z0, const double t0,
                                const double e0, const double dx0,
                                const double dy0, const double dz0, 
                                int& nel, int& ni) {

  nel = 0;
  ni = 0;

  // Make sure the energy is positive.
  if (e0 <= 0.) {
    std::cerr << m_className << "::TransportPhoton:\n"
              << "    Photon energy must be positive.\n";
    return;
  }

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportPhoton: Sensor is not defined.\n";
    m_ready = false;
    return;
  }

  bool update = false;
  if (!UpdateBoundingBox(update)) return;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportPhoton:\n"
              << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportPhoton:\n"
              << "    Medium at initial position is not ionisable.\n";
    m_ready = false;
    return;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != m_mediumName ||
      fabs(medium->GetMassDensity() - m_mediumDensity) > 1.e-9) {
    m_isChanged = true;
    update = true;
    m_ready = false;
  }

  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    m_ready = true;
    m_mediumName = medium->GetName();
    m_mediumDensity = medium->GetMassDensity();
  }

  // Delete the particle bank.
  // Clusters from the current track will be lost.
  m_hasActiveTrack = false;
  ClearParticleBank();
  m_deltaElectrons.clear();
  m_conductionElectrons.clear();
  m_conductionIons.clear();

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d <= 0.) {
    // Null vector. Sample the direction isotropically.
    RndmDirection(dx, dy, dz);
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  Heed::vec velocity(dx, dy, dz);
  velocity = velocity * Heed::CLHEP::c_light;

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  Heed::point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);

  // Create and transport the photon.
  Heed::HeedPhoton photon(m_chamber.get(), p0, velocity, t0, 0, e0 * 1.e-6, 
                          &m_fieldMap);
  std::vector<Heed::gparticle*> secondaries;
  photon.fly(secondaries);

  while (!secondaries.empty()) {
    std::vector<Heed::gparticle*> newSecondaries;
    // Loop over the particle bank and look for daughter particles.
    std::vector<Heed::gparticle*>::iterator it;
    for (it = secondaries.begin(); it != secondaries.end(); ++it) {
      // Check if it is a delta electron.
      auto delta = dynamic_cast<Heed::HeedDeltaElectron*>(*it);
      if (delta) {
        if (m_doDeltaTransport) {
          // Transport the delta electron.
          delta->fly(newSecondaries);
          // Add the conduction electrons to the list.
          m_conductionElectrons.insert(m_conductionElectrons.end(), 
                                       delta->conduction_electrons.begin(),
                                       delta->conduction_electrons.end());
          m_conductionIons.insert(m_conductionIons.end(), 
                                  delta->conduction_ions.begin(),
                                  delta->conduction_ions.end());
        } else {
          // Add the delta electron to the list, for later use.
          deltaElectron newDeltaElectron;
          newDeltaElectron.x = delta->position().x * 0.1 + m_cX;
          newDeltaElectron.y = delta->position().y * 0.1 + m_cY;
          newDeltaElectron.z = delta->position().z * 0.1 + m_cZ;
          newDeltaElectron.t = delta->time();
          newDeltaElectron.e = delta->kinetic_energy() * 1.e6;
          newDeltaElectron.dx = delta->direction().x;
          newDeltaElectron.dy = delta->direction().y;
          newDeltaElectron.dz = delta->direction().z;
          m_deltaElectrons.push_back(std::move(newDeltaElectron));
        }
        continue;
      }
      // Check if it is a fluorescence photon.
      auto fluorescencePhoton = dynamic_cast<Heed::HeedPhoton*>(*it);
      if (!fluorescencePhoton) {
        std::cerr << m_className << "::TransportPhoton:\n"
                  << "    Unknown secondary particle.\n";
        ClearBank(secondaries);
        ClearBank(newSecondaries);
        return;
      }
      fluorescencePhoton->fly(newSecondaries);
    }
    secondaries.swap(newSecondaries);
    ClearBank(newSecondaries);
  }
  ClearBank(secondaries);
  // Get the total number of electrons produced in this step.
  nel = m_doDeltaTransport ? m_conductionElectrons.size() : 
                             m_deltaElectrons.size();
  ni = m_conductionIons.size();
}

void TrackHeed::EnableElectricField() { m_fieldMap.UseEfield(true); }
void TrackHeed::DisableElectricField() { m_fieldMap.UseEfield(false); }
void TrackHeed::EnableMagneticField() { m_fieldMap.UseBfield(true); }
void TrackHeed::DisableMagneticField() { m_fieldMap.UseBfield(false); }

void TrackHeed::SetEnergyMesh(const double e0, const double e1,
                              const int nsteps) {

  if (fabs(e1 - e0) < Small) {
    std::cerr << m_className << "::SetEnergyMesh:\n"
              << "    Invalid energy range:\n"
              << "    " << e0 << " < E [eV] < " << e1 << "\n";
    return;
  }

  if (nsteps <= 0) {
    std::cerr << m_className << "::SetEnergyMesh:\n"
              << "    Number of intervals must be > 0.\n";
    return;
  }

  m_emin = std::min(e0, e1);
  m_emax = std::max(e0, e1);
  m_emin *= 1.e-6;
  m_emax *= 1.e-6;
  m_nEnergyIntervals = nsteps;
}

void TrackHeed::SetParticleUser(const double m, const double z) {

  if (fabs(z) < Small) {
    std::cerr << m_className << "::SetParticleUser:\n"
              << "    Particle cannot have zero charge.\n";
    return;
  }
  if (m < Small) {
    std::cerr << m_className << "::SetParticleUser:\n"
              << "    Particle mass must be greater than zero.\n";
  }
  m_q = z;
  m_mass = m;
  m_isElectron = false;
  m_spin = 0;
  m_particleName = "exotic";
}

bool TrackHeed::Setup(Medium* medium) {

  // Make sure the path to the Heed database is known.
  std::string databasePath;
  char* dbPath = std::getenv("HEED_DATABASE");
  if (dbPath == NULL) {
    // Try GARFIELD_HOME.
    dbPath = std::getenv("GARFIELD_HOME");
    if (dbPath == NULL) {
      std::cerr << m_className << "::Setup:\n    Cannot retrieve database path "
                << "(environment variables HEED_DATABASE and GARFIELD_HOME "
                << "are not defined).\n    Cannot proceed.\n";
      return false;
    }
    databasePath = std::string(dbPath) + "/Heed/heed++/database";
  } else {
    databasePath = dbPath;
  }
  if (databasePath[databasePath.size() - 1] != '/') {
    databasePath.append("/");
  }

  // Check once more that the medium exists.
  if (!medium) {
    std::cerr << m_className << "::Setup: Null pointer.\n";
    return false;
  }

  // Setup the energy mesh.
  m_energyMesh.reset(new Heed::EnergyMesh(m_emin, m_emax, m_nEnergyIntervals));

  if (medium->IsGas()) {
    if (!SetupGas(medium)) return false;
  } else {
    if (!SetupMaterial(medium)) return false;
  }

  // Energy transfer cross-section
  // Set a flag indicating whether the primary particle is an electron.
  m_transferCs.reset(new Heed::EnTransfCS(1.e-6 * m_mass, GetGamma() - 1., 
                                          m_isElectron, m_matter.get(), 
                                          long(m_q)));

  if (!SetupDelta(databasePath)) return false;

  if (m_debug) {
    const double nc = m_transferCs->quanC;
    const double dedx = m_transferCs->meanC * 1.e3;
    const double dedx1 = m_transferCs->meanC1 * 1.e3;
    const double w = m_matter->W * 1.e6;
    const double f = m_matter->F;
    const double minI = m_matter->min_ioniz_pot * 1.e6;
    std::cout << m_className << "::Setup:\n";
    std::cout << "    Cluster density:             " << nc << " cm-1\n";
    std::cout << "    Stopping power (restricted): " << dedx << " keV/cm\n";
    std::cout << "    Stopping power (incl. tail): " << dedx1 << " keV/cm\n";
    std::cout << "    W value:                     " << w << " eV\n";
    std::cout << "    Fano factor:                 " << f << "\n";
    std::cout << "    Min. ionization potential:   " << minI << " eV\n";
  }

  Heed::fixsyscoor primSys(Heed::point(0., 0., 0.), 
                           Heed::basis("primary"), "primary");
  m_chamber.reset(new HeedChamber(primSys, m_lX, m_lY, m_lZ, 
                                  *m_transferCs.get(), *m_deltaCs.get()));
  m_fieldMap.SetSensor(m_sensor);
  return true;
}

bool TrackHeed::SetupGas(Medium* medium) {

  // Get temperature and pressure.
  double pressure = medium->GetPressure();
  pressure = (pressure / AtmosphericPressure) * Heed::CLHEP::atmosphere;
  double temperature = medium->GetTemperature();

  const int nComponents = medium->GetNumberOfComponents();
  if (nComponents < 1) {
    std::cerr << m_className << "::SetupGas:\n";
    std::cerr << "    Gas " << medium->GetName() << " has zero constituents.\n";
    return false;
  }

  std::vector<Heed::MolecPhotoAbsCS*> molPacs(nComponents, nullptr);
  std::vector<std::string> notations;
  std::vector<double> fractions;

  for (int i = 0; i < nComponents; ++i) {
    std::string gasname;
    double frac;
    medium->GetComponent(i, gasname, frac);
    // If necessary, change the Magboltz name to the Heed internal name.
    if (gasname == "He-3") gasname = "He";
    if (gasname == "CD4") gasname = "CH4";
    if (gasname == "iC4H10" || gasname == "nC4H10") gasname = "C4H10";
    if (gasname == "neoC5H12" || gasname == "nC5H12") gasname = "C5H12";
    if (gasname == "H2O") gasname = "Water";
    if (gasname == "D2") gasname = "H2";
    if (gasname == "cC3H6") gasname = "C3H6";
    // Find the corresponding photoabsorption cross-section.
    if (gasname == "CF4")
      molPacs[i] = &Heed::CF4_MPACS;
    else if (gasname == "Ar")
      molPacs[i] = &Heed::Ar_MPACS;
    else if (gasname == "He")
      molPacs[i] = &Heed::He_MPACS;
    else if (gasname == "Ne")
      molPacs[i] = &Heed::Ne_MPACS;
    else if (gasname == "Kr")
      molPacs[i] = &Heed::Kr_MPACS;
    else if (gasname == "Xe")
      molPacs[i] = &Heed::Xe_MPACS;
    else if (gasname == "CH4")
      molPacs[i] = &Heed::CH4_MPACS;
    else if (gasname == "C2H6")
      molPacs[i] = &Heed::C2H6_MPACS;
    else if (gasname == "C3H8")
      molPacs[i] = &Heed::C3H8_MPACS;
    else if (gasname == "C4H10")
      molPacs[i] = &Heed::C4H10_MPACS;
    else if (gasname == "CO2")
      molPacs[i] = &Heed::CO2_MPACS;
    else if (gasname == "C5H12")
      molPacs[i] = &Heed::C5H12_MPACS;
    else if (gasname == "Water")
      molPacs[i] = &Heed::H2O_MPACS;
    else if (gasname == "O2")
      molPacs[i] = &Heed::O2_MPACS;
    else if (gasname == "N2" || gasname == "N2 (Phelps)")
      molPacs[i] = &Heed::N2_MPACS;
    else if (gasname == "NO")
      molPacs[i] = &Heed::NO_MPACS;
    else if (gasname == "N2O")
      molPacs[i] = &Heed::N2O_MPACS;
    else if (gasname == "C2H4")
      molPacs[i] = &Heed::C2H4_MPACS;
    else if (gasname == "C2H2")
      molPacs[i] = &Heed::C2H2_MPACS;
    else if (gasname == "H2")
      molPacs[i] = &Heed::H2_MPACS;
    else if (gasname == "CO")
      molPacs[i] = &Heed::CO_MPACS;
    else if (gasname == "Methylal")
      molPacs[i] = &Heed::Methylal_MPACS;
    else if (gasname == "DME")
      molPacs[i] = &Heed::DME_MPACS;
    else if (gasname == "C2F6")
      molPacs[i] = &Heed::C2F6_MPACS;
    else if (gasname == "SF6")
      molPacs[i] = &Heed::SF6_MPACS;
    else if (gasname == "NH3")
      molPacs[i] = &Heed::NH3_MPACS;
    else if (gasname == "C3H6")
      molPacs[i] = &Heed::C3H6_MPACS;
    else if (gasname == "CH3OH")
      molPacs[i] = &Heed::CH3OH_MPACS;
    else if (gasname == "C2H5OH")
      molPacs[i] = &Heed::C2H5OH_MPACS;
    else if (gasname == "C3H7OH")
      molPacs[i] = &Heed::C3H7OH_MPACS;
    else if (gasname == "Cs")
      molPacs[i] = &Heed::Cs_MPACS;
    else if (gasname == "F2")
      molPacs[i] = &Heed::F2_MPACS;
    else if (gasname == "CS2")
      molPacs[i] = &Heed::CS2_MPACS;
    else if (gasname == "COS")
      molPacs[i] = &Heed::COS_MPACS;
    else if (gasname == "CD4")
      molPacs[i] = &Heed::CH4_MPACS;
    else if (gasname == "BF3")
      molPacs[i] = &Heed::BF3_MPACS;
    else if (gasname == "C2HF5")
      molPacs[i] = &Heed::C2HF5_MPACS;
    else if (gasname == "C2H2F4")
      molPacs[i] = &Heed::C2H2F4_MPACS;
    else if (gasname == "CHF3")
      molPacs[i] = &Heed::CHF3_MPACS;
    else if (gasname == "CF3Br")
      molPacs[i] = &Heed::CF3Br_MPACS;
    else if (gasname == "C3F8")
      molPacs[i] = &Heed::C3F8_MPACS;
    else if (gasname == "O3")
      molPacs[i] = &Heed::O3_MPACS;
    else if (gasname == "Hg")
      molPacs[i] = &Heed::Hg_MPACS;
    else if (gasname == "H2S")
      molPacs[i] = &Heed::H2S_MPACS;
    else if (gasname == "GeH4")
      molPacs[i] = &Heed::GeH4_MPACS;
    else if (gasname == "SiH4")
      molPacs[i] = &Heed::SiH4_MPACS;
    else {
      std::cerr << m_className << "::SetupGas:\n    Photoabsorption "
                << "cross-section for " << gasname << " not available.\n";
      return false;
    }
    notations.push_back(gasname);
    fractions.push_back(frac);
  }
  if (m_usePacsOutput) {
    std::ofstream pacsfile;
    pacsfile.open("heed_pacs.txt", std::ios::out);
    const int nValues = m_energyMesh->get_q();
    if (nValues > 0) {
      for (int i = 0; i < nValues; ++i) {
        double e = m_energyMesh->get_e(i);
        pacsfile << 1.e6 * e << "  ";
        for (int j = 0; j < nComponents; ++j) {
          pacsfile << molPacs[j]->get_ACS(e) << "  "
                   << molPacs[j]->get_ICS(e) << "  ";
        }
        pacsfile << "\n";
      }
    }
    pacsfile.close();
  }

  const std::string gasname = FindUnusedMaterialName(medium->GetName());
  m_gas.reset(new Heed::GasDef(gasname, gasname, nComponents, notations, 
                               fractions, pressure, temperature, -1.));

  const double w = std::max(medium->GetW() * 1.e-6, 0.);
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  m_matter.reset(new Heed::HeedMatterDef(m_energyMesh.get(), m_gas.get(), 
                                         molPacs, w, f));

  return true;
}

bool TrackHeed::SetupMaterial(Medium* medium) {

  // Get temperature and density.
  double temperature = medium->GetTemperature();
  const double density = medium->GetMassDensity() * Heed::CLHEP::gram / 
                         Heed::CLHEP::cm3;

  const int nComponents = medium->GetNumberOfComponents();
  std::vector<Heed::AtomPhotoAbsCS*> atPacs(nComponents, nullptr);

  std::vector<std::string> notations;
  std::vector<double> fractions;
  for (int i = 0; i < nComponents; ++i) {
    std::string materialName;
    double frac;
    medium->GetComponent(i, materialName, frac);
    if (materialName == "C")
      atPacs[i] = &Heed::Carbon_PACS;
    else if (materialName == "Si")
      atPacs[i] = &Heed::Silicon_crystal_PACS;
    // else if (materialName == "Si") atPacs[i] = &Heed::Silicon_G4_PACS;
    else if (materialName == "Ga")
      atPacs[i] = &Heed::Gallium_PACS;
    else if (materialName == "Ge")
      atPacs[i] = &Heed::Germanium_PACS;
    else if (materialName == "As")
      atPacs[i] = &Heed::Arsenic_PACS;
    else if (materialName == "Cd")
      atPacs[i] = &Heed::Cadmium_PACS;
    else if (materialName == "Te")
      atPacs[i] = &Heed::Tellurium_PACS;
    else {
      std::cerr << m_className << "::SetupMaterial:\n";
      std::cerr << "    Photoabsorption cross-section data for " << materialName
                << " are not implemented.\n";
      return false;
    }
    notations.push_back(materialName);
    fractions.push_back(frac);
  }
  if (m_usePacsOutput) {
    std::ofstream pacsfile;
    pacsfile.open("heed_pacs.txt", std::ios::out);
    const int nValues = m_energyMesh->get_q();
    if (nValues > 0) {
      for (int i = 0; i < nValues; ++i) {
        double e = m_energyMesh->get_e(i);
        pacsfile << 1.e6 * e << "  ";
        for (int j = 0; j < nComponents; ++j) {
          pacsfile << atPacs[j]->get_ACS(e) << "  " << atPacs[j]->get_ICS(e)
                   << "  ";
        }
        pacsfile << "\n";
      }
    }
    pacsfile.close();
  }
  const std::string materialName = FindUnusedMaterialName(medium->GetName());
  m_material.reset(new Heed::MatterDef(materialName, materialName, nComponents,
                                       notations, fractions, density, 
                                       temperature));

  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  m_matter.reset(new Heed::HeedMatterDef(m_energyMesh.get(), m_material.get(), atPacs, w, f));

  return true;
}

bool TrackHeed::SetupDelta(const std::string& databasePath) {

  // Load elastic scattering data.
  std::string filename = databasePath + "cbdel.dat";
  m_elScat.reset(new Heed::ElElasticScat(filename));

  filename = databasePath + "elastic_disp.dat";
  m_lowSigma.reset(new Heed::ElElasticScatLowSigma(m_elScat.get(), filename));

  // Load data for calculation of ionization.
  // Get W value and Fano factor.
  const double w = m_matter->W * 1.e6;
  const double f = m_matter->F;
  filename = databasePath + "delta_path.dat";
  m_pairProd.reset(new Heed::PairProd(filename, w, f));

  m_deltaCs.reset(new Heed::HeedDeltaElectronCS(m_matter.get(), m_elScat.get(), m_lowSigma.get(), m_pairProd.get()));
  return true;
}

double TrackHeed::GetW() const { return m_matter->W * 1.e6; }
double TrackHeed::GetFanoFactor() const { return m_matter->F; }

std::string TrackHeed::FindUnusedMaterialName(const std::string& namein) {

  std::string nameout = namein;
  unsigned int counter = 0;
  while (Heed::MatterDef::get_MatterDef(nameout)) {
    nameout = namein + "_" + std::to_string(counter);
    ++counter;
  }
  return nameout;

}

void TrackHeed::ClearParticleBank() {

  Heed::last_particle_number = 0;
  ClearBank(m_particleBank);
  m_bankIterator = m_particleBank.end();
}

bool TrackHeed::IsInside(const double x, const double y, const double z) {

  // Check if the point is inside the drift area.
  if (!m_sensor->IsInArea(x, y, z)) return false;
  // Check if the point is inside a medium.
  Medium* medium = nullptr;
  if (!m_sensor->GetMedium(x, y, z, medium)) return false;
  // Make sure the medium has not changed.
  if (medium->GetName() != m_mediumName ||
      fabs(medium->GetMassDensity() - m_mediumDensity) > 1.e-9 ||
      !medium->IsIonisable()) {
    return false;
  }
  return true;
}

bool TrackHeed::UpdateBoundingBox(bool& update) {

  // Get the bounding box.
  double xmin = 0., ymin = 0., zmin = 0.;
  double xmax = 0., ymax = 0., zmax = 0.;
  if (!m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::UpdateBoundingBox: Drift area is not set.\n";
    m_ready = false;
    return false;
  }
  // Check if the bounding box has changed.
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (m_debug) {
    std::cout << m_className << "::UpdateBoundingBox:\n"
              << "    Bounding box dimensions:\n"
              << "      x: " << lx << " cm\n"
              << "      y: " << ly << " cm\n"
              << "      z: " << lz << " cm\n";
  }
  if (fabs(lx - m_lX) > Small || fabs(ly - m_lY) > Small || 
      fabs(lz - m_lZ) > Small) {
    m_lX = lx;
    m_lY = ly;
    m_lZ = lz;
    m_isChanged = true;
    update = true;
    m_hasActiveTrack = false;
  }
  // Update the center of the bounding box.
  m_cX = (std::isinf(xmin) || std::isinf(xmax)) ? 0. : 0.5 * (xmin + xmax);
  m_cY = (std::isinf(ymin) || std::isinf(ymax)) ? 0. : 0.5 * (ymin + ymax);
  m_cZ = (std::isinf(zmin) || std::isinf(zmax)) ? 0. : 0.5 * (zmin + zmax);
  if (m_debug) {
    std::cout << m_className << "::UpdateBoundingBox:\n"
              << "    Center of bounding box:\n"
              << "      x: " << m_cX << " cm\n"
              << "      y: " << m_cY << " cm\n"
              << "      z: " << m_cZ << " cm\n";
  }

  m_fieldMap.SetSensor(m_sensor);
  m_fieldMap.SetCentre(m_cX, m_cY, m_cZ);

  return true;
}

}
