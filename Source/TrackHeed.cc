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

// Global functions and variables required by Heed
namespace Heed {

// Particle id number for book-keeping
long last_particle_number;

}

extern trajestep_limit Heed::gtrajlim;
trajestep_limit Heed::gtrajlim(100. * Heed::cm, 1000. * Heed::cm, 0.1 * Heed::rad,
                               0.2 * Heed::rad);

// Actual class implementation

namespace Garfield {

TrackHeed::TrackHeed()
    : m_ready(false),
      m_hasActiveTrack(false),
      m_mediumDensity(-1.),
      m_mediumName(""),
      m_usePhotonReabsorption(true),
      m_usePacsOutput(false),
      m_useDelta(true),
      m_particle(NULL),
      m_matter(NULL),
      m_gas(NULL),
      m_material(NULL),
      m_atPacs(NULL),
      m_molPacs(NULL),
      m_emin(2.e-6),
      m_emax(2.e-1),
      m_nEnergyIntervals(200),
      m_energyMesh(NULL),
      m_transferCs(NULL),
      m_elScat(NULL),
      m_lowSigma(NULL),
      m_pairProd(NULL),
      m_deltaCs(NULL),
      m_chamber(NULL),
      m_lX(0.),
      m_lY(0.),
      m_lZ(0.),
      m_cX(0.),
      m_cY(0.),
      m_cZ(0.) {

  m_className = "TrackHeed";
}

TrackHeed::~TrackHeed() {

  if (m_particle) delete m_particle;
  if (m_matter) delete m_matter;
  if (m_gas) delete m_gas;
  if (m_material) delete m_material;
  if (m_atPacs) delete m_atPacs;
  if (m_molPacs) delete m_molPacs;
  if (m_energyMesh) delete m_energyMesh;
  if (m_transferCs) delete m_transferCs;
  if (m_elScat) delete m_elScat;
  if (m_lowSigma) delete m_lowSigma;
  if (m_pairProd) delete m_pairProd;
  if (m_deltaCs) delete m_deltaCs;
  if (m_chamber) delete m_chamber;
}

bool TrackHeed::NewTrack(const double x0, const double y0, const double z0,
                         const double t0, const double dx0, const double dy0,
                         const double dz0) {

  m_hasActiveTrack = false;
  m_ready = false;

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Sensor is not defined.\n";
    return false;
  }

  // Get the bounding box.
  double xmin = 0., ymin = 0., zmin = 0.;
  double xmax = 0., ymax = 0., zmax = 0.;
  if (!m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Drift area is not set.\n";
    return false;
  }
  // Check if the bounding box has changed.
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (m_debug) {
    std::cout << m_className << "::NewTrack:\n"
              << "    Bounding box dimensions:\n"
              << "      x: " << lx << " cm\n"
              << "      y: " << ly << " cm\n"
              << "      z: " << lz << " cm\n";
  }
  if (fabs(lx - m_lX) > Small || fabs(ly - m_lY) > Small || fabs(lz - m_lZ) > Small) {
    m_lX = lx;
    m_lY = ly;
    m_lZ = lz;
    m_isChanged = true;
  }
  // Update the center of the bounding box.
  if (std::isinf(xmin) || std::isinf(xmax)) {
    m_cX = 0.;
  } else {
    m_cX = 0.5 * (xmin + xmax);
  }
  if (std::isinf(ymin) || std::isinf(ymax)) {
    m_cY = 0.;
  } else {
    m_cY = 0.5 * (ymin + ymax);
  }
  if (std::isinf(zmin) || std::isinf(zmax)) {
    m_cZ = 0.;
  } else {
    m_cZ = 0.5 * (zmin + zmax);
  }
  if (m_debug) {
    std::cout << m_className << "::NewTrack:\n"
              << "    Center of bounding box:\n"
              << "      x: " << m_cX << " cm\n"
              << "      y: " << m_cY << " cm\n"
              << "      z: " << m_cZ << " cm\n";
  }

  m_fieldMap.SetSensor(m_sensor);

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = NULL;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
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

  std::list<ActivePtr<Heed::gparticle> >::iterator it = m_particleBank.begin();
  for (; it != m_particleBank.end(); ++it) (*it).clear();
  m_particleBank.clear();
  m_deltaElectrons.clear();
  m_chamber->conduction_electron_bank.reserve(1000);
  m_chamber->conduction_electron_bank.clear();

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
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  vec velocity(dx, dy, dz);
  velocity = velocity * Heed::c_light * GetBeta();

  if (m_debug) {
    std::cout << m_className << "::NewTrack:\n"
              << "    Track starts at (" << x0 << ", " << y0 << ", " << z0
              << ") at time " << t0 << "\n"
              << "    Initial direction: (" << dx << ", " << dy << ", " << dz
              << ")\n";
  }

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);
  // Setup the particle.
  if (m_particle) {
    delete m_particle;
    m_particle = NULL;
  }

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

  m_particle = new Heed::HeedParticle(m_chamber, p0, velocity, t0,
                                      particleType, m_particleBank, &m_fieldMap);
  // Transport the particle.
  m_particle->fly();
  m_hasActiveTrack = true;
  m_ready = true;

  // Plot the new track.
  if (m_usePlotting) PlotNewTrack(x0, y0, z0);
  return true;
}

double TrackHeed::GetClusterDensity() {

  if (!m_ready) {
    std::cerr << m_className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (!m_transferCs) {
    std::cerr << m_className << "::GetClusterDensity:\n";
    std::cerr << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return m_transferCs->quanC;
}

double TrackHeed::GetStoppingPower() {

  if (!m_ready) {
    std::cerr << m_className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (!m_transferCs) {
    std::cerr << m_className << "::GetStoppingPower:\n";
    std::cerr << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return m_transferCs->meanC1 * 1.e6;
}

bool TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls,
                           double& tcls, int& n, double& e, double& extra) {

  // Initial settings.
  xcls = ycls = zcls = tcls = 0.;
  extra = 0.;
  n = 0;
  e = 0.;

  // Make sure NewTrack has successfully been called.
  if (!m_ready) {
    std::cerr << m_className << "::GetCluster:\n";
    std::cerr << "    Track has not been initialized.\n";
    std::cerr << "    Call NewTrack first.\n";
    return false;
  }

  if (!m_hasActiveTrack) {
    std::cerr << m_className << "::GetCluster:\n";
    std::cerr << "    There are no more clusters.\n";
    return false;
  }

  bool ok = false;
  Medium* medium = NULL;
  // Get the first element from the particle bank.
  std::list<ActivePtr<Heed::gparticle> >::iterator it = m_particleBank.begin();
  Heed::HeedPhoton* virtualPhoton = NULL;
  while (!ok) {
    if (it == m_particleBank.end()) {
      m_hasActiveTrack = false;
      return false;
    }

    // Convert the particle to a (virtual) photon.
    virtualPhoton = dynamic_cast<Heed::HeedPhoton*>((*it).get());
    if (!virtualPhoton) {
      std::cerr << m_className << "::GetCluster:\n";
      std::cerr << "    Particle is not a virtual photon.\n";
      std::cerr << "    Program bug!\n";
      // Try the next element.
      (*it).clear();
      it = m_particleBank.erase(it);
      continue;
    }

    // Get the location of the interaction (convert from mm to cm
    // and shift with respect to bounding box center).
    xcls = virtualPhoton->currpos.pt.v.x * 0.1 + m_cX;
    ycls = virtualPhoton->currpos.pt.v.y * 0.1 + m_cY;
    zcls = virtualPhoton->currpos.pt.v.z * 0.1 + m_cZ;
    tcls = virtualPhoton->currpos.time;
    // Make sure the cluster is inside the drift area.
    if (!m_sensor->IsInArea(xcls, ycls, zcls)) {
      // Delete this virtual photon and proceed with the next one.
      (*it).clear();
      it = m_particleBank.erase(it);
      continue;
    }
    // Make sure the cluster is inside a medium.
    if (!m_sensor->GetMedium(xcls, ycls, zcls, medium)) {
      // Delete this virtual photon and proceed with the next one.
      (*it).clear();
      it = m_particleBank.erase(it);
      continue;
    }
    // Make sure the medium has not changed.
    if (medium->GetName() != m_mediumName ||
        fabs(medium->GetMassDensity() - m_mediumDensity) > 1.e-9 ||
        !medium->IsIonisable()) {
      // Delete this virtual photon and proceed with the next one.
      (*it).clear();
      it = m_particleBank.erase(it);
      continue;
    }
    // Seems to be ok.
    ok = true;
  }

  // Plot the cluster, if requested.
  if (m_usePlotting) PlotCluster(xcls, ycls, zcls);

  // Transport the virtual photon.
  virtualPhoton->fly();
  // Get the transferred energy (convert from MeV to eV).
  e = virtualPhoton->energy * 1.e6;

  // Make a list of parent particle id numbers.
  std::vector<int> ids;
  // At the beginning, there is only the virtual photon.
  ids.push_back(virtualPhoton->particle_number);
  int nIds = 1;

  // Look for daughter particles.
  m_deltaElectrons.clear();
  m_chamber->conduction_electron_bank.clear();
  bool deleteNode = false;
  Heed::HeedDeltaElectron* delta = NULL;
  Heed::HeedPhoton* photon = NULL;
  it = m_particleBank.erase(it);
  // Loop over the particle bank.
  while (it != m_particleBank.end()) {
    deleteNode = false;
    // Check if it is a delta electron.
    delta = dynamic_cast<Heed::HeedDeltaElectron*>((*it).get());
    if (delta) {
      // Check if the delta electron was produced by one of the photons
      // belonging to this cluster.
      for (int i = nIds; i--;) {
        if (delta->parent_particle_number == ids[i]) {
          if (m_useDelta) {
            // Transport the delta electron.
            delta->fly();
          } else {
            // Add the delta electron to the list, for later use.
            deltaElectron newDeltaElectron;
            newDeltaElectron.x = delta->currpos.pt.v.x * 0.1 + m_cX;
            newDeltaElectron.y = delta->currpos.pt.v.y * 0.1 + m_cY;
            newDeltaElectron.z = delta->currpos.pt.v.z * 0.1 + m_cZ;
            newDeltaElectron.t = delta->currpos.time;
            newDeltaElectron.e = delta->curr_kin_energy * 1.e6;
            newDeltaElectron.dx = delta->currpos.dir.x;
            newDeltaElectron.dy = delta->currpos.dir.y;
            newDeltaElectron.dz = delta->currpos.dir.z;
            m_deltaElectrons.push_back(newDeltaElectron);
          }
          deleteNode = true;
          break;
        }
      }
    } else {
      // Check if it is a real photon.
      photon = dynamic_cast<Heed::HeedPhoton*>((*it).get());
      if (!photon) {
        std::cerr << m_className << "::GetCluster:\n";
        std::cerr << "    Particle is neither an electron nor a photon.\n";
        return false;
      }
      for (int i = nIds; i--;) {
        if (photon->parent_particle_number == ids[i]) {
          // Transport the photon and add its number to the list of ids.
          if (m_usePhotonReabsorption) photon->fly();
          deleteNode = true;
          ids.push_back(photon->particle_number);
          ++nIds;
          break;
        }
      }
    }
    // Proceed with the next node in the particle bank.
    if (deleteNode) {
      (*it).clear();
      it = m_particleBank.erase(it);
    } else {
      ++it;
    }
  }

  // Get the total number of electrons produced in this step.
  if (m_useDelta) {
    n = m_chamber->conduction_electron_bank.size();
  } else {
    n = m_deltaElectrons.size();
  }

  // Remove the virtual photon from the particle bank.
  if (it != m_particleBank.end()) {
    (*it).clear();
    m_particleBank.erase(it);
  }

  return true;
}

bool TrackHeed::GetElectron(const unsigned int i, 
                            double& x, double& y, double& z, double& t,
                            double& e, double& dx, double& dy, double& dz) {

  // Make sure NewTrack has successfully been called.
  if (!m_ready) {
    std::cerr << m_className << "::GetElectron:\n"
              << "    Track has not been initialized.\n"
              << "    Call NewTrack first.\n";
    return false;
  }

  if (m_useDelta) {
    // Make sure an electron with this number exists.
    const unsigned int n = m_chamber->conduction_electron_bank.size();
    if (i >= n) {
      std::cerr << m_className << "::GetElectron:\n"
                << "    Electron number out of range.\n";
      return false;
    }

    x = m_chamber->conduction_electron_bank[i].ptloc.v.x * 0.1 + m_cX;
    y = m_chamber->conduction_electron_bank[i].ptloc.v.y * 0.1 + m_cY;
    z = m_chamber->conduction_electron_bank[i].ptloc.v.z * 0.1 + m_cZ;
    t = m_chamber->conduction_electron_bank[i].time;
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

void TrackHeed::TransportDeltaElectron(const double x0, const double y0,
                                       const double z0, const double t0,
                                       const double e0, const double dx0,
                                       const double dy0, const double dz0,
                                       int& nel) {

  nel = 0;

  // Check if delta electron transport was disabled.
  if (!m_useDelta) {
    std::cerr << m_className << "::TransportDeltaElectron:\n";
    std::cerr << "    Delta electron transport has been switched off.\n";
    return;
  }

  // Make sure the kinetic energy is positive.
  if (e0 <= 0.) {
    std::cerr << m_className << "::TransportDeltaElectron:\n";
    std::cerr << "    Kinetic energy must be positive.\n";
    return;
  }

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportDeltaElectron:\n"
              << "    Sensor is not defined.\n";
    m_ready = false;
    return;
  }

  // Get the bounding box.
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::TransportDeltaElectron:\n"
              << "    Drift area is not set.\n";
    m_ready = false;
    return;
  }
  // Check if the bounding box has changed.
  bool update = false;
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (fabs(lx - m_lX) > Small || fabs(ly - m_lY) > Small || fabs(lz - m_lZ) > Small) {
    m_lX = lx;
    m_lY = ly;
    m_lZ = lz;
    m_isChanged = true;
    update = true;
    m_hasActiveTrack = false;
  }
  // Update the center of the bounding box.
  m_cX = 0.5 * (xmin + xmax);
  m_cY = 0.5 * (ymin + ymax);
  m_cZ = 0.5 * (zmin + zmax);

  m_fieldMap.SetSensor(m_sensor);

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = NULL;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportDeltaElectron:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportDeltaElectron:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
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
  m_chamber->conduction_electron_bank.clear();

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d <= 0.) {
    // Null vector. Sample the direction isotropically.
    const double phi = TwoPi * RndmUniform();
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  vec velocity(dx, dy, dz);

  // Calculate the speed for the given kinetic energy.
  const double gamma = 1. + e0 / ElectronMass;
  const double beta = sqrt(1. - 1. / (gamma * gamma));
  double speed = Heed::c_light * beta;
  velocity = velocity * speed;

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);

  // Transport the electron.
  Heed::HeedDeltaElectron delta(m_chamber, p0, velocity, t0, 0, &m_fieldMap);
  delta.fly();

  nel = m_chamber->conduction_electron_bank.size();
}

void TrackHeed::TransportPhoton(const double x0, const double y0,
                                const double z0, const double t0,
                                const double e0, const double dx0,
                                const double dy0, const double dz0, int& nel) {

  nel = 0;

  // Make sure the energy is positive.
  if (e0 <= 0.) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    Photon energy must be positive.\n";
    return;
  }

  // Make sure the sensor has been set.
  if (!m_sensor) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    Sensor is not defined.\n";
    m_ready = false;
    return;
  }

  // Get the bounding box.
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    Drift area is not set.\n";
    m_ready = false;
    return;
  }
  // Check if the bounding box has changed.
  bool update = false;
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (fabs(lx - m_lX) > Small || fabs(ly - m_lY) > Small || fabs(lz - m_lZ) > Small) {
    m_lX = lx;
    m_lY = ly;
    m_lZ = lz;
    m_isChanged = true;
    update = true;
    m_hasActiveTrack = false;
  }
  // Update the center of the bounding box.
  m_cX = 0.5 * (xmin + xmax);
  m_cY = 0.5 * (ymin + ymax);
  m_cZ = 0.5 * (zmin + zmax);

  m_fieldMap.SetSensor(m_sensor);

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium = NULL;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::TransportPhoton:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportPhoton:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
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
  Heed::last_particle_number = 0;
  std::list<ActivePtr<Heed::gparticle> >::iterator it = m_particleBank.begin();
  for (; it != m_particleBank.end(); ++it) (*it).clear();
  m_particleBank.clear();
  m_deltaElectrons.clear();
  m_chamber->conduction_electron_bank.clear();

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d <= 0.) {
    // Null vector. Sample the direction isotropically.
    const double phi = TwoPi * RndmUniform();
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
    // Normalise the direction vector.
    dx /= d;
    dy /= d;
    dz /= d;
  }
  vec velocity(dx, dy, dz);
  velocity = velocity * Heed::c_light;

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  point p0((x0 - m_cX) * 10., (y0 - m_cY) * 10., (z0 - m_cZ) * 10.);

  // Create and transport the photon.
  Heed::HeedPhoton photon(m_chamber, p0, velocity, t0, 0, e0 * 1.e-6, 
                          m_particleBank, &m_fieldMap);
  photon.fly();

  // Make a list of parent particle id numbers.
  std::vector<int> ids;
  // At the beginning, there is only the original photon.
  ids.push_back(photon.particle_number);
  int nIds = 1;

  // Look for daughter particles.
  Heed::HeedDeltaElectron* delta = NULL;
  Heed::HeedPhoton* fluorescencePhoton = NULL;

  // Get the first element from the particle bank.
  it = m_particleBank.begin();
  // Loop over the particle bank.
  while (it != m_particleBank.end()) {
    // Check if it is a delta electron.
    delta = dynamic_cast<Heed::HeedDeltaElectron*>((*it).get());
    if (delta) {
      // Check if the delta electron was produced by one of the photons
      // belonging to this cluster.
      bool gotParent = false;
      for (int i = nIds; i--;) {
        if (delta->parent_particle_number == ids[i]) {
          gotParent = true;
          if (m_useDelta) {
            // Transport the delta electron.
            delta->fly();
          } else {
            // Add the delta electron to the list, for later use.
            deltaElectron newDeltaElectron;
            newDeltaElectron.x = delta->currpos.pt.v.x * 0.1 + m_cX;
            newDeltaElectron.y = delta->currpos.pt.v.y * 0.1 + m_cY;
            newDeltaElectron.z = delta->currpos.pt.v.z * 0.1 + m_cZ;
            newDeltaElectron.t = delta->currpos.time;
            newDeltaElectron.e = delta->curr_kin_energy * 1.e6;
            newDeltaElectron.dx = delta->currpos.dir.x;
            newDeltaElectron.dy = delta->currpos.dir.y;
            newDeltaElectron.dz = delta->currpos.dir.z;
            m_deltaElectrons.push_back(newDeltaElectron);
          }
          break;
        }
      }
      if (!gotParent) {
        std::cerr << m_className << "::TransportPhoton:\n";
        std::cerr << "    Delta electron with unknown parent.\n";
      }
    } else {
      // Check if it is a fluorescence photon.
      fluorescencePhoton = dynamic_cast<Heed::HeedPhoton*>((*it).get());
      if (!fluorescencePhoton) {
        std::cerr << m_className << "::TransportPhoton:\n";
        std::cerr << "    Unknown secondary particle.\n";
        return;
      }
      for (int i = nIds; i--;) {
        if (fluorescencePhoton->parent_particle_number == ids[i]) {
          // Transport the photon and add its number to the list of ids.
          fluorescencePhoton->fly();
          ids.push_back(fluorescencePhoton->particle_number);
          ++nIds;
          break;
        }
      }
    }
    // Proceed with the next element in the particle bank.
    (*it).clear();
    it = m_particleBank.erase(it);
  }

  // Get the total number of electrons produced in this step.
  if (m_useDelta) {
    nel = m_chamber->conduction_electron_bank.size();
  } else {
    nel = m_deltaElectrons.size();
  }
}

void TrackHeed::EnableElectricField() { m_fieldMap.UseEfield(true); }
void TrackHeed::DisableElectricField() { m_fieldMap.UseEfield(false); }
void TrackHeed::EnableMagneticField() { m_fieldMap.UseBfield(true); }
void TrackHeed::DisableMagneticField() { m_fieldMap.UseBfield(false); }

void TrackHeed::SetEnergyMesh(const double e0, const double e1,
                              const int nsteps) {

  if (fabs(e1 - e0) < Small) {
    std::cerr << m_className << "::SetEnergyMesh:\n";
    std::cerr << "    Invalid energy range:\n";
    std::cerr << "    " << e0 << " < E [eV] < " << e1 << "\n";
    return;
  }

  if (nsteps <= 0) {
    std::cerr << m_className << "::SetEnergyMesh:\n";
    std::cerr << "    Number of intervals must be > 0.\n";
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
  char* dbPath = getenv("HEED_DATABASE");
  if (dbPath == 0) {
    std::cerr << m_className << "::Setup:\n";
    std::cerr << "    Database path is not defined.\n";
    std::cerr << "    Environment variable HEED_DATABASE is not set.\n";
    std::cerr << "    Cannot proceed with initialization.\n";
    return false;
  }

  std::string databasePath = dbPath;
  if (databasePath[databasePath.size() - 1] != '/') {
    databasePath.append("/");
  }

  // Check once more that the medium exists.
  if (!medium) {
    std::cerr << m_className << "::Setup:\n"
              << "    Medium pointer is null.\n";
    return false;
  }

  // Setup the energy mesh.
  if (m_energyMesh) {
    delete m_energyMesh;
    m_energyMesh = NULL;
  }
  m_energyMesh = new Heed::EnergyMesh(m_emin, m_emax, m_nEnergyIntervals);

  if (medium->IsGas()) {
    if (!SetupGas(medium)) return false;
  } else {
    if (!SetupMaterial(medium)) return false;
  }

  // Energy transfer cross-section
  // Set a flag indicating whether the primary particle is an electron.
  const int sel = m_isElectron ? 1 : 0;
  const double gamma = GetGamma();

  if (m_transferCs != 0) {
    delete m_transferCs;
    m_transferCs = 0;
  }
  m_transferCs =
      new Heed::EnTransfCS(m_mass / 1.e6, gamma - 1, sel, m_matter, long(m_q));

  if (!SetupDelta(databasePath)) return false;

  if (m_debug) {
    const double nc = m_transferCs->quanC;
    const double dedx = m_transferCs->meanC * 1.e3;
    const double dedxLeft = m_transferCs->meanCleft * 1.e3;
    const double dedx1 = m_transferCs->meanC1 * 1.e3;
    const double w = m_matter->W * 1.e6;
    const double f = m_matter->F;
    const double minI = m_matter->min_ioniz_pot * 1.e6;
    std::cout << m_className << "::Setup:\n";
    std::cout << "    Cluster density:             " << nc << " cm-1\n";
    std::cout << "    Stopping power (restricted): " << dedxLeft << " - "
              << dedx << " keV/cm\n";
    std::cout << "    Stopping power (incl. tail): " << dedx1 << " keV/cm\n";
    std::cout << "    W value:                     " << w << " eV\n";
    std::cout << "    Fano factor:                 " << f << "\n";
    std::cout << "    Min. ionization potential:   " << minI << " eV\n";
  }

  fixsyscoor primSys(point(0., 0., 0.), basis("primary"), "primary");
  if (m_chamber) {
    delete m_chamber;
    m_chamber = NULL;
  }
  m_chamber = new HeedChamber(primSys, m_lX, m_lY, m_lZ, m_transferCs, m_deltaCs);
  m_fieldMap.SetSensor(m_sensor);
  return true;
}

bool TrackHeed::SetupGas(Medium* medium) {

  // Get temperature and pressure.
  double pressure = medium->GetPressure();
  pressure = (pressure / AtmosphericPressure) * Heed::atmosphere;
  double temperature = medium->GetTemperature();

  const int nComponents = medium->GetNumberOfComponents();
  if (nComponents < 1) {
    std::cerr << m_className << "::SetupGas:\n";
    std::cerr << "    Gas " << medium->GetName() << " has zero constituents.\n";
    return false;
  }

  if (m_molPacs != 0) {
    delete m_molPacs;
    m_molPacs = 0;
  }
  m_molPacs = new Heed::MolecPhotoAbsCS* [nComponents];
  DynLinArr<std::string> notations;
  notations.clear();
  DynLinArr<double> fractions;
  fractions.clear();

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
      m_molPacs[i] = &Heed::CF4_MPACS;
    else if (gasname == "Ar")
      m_molPacs[i] = &Heed::Ar_MPACS;
    else if (gasname == "He")
      m_molPacs[i] = &Heed::He_MPACS;
    else if (gasname == "Ne")
      m_molPacs[i] = &Heed::Ne_MPACS;
    else if (gasname == "Kr")
      m_molPacs[i] = &Heed::Kr_MPACS;
    else if (gasname == "Xe")
      m_molPacs[i] = &Heed::Xe_MPACS;
    else if (gasname == "CH4")
      m_molPacs[i] = &Heed::CH4_MPACS;
    else if (gasname == "C2H6")
      m_molPacs[i] = &Heed::C2H6_MPACS;
    else if (gasname == "C3H8")
      m_molPacs[i] = &Heed::C3H8_MPACS;
    else if (gasname == "C4H10")
      m_molPacs[i] = &Heed::C4H10_MPACS;
    else if (gasname == "CO2")
      m_molPacs[i] = &Heed::CO2_MPACS;
    else if (gasname == "C5H12")
      m_molPacs[i] = &Heed::C5H12_MPACS;
    else if (gasname == "Water")
      m_molPacs[i] = &Heed::H2O_MPACS;
    else if (gasname == "O2")
      m_molPacs[i] = &Heed::O2_MPACS;
    else if (gasname == "N2" || gasname == "N2 (Phelps)")
      m_molPacs[i] = &Heed::N2_MPACS;
    else if (gasname == "NO")
      m_molPacs[i] = &Heed::NO_MPACS;
    else if (gasname == "N2O")
      m_molPacs[i] = &Heed::N2O_MPACS;
    else if (gasname == "C2H4")
      m_molPacs[i] = &Heed::C2H4_MPACS;
    else if (gasname == "C2H2")
      m_molPacs[i] = &Heed::C2H2_MPACS;
    else if (gasname == "H2")
      m_molPacs[i] = &Heed::H2_MPACS;
    else if (gasname == "CO")
      m_molPacs[i] = &Heed::CO_MPACS;
    else if (gasname == "Methylal")
      m_molPacs[i] = &Heed::Methylal_MPACS;
    else if (gasname == "DME")
      m_molPacs[i] = &Heed::DME_MPACS;
    else if (gasname == "C2F6")
      m_molPacs[i] = &Heed::C2F6_MPACS;
    else if (gasname == "SF6")
      m_molPacs[i] = &Heed::SF6_MPACS;
    else if (gasname == "NH3")
      m_molPacs[i] = &Heed::NH3_MPACS;
    else if (gasname == "C3H6")
      m_molPacs[i] = &Heed::C3H6_MPACS;
    else if (gasname == "CH3OH")
      m_molPacs[i] = &Heed::CH3OH_MPACS;
    else if (gasname == "C2H5OH")
      m_molPacs[i] = &Heed::C2H5OH_MPACS;
    else if (gasname == "C3H7OH")
      m_molPacs[i] = &Heed::C3H7OH_MPACS;
    else if (gasname == "Cs")
      m_molPacs[i] = &Heed::Cs_MPACS;
    else if (gasname == "F2")
      m_molPacs[i] = &Heed::F2_MPACS;
    else if (gasname == "CS2")
      m_molPacs[i] = &Heed::CS2_MPACS;
    else if (gasname == "COS")
      m_molPacs[i] = &Heed::COS_MPACS;
    else if (gasname == "CD4")
      m_molPacs[i] = &Heed::CH4_MPACS;
    else if (gasname == "BF3")
      m_molPacs[i] = &Heed::BF3_MPACS;
    else if (gasname == "C2HF5")
      m_molPacs[i] = &Heed::C2HF5_MPACS;
    else if (gasname == "C2H2F4")
      m_molPacs[i] = &Heed::C2H2F4_MPACS;
    else if (gasname == "CHF3")
      m_molPacs[i] = &Heed::CHF3_MPACS;
    else if (gasname == "CF3Br")
      m_molPacs[i] = &Heed::CF3Br_MPACS;
    else if (gasname == "C3F8")
      m_molPacs[i] = &Heed::C3F8_MPACS;
    else if (gasname == "O3")
      m_molPacs[i] = &Heed::O3_MPACS;
    else if (gasname == "Hg")
      m_molPacs[i] = &Heed::Hg_MPACS;
    else if (gasname == "H2S")
      m_molPacs[i] = &Heed::H2S_MPACS;
    else if (gasname == "GeH4")
      m_molPacs[i] = &Heed::GeH4_MPACS;
    else if (gasname == "SiH4")
      m_molPacs[i] = &Heed::SiH4_MPACS;
    else {
      std::cerr << m_className << "::SetupGas:\n";
      std::cerr << "    Photoabsorption cross-section data for " << gasname
                << " are not available.\n";
      return false;
    }
    notations.increment(gasname);
    fractions.increment(frac);
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
          pacsfile << m_molPacs[j]->get_ACS(e) << "  "
                   << m_molPacs[j]->get_ICS(e) << "  ";
        }
        pacsfile << "\n";
      }
    }
    pacsfile.close();
  }

  std::string gasname = medium->GetName();
  if (m_gas != 0) {
    delete m_gas;
    m_gas = 0;
  }

  m_gas = new Heed::GasDef(gasname, gasname, nComponents, notations, fractions,
                         pressure, temperature, -1.);

  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  if (m_matter != 0) {
    delete m_matter;
    m_matter = 0;
  }
  m_matter = new Heed::HeedMatterDef(m_energyMesh, m_gas, m_molPacs, w, f);

  return true;
}

bool TrackHeed::SetupMaterial(Medium* medium) {

  // Get temperature and density.
  double temperature = medium->GetTemperature();
  double density = medium->GetMassDensity() * Heed::g / Heed::cm3;

  const int nComponents = medium->GetNumberOfComponents();
  if (m_atPacs != 0) {
    delete m_atPacs;
    m_atPacs = 0;
  }
  m_atPacs = new Heed::AtomPhotoAbsCS* [nComponents];

  DynLinArr<std::string> notations;
  notations.clear();
  DynLinArr<double> fractions;
  fractions.clear();
  for (int i = 0; i < nComponents; ++i) {
    std::string materialName;
    double frac;
    medium->GetComponent(i, materialName, frac);
    if (materialName == "C")
      m_atPacs[i] = &Heed::Carbon_PACS;
    else if (materialName == "Si")
      m_atPacs[i] = &Heed::Silicon_crystal_PACS;
    // else if (materialName == "Si") m_atPacs[i] = &Heed::Silicon_G4_PACS;
    else if (materialName == "Ga")
      m_atPacs[i] = &Heed::Gallium_PACS;
    else if (materialName == "Ge")
      m_atPacs[i] = &Heed::Germanium_PACS;
    else if (materialName == "As")
      m_atPacs[i] = &Heed::Arsenic_PACS;
    else if (materialName == "Cd")
      m_atPacs[i] = &Heed::Cadmium_PACS;
    else if (materialName == "Te")
      m_atPacs[i] = &Heed::Tellurium_PACS;
    else {
      std::cerr << m_className << "::SetupMaterial:\n";
      std::cerr << "    Photoabsorption cross-section data for " << materialName
                << " are not implemented.\n";
      return false;
    }
    notations.increment(materialName);
    fractions.increment(frac);
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
          pacsfile << m_atPacs[j]->get_ACS(e) << "  " << m_atPacs[j]->get_ICS(e)
                   << "  ";
        }
        pacsfile << "\n";
      }
    }
    pacsfile.close();
  }
  if (m_material != 0) {
    delete m_material;
    m_material = 0;
  }
  std::string materialName = medium->GetName();
  m_material = new Heed::MatterDef(materialName, materialName, nComponents,
                                 notations, fractions, density, temperature);

  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  if (m_matter != 0) {
    delete m_matter;
    m_matter = 0;
  }
  m_matter = new Heed::HeedMatterDef(m_energyMesh, m_material, m_atPacs, w, f);

  return true;
}

bool TrackHeed::SetupDelta(const std::string& databasePath) {

  // Load elastic scattering data.
  std::string filename = databasePath + "cbdel.dat";
  if (m_elScat) {
    delete m_elScat;
    m_elScat = NULL;
  }
  m_elScat = new Heed::ElElasticScat(filename);

  filename = databasePath + "elastic_disp.dat";
  if (m_lowSigma != 0) {
    delete m_lowSigma;
    m_lowSigma = 0;
  }
  m_lowSigma = new Heed::ElElasticScatLowSigma(m_elScat, filename);

  // Load data for calculation of ionization.
  // Get W value and Fano factor.
  const double w = m_matter->W * 1.e6;
  const double f = m_matter->F;
  filename = databasePath + "delta_path.dat";
  if (m_pairProd != 0) {
    delete m_pairProd;
    m_pairProd = 0;
  }
  m_pairProd = new Heed::PairProd(filename, w, f);

  if (m_deltaCs != 0) {
    delete m_deltaCs;
    m_deltaCs = 0;
  }
  m_deltaCs = new Heed::HeedDeltaElectronCS(m_matter, m_elScat, m_lowSigma, m_pairProd);
  return true;
}

double TrackHeed::GetW() const { return m_matter->W * 1.e6; }
double TrackHeed::GetFanoFactor() const { return m_matter->F; }

}
