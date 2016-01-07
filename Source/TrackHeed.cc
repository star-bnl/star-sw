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

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "HeedChamber.hh"
#include "TrackHeed.hh"

namespace Garfield {

namespace HeedInterface {

Sensor* sensor;
Medium* medium;

bool useEfield;
bool useBfield;
}
}

// Global functions and variables required by Heed
namespace Heed {

BlkArr<HeedCluster> cluster_bank;
AbsList<ActivePtr<gparticle> > particle_bank;

void field_map(const point& pt, vec& efield, vec& bfield, vfloat& mrange) {

  const double x = pt.v.x;
  const double y = pt.v.y;
  const double z = pt.v.z;

  // Initialise the electric and magnetic field.
  efield = vec(0., 0., 0.);
  bfield = vec(0., 0., 0.);
  mrange = DBL_MAX;

  if (Garfield::HeedInterface::sensor == 0) {
    std::cerr << "TrackHeedGlobals::field_map:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  // TODO: check correct dimensions of E and B fields
  if (Garfield::HeedInterface::useEfield) {
    double ex = 0., ey = 0., ez = 0.;
    int status = 0;
    Garfield::HeedInterface::sensor->ElectricField(
        x, y, z, ex, ey, ez, Garfield::HeedInterface::medium, status);
    efield.x = ex * 1.e-5;
    efield.y = ey * 1.e-5;
    efield.z = ez * 1.e-5;
  }

  if (Garfield::HeedInterface::useBfield) {
    double bx = 0., by = 0., bz = 0.;
    int status = 0;
    Garfield::HeedInterface::sensor->MagneticField(x, y, z, bx, by, bz, status);
    bfield.x = bx * 1.e-3;
    bfield.y = by * 1.e-3;
    bfield.z = bz * 1.e-3;
  }
}
}

// This function is called by Heed after each step
void check_point(gparticle* /*gp*/) {}

extern trajestep_limit gtrajlim;
trajestep_limit gtrajlim(100. * Heed::cm, 1000. * Heed::cm, 0.1 * Heed::rad,
                         0.2 * Heed::rad);

// Actual class implementation

namespace Garfield {

TrackHeed::TrackHeed()
    : ready(false),
      hasActiveTrack(false),
      mediumDensity(-1.),
      mediumName(""),
      usePhotonReabsorption(true),
      usePacsOutput(false),
      useDelta(true),
      nDeltas(0),
      particle(0),
      matter(0),
      gas(0),
      material(0),
      m_atPacs(0),
      m_molPacs(0),
      emin(2.e-6),
      emax(2.e-1),
      nEnergyIntervals(200),
      energyMesh(0),
      transferCs(0),
      elScat(0),
      lowSigma(0),
      pairProd(0),
      deltaCs(0),
      chamber(0),
      lX(0.),
      lY(0.),
      lZ(0.),
      cX(0.),
      cY(0.),
      cZ(0.) {

  className = "TrackHeed";

  HeedInterface::sensor = 0;
  HeedInterface::useEfield = false;
  HeedInterface::useBfield = false;

  deltaElectrons.clear();
}

TrackHeed::~TrackHeed() {

  if (particle != 0) delete particle;
  if (matter != 0) delete matter;
  if (gas != 0) delete gas;
  if (material != 0) delete material;
  if (m_atPacs != 0) delete m_atPacs;
  if (m_molPacs != 0) delete m_molPacs;
  if (energyMesh != 0) delete energyMesh;
  if (transferCs != 0) delete transferCs;
  if (elScat != 0) delete elScat;
  if (lowSigma != 0) delete lowSigma;
  if (pairProd != 0) delete pairProd;
  if (deltaCs != 0) delete deltaCs;
  if (chamber != 0) delete chamber;

  Garfield::HeedInterface::sensor = 0;
}

bool TrackHeed::NewTrack(const double x0, const double y0, const double z0,
                         const double t0, const double dx0, const double dy0,
                         const double dz0) {

  hasActiveTrack = false;
  ready = false;

  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  // Get the bounding box.
  double xmin = 0., ymin = 0., zmin = 0.;
  double xmax = 0., ymax = 0., zmax = 0.;
  if (!sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Drift area is not set.\n";
    return false;
  }
  // Check if the bounding box has changed.
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (debug) {
    std::cout << className << "::NewTrack:\n";
    std::cout << "    Bounding box dimensions:\n";
    std::cout << "      x: " << lx << " cm\n";
    std::cout << "      y: " << ly << " cm\n";
    std::cout << "      z: " << lz << " cm\n";
  }
  if (fabs(lx - lX) > Small || fabs(ly - lY) > Small || fabs(lz - lZ) > Small) {
    lX = lx;
    lY = ly;
    lZ = lz;
    isChanged = true;
  }
  // Update the center of the bounding box.
  if (std::isinf(xmin) || std::isinf(xmax)) {
    cX = 0.;
  } else {
    cX = 0.5 * (xmin + xmax);
  }
  if (std::isinf(ymin) || std::isinf(ymax)) {
    cY = 0.;
  } else {
    cY = 0.5 * (ymin + ymax);
  }
  if (std::isinf(zmin) || std::isinf(zmax)) {
    cZ = 0.;
  } else {
    cZ = 0.5 * (zmin + zmax);
  }
  if (debug) {
    std::cout << className << "::NewTrack:\n";
    std::cout << "    Center of bounding box:\n";
    std::cout << "      x: " << cX << " cm\n";
    std::cout << "      y: " << cY << " cm\n";
    std::cout << "      z: " << cZ << " cm\n";
  }

  HeedInterface::sensor = sensor;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    return false;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != mediumName ||
      fabs(medium->GetMassDensity() - mediumDensity) > 1.e-9) {
    isChanged = true;
  }

  // If medium, particle or bounding box have changed,
  // update the cross-sections.
  if (isChanged) {
    if (!Setup(medium)) return false;
    isChanged = false;
    mediumName = medium->GetName();
    mediumDensity = medium->GetMassDensity();
  }

  Heed::particle_bank.clear();
  deltaElectrons.clear();
  Heed::cluster_bank.allocate_block(100);
  chamber->conduction_electron_bank.allocate_block(1000);

  // Check the direction vector.
  double dx = dx0, dy = dy0, dz = dz0;
  const double d = sqrt(dx * dx + dy * dy + dz * dz);
  if (d < Small) {
    if (debug) {
      std::cout << className << "::NewTrack:\n";
      std::cout << "    Direction vector has zero norm.\n";
      std::cout << "    Initial direction is randomized.\n";
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

  if (debug) {
    std::cout << className << "::NewTrack:\n";
    std::cout << "    Track starts at (" << x0 << ", " << y0 << ", " << z0
              << ") at time " << t0 << "\n";
    std::cout << "    Initial direction: (" << dx << ", " << dy << ", " << dz
              << ")\n";
  }

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);
  // Setup the particle.
  if (particle != 0) {
    delete particle;
    particle = 0;
  }

  Heed::particle_def* particleType = &Heed::muon_minus_def;
  if (particleName == "e-") {
    particleType = &Heed::electron_def;
  } else if (particleName == "e+") {
    particleType = &Heed::positron_def;
  } else if (particleName == "mu-") {
    particleType = &Heed::muon_minus_def;
  } else if (particleName == "mu+") {
    particleType = &Heed::muon_plus_def;
  } else if (particleName == "pi-") {
    particleType = &Heed::pi_minus_meson_def;
  } else if (particleName == "pi+") {
    particleType = &Heed::pi_plus_meson_def;
  } else if (particleName == "K-") {
    particleType = &Heed::K_minus_meson_def;
  } else if (particleName == "K+") {
    particleType = &Heed::K_plus_meson_def;
  } else if (particleName == "p") {
    particleType = &Heed::proton_def;
  } else if (particleName == "pbar") {
    particleType = &Heed::anti_proton_def;
  } else if (particleName == "d") {
    particleType = &Heed::deuteron_def;
  } else if (particleName == "alpha") {
    particleType = &Heed::alpha_particle_def;
  } else if (particleName == "exotic") {
    // User defined particle
    Heed::user_particle_def.set_mass(mass * 1.e-6);
    Heed::user_particle_def.set_charge(q);
    particleType = &Heed::user_particle_def;
  } else {
    // Not a predefined particle, use muon definition.
    if (q > 0.) {
      particleType = &Heed::muon_minus_def;
    } else {
      particleType = &Heed::muon_plus_def;
    }
  }

  particle = new Heed::HeedParticle(chamber, p0, velocity, t0, particleType);
  // Transport the particle.
  particle->fly();
  hasActiveTrack = true;
  ready = true;

  // Plot the new track.
  if (usePlotting) PlotNewTrack(x0, y0, z0);

  return true;
}

double TrackHeed::GetClusterDensity() {

  if (!ready) {
    std::cerr << className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (transferCs == 0) {
    std::cerr << className << "::GetClusterDensity:\n";
    std::cerr << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return transferCs->quanC;
}

double TrackHeed::GetStoppingPower() {

  if (!ready) {
    std::cerr << className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (transferCs == 0) {
    std::cerr << className << "::GetStoppingPower:\n";
    std::cerr << "    Ionisation cross-section is not available.\n";
    return 0.;
  }

  return transferCs->meanC1 * 1.e6;
}

bool TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls,
                           double& tcls, int& n, double& e, double& extra) {

  // Initial settings.
  xcls = ycls = zcls = tcls = 0.;
  extra = 0.;
  n = 0;
  e = 0.;

  // Make sure NewTrack has successfully been called.
  if (!ready) {
    std::cerr << className << "::GetCluster:\n";
    std::cerr << "    Track has not been initialized.\n";
    std::cerr << "    Call NewTrack first.\n";
    return false;
  }

  if (!hasActiveTrack) {
    std::cerr << className << "::GetCluster:\n";
    std::cerr << "    There are no more clusters.\n";
    return false;
  }

  bool ok = false;
  Medium* medium = 0;
  AbsListNode<ActivePtr<gparticle> >* node = 0;
  Heed::HeedPhoton* virtualPhoton = 0;
  while (!ok) {
    // Get the first element from the particle bank.
    node = Heed::particle_bank.get_first_node();

    // Make sure the particle bank is not empty.
    if (node == 0) {
      hasActiveTrack = false;
      return false;
    }

    // Convert the particle to a (virtual) photon.
    virtualPhoton = dynamic_cast<Heed::HeedPhoton*>(node->el.get());
    if (virtualPhoton == 0) {
      std::cerr << className << "::GetCluster:\n";
      std::cerr << "    Particle is not a virtual photon.\n";
      std::cerr << "    Program bug!\n";
      // Delete the node.
      Heed::particle_bank.erase(node);
      // Try the next node.
      continue;
    }

    if (virtualPhoton->parent_particle_number != 0) {
      std::cerr << className << "::GetCluster:\n";
      std::cerr << "    Virtual photon has an unexpected parent.\n";
      // Delete this virtual photon.
      Heed::particle_bank.erase(node);
      continue;
    }
    // Get the location of the interaction (convert from mm to cm
    // and shift with respect to bounding box center).
    xcls = virtualPhoton->currpos.pt.v.x * 0.1 + cX;
    ycls = virtualPhoton->currpos.pt.v.y * 0.1 + cY;
    zcls = virtualPhoton->currpos.pt.v.z * 0.1 + cZ;
    tcls = virtualPhoton->currpos.time;
    // Make sure the cluster is inside the drift area.
    if (!sensor->IsInArea(xcls, ycls, zcls)) {
      // Delete this virtual photon and proceed with the next one.
      Heed::particle_bank.erase(node);
      continue;
    }
    // Make sure the cluster is inside a medium.
    if (!sensor->GetMedium(xcls, ycls, zcls, medium)) {
      // Delete this virtual photon and proceed with the next one.
      Heed::particle_bank.erase(node);
      continue;
    }
    // Make sure the medium has not changed.
    if (medium->GetName() != mediumName ||
        fabs(medium->GetMassDensity() - mediumDensity) > 1.e-9 ||
        !medium->IsIonisable()) {
      // Delete this virtual photon and proceed with the next one.
      Heed::particle_bank.erase(node);
      continue;
    }
    // Seems to be ok.
    ok = true;
  }

  // Plot the cluster, if requested.
  if (usePlotting) PlotCluster(xcls, ycls, zcls);

  // Transport the virtual photon.
  virtualPhoton->fly();
  // Get the transferred energy (convert from MeV to eV).
  e = virtualPhoton->energy * 1.e6;

  // Make a list of parent particle id numbers.
  std::vector<int> ids;
  ids.clear();
  // At the beginning, there is only the virtual photon.
  ids.push_back(virtualPhoton->particle_number);
  int nIds = 1;

  // Look for daughter particles.
  deltaElectrons.clear();
  nDeltas = 0;
  chamber->conduction_electron_bank.allocate_block(1000);
  bool deleteNode = false;
  Heed::HeedDeltaElectron* delta = 0;
  Heed::HeedPhoton* photon = 0;
  AbsListNode<ActivePtr<gparticle> >* nextNode = node->get_next_node();
  AbsListNode<ActivePtr<gparticle> >* tempNode = 0;
  // Loop over the particle bank.
  while (nextNode != 0) {
    deleteNode = false;
    // Check if it is a delta electron.
    delta = dynamic_cast<Heed::HeedDeltaElectron*>(nextNode->el.get());
    if (delta != 0) {
      // Check if the delta electron was produced by one of the photons
      // belonging to this cluster.
      for (int i = nIds; i--;) {
        if (delta->parent_particle_number == ids[i]) {
          if (useDelta) {
            // Transport the delta electron.
            delta->fly();
          } else {
            // Add the delta electron to the list, for later use.
            deltaElectron newDeltaElectron;
            newDeltaElectron.x = delta->currpos.pt.v.x * 0.1 + cX;
            newDeltaElectron.y = delta->currpos.pt.v.y * 0.1 + cY;
            newDeltaElectron.z = delta->currpos.pt.v.z * 0.1 + cZ;
            newDeltaElectron.t = delta->currpos.time;
            newDeltaElectron.e = delta->curr_kin_energy * 1.e6;
            newDeltaElectron.dx = delta->currpos.dir.x;
            newDeltaElectron.dy = delta->currpos.dir.y;
            newDeltaElectron.dz = delta->currpos.dir.z;
            deltaElectrons.push_back(newDeltaElectron);
            ++nDeltas;
          }
          deleteNode = true;
          break;
        }
      }
    } else {
      // Check if it is a real photon.
      photon = dynamic_cast<Heed::HeedPhoton*>(nextNode->el.get());
      if (photon == 0) {
        std::cerr << className << "::GetCluster:\n";
        std::cerr << "    Particle is neither an electron nor a photon.\n";
        return false;
      }
      for (int i = nIds; i--;) {
        if (photon->parent_particle_number == ids[i]) {
          // Transport the photon and add its number to the list of ids.
          if (usePhotonReabsorption) photon->fly();
          deleteNode = true;
          ids.push_back(photon->particle_number);
          ++nIds;
          break;
        }
      }
    }
    // Proceed with the next node in the particle bank.
    if (deleteNode) {
      tempNode = nextNode->get_next_node();
      Heed::particle_bank.erase(nextNode);
      nextNode = tempNode;
    } else {
      nextNode = nextNode->get_next_node();
    }
  }

  // Get the total number of electrons produced in this step.
  if (useDelta) {
    n = chamber->conduction_electron_bank.get_qel();
  } else {
    n = nDeltas;
  }

  // Remove the virtual photon from the particle bank.
  Heed::particle_bank.erase(node);

  return true;
}

bool TrackHeed::GetElectron(const int i, double& x, double& y, double& z,
                            double& t, double& e, double& dx, double& dy,
                            double& dz) {

  // Make sure NewTrack has successfully been called.
  if (!ready) {
    std::cerr << className << "::GetElectron:\n";
    std::cerr << "    Track has not been initialized.\n";
    std::cerr << "    Call NewTrack first.\n";
    return false;
  }

  if (useDelta) {
    // Make sure an electron with this number exists.
    const int n = chamber->conduction_electron_bank.get_qel();
    if (i < 0 || i >= n) {
      std::cerr << className << "::GetElectron:\n";
      std::cerr << "    Electron number out of range.\n";
      return false;
    }

    x = chamber->conduction_electron_bank[i].ptloc.v.x * 0.1 + cX;
    y = chamber->conduction_electron_bank[i].ptloc.v.y * 0.1 + cY;
    z = chamber->conduction_electron_bank[i].ptloc.v.z * 0.1 + cZ;
    t = chamber->conduction_electron_bank[i].time;
    e = 0.;
    dx = dy = dz = 0.;

  } else {
    // Make sure a delta electron with this number exists.
    if (i < 0 || i >= nDeltas) {
      std::cerr << className << "::GetElectron:\n";
      std::cerr << "    Delta electron number out of range.\n";
      return false;
    }

    x = deltaElectrons[i].x;
    y = deltaElectrons[i].y;
    z = deltaElectrons[i].z;
    t = deltaElectrons[i].t;
    e = deltaElectrons[i].e;
    dx = deltaElectrons[i].dx;
    dy = deltaElectrons[i].dy;
    dz = deltaElectrons[i].dz;
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
  if (!useDelta) {
    std::cerr << className << "::TransportDeltaElectron:\n";
    std::cerr << "    Delta electron transport has been switched off.\n";
    return;
  }

  // Make sure the kinetic energy is positive.
  if (e0 <= 0.) {
    std::cerr << className << "::TransportDeltaElectron:\n";
    std::cerr << "    Kinetic energy must be positive.\n";
    return;
  }

  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::TransportDeltaElectron:\n";
    std::cerr << "    Sensor is not defined.\n";
    ready = false;
    return;
  }

  // Get the bounding box.
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << className << "::TransportDeltaElectron:\n";
    std::cerr << "    Drift area is not set.\n";
    ready = false;
    return;
  }
  // Check if the bounding box has changed.
  bool update = false;
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (fabs(lx - lX) > Small || fabs(ly - lY) > Small || fabs(lz - lZ) > Small) {
    lX = lx;
    lY = ly;
    lZ = lz;
    isChanged = true;
    update = true;
    hasActiveTrack = false;
  }
  // Update the center of the bounding box.
  cX = 0.5 * (xmin + xmax);
  cY = 0.5 * (ymin + ymax);
  cZ = 0.5 * (zmin + zmax);

  HeedInterface::sensor = sensor;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::TransportDeltaElectron:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportDeltaElectron:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    ready = false;
    return;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != mediumName ||
      fabs(medium->GetMassDensity() - mediumDensity) > 1.e-9) {
    isChanged = true;
    update = true;
    ready = false;
    hasActiveTrack = false;
  }

  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    ready = true;
    mediumName = medium->GetName();
    mediumDensity = medium->GetMassDensity();
  }

  deltaElectrons.clear();
  chamber->conduction_electron_bank.allocate_block(1000);

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
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);

  // Transport the electron.
  Heed::HeedDeltaElectron delta(chamber, p0, velocity, t0, 0);
  delta.fly();

  nel = chamber->conduction_electron_bank.get_qel();
}

void TrackHeed::TransportPhoton(const double x0, const double y0,
                                const double z0, const double t0,
                                const double e0, const double dx0,
                                const double dy0, const double dz0, int& nel) {

  nel = 0;

  // Make sure the energy is positive.
  if (e0 <= 0.) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    Photon energy must be positive.\n";
    return;
  }

  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    Sensor is not defined.\n";
    ready = false;
    return;
  }

  // Get the bounding box.
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    Drift area is not set.\n";
    ready = false;
    return;
  }
  // Check if the bounding box has changed.
  bool update = false;
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (fabs(lx - lX) > Small || fabs(ly - lY) > Small || fabs(lz - lZ) > Small) {
    lX = lx;
    lY = ly;
    lZ = lz;
    isChanged = true;
    update = true;
    hasActiveTrack = false;
  }
  // Update the center of the bounding box.
  cX = 0.5 * (xmin + xmax);
  cY = 0.5 * (ymin + ymax);
  cZ = 0.5 * (zmin + zmax);

  HeedInterface::sensor = sensor;

  // Make sure the initial position is inside an ionisable medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:TransportPhoton:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    ready = false;
    return;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName() != mediumName ||
      fabs(medium->GetMassDensity() - mediumDensity) > 1.e-9) {
    isChanged = true;
    update = true;
    ready = false;
  }

  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    ready = true;
    mediumName = medium->GetName();
    mediumDensity = medium->GetMassDensity();
  }

  // Delete the particle bank.
  // Clusters from the current track will be lost.
  hasActiveTrack = false;
  Heed::particle_bank.clear();
  deltaElectrons.clear();
  nDeltas = 0;
  chamber->conduction_electron_bank.allocate_block(1000);

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
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);

  // Create and transport the photon.
  Heed::HeedPhoton photon(chamber, p0, velocity, t0, 0, e0 * 1.e-6, 0);
  photon.fly();

  // Make a list of parent particle id numbers.
  std::vector<int> ids;
  ids.clear();
  // At the beginning, there is only the original photon.
  ids.push_back(photon.particle_number);
  int nIds = 1;

  // Look for daughter particles.
  Heed::HeedDeltaElectron* delta = 0;
  Heed::HeedPhoton* fluorescencePhoton = 0;

  // Get the first element from the particle bank.
  AbsListNode<ActivePtr<gparticle> >* nextNode =
      Heed::particle_bank.get_first_node();
  AbsListNode<ActivePtr<gparticle> >* tempNode = 0;
  // Loop over the particle bank.
  while (nextNode != 0) {
    // Check if it is a delta electron.
    delta = dynamic_cast<Heed::HeedDeltaElectron*>(nextNode->el.get());
    if (delta != 0) {
      // Check if the delta electron was produced by one of the photons
      // belonging to this cluster.
      bool gotParent = false;
      for (int i = nIds; i--;) {
        if (delta->parent_particle_number == ids[i]) {
          gotParent = true;
          if (useDelta) {
            // Transport the delta electron.
            delta->fly();
          } else {
            // Add the delta electron to the list, for later use.
            deltaElectron newDeltaElectron;
            newDeltaElectron.x = delta->currpos.pt.v.x * 0.1 + cX;
            newDeltaElectron.y = delta->currpos.pt.v.y * 0.1 + cY;
            newDeltaElectron.z = delta->currpos.pt.v.z * 0.1 + cZ;
            newDeltaElectron.t = delta->currpos.time;
            newDeltaElectron.e = delta->curr_kin_energy * 1.e6;
            newDeltaElectron.dx = delta->currpos.dir.x;
            newDeltaElectron.dy = delta->currpos.dir.y;
            newDeltaElectron.dz = delta->currpos.dir.z;
            deltaElectrons.push_back(newDeltaElectron);
            ++nDeltas;
          }
          break;
        }
      }
      if (!gotParent) {
        std::cerr << className << "::TransportPhoton:\n";
        std::cerr << "    Delta electron with unknown parent.\n";
      }
    } else {
      // Check if it is a fluorescence photon.
      fluorescencePhoton = dynamic_cast<Heed::HeedPhoton*>(nextNode->el.get());
      if (fluorescencePhoton == 0) {
        std::cerr << className << "::TransportPhoton:\n";
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
    // Proceed with the next node in the particle bank.
    tempNode = nextNode->get_next_node();
    Heed::particle_bank.erase(nextNode);
    nextNode = tempNode;
  }

  // Get the total number of electrons produced in this step.
  if (useDelta) {
    nel = chamber->conduction_electron_bank.get_qel();
  } else {
    nel = nDeltas;
  }
}

void TrackHeed::EnableElectricField() { HeedInterface::useEfield = true; }

void TrackHeed::DisableElectricField() { HeedInterface::useEfield = false; }

void TrackHeed::EnableMagneticField() { HeedInterface::useBfield = true; }

void TrackHeed::DisableMagneticField() { HeedInterface::useBfield = false; }

void TrackHeed::SetEnergyMesh(const double e0, const double e1,
                              const int nsteps) {

  if (fabs(e1 - e0) < Small) {
    std::cerr << className << "::SetEnergyMesh:\n";
    std::cerr << "    Invalid energy range:\n";
    std::cerr << "    " << e0 << " < E [eV] < " << e1 << "\n";
    return;
  }

  if (nsteps <= 0) {
    std::cerr << className << "::SetEnergyMesh:\n";
    std::cerr << "    Number of intervals must be > 0.\n";
    return;
  }

  emin = std::min(e0, e1);
  emax = std::max(e0, e1);
  emin *= 1.e-6;
  emax *= 1.e-6;
  nEnergyIntervals = nsteps;
}

void TrackHeed::SetParticleUser(const double m, const double z) {

  if (fabs(z) < Small) {
    std::cerr << className << "::SetParticleUser:\n";
    std::cerr << "    Particle cannot have zero charge.\n";
    return;
  }
  if (m < Small) {
    std::cerr << className << "::SetParticleUser:\n";
    std::cerr << "    Particle mass must be greater than zero.\n";
  }
  q = z;
  mass = m;
  isElectron = false;
  spin = 0;
  particleName = "exotic";
}

bool TrackHeed::Setup(Medium* medium) {

  // Make sure the path to the Heed database is known.
  char* dbPath = getenv("HEED_DATABASE");
  if (dbPath == 0) {
    std::cerr << className << "::Setup:\n";
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
  if (medium == 0) {
    std::cerr << className << "::Setup:\n";
    std::cerr << "    Medium pointer is null.\n";
    return false;
  }

  // Setup the energy mesh.
  if (energyMesh != 0) {
    delete energyMesh;
    energyMesh = 0;
  }
  energyMesh = new Heed::EnergyMesh(emin, emax, nEnergyIntervals);

  if (medium->IsGas()) {
    if (!SetupGas(medium)) return false;
  } else {
    if (!SetupMaterial(medium)) return false;
  }

  // Energy transfer cross-section
  // Set a flag indicating whether the primary particle is an electron.
  int sel = 0;
  if (isElectron) sel = 1;
  const double gamma = GetGamma();

  if (transferCs != 0) {
    delete transferCs;
    transferCs = 0;
  }
  transferCs =
      new Heed::EnTransfCS(mass / 1.e6, gamma - 1, sel, matter, long(q));

  if (!SetupDelta(databasePath)) return false;

  if (debug) {
    const double nc = transferCs->quanC;
    const double dedx = transferCs->meanC * 1.e3;
    const double dedxLeft = transferCs->meanCleft * 1.e3;
    const double dedx1 = transferCs->meanC1 * 1.e3;
    const double w = matter->W * 1.e6;
    const double f = matter->F;
    const double minI = matter->min_ioniz_pot * 1.e6;
    std::cout << className << "::Setup:\n";
    std::cout << "    Cluster density:             " << nc << " cm-1\n";
    std::cout << "    Stopping power (restricted): " << dedxLeft << " - "
              << dedx << " keV/cm\n";
    std::cout << "    Stopping power (incl. tail): " << dedx1 << " keV/cm\n";
    std::cout << "    W value:                     " << w << " eV\n";
    std::cout << "    Fano factor:                 " << f << "\n";
    std::cout << "    Min. ionization potential:   " << minI << " eV\n";
  }

  fixsyscoor primSys(point(0., 0., 0.), basis("primary"), "primary");
  if (chamber != 0) {
    delete chamber;
    chamber = 0;
  }
  chamber = new HeedChamber(primSys, lX, lY, lZ, transferCs, deltaCs);

  return true;
}

bool TrackHeed::SetupGas(Medium* medium) {

  // Get temperature and pressure.
  double pressure = medium->GetPressure();
  pressure = (pressure / AtmosphericPressure) * Heed::atmosphere;
  double temperature = medium->GetTemperature();

  const int nComponents = medium->GetNumberOfComponents();
  if (nComponents < 1) {
    std::cerr << className << "::SetupGas:\n";
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
      std::cerr << className << "::SetupGas:\n";
      std::cerr << "    Photoabsorption cross-section data for " << gasname
                << " are not available.\n";
      return false;
    }
    notations.increment(gasname);
    fractions.increment(frac);
  }
  if (usePacsOutput) {
    std::ofstream pacsfile;
    pacsfile.open("heed_pacs.txt", std::ios::out);
    const int nValues = energyMesh->get_q();
    if (nValues > 0) {
      for (int i = 0; i < nValues; ++i) {
        double e = energyMesh->get_e(i);
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
  if (gas != 0) {
    delete gas;
    gas = 0;
  }

  gas = new Heed::GasDef(gasname, gasname, nComponents, notations, fractions,
                         pressure, temperature, -1.);

  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  if (matter != 0) {
    delete matter;
    matter = 0;
  }
  matter = new Heed::HeedMatterDef(energyMesh, gas, m_molPacs, w, f);

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
      std::cerr << className << "::SetupMaterial:\n";
      std::cerr << "    Photoabsorption cross-section data for " << materialName
                << " are not implemented.\n";
      return false;
    }
    notations.increment(materialName);
    fractions.increment(frac);
  }
  if (usePacsOutput) {
    std::ofstream pacsfile;
    pacsfile.open("heed_pacs.txt", std::ios::out);
    const int nValues = energyMesh->get_q();
    if (nValues > 0) {
      for (int i = 0; i < nValues; ++i) {
        double e = energyMesh->get_e(i);
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
  if (material != 0) {
    delete material;
    material = 0;
  }
  std::string materialName = medium->GetName();
  material = new Heed::MatterDef(materialName, materialName, nComponents,
                                 notations, fractions, density, temperature);

  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = Heed::standard_factor_Fano;

  if (matter != 0) {
    delete matter;
    matter = 0;
  }
  matter = new Heed::HeedMatterDef(energyMesh, material, m_atPacs, w, f);

  return true;
}

bool TrackHeed::SetupDelta(const std::string databasePath) {

  // Load elastic scattering data.
  std::string filename = databasePath + "cbdel.dat";
  if (elScat != 0) {
    delete elScat;
    elScat = 0;
  }
  elScat = new Heed::ElElasticScat(filename);

  filename = databasePath + "elastic_disp.dat";
  if (lowSigma != 0) {
    delete lowSigma;
    lowSigma = 0;
  }
  lowSigma = new Heed::ElElasticScatLowSigma(elScat, filename);

  // Load data for calculation of ionization.
  // Get W value and Fano factor.
  const double w = matter->W * 1.e6;
  const double f = matter->F;
  filename = databasePath + "delta_path.dat";
  if (pairProd != 0) {
    delete pairProd;
    pairProd = 0;
  }
  pairProd = new Heed::PairProd(filename, w, f);

  if (deltaCs != 0) {
    delete deltaCs;
    deltaCs = 0;
  }
  deltaCs = new Heed::HeedDeltaElectronCS(matter, elScat, lowSigma, pairProd);
  return true;
}

double TrackHeed::GetW() const { return matter->W * 1.e6; }
double TrackHeed::GetFanoFactor() const { return matter->F; }

}
