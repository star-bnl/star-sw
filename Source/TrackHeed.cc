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
BlkArr<HeedCluster> cluster_bank;
AbsList<ActivePtr<gparticle> > particle_bank;

void 
field_map(const point& pt, vec& efield, vec& bfield, vfloat& mrange) {

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
        x, y, z, ex, ey, ez, 
        Garfield::HeedInterface::medium, status);
    efield.x = ex * 1.e-5; 
    efield.y = ey * 1.e-5; 
    efield.z = ez * 1.e-5;
  }
  
  if (Garfield::HeedInterface::useBfield) {
    double bx = 0., by = 0., bz = 0.;
    int status = 0;
    Garfield::HeedInterface::sensor->MagneticField(x, y, z, 
                                                   bx, by, bz, 
                                                   status);
    bfield.x = bx; bfield.y = by; bfield.z = bz;
  }

}

// This function is called by Heed after each step
void
check_point(gparticle* gp) { }

// Particle id number for book-keeping
long last_particle_number;

extern trajestep_limit gtrajlim;
trajestep_limit gtrajlim(100.*cm, 1000.*cm, 0.1*rad, 0.2* rad);

double mparticle::speed_of_light=c_light;

// Actual class implementation

namespace Garfield {

TrackHeed::TrackHeed() : 
  ready(false), hasActiveTrack(false),
  mediumDensity(-1.), mediumName(""),
  useDelta(true), nDeltas(0),
  particle(0), 
  matter(0), gas(0), material(0),
  atPacs(0), molPacs(0),
  emin(2.e-6), emax(2.e-1), nEnergyIntervals(200),
  energyMesh(0), transferCs(0),
  elScat(0), lowSigma(0), pairProd(0), deltaCs(0),
  chamber(0), lX(0.), lY(0.), lZ(0.), cX(0.), cY(0.), cZ(0.) {
 
  className = "TrackHeed";
 
  HeedInterface::sensor = 0;
  HeedInterface::useEfield = false;
  HeedInterface::useBfield = false;
  
  deltaElectrons.clear();
  
}

TrackHeed::~TrackHeed() {

  if (particle   != 0) delete particle;
  if (matter     != 0) delete matter;
  if (gas        != 0) delete gas;
  if (material   != 0) delete material;
  if (atPacs     != 0) delete atPacs;
  if (molPacs    != 0) delete molPacs;
  if (energyMesh != 0) delete energyMesh;
  if (transferCs != 0) delete transferCs;
  if (elScat     != 0) delete elScat;
  if (lowSigma   != 0) delete lowSigma;
  if (pairProd   != 0) delete pairProd;
  if (deltaCs    != 0) delete deltaCs;
  if (chamber    != 0) delete chamber;
  
  Garfield::HeedInterface::sensor = 0;

}

void
TrackHeed::NewTrack(
        const double x0, const double y0, const double z0, const double t0,
        const double dx0, const double dy0, const double dz0) {

  hasActiveTrack = false;
  
  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    ready = false;
    return;
  }
  
  // Get the bounding box.
  double xmin = 0., ymin = 0., zmin = 0.;
  double xmax = 0., ymax = 0., zmax = 0.;
  if (!sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Drift area is not set.\n";
    ready = false;
    return;
  }
  // Check if the bounding box has changed.
  const double lx = fabs(xmax - xmin);
  const double ly = fabs(ymax - ymin);
  const double lz = fabs(zmax - zmin);
  if (fabs(lx - lX) > Small || 
      fabs(ly - lY) > Small || 
      fabs(lz - lZ) > Small) {
    lX = lx; lY = ly; lZ = lz;
    isChanged = true;
  }
  // Update the center of the bounding box.
  cX = 0.5 * (xmin + xmax);
  cY = 0.5 * (ymin + ymax);
  cZ = 0.5 * (zmin + zmax);
  
  HeedInterface::sensor = sensor;
  
  // Make sure the initial position is inside an ionisable medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    ready = false;
    return;
  } else if (!medium->IsIonisable()) {
    std::cerr << "TrackHeed:NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    ready = false;
    return;
  }

  // Check if the medium has changed since the last call.
  if (medium->GetName()        != mediumName || 
      medium->GetMassDensity() != mediumDensity) {
    isChanged = true;
  }
  
  // If medium, particle or bounding box have changed, 
  // update the cross-sections.
  if (isChanged) {
    ready = false;
    if (!Setup(medium)) return;
    ready = true;
    isChanged = false;
    mediumName    = medium->GetName();
    mediumDensity = medium->GetMassDensity();
  }
  
  particle_bank.clear();
  deltaElectrons.clear();
  cluster_bank.allocate_block(100);
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
    dx /= d; dy /= d; dz /= d;
  }
  vec velocity(dx, dy, dz);
  velocity = velocity * mparticle::speed_of_light * GetBeta();
  
  if (debug) {
    std::cout << className << "::NewTrack:\n";
    std::cout << "    Track starts at (" 
              << x0 << ", " << y0 << ", " << z0 << ") at time " 
              << t0 << "\n";
    std::cout << "    Initial direction: ("
              << dx << ", " << dy << ", " << dz << ")\n";
  }

  // Initial position (shift with respect to bounding box center and
  // convert from cm to mm).
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);
  // Setup the particle.
  last_particle_number = 0;
  if (particle != 0) {
    delete particle;
    particle = 0;
  }
  
  particle_def* particleType = &muon_minus_def;
  if (particleName == "e-") {
    particleType = &electron_def;
  } else if (particleName == "e+") {
    particleType = &positron_def;
  } else if (particleName == "mu-") {
    particleType = &muon_minus_def;
  } else if (particleName == "mu+") {
    particleType = &muon_plus_def;
  } else if (particleName == "pi-") {
    particleType = &pi_minus_meson_def;
  } else if (particleName == "pi+") {
    particleType = &pi_plus_meson_def;
  } else if (particleName == "K-") {
    particleType = &K_minus_meson_def;
  } else if (particleName == "K+") {
    particleType = &K_plus_meson_def;
  } else if (particleName == "p") {
    particleType = &proton_def;
  } else if (particleName == "pbar") {
    particleType = &anti_proton_def;
  } else if (particleName == "d") {
    particleType = &deuteron_def;
  } else {
    // Not a predefined particle, use muon definition.
    if (q > 0.) {
      particleType = &muon_minus_def;
    } else {
      particleType = &muon_plus_def;
    }
  }
    
  particle = new HeedParticle(chamber, 
                                p0, velocity, t0, 
                                particleType);
  // Transport the particle.
  particle->fly();
  hasActiveTrack = true;

  // Plot the new track.
  if (usePlotting) PlotNewTrack(x0, y0, z0);

}

double
TrackHeed::GetClusterDensity() {

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

bool
TrackHeed::GetCluster(double& xcls, double& ycls, double& zcls, 
                      double& tcls,
                      int& n, double& e, double& extra) {

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
  HeedPhoton* virtualPhoton = 0;
  while (!ok) {
    // Get the first element from the particle bank.
    node = particle_bank.get_first_node();
  
    // Make sure the particle bank is not empty.
    if (node == 0) {
      hasActiveTrack = false;
      return false;
    }
  
    // Convert the particle to a (virtual) photon.
    virtualPhoton = dynamic_cast<HeedPhoton*>(node->el.get());
    if (virtualPhoton == 0) {
      std::cerr << className << "::GetCluster:\n";
      std::cerr << "    Particle is not a virtual photon.\n";
      std::cerr << "    Program bug!\n";
      // Delete the node.
      particle_bank.erase(node);
      // Try the next node.
      continue;
    }

    if (virtualPhoton->parent_particle_number != 0) {
      std::cerr << className << "::GetCluster:\n";
      std::cerr << "    Virtual photon has an unexpected parent.\n";
      // Delete this virtual photon.
      particle_bank.erase(node);
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
      particle_bank.erase(node);
      continue;
    }
    // Make sure the cluster is inside a medium.
    if (!sensor->GetMedium(xcls, ycls, zcls, medium)) {
      // Delete this virtual photon and proceed with the next one.
      particle_bank.erase(node);
      continue;
    }
    // Make sure the medium has not changed.
    if (medium->GetName()        != mediumName || 
        medium->GetMassDensity() != mediumDensity || 
        !medium->IsIonisable()) {
      // Delete this virtual photon and proceed with the next one.
      particle_bank.erase(node);
      continue;
    }
    // Seems to be ok.
    ok = true;
  }
  
  // Plot the cluster, if required.
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
  HeedDeltaElectron* delta = 0;
  HeedPhoton* photon = 0;
  AbsListNode<ActivePtr<gparticle> >* nextNode = node->get_next_node();
  AbsListNode<ActivePtr<gparticle> >* tempNode = 0;
  // Loop over the particle bank.
  while (nextNode != 0) {
    deleteNode = false;
    // Check if it is a delta electron.
    delta = dynamic_cast<HeedDeltaElectron*>(nextNode->el.get());
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
      photon = dynamic_cast<HeedPhoton*>(nextNode->el.get());
      if (photon == 0) {
        std::cerr << className << "::GetCluster:\n";
        std::cerr << "    Particle is neither an electron nor a photon.\n";
        return false;
      }
      for (int i = nIds; i--;) {
        if (photon->parent_particle_number == ids[i]) {
          // Transport the photon and add its number to the list of ids.
          photon->fly();
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
      particle_bank.erase(nextNode);
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
  particle_bank.erase(node);

  return true;

}

bool
TrackHeed::GetElectron(const int i, double& x, double& y, double& z, 
                       double& t, double& e, 
                       double& dx, double& dy, double& dz) {

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

void
TrackHeed::TransportDeltaElectron(
      const double x0, const double y0, const double z0, 
      const double t0, const double e0, 
      const double dx0, const double dy0, const double dz0,
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
  if (fabs(lx - lX) > Small || 
      fabs(ly - lY) > Small || 
      fabs(lz - lZ) > Small) {
    lX = lx; lY = ly; lZ = lz;
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
  if (medium->GetName()        != mediumName || 
      medium->GetMassDensity() != mediumDensity) {
    isChanged = true;
    update = true;
    ready = false;
    hasActiveTrack = false;
  }

  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    ready = true;
    mediumName    = medium->GetName();
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
    dx /= d; dy /= d; dz /= d;
  }
  vec velocity(dx, dy, dz);
  
  // Calculate the speed for the given kinetic energy.
  const double gamma = 1. + e0 / ElectronMass;
  const double beta = sqrt(1. - 1. / (gamma * gamma)); 
  double speed = mparticle::speed_of_light * beta;
  velocity = velocity * speed;
  
  // Initial position (shift with respect to bounding box center and 
  // convert from cm to mm).
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);
 
  // Transport the electron.
  HeedDeltaElectron delta(chamber, p0, velocity, t0, 0);
  delta.fly();
  
  nel = chamber->conduction_electron_bank.get_qel();
  
}

void
TrackHeed::TransportPhoton(
          const double x0, const double y0, const double z0,
          const double t0, const double e0,
          const double dx0, const double dy0, const double dz0,
          int& nel) {
 
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
  if (fabs(lx - lX) > Small || 
      fabs(ly - lY) > Small || 
      fabs(lz - lZ) > Small) {
    lX = lx; lY = ly; lZ = lz;
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
  if (medium->GetName()        != mediumName || 
      medium->GetMassDensity() != mediumDensity) {
    isChanged = true;
    update = true;
    ready = false;
  }
  
  // If medium or bounding box have changed, update the "chamber".
  if (update) {
    if (!Setup(medium)) return;
    ready = true;
    mediumName    = medium->GetName();
    mediumDensity = medium->GetMassDensity();
  }
  
  // Delete the particle bank.
  // Clusters from the current track will be lost.
  hasActiveTrack = false;
  last_particle_number = 0;
  particle_bank.clear();
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
    dx /= d; dy /= d; dz /= d;
  }
  vec velocity(dx, dy, dz);
  velocity = velocity * mparticle::speed_of_light;
  
  // Initial position (shift with respect to bounding box center and 
  // convert from cm to mm).
  point p0((x0 - cX) * 10., (y0 - cY) * 10., (z0 - cZ) * 10.);
 
  // Create and transport the photon.
  HeedPhoton photon(chamber, p0, velocity, t0, 0, e0 * 1.e-6, 0);
  photon.fly();
  
  // Make a list of parent particle id numbers. 
  std::vector<int> ids;
  ids.clear();
  // At the beginning, there is only the original photon.
  ids.push_back(photon.particle_number);
  int nIds = 1;
  
  // Look for daughter particles.
  HeedDeltaElectron* delta = 0;
  HeedPhoton* fluorescencePhoton = 0;
  
  // Get the first element from the particle bank.
  AbsListNode<ActivePtr<gparticle> >* nextNode = 
                                      particle_bank.get_first_node();
  AbsListNode<ActivePtr<gparticle> >* tempNode = 0;
  // Loop over the particle bank.
  while (nextNode != 0) {
    // Check if it is a delta electron.
    delta = dynamic_cast<HeedDeltaElectron*>(nextNode->el.get());
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
      fluorescencePhoton = dynamic_cast<HeedPhoton*>(nextNode->el.get());
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
    particle_bank.erase(nextNode);
    nextNode = tempNode;
  }
  
  // Get the total number of electrons produced in this step.
  if (useDelta) {
    nel = chamber->conduction_electron_bank.get_qel();
  } else {
    nel = nDeltas;
  }

}
      
void
TrackHeed::EnableElectricField() {

  HeedInterface::useEfield = true;
  
}

void
TrackHeed::DisableElectricField() {

  HeedInterface::useEfield = false;
  
}

void
TrackHeed::EnableMagneticField() {

  HeedInterface::useBfield = true;
  
}

void
TrackHeed::DisableMagneticField() {

  HeedInterface::useBfield = false;

}

void
TrackHeed::SetEnergyMesh(const double e0, const double e1, 
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

bool
TrackHeed::Setup(Medium* medium) {

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
    delete energyMesh; energyMesh = 0;
  }
  energyMesh = new EnergyMesh(emin, emax, nEnergyIntervals);
  
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
  transferCs = new EnTransfCS(mass / 1.e6, gamma - 1, sel, matter, long(q));
  
  if (!SetupDelta(databasePath)) return false;  

  if (debug) {
    const double nc = transferCs->quanC;
    const double dedx = transferCs->meanC * 1.e3;
    const double dedxLeft = transferCs->meanCleft * 1.e3;
    const double dedx1 = transferCs->meanC1 * 1.e3;
    const double w = matter->W * 1.e6;
    const double f = matter->F;
    std::cout << className << "::Setup:\n";
    std::cout << "    Cluster density:             " << nc << " cm-1\n";
    std::cout << "    Stopping power (restricted): " << dedxLeft << " - " 
                                         << dedx << " keV/cm\n";
    std::cout << "    Stopping power (incl. tail): " << dedx1 
                                                      << " keV/cm\n";
    std::cout << "    W value:                     " << w << " eV\n";
    std::cout << "    Fano factor:                 " << f << "\n";
  }

  fixsyscoor primSys(point(0., 0., 0.), basis("primary"), "primary");
  if (chamber != 0) {
    delete chamber; chamber = 0;
  }
  chamber = new HeedChamber(primSys, lX, lY, lZ,
                            transferCs, deltaCs);

  return true;
  
}

bool
TrackHeed::SetupGas(Medium* medium) {

  // Get temperature and pressure.
  double pressure = medium->GetPressure();
  pressure = (pressure / AtmosphericPressure) * atmosphere;
  double temperature = medium->GetTemperature();
  
  const int nComponents = medium->GetNumberOfComponents();
  if (nComponents < 1) {
    std::cerr << className << "::SetupGas:\n";
    std::cerr << "    Gas " << medium->GetName() 
              << " has zero constituents.\n";
    return false;
  }

  if (molPacs != 0) {
    delete molPacs;
    molPacs = 0;
  }
  molPacs = new MolecPhotoAbsCS*[nComponents];
  DynLinArr<std::string> notations; notations.clear();
  DynLinArr<double> fractions; fractions.clear();
  
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
    if (gasname == "CF4")         molPacs[i] = &CF4_MPACS;
    else if (gasname == "Ar")     molPacs[i] = &Ar_MPACS;
    else if (gasname == "He")     molPacs[i] = &He_MPACS;
    else if (gasname == "Ne")     molPacs[i] = &Ne_MPACS;
    else if (gasname == "Kr")     molPacs[i] = &Kr_MPACS;
    else if (gasname == "Xe")     molPacs[i] = &Xe_MPACS;
    else if (gasname == "CH4")    molPacs[i] = &CH4_MPACS;
    else if (gasname == "C2H6")   molPacs[i] = &C2H6_MPACS;
    else if (gasname == "C3H8")   molPacs[i] = &C3H8_MPACS;
    else if (gasname == "C4H10")  molPacs[i] = &C4H10_MPACS;
    else if (gasname == "CO2")    molPacs[i] = &CO2_MPACS;
    else if (gasname == "C5H12")  molPacs[i] = &C5H12_MPACS;
    else if (gasname == "Water")  molPacs[i] = &H2O_MPACS;
    else if (gasname == "O2")     molPacs[i] = &O2_MPACS;
    else if (gasname == "N2")     molPacs[i] = &N2_MPACS;
    else if (gasname == "NO")     molPacs[i] = &NO_MPACS;
    else if (gasname == "N2O")    molPacs[i] = &N2O_MPACS;
    else if (gasname == "C2H4")   molPacs[i] = &C2H4_MPACS;
    else if (gasname == "C2H2")   molPacs[i] = &C2H2_MPACS;
    else if (gasname == "H2")     molPacs[i] = &H2_MPACS;
    else if (gasname == "CO")     molPacs[i] = &CO_MPACS;
    else if (gasname == "Methylal") molPacs[i] = &Methylal_MPACS;
    else if (gasname == "DME")    molPacs[i] = &DME_MPACS;
    else if (gasname == "C2F6")   molPacs[i] = &C2F6_MPACS;
    else if (gasname == "SF6")    molPacs[i] = &SF6_MPACS;
    else if (gasname == "NH3")    molPacs[i] = &NH3_MPACS;
    else if (gasname == "C3H6")   molPacs[i] = &C3H6_MPACS;
    else if (gasname == "CH3OH")  molPacs[i] = &CH3OH_MPACS;
    else if (gasname == "C2H5OH") molPacs[i] = &C2H5OH_MPACS;
    else if (gasname == "C3H7OH") molPacs[i] = &C3H7OH_MPACS;
    else if (gasname == "Cs")     molPacs[i] = &Cs_MPACS;
    else if (gasname == "F2")     molPacs[i] = &F2_MPACS;
    else if (gasname == "CS2")    molPacs[i] = &CS2_MPACS;
    else if (gasname == "COS")    molPacs[i] = &COS_MPACS;
    else if (gasname == "CD4")    molPacs[i] = &CH4_MPACS;
    else if (gasname == "BF3")    molPacs[i] = &BF3_MPACS;
    else if (gasname == "C2HF5")  molPacs[i] = &C2HF5_MPACS;
    else if (gasname == "CHF3")   molPacs[i] = &CHF3_MPACS;
    else if (gasname == "CF3Br")  molPacs[i] = &CF3Br_MPACS;
    else if (gasname == "C3F8")   molPacs[i] = &C3F8_MPACS;
    else if (gasname == "O3")     molPacs[i] = &O3_MPACS;
    else if (gasname == "Hg")     molPacs[i] = &Hg_MPACS;
    else if (gasname == "H2S")    molPacs[i] = &H2S_MPACS;
    else if (gasname == "GeH4")   molPacs[i] = &GeH4_MPACS;
    else if (gasname == "SiH4")   molPacs[i] = &SiH4_MPACS;
    else {
      std::cerr << className << "::SetupGas:\n";
      std::cerr << "    Photoabsorption cross-section data for " << gasname 
                << " are not available.\n";
      return false;
    }
    notations.increment(gasname);
    fractions.increment(frac);
  }
  std::string gasname = medium->GetName();
  if (gas != 0) {
    delete gas; gas = 0;
  }

  gas = new GasDef(gasname, gasname, nComponents,
                   notations, fractions, pressure, temperature, -1.);
    
  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = standard_factor_Fano;

  if (matter != 0) {
    delete matter;
    matter = 0;
  }
  matter = new HeedMatterDef(energyMesh, gas, molPacs, w, f);
  
  return true;

}

bool
TrackHeed::SetupMaterial(Medium* medium) {

  // Get temperature and density.
  double temperature = medium->GetTemperature();
  double density = medium->GetMassDensity() * g / cm3;
  
  const int nComponents = medium->GetNumberOfComponents();
  if (atPacs != 0) {
    delete atPacs;
    atPacs = 0;
  }
  atPacs = new AtomPhotoAbsCS*[nComponents];
  
  DynLinArr<std::string> notations; notations.clear();
  DynLinArr<double> fractions; fractions.clear();
  for (int i = 0; i < nComponents; ++i) {
    std::string materialName;
    double frac;
    medium->GetComponent(i, materialName, frac);
    if (materialName == "C")       atPacs[i] = &Carbon_PACS;
    else if (materialName == "Si") atPacs[i] = &Silicon_crystal_PACS;
    else if (materialName == "Ge") atPacs[i] = &Germanium_crystal_PACS;
    else {
      std::cerr << className << "::SetupMaterial:\n";
      std::cerr << "    Photoabsorption cross-section data for " 
                << materialName << " are not implemented.\n";
      return false;
    }
    notations.increment(materialName);
    fractions.increment(frac);
  }
  if (material != 0) {
    delete material; material = 0;
  }
  std::string materialName = medium->GetName();
  material = new MatterDef(materialName, materialName, nComponents,
                           notations, fractions, density, temperature);


  double w = medium->GetW() * 1.e-6;
  if (w < 0.) w = 0.;
  double f = medium->GetFanoFactor();
  if (f <= 0.) f = standard_factor_Fano;
    
  if (matter != 0) {
    delete matter;
    matter = 0;
  }
  matter = new HeedMatterDef(energyMesh, material, atPacs, w, f);
  
  return true;
 
}

bool
TrackHeed::SetupDelta(const std::string databasePath) {

  // Load elastic scattering data.
  std::string filename = databasePath + "cbdel.dat";
  if (elScat != 0) {
    delete elScat; elScat = 0;
  }
  elScat = new ElElasticScat(filename);
  
  filename = databasePath + "elastic_disp.dat";
  if (lowSigma != 0) {
    delete lowSigma; lowSigma = 0;
  }
  lowSigma = new ElElasticScatLowSigma(elScat, filename);
  
  // Load data for calculation of ionization.
  // Get W value and Fano factor.
  const double w = matter->W * 1.e6;
  const double f = matter->F;
  filename = databasePath + "delta_path.dat";
  if (pairProd != 0) {
    delete pairProd; pairProd = 0;
  }
  pairProd = new PairProd(filename, w, f);
  
  if (deltaCs != 0) {
    delete deltaCs; deltaCs = 0;
  }
  deltaCs = new HeedDeltaElectronCS(matter, elScat, lowSigma, pairProd);
  return true;

}

}
