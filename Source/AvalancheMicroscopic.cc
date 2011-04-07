#include <iostream>
#include <cmath>
#include <string>

#include "AvalancheMicroscopic.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

AvalancheMicroscopic::AvalancheMicroscopic() :
  sensor(0), 
  nPhotons(0), nElectrons(0), nHoles(0), nIons(0), 
  nElectronEndpoints(0), nHoleEndpoints(0),
  usePlotting(false), viewer(0),
  plotExcitations(true), plotIonisations(true), plotAttachments(true), 
  histElectronEnergy(0), histHoleEnergy(0),
  hasElectronEnergyHistogram(false), hasHoleEnergyHistogram(false),
  histDistance(0), hasDistanceHistogram(false), distanceOption('r'),
  nDistanceHistogramTypes(0),
  histSecondary(0), hasSecondaryHistogram(false),
  useSignal(false), useInducedCharge(false),
  useDriftLines(false), usePhotons(false), 
  useBandStructureDefault(true),
  useNullCollisionSteps(false), useBfield(false),
  rb11(1.), rb12(0.), rb13(0.), rb21(0.), rb22(1.), rb23(0.),
  rb31(0.), rb32(0.), rb33(1.), rx22(1.), rx23(0.), rx32(0.), rx33(1.),
  deltaCut(0.), gammaCut(0.),
  sizeCut(-1), nCollSkip(100),
  hasUserHandleStep(false), 
  hasUserHandleAttachment(false),
  hasUserHandleInelastic(false),
  hasUserHandleIonisation(false),
  userHandleStep(0), 
  userHandleAttachment(0), 
  userHandleInelastic(0),
  userHandleIonisation(0),
  debug(false) {
  
  className = "AvalancheMicroscopic";
  distanceHistogramType.clear();
  
  stack.clear();
  endpointsElectrons.clear();
  endpointsHoles.clear();
  photons.clear();

  distanceHistogramType.clear();
  
}

void 
AvalancheMicroscopic::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }
  sensor = s;

}

void
AvalancheMicroscopic::EnablePlotting(ViewDrift* view) {

  if (view == 0) {
    std::cerr << className << "::EnablePlotting:\n";
    std::cerr << "    Viewer pointer is null.\n";
    return;
  }
  
  viewer = view;
  usePlotting = true;
  if (!useDriftLines) {
    std::cout << className << "::EnablePlotting:\n";
    std::cout << "    Enabling storage of drift line.\n";
    EnableDriftLines();
  }

}

void
AvalancheMicroscopic::DisablePlotting() {
 
  viewer = 0;
  usePlotting = false;

}

void 
AvalancheMicroscopic::EnableElectronEnergyHistogramming(TH1* histo) {

  if (histo == 0) {
    std::cerr << className << "::EnableElectronEnergyHistogramming:\n"; 
    std::cerr << "    Histogram pointer is null.\n";
    return;
  }
  
  histElectronEnergy = histo;
  hasElectronEnergyHistogram = true;
  
}

void 
AvalancheMicroscopic::DisableElectronEnergyHistogramming() {

  hasElectronEnergyHistogram = false;
  
}

void 
AvalancheMicroscopic::EnableHoleEnergyHistogramming(TH1* histo) {

  if (histo == 0) {
    std::cerr << className << "::EnableHoleEnergyHistogramming:\n"; 
    std::cerr << "    Histogram pointer is null.\n";
    return;
  }
  
  histHoleEnergy = histo;
  hasHoleEnergyHistogram = true;
  
}

void 
AvalancheMicroscopic::DisableHoleEnergyHistogramming() {

  hasHoleEnergyHistogram = false;
  
}

void 
AvalancheMicroscopic::SetDistanceHistogram(TH1* histo, const char opt) {

  if (histo == 0) {
    std::cerr << className << "::SetDistanceHistogram:\n"; 
    std::cerr << "    Histogram pointer is null.\n";
    return;
  }
  
  histDistance = histo;
  hasDistanceHistogram = true;

  if (opt == 'x' || opt == 'y' || opt == 'z' || opt == 'r') {
    distanceOption = opt;
  } else {
    std::cerr << className << "::SetDistanceHistogram:";
    std::cerr << "    Unknown option " << opt << ".\n";
    std::cerr << "    Valid options are x, y, z, r.\n";
    std::cerr << "    Using default value (r).\n";
    distanceOption = 'r';
  }

  if (nDistanceHistogramTypes <= 0) {
    std::cout << className << "::SetDistanceHistogram:\n";
    std::cout << "    Don't forget to call EnableDistanceHistogramming.\n";
  }
  
}

void
AvalancheMicroscopic::EnableDistanceHistogramming(const int type) {

  // Check if this type of collision is already registered 
  // for histogramming.
  if (nDistanceHistogramTypes > 0) {
    for (int i = nDistanceHistogramTypes; i--;) {
      if (distanceHistogramType[i] == type) {
        std::cout << className << "::EnableDistanceHistogramming:\n";
        std::cout << "    Collision type " << type 
                  << " is already histogrammed.\n";
        return;
      }
    }
  }

  distanceHistogramType.push_back(type);
  ++nDistanceHistogramTypes;
  std::cout << className << "::EnableDistanceHistogramming:\n";
  std::cout << "    Histogramming of collision type " 
            << type << " enabled.\n";
  if (!hasDistanceHistogram) {
    std::cout << "    Don't forget to set the histogram.\n";
  }

}

void
AvalancheMicroscopic::DisableDistanceHistogramming(const int type) {

  if (nDistanceHistogramTypes <= 0) {
    std::cerr << className << "::DisableDistanceHistogramming:\n";
    std::cerr << "    Collision type " << type 
              << " is not histogrammed.\n";
    return;
  } 

  for (int i = nDistanceHistogramTypes; i--;) {
    if (distanceHistogramType[i] == type) {
      distanceHistogramType.erase(distanceHistogramType.begin() + i);
      --nDistanceHistogramTypes;
      std::cout << "    Histogramming of collision type "
                << type << " disabled.\n";
      return;
    }
  }
  
  std::cerr << className << "::DisableDistanceHistogramming:\n"; 
  std::cerr << "    Collision type " << type 
            << " is not histogrammed.\n";

}

void 
AvalancheMicroscopic::DisableDistanceHistogramming() {

  hasDistanceHistogram = false;
  nDistanceHistogramTypes = 0;
  distanceHistogramType.clear();
  
}

void 
AvalancheMicroscopic::EnableSecondaryEnergyHistogramming(TH1* histo) {

  if (histo == 0) {
    std::cerr << className << "::EnableSecondaryEnergyHistogramming:\n"; 
    std::cerr << "    Histogram pointer is null.\n";
    return;
  }
  
  histSecondary = histo;
  hasSecondaryHistogram = true;
  
}

void 
AvalancheMicroscopic::DisableSecondaryEnergyHistogramming() {

  hasSecondaryHistogram = false;
  
}

void 
AvalancheMicroscopic::SetCollisionSteps(const int n) {

  if (n <= 0) {
    std::cerr << className << "::SetCollisionSteps:\n";
    std::cerr << "    Number of collisions to be skipped set to" 
              << " default value (100).\n";
    nCollSkip = 100;
    return;
  }
  
  nCollSkip = n;
  
}

void 
AvalancheMicroscopic::GetElectronEndpoint(const int i, 
  double& x0, double& y0, double& z0, double& t0, double& e0,
  double& x1, double& y1, double& z1, double& t1, double& e1,
  int& status) const {
  
  if (i < 0 || i >= nElectronEndpoints) {
    std::cerr << className << "::GetElectronEndpoint:\n";
    std::cerr << "    Endpoint index " << i << " out of range.\n";
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = t1 = t1 = e1 = 0.;
    status = 0;
    return;
  }

  x0 = endpointsElectrons[i].x0; 
  y0 = endpointsElectrons[i].y0; 
  z0 = endpointsElectrons[i].z0;
  t0 = endpointsElectrons[i].t0; 
  e0 = endpointsElectrons[i].e0;
  x1 = endpointsElectrons[i].x;  
  y1 = endpointsElectrons[i].y;  
  z1 = endpointsElectrons[i].z;
  t1 = endpointsElectrons[i].t;  
  e1 = endpointsElectrons[i].energy;  
  status = endpointsElectrons[i].status; 

}

void 
AvalancheMicroscopic::GetHoleEndpoint(const int i, 
  double& x0, double& y0, double& z0, double& t0, double& e0,
  double& x1, double& y1, double& z1, double& t1, double& e1,
  int& status) const {
  
  if (i < 0 || i >= nHoleEndpoints) {
    std::cerr << className << "::GetHoleEndpoint:\n";
    std::cerr << "    Endpoint index " << i << " out of range.\n";
    x0 = y0 = z0 = t0 = e0 = 0.;
    x1 = y1 = t1 = t1 = e1 = 0.;
    status = 0;
    return;
  }

  x0 = endpointsHoles[i].x0; 
  y0 = endpointsHoles[i].y0; 
  z0 = endpointsHoles[i].z0;
  t0 = endpointsHoles[i].t0; 
  e0 = endpointsHoles[i].e0;
  x1 = endpointsHoles[i].x;  
  y1 = endpointsHoles[i].y;  
  z1 = endpointsHoles[i].z;
  t1 = endpointsHoles[i].t;  
  e1 = endpointsHoles[i].energy;  
  status = endpointsHoles[i].status; 

}

int 
AvalancheMicroscopic::GetNumberOfElectronDriftLinePoints(const int i) const {

  if (i < 0 || i >= nElectronEndpoints) {
    std::cerr << className << "::GetNumberOfElectronDriftLinePoints:\n"; 
    std::cerr << "    Endpoint index (" << i << ") out of range.\n";
    return 0;
  }
  
  if (!useDriftLines) return 2;

  return endpointsElectrons[i].driftLine.size() + 2;

}

int 
AvalancheMicroscopic::GetNumberOfHoleDriftLinePoints(const int i) const {

  if (i < 0 || i >= nHoleEndpoints) {
    std::cerr << className << "::GetNumberOfHoleDriftLinePoints:\n"; 
    std::cerr << "    Endpoint index (" << i << ") out of range.\n";
    return 0;
  }
  
  if (!useDriftLines) return 2;

  return endpointsHoles[i].driftLine.size() + 2;

}

void 
AvalancheMicroscopic::GetElectronDriftLinePoint(
  double& x, double& y, double& z, double &t,
  const int ip, const int iel) const {
  
  if (iel < 0 || iel >= nElectronEndpoints) {
    std::cerr << className << "::GetElectronDriftLinePoint:\n";
    std::cerr << "    Endpoint index (" << iel << ") out of range.\n";
    return;
  }

  if (ip <= 0) {
    x = endpointsElectrons[iel].x0; 
    y = endpointsElectrons[iel].y0; 
    z = endpointsElectrons[iel].z0;
    t = endpointsElectrons[iel].t0;
    return;
  }

  const int np = endpointsElectrons[iel].driftLine.size();
  if (ip > np) {
    x = endpointsElectrons[iel].x; 
    y = endpointsElectrons[iel].y; 
    z = endpointsElectrons[iel].z;
    t = endpointsElectrons[iel].t;
    return;
  }

  x = endpointsElectrons[iel].driftLine[ip - 1].x;
  y = endpointsElectrons[iel].driftLine[ip - 1].y;
  z = endpointsElectrons[iel].driftLine[ip - 1].z;
  t = endpointsElectrons[iel].driftLine[ip - 1].t;

}

void 
AvalancheMicroscopic::GetHoleDriftLinePoint(
  double& x, double& y, double& z, double &t,
  const int ip, const int ih) const {
  
  if (ih < 0 || ih >= nHoleEndpoints) {
    std::cerr << className << "::GetHoleDriftLinePoint:\n";
    std::cerr << "    Endpoint index (" << ih << ") out of range.\n";
    return;
  }

  if (ip <= 0) {
    x = endpointsHoles[ih].x0; 
    y = endpointsHoles[ih].y0; 
    z = endpointsHoles[ih].z0;
    t = endpointsHoles[ih].t0;
    return;
  }

  const int np = endpointsHoles[ih].driftLine.size();
  if (ip > np) {
    x = endpointsHoles[ih].x; 
    y = endpointsHoles[ih].y; 
    z = endpointsHoles[ih].z;
    t = endpointsHoles[ih].t;
    return;
  }

  x = endpointsHoles[ih].driftLine[ip - 1].x;
  y = endpointsHoles[ih].driftLine[ip - 1].y;
  z = endpointsHoles[ih].driftLine[ip - 1].z;
  t = endpointsHoles[ih].driftLine[ip - 1].t;

}

void 
AvalancheMicroscopic::GetPhoton(const int i, double& e,
  double& x0, double& y0, double& z0, double& t0,
  double& x1, double& y1, double& z1, double& t1,
  int& status) const {
 
  if (i < 0 || i >= nPhotons) {
    std::cerr << className << "::GetPhoton:\n";
    std::cerr << "    Photon " << i << " does not exist.\n";
    return;
  }

  x0 = photons[i].x0; x1 = photons[i].x1;
  y0 = photons[i].y0; y1 = photons[i].y1;
  z0 = photons[i].z0; z1 = photons[i].z1;
  t0 = photons[i].t0; t1 = photons[i].t1;
  status = photons[i].status;
  e = photons[i].energy;


}

void
AvalancheMicroscopic::SetUserHandleStep(
    void (*f)(double x, double y, double z, double t,
              double e, double dx, double dy, double dz,
              bool hole)) {

  if (f == 0) {
    std::cerr << className << "::SetUserHandleStep:\n";
    std::cerr << "    Function pointer is null.\n";
    return;
  }
  userHandleStep = f;
  hasUserHandleStep = true;

}

void
AvalancheMicroscopic::UnsetUserHandleStep() {

  userHandleStep = 0;
  hasUserHandleStep = false;

}

void 
AvalancheMicroscopic::SetUserHandleAttachment(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleAttachment = f;
  hasUserHandleAttachment = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleAttachment() {
  
  userHandleAttachment = 0;
  hasUserHandleAttachment = false;
  
}

void 
AvalancheMicroscopic::SetUserHandleInelastic(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleInelastic = f;
  hasUserHandleInelastic = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleInelastic() {
  
  userHandleInelastic = 0;
  hasUserHandleInelastic = false;
  
}

void 
AvalancheMicroscopic::SetUserHandleIonisation(
    void (*f)(double x, double y, double z, double t, 
              int type, int level, Medium* m)) {
         
  userHandleIonisation = f;
  hasUserHandleIonisation = true;
  
}

void 
AvalancheMicroscopic::UnsetUserHandleIonisation() {
  
  userHandleIonisation = 0;
  hasUserHandleIonisation = false;
  
}

bool 
AvalancheMicroscopic::DriftElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0) {

  // Clear the list of electrons and photons.
  endpointsElectrons.clear(); 
  endpointsHoles.clear();
  photons.clear();
  // Reset the particle counters.
  nPhotons = nElectrons = nHoles = nIons = 0; 
  nElectronEndpoints = nHoleEndpoints = 0;

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, false, false);

}

bool 
AvalancheMicroscopic::AvalancheElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0) {

  // Clear the list of electrons, holes and photons.
  endpointsElectrons.clear();
  endpointsHoles.clear();
  photons.clear();

  // Reset the particle counters.
  nPhotons = nElectrons = nHoles = nIons = 0; 
  nElectronEndpoints = nHoleEndpoints = 0;

  return TransportElectron(x0, y0, z0, t0, e0, dx0, dy0, dz0, true, false);

}

bool 
AvalancheMicroscopic::TransportElectron(
    const double x0, const double y0, const double z0, const double t0, 
    const double e0, const double dx0, const double dy0, const double dz0,
    const bool aval, bool hole) {
  
  // Make sure that the sensor is defined.
  if (sensor == 0) {
    std::cerr << className << "::TransportElectron:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium = 0;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::TransportElectron:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  }
  if (medium == 0) {
    std::cerr << className << "::TransportElectron:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  }

  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << className << "::TransportElectron:\n";
    std::cerr << "    Medium at initial position does not provide " 
              << " microscopic tracking data.\n";
    return false;
  }

  // If the medium is a semiconductor, use "band structure" stepping.
  bool useBandStructure = useBandStructureDefault;
  if (medium->IsSemiconductor() && useBandStructureDefault) {
    useBandStructure = true;
  } else {
    useBandStructure = false;
  }
  if (debug) {
    std::cout << className << "::TransportElectron:\n";
    std::cout << "    Starting to drift in medium " 
              << medium->GetName() << ".\n";
  }
  
  // Get the id number of the drift medium.
  int id = medium->GetId();    
  
  // Numerical prefactors in equation of motion
  const double c1 = SpeedOfLight * sqrt(2. / ElectronMass);
  const double c2 = c1 * c1 / 4.;

  // Temporary stack of photons produced in the de-excitation cascade.
  std::vector<double> stackPhotonsTime;   stackPhotonsTime.clear();
  std::vector<double> stackPhotonsEnergy; stackPhotonsEnergy.clear();
    
  // Electric and magnetic field
  double ex = 0., ey = 0., ez = 0., emag = 0.;
  double bx = 0., by = 0., bz = 0., bmag = 0.;
  int status = 0;
  // Angle between electric and magnetic field
  double cbtheta = 1., sbtheta = 0.;
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
  stack.clear();     
  // Add the initial electron to the stack.
  electron newElectron;
  newElectron.status = 0;
  if (hole) {
    newElectron.hole = true;
  } else {
    newElectron.hole = false;
  }
  newElectron.x0 = x0;  newElectron.x = x0;
  newElectron.y0 = y0;  newElectron.y = y0;
  newElectron.z0 = z0;  newElectron.z = z0;
  newElectron.t0 = t0;  newElectron.t  = t0;
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
  newElectron.driftLine.clear();
  stack.push_back(newElectron);
  if (hole) {
    ++nHoles;
  } else {
    ++nElectrons;
  }

  if (useBandStructure) {
    // With band structure, (kx, ky, kz) represents the momentum.
    // No normalization in this case.
    medium->GetElectronMomentum(std::max(e0, Small), kx, ky, kz, band);
    stack[0].kx = kx;
    stack[0].ky = ky;
    stack[0].kz = kz;
    stack[0].band = band;
  } else {
    stack[0].band = 0;
    band = 0;
    // Check the given initial direction.
    const double k = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
    if (fabs(k) < Small) {
      // Direction has zero norm, draw a random direction.
      const double phi = TwoPi * RndmUniform();
      const double ctheta = 1. - 2. * RndmUniform();
      const double stheta = sqrt(1. - ctheta * ctheta);
      stack[0].kx = cos(phi) * stheta;
      stack[0].ky = sin(phi) * stheta;
      stack[0].kz = ctheta;
    } else {
      // Normalise the direction to 1.
      stack[0].kx /= k; 
      stack[0].ky /= k; 
      stack[0].kz /= k;
    }
  }

  // Get the null-collision rate.
  double fLim = medium->GetElectronNullCollisionRate(band);
  if (fLim <= 0.) {
    std::cerr << className << "::TransportElectron:\n";
    std::cerr << "    Got null-collision rate <= 0.\n";
    return false;
  }

  // Status flag
  bool ok = true;
  while (1) {
    // If the list of electrons/holes is exhausted, we're done.
    const int nSize = stack.size();
    if (nSize <= 0) break;
    // Loop over all electrons/holes in the avalanche.
    for (int iE = nSize; iE--;) {
      // Get an electron/hole from the stack.
      x = stack[iE].x; 
      y = stack[iE].y; 
      z = stack[iE].z;
      t = stack[iE].t; 
      energy = stack[iE].energy; 
      band = stack[iE].band; 
      kx = stack[iE].kx; 
      ky = stack[iE].ky; 
      kz = stack[iE].kz;
      hole = stack[iE].hole;

      ok = true;
      
      // Count number of collisions between updates.
      int nCollTemp = 0;

      // Get the local electric field and medium.
      sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Sign change for electrons.
      if (!hole) {
        ex = -ex; ey = -ey; ez = -ez;
      }
      
      if (debug) {
        std::cout << className << "::TransportElectron:\n";
        if (hole) {
          std::cout << "    Drifting hole " << iE << ".\n";
        } else {
          std::cout << "    Drifting electron " << iE << ".\n";
        }
        std::cout << "    Field [V/cm] at (" 
                  << x  << ", " << y  << ", " << z  << "): "
                  << ex << ", " << ey << ", " << ez << "\n";
        std::cout << "    Status: " << status << "\n";
        std::cout << "    Medium: " << medium->GetName() << "\n";
      }

      if (status != 0) {
        // Electron/hole is not inside a drift medium.
        stack[iE].x = x; 
        stack[iE].y = y; 
        stack[iE].z = z;
        stack[iE].t = t; 
        stack[iE].energy = energy; 
        stack[iE].band = band;
        stack[iE].kx = kx; 
        stack[iE].ky = ky; 
        stack[iE].kz = kz;
        stack[iE].status = StatusLeftDriftMedium;
        if (hole) {
          endpointsHoles.push_back(stack[iE]);
        } else {
          endpointsElectrons.push_back(stack[iE]);
        }
        stack.erase(stack.begin() + iE);
        if (debug) {
          std::cout << className << "::TransportElectron:\n";
          if (hole) {
            std::cout << "    Hole left the drift medium.\n";  
          } else {
            std::cout << "    Electron left the drift medium.\n";
          }
          std::cout << "    At " << x << ", " << y << "," << z << "\n";
        }
        continue;
      }

      // If switched on, get the local magnetic field.
      if (useBfield) {
        sensor->MagneticField(x, y, z, bx, by, bz, status);
        if (hole) {
          bx *=  Tesla2Internal; 
          by *=  Tesla2Internal;
          bz *=  Tesla2Internal;
        } else {
          bx *= -Tesla2Internal; 
          by *= -Tesla2Internal; 
          bz *= -Tesla2Internal;
        }
        // Make sure that neither E nor B are zero.
        bmag = sqrt(bx * bx + by * by + bz * bz);
        emag = sqrt(ex * ex + ey * ey + ez * ez);
        if (bmag > Small && emag > Small) bOk = true;
        else bOk = false;
      }
     
      // Trace the electron/hole. 
      while (1) {

        bool isNullCollision = false;

        // Make sure the electron energy exceeds the transport cut.
        if (energy < deltaCut) {
          stack[iE].x = x; 
          stack[iE].y = y; 
          stack[iE].z = z;
          stack[iE].t = t; 
          stack[iE].energy = energy; 
          stack[iE].band = band;
          stack[iE].kx = kx; 
          stack[iE].ky = ky; 
          stack[iE].kz = kz;
          stack[iE].status = StatusBelowTransportCut;
          if (hole) {
            endpointsHoles.push_back(stack[iE]);
          } else {
            endpointsElectrons.push_back(stack[iE]);
          }
          stack.erase(stack.begin() + iE);
          if (debug) {
            std::cout << className << "::TransportElectron:\n";
            std::cout << "    Kinetic energy (" << energy << ")"
                      << " below transport cut.\n";
          }
          ok = false;
          break;
        }
        
        // Fill the energy distribution histogram.
        if (hole && hasHoleEnergyHistogram) {
          histHoleEnergy->Fill(energy);
        } else if (!hole && hasElectronEnergyHistogram) {
          histElectronEnergy->Fill(energy);
        }
        
        if (medium->GetId() != id) {
          // Medium has changed.
          if (!medium->IsMicroscopic()) {
            // Electron/hole has left the microscopic drift medium.
            stack[iE].x = x; 
            stack[iE].y = y; 
            stack[iE].z = z;
            stack[iE].t = t; 
            stack[iE].energy = energy;
            stack[iE].band = band;
            stack[iE].kx = kx; 
            stack[iE].ky = ky; 
            stack[iE].kz = kz;
            stack[iE].status = StatusLeftDriftMedium;
            if (hole) {
              endpointsHoles.push_back(stack[iE]);
            } else {
              endpointsElectrons.push_back(stack[iE]);
            }
            stack.erase(stack.begin() + iE);
            ok = false;
            if (debug) {
              std::cout << className << "::TransportElectron:\n";
              std::cout << "    Medium at " << x << ", " << y << ", " << z 
                        << " does not have microscopic data.\n";
            }
            break;
          }
          id = medium->GetId();
          if (medium->IsSemiconductor() && useBandStructureDefault) {
            useBandStructure = true;
          } else {
            useBandStructure = false;
          }
          // Update the null-collision rate.
          fLim = medium->GetElectronNullCollisionRate(band);
          if (fLim <= 0.) {
            std::cerr << className << "::TransportElectron:\n"; 
            std::cerr << "    Got null-collision rate <= 0.\n";
            return false;
          }          
        }

        if (useBfield && bOk) {
          // Calculate the cyclotron frequency.
          wb = OmegaCyclotronOverB * bmag;
          // Calculate the angle between E and B vector.
          cbtheta = (ex * bx + ey * by + ez * bz) / (emag * bmag);
          sbtheta = sqrt(1. - cbtheta * cbtheta);
          
          // Rotate the direction vector into the local coordinate system.
          ComputeRotationMatrix(bx, by, bz, bmag, ex, ey, ez);
          RotateGlobal2Local(kx, ky, kz);
 
          // Calculate the electric field in the rotated system.
          RotateGlobal2Local(ex, ey, ez);

          // Calculate the velocity vector in the local frame.
          const double v = c1 * sqrt(energy);
          vx = v * kx; vy = v * ky; vz = v * kz;
          
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
          vx = v * kx; vy = v * ky; vz = v * kz;
          
          a1 = vx * ex + vy * ey + vz * ez;
          a2 = c2 * (ex * ex + ey * ey + ez * ez);
        }

        if (hasUserHandleStep) {
          userHandleStep(x, y, z, t, energy, kx, ky, kz, hole);
        }
  
        // Determine the timestep.
        dt = 0.;
        while (1) {
          // Sample the flight time.
          r = RndmUniformPos();
          dt += - log(r) / fLim;
          // Calculate the energy after the proposed step.
          if (useBfield && bOk) {
            cwt = cos(wb * dt); swt = sin(wb * dt);
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt + 
                                 a4 * (a3 * (1. - cwt) + vz * swt), 
                                 Small);
          } else if (useBandStructure) {
            newEnergy = std::max(medium->GetElectronEnergy(
                                              kx + ex * dt * SpeedOfLight,
                                              ky + ey * dt * SpeedOfLight,
                                              kz + ez * dt * SpeedOfLight, 
                                              newVx, newVy, newVz, band), 
                                 Small);
          } else {
            newEnergy = std::max(energy + (a1 + a2 * dt) * dt, Small);
          }
          // Get the real collision rate at the updated energy.
          double fReal = medium->GetElectronCollisionRate(newEnergy, band);
          if (fReal <= 0.) {
            std::cerr << className << "::TransportElectron:\n";
            std::cerr << "    Got collision rate <= 0.\n";
            std::cerr << "    At " << newEnergy << " eV (band " << band << ").\n";
            return false;
          }
          if (fReal > fLim) {
            // Real collision rate is higher than null-collision rate.
            dt += log(r) / fLim;
            // Increase the null collision rate and try again.
            std::cerr << className << "::TransportElectron:\n";
            std::cerr << "    Increasing null-collision rate by 5%.\n"; 
            if (useBandStructure) std::cerr << "    Band " << band << "\n";
            fLim *= 1.05;
            continue;
          }
          // Check for real or null collision.
          if (RndmUniform() <= fReal / fLim) break;
          if (useNullCollisionSteps) {
            isNullCollision = true;
            break;
          }
        }
        if (!ok) break;

        // Increase the collision counter.
        ++nCollTemp;

        // Update the directions (at instant before collision)
        // and calculate the proposed new position.
        if (useBfield && bOk) {
          // Calculate the new velocity.
          newVx = vx + 2. * c2 * ex * dt;
          newVy = vz * swt - a3 * cwt + ez / bmag;
          newVz = vz * cwt + a3 * swt;
          // Normalise and rotate back to the lab frame.
          const double v = sqrt(newVx * newVx + newVy * newVy + 
                                newVz * newVz);
          newKx = newVx / v; newKy = newVy / v; newKz = newVz / v; 
          RotateLocal2Global(newKx, newKy, newKz);
          // Calculate the step in coordinate space.
          vx += c2 * ex * dt;
          ky = (vz * (1. - cwt) - a3 * swt) / (wb * dt) + ez / bmag;
          kz = (vz * swt + a3 * (1. - cwt)) / (wb * dt); 
          vy = ky; vz = kz;
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
        sensor->ElectricField(x + vx * dt, y + vy * dt, z + vz * dt, 
                              ex, ey, ez, medium, status);
        if (!hole) {
          ex = -ex; ey = -ey; ez = -ez;
        }
        
        // Check if the electron is still inside a drift medium.
        if (status != 0) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection.
          stack[iE].x = x; 
          stack[iE].y = y; 
          stack[iE].z = z;
          stack[iE].t = t; 
          stack[iE].energy = energy;
          double dx = vx * dt, dy = vy * dt, dz = vz * dt;
          double d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d; dy /= d; dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx; yM = y + d * dy; zM = z + d * dz; 
            // Check if the mid-point is inside the drift medium.
            sensor->ElectricField(xM, yM, zM, ex, ey, ez, medium, status);
            if (status == 0) {
              x = xM; y = yM; z = zM; t += dt;
            } 
          }
          // Place the endpoint OUTSIDE the drift medium
          x += d * dx; y += d * dy; z += d * dz; 
          if (useSignal) {
            if (hole) {
              sensor->AddSignal(+1, stack[iE].t, 
                                t - stack[iE].t, 
                                0.5 * (x + stack[iE].x), 
                                0.5 * (y + stack[iE].y),
                                0.5 * (z + stack[iE].z), 
                                vx, vy, vz);
            } else {
              sensor->AddSignal(-1, stack[iE].t,
                                t - stack[iE].t,
                                0.5 * (x + stack[iE].x),
                                0.5 * (y + stack[iE].y),
                                0.5 * (z + stack[iE].z),
                                vx, vy, vz);
            }
          }
          stack[iE].x = x; 
          stack[iE].y = y; 
          stack[iE].z = z;
          stack[iE].t = t;
          stack[iE].kx = newKx; 
          stack[iE].ky = newKy; 
          stack[iE].kz = newKz;
          stack[iE].status = StatusLeftDriftMedium;
          if (hole) {
            endpointsHoles.push_back(stack[iE]);
          } else {
            endpointsElectrons.push_back(stack[iE]);
          }
          stack.erase(stack.begin() + iE);
          ok = false;
          if (debug) {
            std::cout << className << "::TransportElectron:\n";
            if (hole) {
              std::cout << "    Hole left the drift medium.\n";
            } else {
              std::cout << "    Electron left the drift medium.\n";
            }
            std::cout << "    At " << x << ", " << y << "," << z << "\n";
          }
          break;
        }

        // Check if the new position is inside the user area.
        if (!sensor->IsInArea(x + vx * dt, y + vy * dt, z + vz * dt)) {
          // Try to terminate the drift line close to the boundary
          // by means of iterative bisection.
          stack[iE].x = x; 
          stack[iE].y = y; 
          stack[iE].z = z;
          stack[iE].t = t; 
          stack[iE].energy = energy;
          double dx = vx * dt, dy = vy * dt, dz = vz * dt;
          double d = sqrt(dx * dx + dy * dy + dz * dz);
          if (d > 0) {
            dx /= d; dy /= d; dz /= d;
          }
          // Mid-point
          double xM = x, yM = y, zM = z;
          while (d > BoundaryDistance) {
            d *= 0.5;
            dt *= 0.5;
            xM = x + d * dx; yM = y + d * dy; zM = z + d * dz; 
            // Check if the mid-point is inside the drift area.
            if (sensor->IsInArea(xM, yM, zM)) {
              x = xM; y = yM; z = zM; t += dt;
            }
          }
          // Place the endpoint OUTSIDE the drift area.
          x += d * dx; y += d * dy; z += d * dz;

          // If switched on, calculate the induced signal over this step.
          if (useSignal) {
            if (hole) {
              sensor->AddSignal(+1, stack[iE].t,
                                t - stack[iE].t,
                                0.5 * (x + stack[iE].x),
                                0.5 * (y + stack[iE].y),
                                0.5 * (z + stack[iE].z),
                                vx, vy, vz);
            } else {
              sensor->AddSignal(-1, stack[iE].t, 
                                t - stack[iE].t, 
                                0.5 * (x + stack[iE].x), 
                                0.5 * (y + stack[iE].y),
                                0.5 * (z + stack[iE].z), 
                                vx, vy, vz);
            }
          }
          stack[iE].x = x; 
          stack[iE].y = y; 
          stack[iE].z = z;
          stack[iE].t = t;
          stack[iE].kx = newKx; 
          stack[iE].ky = newKy; 
          stack[iE].kz = newKz;
          stack[iE].status = StatusLeftDriftArea;
          if (hole) {
            endpointsHoles.push_back(stack[iE]);
          } else {
            endpointsElectrons.push_back(stack[iE]);
          }
          stack.erase(stack.begin() + iE);
          ok = false;
          if (debug) {
            std::cout << className << "::TransportElectron:\n";
            if (hole) {
              std::cout << "    Hole left the drift area.\n";
            } else {
              std::cout << "    Electron left the drift area.\n";
            }
            std::cout << "    At " << x << ", " << y << ", " << z << "\n";
          }
          break;
        }

        // Check if the electron/hole has crossed a wire.
        double xCross = x, yCross = y, zCross = z;
        if (sensor->IsWireCrossed(x, y, z, 
                                  x + vx * dt, y + vy * dt, z + vz * dt,
                                  xCross, yCross, zCross)) {
          // If switched on, calculated the induced signal over this step.
          if (useSignal) {
            dt = sqrt(pow(xCross - x, 2) + 
                      pow(yCross - y, 2) + 
                      pow(zCross - z, 2)) / 
                 sqrt(vx * vx + vy * vy + vz * vz); 
            if (hole) {
              sensor->AddSignal(+1, t, dt, 0.5 * (x + xCross),
                                           0.5 * (y + yCross),
                                           0.5 * (z + zCross), vx, vy, vz);
            } else {
              sensor->AddSignal(-1, t, dt, 0.5 * (x + xCross),
                                           0.5 * (y + yCross),
                                           0.5 * (z + zCross), vx, vy, vz);
            }
          }
          stack[iE].x = xCross; 
          stack[iE].y = yCross; 
          stack[iE].z = zCross;
          stack[iE].t = t + dt;
          stack[iE].kx = newKx; 
          stack[iE].ky = newKy; 
          stack[iE].kz = newKz;
          stack[iE].status = StatusLeftDriftMedium;
          if (hole) {
            endpointsHoles.push_back(stack[iE]);
          } else {
            endpointsElectrons.push_back(stack[iE]);
          }
          stack.erase(stack.begin() + iE);
          ok = false;
          if (debug) {
            std::cout << className << "::TransportElectron:\n";
            std::cout << "    Electron/hole hit a wire.\n";
            std::cout << "    At " << x << ", " << y << "," << z << "\n";
          }
          break;
        }
        
        // If switched on, calculate the induced signal.
        if (useSignal) {
          if (hole) {
            sensor->AddSignal(+1, t, dt, x + 0.5 * vx * dt,
                                         y + 0.5 * vy * dt,
                                         z + 0.5 * vz * dt, vx, vy, vz);
          } else {
            sensor->AddSignal(-1, t, dt, x + 0.5 * vx * dt, 
                                         y + 0.5 * vy * dt,
                                         z + 0.5 * vy * dt, vx, vy, vz);
          }
        }

        // Update the coordinates.
        x += vx * dt; y += vy * dt; z += vz * dt; t += dt;

        // If switched on, get the magnetic field at the new location.
        if (useBfield) {
          sensor->MagneticField(x, y, z, bx, by, bz, status);
          if (hole) {
            bx *=  Tesla2Internal;
            by *=  Tesla2Internal;
            bz *=  Tesla2Internal;
          } else {
            bx *= -Tesla2Internal; 
            by *= -Tesla2Internal; 
            bz *= -Tesla2Internal;
          }
          // Make sure that neither E nor B are zero.
          bmag = sqrt(bx * bx + by * by + bz * bz);
          emag = sqrt(ex * ex + ey * ey + ez * ez);
          if (bmag > Small && emag > Small) bOk = true;
          else bOk = false;
        }

        if (isNullCollision) {
          energy = newEnergy;
          kx = newKx; ky = newKy; kz = newKz;
          continue;
        }
        
        // Get the collision type and parameters.
        medium->GetElectronCollision(newEnergy, cstype, level, 
                                     energy, newKx, newKy, newKz, 
                                     nion, ndxc, band);

        // If activated, histogram the distance with respect to the
        // last collision.
        if (hasDistanceHistogram && histDistance != 0 &&
            nDistanceHistogramTypes > 0) {
          for (int iType = nDistanceHistogramTypes; iType--;) {
            if (distanceHistogramType[iType] != cstype) continue;
            if (debug) {
              std::cout << className << "::TransportElectron:\n";
              std::cout << "    Collision type: " << cstype << "\n";
              std::cout << "    Fill distance histogram.\n";
              getchar();
            }
            switch (distanceOption) {
              case 'x':
                histDistance->Fill(stack[iE].xLast - x);
                break;
              case 'y':
                histDistance->Fill(stack[iE].yLast - y);
                break;
              case 'z':
                histDistance->Fill(stack[iE].zLast - z);
                break;
              case 'r':
                const double r2 = pow(stack[iE].xLast - x, 2) + 
                                  pow(stack[iE].yLast - y, 2) + 
                                  pow(stack[iE].zLast - z, 2);
                histDistance->Fill(sqrt(r2));
                break;
            }
            stack[iE].xLast = x; 
            stack[iE].yLast = y; 
            stack[iE].zLast = z;
            break;  
          }
        }

        switch (cstype) {
          // Elastic collision
          case ElectronCollisionTypeElastic:
            break;
          // Ionising collision
          case ElectronCollisionTypeIonisation:
            if (usePlotting && plotIonisations) {
              viewer->AddIonisationMarker(x, y, z);
            }
            if (hasUserHandleIonisation) {
              userHandleIonisation(x, y, z, t, cstype, level, medium);
            }
            for (int j = nion; j--;) {
              int itype;
              double esec;
              medium->GetIonisationProduct(j, itype, esec);
              if (itype == IonProdTypeElectron) {
                esec = std::max(esec, Small);
                if (hasSecondaryHistogram) histSecondary->Fill(esec);
                // Add the secondary electron to the stack.
                newElectron = stack[iE];
                newElectron.hole = false;
                newElectron.x0 = x; newElectron.x = x;
                newElectron.y0 = y; newElectron.y = y;
                newElectron.z0 = z; newElectron.z = z;
                newElectron.t0 = t; newElectron.t = t;
                newElectron.energy = esec;
                newElectron.e0 = newElectron.energy;
                if (useBandStructure) {
                  newElectron.band = -1;
                  medium->GetElectronMomentum(esec, 
                                              newElectron.kx, 
                                              newElectron.ky,
                                              newElectron.kz, 
                                              newElectron.band);
                } else {
                  // Randomise the secondary electron direction.
                  const double phi = TwoPi * RndmUniform();
                  const double ctheta = 1. - 2. * RndmUniform();
                  const double stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                }
                newElectron.status = 0;
                newElectron.driftLine.clear();
                if (aval && (sizeCut <= 0 || nSize < sizeCut)) {
                  stack.push_back(newElectron);
                }
                // Increment the electron counter.
                ++nElectrons;
              } else if (itype == IonProdTypeHole) {
                esec = std::max(esec, Small);
                // Add the secondary hole to the stack.
                newElectron = stack[iE];
                newElectron.hole = true;
                newElectron.x0 = x; newElectron.x = x;
                newElectron.y0 = y; newElectron.y = y;
                newElectron.z0 = z; newElectron.z = z;
                newElectron.t0 = t; newElectron.t = t;
                newElectron.energy = esec;
                newElectron.e0 = newElectron.energy;
                if (useBandStructure) {
                  newElectron.band = -1;
                  medium->GetElectronMomentum(esec, 
                                              newElectron.kx, 
                                              newElectron.ky,
                                              newElectron.kz, 
                                              newElectron.band);
                } else {
                  // Randomise the secondary hole direction.
                  const double phi = TwoPi * RndmUniform();
                  const double ctheta = 1. - 2. * RndmUniform();
                  const double stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                }
                newElectron.status = 0;
                newElectron.driftLine.clear();
                if (aval && (sizeCut <= 0 || nSize < sizeCut)) {
                  stack.push_back(newElectron);
                }
                // Increment the hole counter.
                ++nHoles;
              } else if (itype == IonProdTypeIon) {
                ++nIons;
              }
            }
            if (debug) {
              std::cout << className << "::TransportElectron:\n";
              std::cout << "    Ionisation.\n";
              std::cout << "    At " << x << "," << y << "," << z << "\n"; 
            }
            break;
          // Attachment
          case ElectronCollisionTypeAttachment:
            if (usePlotting && plotAttachments) {
              viewer->AddAttachmentMarker(x, y, z);
            }
            if (hasUserHandleAttachment) {
              userHandleAttachment(x, y, z, t, cstype, level, medium);
            }
            stack[iE].x = x; 
            stack[iE].y = y; 
            stack[iE].z = z;
            stack[iE].t = t; 
            stack[iE].energy = energy;
            stack[iE].status = StatusAttached;
            if (hole) {
              endpointsHoles.push_back(stack[iE]);
              --nHoles;
            } else {
              endpointsElectrons.push_back(stack[iE]);
              --nElectrons;
            }
            stack.erase(stack.begin() + iE);
            ok = false;
            break;
          // Inelastic collision
          case ElectronCollisionTypeInelastic:
            if (hasUserHandleInelastic) {
              userHandleInelastic(x, y, z, t, cstype, level, medium);
            }
            break;
          // Excitation
          case ElectronCollisionTypeExcitation:
            if (usePlotting && plotExcitations) {
              viewer->AddExcitationMarker(x, y, z);
            }
            if (hasUserHandleInelastic) {
              userHandleInelastic(x, y, z, t, cstype, level, medium);
            }
            if (ndxc > 0) {
              // Get the electrons and photons produced in the 
              // deexcitation cascade.
              double tDxc = 0., sDxc = 0., eDxc = 0.;
              int typeDxc = 0;
              stackPhotonsTime.clear(); stackPhotonsEnergy.clear();
              for (int j = ndxc; j--;) {
                if (!medium->GetDeexcitationProduct(j, tDxc, sDxc,
                                                    typeDxc, eDxc)) {
                  std::cerr << className << "::TransportElectron:\n";
                  std::cerr << "    Cannot retrieve deexcitation product "
                            << j << "/" << ndxc << ".\n";
                  break;
                }
                
                if (typeDxc == DxcProdTypeElectron) {
                  if (!aval) continue;
                  // Penning ionisation
                  newElectron = stack[iE];
                  // Randomise the point of creation.
                  double phi = TwoPi * RndmUniform();
                  double ctheta = 1. - 2 * RndmUniform();
                  double stheta = sqrt(1. - ctheta * ctheta);
                  const double xDxc = x + sDxc * cos(phi) * stheta;
                  const double yDxc = y + sDxc * sin(phi) * stheta;
                  const double zDxc = z + sDxc * ctheta;
                  // Get the electric field and medium at this location.
                  Medium* dxcMedium = 0;
                  double fx = 0., fy = 0., fz = 0.;
                  sensor->ElectricField(xDxc, yDxc, zDxc, 
                                        fx, fy, fz, dxcMedium, status);
                  // Check if this location is inside a drift medium.
                  if (status != 0) continue;
                  // Check if this location is inside the drift area.
                  if (!sensor->IsInArea(xDxc, yDxc, zDxc)) continue;
                  // Make sure we haven't jumped across a wire.
                  double xCross, yCross, zCross;
                  if (sensor->IsWireCrossed(x, y, z, xDxc, yDxc, zDxc, 
                                            xCross, yCross, zCross)) {
                    continue;
                  } 
                  newElectron.x0 = xDxc; newElectron.x = xDxc;
                  newElectron.y0 = yDxc; newElectron.y = yDxc;
                  newElectron.z0 = zDxc; newElectron.z = zDxc;
                  newElectron.t0 = t + tDxc; newElectron.t = t + tDxc;
                  newElectron.energy = std::max(eDxc, Small);
                  newElectron.e0 = newElectron.energy;
                  // Randomise the initial direction.
                  phi = TwoPi * RndmUniform();
                  ctheta = 1. - 2 * RndmUniform();
                  stheta = sqrt(1. - ctheta * ctheta);
                  newElectron.kx = cos(phi) * stheta;
                  newElectron.ky = sin(phi) * stheta;
                  newElectron.kz = ctheta;
                  newElectron.status = 0;
                  newElectron.driftLine.clear();
                  // Add the electron to the list.
                  stack.push_back(newElectron);
                  // Increment the electron and ion counters.
                  ++nElectrons; ++nIons;
                } else if (typeDxc == DxcProdTypePhoton && usePhotons && 
                           eDxc > gammaCut) {
                  // Radiative de-excitation
                  stackPhotonsTime.push_back(t + tDxc);
                  stackPhotonsEnergy.push_back(eDxc);
                }
              }
                
              // Transport the photons (if any)
              const int nSizePhotons = stackPhotonsTime.size();
              for (int j = nSizePhotons; j--;) {
                if (aval) {
                  TransportPhoton(x, y, z, 
                                  stackPhotonsTime[j], 
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
            std::cerr << className << "::TransportElectron:\n"; 
            std::cerr << "    Unknown collision type.\n";
            ok = false;
            break;
        }

        // Continue with the next electron/hole?
        if (!ok || 
            nCollTemp > nCollSkip || 
            cstype == ElectronCollisionTypeIonisation || 
            (plotExcitations && cstype == ElectronCollisionTypeExcitation) ||
            (plotAttachments && cstype == ElectronCollisionTypeAttachment)) {
          break;
        }
        kx = newKx; ky = newKy; kz = newKz;

      }
      
      if (!ok) continue;
      
      if (!useBandStructure) {
        // Normalise the direction vector.
        const double k = sqrt(kx * kx + ky * ky + kz * kz);
        kx /= k; ky /= k; kz /= k;
      }
      // Update the stack.
      stack[iE].energy = energy; 
      stack[iE].t = t;
      stack[iE].x = x; 
      stack[iE].y = y; 
      stack[iE].z = z;
      stack[iE].kx = kx; 
      stack[iE].ky = ky; 
      stack[iE].kz = kz;
      // Add a new point to the drift line (if enabled).
      if (useDriftLines) {
        point newPoint;
        newPoint.x = x; newPoint.y = y; newPoint.z = z; newPoint.t = t;
        stack[iE].driftLine.push_back(newPoint);
      }
    }
  }
  nElectronEndpoints = endpointsElectrons.size();
  nHoleEndpoints     = endpointsHoles.size();

  // Calculate the induced charge.
  if (useInducedCharge) {
    for (int i = nElectronEndpoints; i--;) {
      sensor->AddInducedCharge(-1, 
                               endpointsElectrons[i].x0, 
                               endpointsElectrons[i].y0, 
                               endpointsElectrons[i].z0,
                               endpointsElectrons[i].x,  
                               endpointsElectrons[i].y,  
                               endpointsElectrons[i].z);
    }
    for (int i = nHoleEndpoints; i--;) {
      sensor->AddInducedCharge(+1,
                               endpointsHoles[i].x0,
                               endpointsHoles[i].y0,
                               endpointsHoles[i].z0,
                               endpointsHoles[i].x,
                               endpointsHoles[i].y,
                               endpointsHoles[i].z);
    }
  }

  // Plot the drift paths and photon tracks.
  if (usePlotting) {
    // Electrons
    for (int i = nElectronEndpoints; i--;) {
      const int np = GetNumberOfElectronDriftLinePoints(i);
      int jL;
      if (np <= 0) continue;
      viewer->NewElectronDriftLine(np, jL, 
                                   endpointsElectrons[i].x0, endpointsElectrons[i].y0, 
                                   endpointsElectrons[i].z0);
      for (int jP = np; jP--;) {
        GetElectronDriftLinePoint(x, y, z, t, jP, i);
        viewer->SetDriftLinePoint(jL, jP, x, y, z);
      }
    }
    // Holes
    for (int i = nHoleEndpoints; i--;) {
      const int np = GetNumberOfHoleDriftLinePoints(i);
      int jL;
      if (np <= 0) continue;
      viewer->NewHoleDriftLine(np, jL, 
                               endpointsHoles[i].x0, endpointsHoles[i].y0, 
                               endpointsHoles[i].z0);
      for (int jP = np; jP--;) {
        GetHoleDriftLinePoint(x, y, z, t, jP, i);
        viewer->SetDriftLinePoint(jL, jP, x, y, z);
      }
    }
    // Photons
    for (int i = nPhotons; i--;) {
      viewer->NewPhotonTrack(photons[i].x0, photons[i].y0, photons[i].z0,
                             photons[i].x1, photons[i].y1, photons[i].z1);
    } 
  }
  return true;
    
}

void
AvalancheMicroscopic::TransportPhoton(const double x0, const double y0, 
                                      const double z0, 
                                      const double t0, const double e0) {

  // Make sure that the sensor is defined.
  if (sensor == 0) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  // Make sure that the starting point is inside a medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  }
  
  // Make sure that the medium is "driftable" and microscopic.
  if (!medium->IsDriftable() || !medium->IsMicroscopic()) {
    std::cerr << className << "::TransportPhoton:\n";
    std::cerr << "    Medium at initial position does not provide " 
              << " microscopic tracking data.\n";
    return;
  }
  
  if (debug) {
    std::cout << className << "::TransportPhoton:\n";
    std::cout << "    Starting photon transport in medium " 
              << medium->GetName() << ".\n";
  }
  
  // Get the id number of the drift medium.
  int id = medium->GetId();

  // Position 
  double x = x0, y = y0, z = z0;
  double t = t0;
  // Initial direction (randomised)
  double ctheta = 1. - 2 * RndmUniform();
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

  dt = - log(RndmUniformPos()) / f;
  t += dt;
  dt *= SpeedOfLight;
  x += dt * dx; y += dt * dy; z += dt * dz;

  // Check if the photon is still inside a medium.
  if (!sensor->GetMedium(x, y, z, medium) || medium->GetId() != id) {
    // Try to terminate the photon track close to the boundary
    // by means of iterative bisection.
    dx *= dt; dy *= dt; dz *= dt;
    x -= dx; y -= dy; z -= dz;
    double delta = sqrt(dx * dx + dy * dy + dz * dz);
    if (delta > 0) {
      dx /= delta; dy /= delta; dz /= delta;
    }
    // Mid-point
    double xM = x, yM = y, zM = z;
    while (delta > BoundaryDistance) {
      delta *= 0.5;
      dt *= 0.5;
      xM = x + delta * dx; yM = y + delta * dy; zM = z + delta * dz; 
      // Check if the mid-point is inside the drift medium.
      if (sensor->GetMedium(xM, yM, zM, medium) && medium->GetId() == id) {
        x = xM; y = yM; z = zM; t += dt;
      }
    }
    photon newPhoton;
    newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
    newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
    newPhoton.energy = e0;
    newPhoton.status = StatusLeftDriftMedium;
    photons.push_back(newPhoton);
    ++nPhotons;
    return;
  }

  if (!medium->GetPhotonCollision(e, type, level, e1, ctheta, 
                                  nsec, esec)) return;
 
  if (type == PhotonCollisionTypeIonisation) {
    // Randomise secondary electron direction.
    phi = TwoPi * RndmUniform();
    ctheta = 1. - 2. * RndmUniform();
    stheta = sqrt(1. - ctheta * ctheta);
    // Add the secondary electron to the stack.
    electron newElectron;
    newElectron.x0 = x; newElectron.x = x;
    newElectron.y0 = y; newElectron.y = y;
    newElectron.z0 = z; newElectron.z = z;
    newElectron.t0 = t; newElectron.t = t; 
    newElectron.energy = std::max(esec, Small);
    newElectron.e0 = newElectron.energy;
    newElectron.kx = cos(phi) * stheta;
    newElectron.ky = sin(phi) * stheta;
    newElectron.kz = ctheta;
    newElectron.status = 0;
    newElectron.driftLine.clear();
    stack.push_back(newElectron);
    // Increment the electron and ion counters.        
    ++nElectrons; ++nIons;
  } else if (type == PhotonCollisionTypeExcitation) {
    double tDxc = 0.;
    double sDxc = 0.;
    int typeDxc = 0;
    std::vector<double> stackPhotonsTime;   stackPhotonsTime.clear();
    std::vector<double> stackPhotonsEnergy; stackPhotonsEnergy.clear();
    for (int j = nsec; j--;) {
      if (!medium->GetDeexcitationProduct(j, tDxc, sDxc,
                                          typeDxc, esec)) continue;
      if (typeDxc == DxcProdTypeElectron) {
        // Ionisation
        phi = TwoPi * RndmUniform();
        ctheta = 1. - 2 * RndmUniform();
        stheta = sqrt(1. - ctheta * ctheta);
        // Add the electron to the stack.
        electron newElectron;
        newElectron.x0 = x; newElectron.x = x;
        newElectron.y0 = y; newElectron.y = y;
        newElectron.z0 = z; newElectron.z = z;
        newElectron.t0 = t + tDxc; newElectron.t = t + tDxc; 
        newElectron.energy = std::max(esec, Small);
        newElectron.e0 = newElectron.energy;
        newElectron.kx = cos(phi) * stheta;
        newElectron.ky = sin(phi) * stheta;
        newElectron.kz = ctheta;
        newElectron.status = 0;
        newElectron.driftLine.clear();
        stack.push_back(newElectron);
        // Increment the electron and ion counters.        
        ++nElectrons; ++nIons;
      } else if (typeDxc == DxcProdTypePhoton && 
                 usePhotons && esec > gammaCut) {
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
  newPhoton.x0 = x0; newPhoton.y0 = y0; newPhoton.z0 = z0;
  newPhoton.x1 = x;  newPhoton.y1 = y;  newPhoton.z1 = z;
  newPhoton.energy = e0;
  newPhoton.status = -2;
  photons.push_back(newPhoton);
  ++nPhotons;

}

void 
AvalancheMicroscopic::ComputeRotationMatrix(
    const double bx, const double by, const double bz, const double bmag, 
    const double ex, const double ey, const double ez) {

  // Adopting the Magboltz convention, the stepping is performed
  // in a coordinate system with the B field along the x axis
  // and the electric field at an angle btheta in the x-z plane.

  // Calculate the first rotation matrix (to align B with x axis).
  const double bt = by * by + bz * bz;
  if (bt < Small) {
    // B field is already along axis.
    rb11 = rb22 = rb33 = 1.;
    rb12 = rb13 = rb21 = rb23 = rb31 = rb32 = 0.;
  } else {
    rb11 = bx / bmag;
    rb12 = by / bmag; rb21 = -rb12;
    rb13 = bz / bmag; rb31 = -rb13;
    rb22 = (rb11 * by * by + bz * bz) / bt;
    rb33 = (rb11 * bz * bz + by * by) / bt;
    rb23 = rb32 = (rb11 - 1.) * by * bz / bt;
  }
  // Calculate the second rotation matrix (rotation around x axis).
  const double fy = rb21 * ex + rb22 * ey + rb23 * ez;
  const double fz = rb31 * ex + rb32 * ey + rb33 * ez;
  const double ft = sqrt(fy * fy + fz * fz);
  if (ft < Small) {
    // E and B field are parallel.
    rx22 = rx33 = 1.;
    rx23 = rx32 = 0.;
  } else {
    rx22 = rx33 = fz / ft;
    rx23 = - fy / ft; rx32 = -rx23;
  }

}

void
AvalancheMicroscopic::RotateGlobal2Local(double& dx, double& dy, double& dz) {

  const double dx1 = rb11 * dx + rb12 * dy + rb13 * dz;
  const double dy1 = rb21 * dx + rb22 * dy + rb23 * dz;
  const double dz1 = rb31 * dx + rb32 * dy + rb33 * dz;
  
  dx = dx1;
  dy = rx22 * dy1 + rx23 * dz1;
  dz = rx32 * dy1 + rx33 * dz1;

}

void
AvalancheMicroscopic::RotateLocal2Global(double& dx, double& dy, double& dz) {

  const double dx1 = dx;
  const double dy1 = rx22 * dy + rx32 * dz;
  const double dz1 = rx23 * dy + rx33 * dz;
  
  dx = rb11 * dx1 + rb21 * dy1 + rb31 * dz1;
  dy = rb12 * dx1 + rb22 * dy1 + rb32 * dz1;
  dz = rb13 * dx1 + rb23 * dy1 + rb33 * dz1;

}

}
