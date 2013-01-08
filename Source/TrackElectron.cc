#include <iostream>
#include <cmath>

#include "Sensor.hh"
#include "TrackElectron.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackElectron::TrackElectron() : 
  ready(false),
  x(0.), y(0.), z(0.), t(0.), 
  dx(0.), dy(0), dz(1.),
  nComponents(0), nElectrons(0),
  mediumName(""), mediumDensity(0.), mfp(0.) {
  
  className = "TrackElectron";
  
  // Setup the particle properties.
  q = -1;
  spin = 1;
  mass = ElectronMass;
  isElectron = true;
  SetBetaGamma(3.);
  particleName == "electron";

  components.clear(); 
  electrons.clear(); 
  
}

void
TrackElectron::SetParticle(std::string particle) {

  if (particle != "electron" && 
      particle != "e" && 
      particle != "e-") {
    std::cerr << className << "::SetParticle:\n";
    std::cerr << "    Only electrons can be transported.\n";   
  }
  
}

bool 
TrackElectron::NewTrack(const double x0, const double y0, const double z0,
                        const double t0, 
                        const double dx0, const double dy0, const double dz0) {

  ready = false;
  
  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    return false;
  }
  
  // Get the medium at this location and check if it is "ionisable".
  Medium* medium = 0;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    return false;
  }
  if (!medium->IsIonisable()) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    return false;
  }
  
  // Check if the medium is a gas.
  if (!medium->IsGas()) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Medium at initial position is not a gas.\n";
    return false;
  }
  
  if (!SetupGas(medium)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Properties of medium "
              << medium->GetName() << " are not available.\n";
    return false;
  }
  
  if (!UpdateCrossSection()) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Cross-sections could not be calculated.\n";
    return false;
  }
  
  mediumName = medium->GetName();
  
  x = x0; y = y0; z = z0; t = t0;
  const double dd = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (dd < Small) {
    if (debug) {
      std::cout << className << "::NewTrack:\n";
      std::cout << "    Direction vector has zero norm.\n";
      std::cout << "    Initial direction is randomized.\n";
    }
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    const double phi = TwoPi * RndmUniform();
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
    // Normalize the direction vector.
    dx = dx0 / dd; dy = dy0 / dd; dz = dz0 / dd;
  }
  
  ready = true;
  return true;

}

bool
TrackElectron::GetCluster(double& xcls, double& ycls, double& zcls, 
                          double& tcls, int& ncls, double& edep, double& extra) {

  edep = extra = 0.;
  ncls = 0;
  
  nElectrons = 0;
  electrons.clear();
  
  if (!ready) {
    std::cerr << className << "::GetCluster:\n";
    std::cerr << "    Track not initialized.\n";
    std::cerr << "    Call NewTrack first.\n";
    return false;
  }
  
  // Draw a step length and propagate the electron.
  const double d = - mfp * log(RndmUniformPos());
  x += d * dx; y += d * dy; z += d * dz; 
  t += d / (sqrt(beta2) * SpeedOfLight);
  
  if (!sensor->IsInArea(x, y, z)) {
    ready = false;
    return false;
  }
  
  Medium* medium = 0;
  if (!sensor->GetMedium(x, y, z, medium)) {
    ready = false;
    return false;
  }
  
  if (medium->GetName() != mediumName || 
      medium->GetNumberDensity() != mediumDensity ||
      !medium->IsIonisable()) {
    ready = false;
    return false;
  }
  
  xcls = x; ycls = y; zcls = z; tcls = t;
  const double r = RndmUniform();
  int iComponent = 0;
  for (int i = 0; i < nComponents; ++i) {
    if (r <= RndmUniform()) {
      iComponent = i;
      break;
    }
  }
  
  // Sample secondary electron energy according to 
  // Opal-Beaty-Peterson splitting function.
  const double e0 = ElectronMass * (sqrt(1. / (1. - beta2)) - 1.);
  double esec = components[iComponent].wSplit * 
                tan(RndmUniform() * 
                    atan((e0 - components[iComponent].ethr) / 
                         (2. * components[iComponent].wSplit)));
  esec = components[iComponent].wSplit * 
         pow(esec / components[iComponent].wSplit, 0.9524);
  nElectrons = 1;
  electrons.resize(nElectrons);
  electrons[0].energy = esec;
  electrons[0].x = xcls; electrons[0].y = ycls; electrons[0].z = zcls;

  ncls = nElectrons;
  edep = esec;

  return true;

}

double 
TrackElectron::GetClusterDensity() {

  if (!ready) {
    std::cerr << className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (mfp <= 0.) {
    std::cerr << className << "::GetClusterDensity:\n";
    std::cerr << "    Mean free path is not available.\n";
    return 0.;
  }
  
  return 1. / mfp;
  
}

double 
TrackElectron::GetStoppingPower() {

  if (!ready) {
    std::cerr << className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialised.\n";
    return 0.;
  }
  
  const double prefactor = 4 * Pi * pow(HbarC / ElectronMass, 2);
  const double lnBg2 = log(beta2 / (1. - beta2));

  double dedx = 0.;
  // Primary energy
  const double e0 = ElectronMass * (sqrt(1. / (1. - beta2)) - 1.);
  for (int i = nComponents; i--;) {
    // Calculate the mean number of clusters per cm.
    const double cmean = mediumDensity * components[i].fraction * 
                         (prefactor / beta2) *
                         (components[i].m2Ion * (lnBg2 - beta2) + 
                          components[i].cIon);
    const double ew = (e0 - components[i].ethr) / 
                      (2 * components[i].wSplit);
    // Calculate the mean secondary electron energy.
    const double emean = (components[i].wSplit / (2 * atan(ew))) * 
                         log(1. + ew * ew);
    dedx += cmean * emean;
  }


  return dedx;

}

bool
TrackElectron::SetupGas(Medium* gas) {


  nComponents = 0;
  components.clear();
    
  if (gas == 0) {
    std::cerr << className << "::SetupGas:\n";
    std::cerr << "     Medium is not defined.\n";
    return false;
  }
  
  mediumDensity = gas->GetNumberDensity();
  nComponents = gas->GetNumberOfComponents();
  if (nComponents <= 0) {
    std::cerr << className << "::SetupGas:\n";
    std::cerr << "    Medium composition is not defined.\n";
    nComponents = 0;
    return false;
  }
  components.resize(nComponents);
 
  // Density correction parameters from
  //   R. M. Sternheimer, M. J. Berger, S. M. Seltzer,
  //   Atomic Data and Nuclear Data Tables 30 (1984), 261-271 
  bool ok = true;
  for (int i = nComponents; i--;) {
    std::string gasname = "";
    double frac = 0.;
    gas->GetComponent(i, gasname, frac);
    components[i].fraction = frac;
    components[i].p = 0.;
    if (gasname == "CF4") {
      components[i].m2Ion = 7.2;
      components[i].cIon = 93.;
      components[i].x0Dens = 1.;
      components[i].x1Dens = 0.;
      components[i].cDens = 0.;
      components[i].aDens = 0.;
      components[i].mDens = 0.;
      components[i].ethr = 15.9;
      components[i].wSplit = 19.5;
    } else if (gasname == "Ar") {
      components[i].m2Ion = 3.593;
      components[i].cIon = 39.7;
      components[i].x0Dens = 1.7635;
      components[i].x1Dens = 4.4855;
      components[i].cDens = 11.9480;
      components[i].aDens = 0.19714;
      components[i].mDens = 2.9618;
      components[i].ethr = 15.75961;
      components[i].wSplit = 15.;
    } else if (gasname == "He") {
      components[i].m2Ion = 0.489;
      components[i].cIon = 5.5;
      components[i].x0Dens = 2.2017;
      components[i].x1Dens = 3.6122;
      components[i].cDens = 11.1393;
      components[i].aDens = 0.13443;
      components[i].mDens = 5.8347;
      components[i].ethr = 24.58739;
      components[i].wSplit = 10.5;
    } else if (gasname == "He-3") {
      components[i].m2Ion = 0.489;
      components[i].cIon = 5.5;
      components[i].x0Dens = 2.2017;
      components[i].x1Dens = 3.6122;
      components[i].cDens = 11.1393;
      components[i].aDens = 0.13443;
      components[i].mDens = 5.8347;
      components[i].ethr = 24.58739;
      components[i].wSplit = 10.5;
    } else if (gasname == "Ne") {
      components[i].m2Ion = 1.69;
      components[i].cIon = 17.8;
      components[i].x0Dens = 2.0735;
      components[i].x1Dens = 4.6421;
      components[i].cDens = 11.9041;
      components[i].aDens = 0.08064;
      components[i].mDens = 3.5771;
      components[i].ethr = 21.56454;
      components[i].wSplit = 19.5;
    } else if (gasname == "Kr") {
      components[i].m2Ion = 5.5;
      components[i].cIon = 56.9;
      components[i].x0Dens = 1.7158;
      components[i].x1Dens = 5.0748;
      components[i].cDens = 12.5115;
      components[i].aDens = 0.07446;
      components[i].mDens = 3.4051;
      components[i].ethr = 13.996;
      components[i].wSplit = 21.;
    } else if (gasname == "Xe") {
      components[i].m2Ion = 8.04;
      components[i].cIon = 75.25;
      components[i].x0Dens = 1.5630;
      components[i].x1Dens = 4.7371;
      components[i].cDens = 12.7281;
      components[i].aDens = 0.23314;
      components[i].mDens = 2.7414;
      components[i].ethr = 12.129843;
      components[i].wSplit = 23.7;
    } else if (gasname == "CH4") {
      components[i].m2Ion = 3.75;
      components[i].cIon = 42.5;
      components[i].x0Dens = 1.6263;
      components[i].x1Dens = 3.9716;
      components[i].cDens = 9.5243;
      components[i].aDens = 0.09253;
      components[i].mDens = 3.6257;
      components[i].ethr = 12.65;
      components[i].wSplit = 8.;
    } else if (gasname == "iC4H10") {
      components[i].m2Ion = 15.5;
      components[i].cIon = 160.;
      components[i].x0Dens = 1.3788;
      components[i].x1Dens = 3.7524;
      components[i].cDens = 8.5633;
      components[i].aDens = 0.10852;
      components[i].mDens = 3.4884;
      components[i].ethr = 10.67;
      components[i].wSplit = 7.;
    } else if (gasname == "CO2") {
      components[i].m2Ion = 5.6;
      components[i].cIon = 57.91;
      components[i].x0Dens = 1.6294;
      components[i].x1Dens = 4.1825;
      components[i].aDens = 0.11768;
      components[i].mDens = 3.3227;
      components[i].ethr = 13.777;
      components[i].wSplit = 13.;
    } else if (gasname == "N2") {
      components[i].m2Ion = 3.35;
      components[i].cIon = 38.1;
      components[i].x0Dens = 1.7378;
      components[i].x1Dens = 4.1323;
      components[i].cDens = 10.5400;
      components[i].aDens = 0.15349;
      components[i].mDens = 3.2125;
      components[i].ethr = 15.581;
      components[i].wSplit = 13.8;
    } else {
      std::cerr << className << "::SetupGas:\n";
      std::cerr << "    Cross-section for " << gasname 
                << " is not available.\n";
      ok =  false;
    }
  }
  
  if (!ok) {
    nComponents = 0;
    components.clear();
  }
  
  return true;

}

bool
TrackElectron::UpdateCrossSection() {

  const double prefactor = 4 * Pi * pow(HbarC / ElectronMass, 2);
  const double lnBg2 = log(beta2 / (1. - beta2));
  // Parameter X in the Sternheimer fit formula 
  const double eta = mediumDensity / LoschmidtNumber;
  const double x = 0.5 * (lnBg2 + log(eta)) / log(10.);
  double csSum = 0.;
  for (int i = nComponents; i--;) {
    double delta = 0.;
    if (components[i].x0Dens < components[i].x1Dens && 
        x >= components[i].x0Dens) {
      delta = 2 * log(10.) * x - components[i].cDens;
      if (x < components[i].x1Dens) {
        delta += components[i].aDens * pow(components[i].x1Dens - x, 
                                           components[i].mDens);
      }
    }
    double cs = (components[i].fraction * prefactor / beta2) * 
                (components[i].m2Ion * (lnBg2 - beta2 - delta) + 
                 components[i].cIon);
    components[i].p = cs;
    csSum += cs;
  }
  
  if (csSum <= 0.) {
    std::cerr << className << "::UpdateCrossSection:\n";
    std::cerr << "    Total cross-section <= 0.\n";
    return false;
  }
  
  mfp = 1. / (csSum * mediumDensity);
  
  for (int i = 0; i < nComponents; ++i) {
    components[i].p /= csSum;
    if (i > 0) components[i].p += components[i - 1].p;
  }
  
  return true;
  
}

}
