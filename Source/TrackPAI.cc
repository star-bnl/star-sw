#include <iostream>
#include <fstream>
#include <cmath>

#include "Sensor.hh"
#include "TrackPAI.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackPAI::TrackPAI() : 
  ready(false),
  x(0.), y(0.), z(0.), t(0.), 
  dx(0.), dy(0), dz(1.),
  e(0.), speed(0.), emax(0.),
  imfp(0.), dedx(0.), 
  nSteps(1000),
  mediumName(""), mediumDensity(0.), electronDensity(0.) {
  
  className = "TrackPAI";
  
  electrons.clear();
  holes.clear();
  
}

TrackPAI::~TrackPAI() {
      
}

void 
TrackPAI::NewTrack(const double x0, const double y0, const double z0,
                   const double t0, 
                   const double dx0, const double dy0, const double dz0) {

  ready = false;

  // Make sure the sensor has been set.
  if (sensor == 0) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }
  
  // Get the medium at this location and check if it is "ionisable".
  Medium* medium = 0;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    return;
  }
  if (!medium->IsIonisable()) {
    std::cerr << className << "::NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n";
    return;
  }
  
  if (medium->GetName() != mediumName ||
      medium->GetNumberDensity() != mediumDensity) { 
    isChanged = true;
    if (!SetupMedium(medium)) {
      std::cerr << className << "::NewTrack:\n";
      std::cerr << "    Properties of medium "
                << medium->GetName() << " are not available.\n";
      return;
    }
    mediumName = medium->GetName();
    mediumDensity = medium->GetNumberDensity();
  }
 
  ready = true;
 
  if (isChanged) {
    if (!SetupCrossSectionTable()) {
      std::cerr << className << "::NewTrack:\n";
      std::cerr << "    Calculation of ionisation cross-section failed.\n";
      ready = false;
      return;
    }
    isChanged = false;
  }

  x = x0; y = y0; z = z0; t = t0;
  const double d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (d < Small) {
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
    dx = dx0 / d; dy = dy0 / d; dz = dz0 / d;
  }
               
}

bool
TrackPAI::GetCluster(double& xcls, double& ycls, double& zcls, 
                     double& tcls, int& ncls, double& edep, double& extra) {

  ncls = 0;
  edep = extra = 0.;
  
  // Clear the stack.
  electrons.clear();
  holes.clear();
  
  if (!ready) {
    std::cerr << className << "::GetCluster:\n";
    std::cerr << "    Track not initialized.\n";
    std::cerr << "    Call NewTrack first.\n";
    return false;
  }

  if (isChanged) {
    if (SetupCrossSectionTable()) {
      isChanged = false;
    } else {
      std::cerr << className << "::GetCluster:\n";
      std::cerr << "    Calculation of ionisation cross-section failed.\n";
      return false;
    }
  }
  
  // Draw a step length and propagate the particle.
  const double d = - imfp * log(RndmUniformPos());
  x += d * dx;
  y += d * dy;
  z += d * dz;
  t += d / speed;
 
  // Check the medium at this location.
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
  
  // Check if the particle is still inside the drift area.
  if (!sensor->IsInArea(x, y, z)) {
    ready = false;
    return false;
  }

  xcls = x; ycls = y; zcls = z; tcls = t;
  
  // Sample the energy deposition.
  double f = 0.;
  const double u = RndmUniform();
  if (u < cdf.back()) {
    if (u <= cdf[0]) {
      edep = energies[0];
    } else if (u >= 1.) {
      edep = energies.back();
    } else {
      // Find the energy loss by interpolation 
      // from the cumulative distribution table
      int iLow = 0, iUp = cdf.size(), iMid;    
      while (iUp - iLow > 1) {
        iMid = (iUp + iLow) >> 1;
        if (u >= cdf[iMid]) {
          iLow = iMid;
        } else {
          iUp = iMid;
        }
      }
      if (edep < 100.) {
        edep = energies[iLow] + (u - cdf[iLow]) * 
               (energies[iUp] - energies[iLow]) / (cdf[iUp] - cdf[iLow]);
        f = rutherford[iLow] + (edep - energies[iLow]) * 
            (rutherford[iUp] - rutherford[iLow]) / (energies[iUp] - energies[iLow]);               
      } else {
        edep = log(energies[iLow]) + (log(u) - log(cdf[iLow])) * 
               (log(energies[iUp]) - log(energies[iLow])) / (log(cdf[iUp]) - log(cdf[iLow]));
        edep = exp(edep);
        f = rutherford[iLow] + (log(edep) - log(energies[iLow])) * 
            (rutherford[iUp] - rutherford[iLow]) / (log(energies[iUp]) - log(energies[iLow]));        
      }
    }
  } else {
    // Use the free-electron differential cross-section.
    f = 1.;
    edep = SampleAsymptoticCs(u);
  }
  // Update the particle energy.
  e -= edep;
  
  // Number of electron/hole (or electron/ion pairs) produced.
  ncls = 1;
  
  return true;
                       
}

bool 
TrackPAI::SetupMedium(Medium* medium) {

  // Make sure that the medium is defined.
  if (medium == 0) {
    std::cerr << className << "::SetupMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return false;
  }
  
  // Get the density and effective Z.
  electronDensity = medium->GetNumberDensity() * medium->GetAtomicNumber();
  if (electronDensity <= 0.) {
    std::cerr << className << "::SetupMedium:\n";
    std::cerr << "    Medium has an unphysical electron density ("
              << electronDensity << ")\n";
    return false;
  }
 
  // Get the dielectric function.
  double emin, emax;
  if (!medium->GetOpticalDataRange(emin, emax)) {
    std::cerr << className << "::SetupMedium:\n";
    std::cerr << "    Could not load optical data for medium " 
              << mediumName << ".\n";
    return false;              
  }
  
  // Make sure the minimum energy is positive.
  if (emin < Small) emin = Small;  
  
  // Reset the arrays.
  energies.clear();
  opticalDataTable.clear();
  opticalData newEpsilon;

  // Use logarithmically spaced energy steps.
  const double r = pow(emax / emin, 1. / double(nSteps));  
  double eps1, eps2;  
  
  double eC = 0.5 * emin * (1. + r);
  for (int i = 0; i < nSteps; ++i) {
    medium->GetDielectricFunction(eC, eps1, eps2);
    newEpsilon.eps1 = eps1;
    newEpsilon.eps2 = eps2;    
    opticalDataTable.push_back(newEpsilon);
    energies.push_back(eC);
    eC *= r;
  }    
  
  // Compute the integral of loss function times energy.
  opticalDataTable[0].integral = 0.;
  double integral = 0.;
  double f1 = energies[0] * LossFunction(opticalDataTable[0].eps1,
                                         opticalDataTable[0].eps2);
  double f2 = f1;
  double eM, fM;
  for (int i = 1; i < nSteps; ++i) {
    f2 = energies[i] * LossFunction(opticalDataTable[i].eps1,
                                    opticalDataTable[i].eps2);
    eM = 0.5 * (energies[i - 1] + energies[i]);
    medium->GetDielectricFunction(eM, eps1, eps2);
    fM = eM * LossFunction(eps1, eps2);
    // Simpson's rule
    integral += (f1 + 4 * fM + f2) * (energies[i] - energies[i - 1]) / 6.;
    opticalDataTable[i].integral = integral;
    f1 = f2;
  }  
  
  // Check the consistency of the optical data by means of the TRK sum rule
  const double trk = 2 * Pi2 * FineStructureConstant * pow(HbarC, 3) * 
                     electronDensity / ElectronMass;
  if (fabs(integral - trk) > 0.2 * trk) {
    std::cerr << className << "::SetupMedium:\n";
    std::cerr << "    Deviation from Thomas-Reiche-Kuhn sum rule by > 20%.\n"; 
    std::cerr << "    Optical data are probably incomplete or erroneous!\n";
  }
  
  return true;
  
}

double 
TrackPAI::GetClusterDensity() {

  if (!ready) {
    std::cerr << className << "::GetClusterDensity:\n";
    std::cerr << "    Track has not been initialized.\n";
    return 0.;
  }

  if (isChanged) {
    if (SetupCrossSectionTable()) {
      isChanged = false;
    } else {
      std::cerr << className << "::GetClusterDensity:\n";
      std::cerr << "    Ionisation cross-section could not be calculated.\n";
      return 0.;
    }
  }
  
  return 1. / imfp;
  
}

double 
TrackPAI::GetStoppingPower() {

  if (!ready) {
    std::cerr << className << "::GetStoppingPower:\n";
    std::cerr << "    Track has not been initialised.\n";
    return 0.;
  }

  if (isChanged) {
    if (SetupCrossSectionTable()) {
      isChanged = false;
    } else {
      std::cerr << className << "::GetStoppingPower:\n";
      std::cerr << "    Ionisation cross-section could not be calculated.\n";
      return 0.;
    }
  }
  
  return dedx;
  
}

bool 
TrackPAI::SetupCrossSectionTable() {

  if (!ready) {
    std::cerr << className << "::SetupCrossSectionTable:\n";
    std::cerr << "    Medium not set up.\n";
    return false;
  }  
  
  const double c1 = 2. * Pi2 * FineStructureConstant * pow(HbarC, 3) * 
                    electronDensity / ElectronMass;
  const double c2 = q * q * FineStructureConstant / (beta2 * Pi * HbarC);
  
  // Get the max. allowed energy transfer.
  emax = ComputeMaxTransfer();                      
  
  std::ofstream outfile;
  if (debug) outfile.open("dcs.txt", std::ios::out);
  
  // Compute the differential cross-section.
  std::vector<double> dcs;
  dcs.clear(); 
  rutherford.clear();

  for (int i = 0; i < nSteps; ++i) {
    // Define shorthand variables for photon energy and dielectric function.
    const double egamma = energies[i];
    const double eps1 = opticalDataTable[i].eps1;
    const double eps2 = opticalDataTable[i].eps2;
    const double integral = opticalDataTable[i].integral;

    // First, calculate the distant-collision terms.
    double dcsLog = 0., dcsDensity = 0., dcsCerenkov = 0.;
    if (eps2 > 0.) {
      // Normal case (loss function > 0).
      // Non-relativistic logarithmic term.
      dcsLog = LossFunction(eps1, eps2) * 
               log(2 * ElectronMass * beta2 / egamma);
      // Relativistic logarithmic term (density effect)
      const double u = 1. - beta2 * eps1;
      const double v = beta2 * eps2;
      dcsDensity = - 0.5 * LossFunction(eps1, eps2) * log(u * u + v * v);
      // "Cerenkov" term
      dcsCerenkov = (beta2 - eps1 / (eps1 * eps1 + eps2 * eps2)) * 
                    (HalfPi - atan(u / v));
    } else if (eps1 > 1. / beta2) {
      // Imaginary part is zero, only the Cerenkov term contributes.
      dcsCerenkov = Pi * (beta2 - 1. / eps1);
    }
    
    // Calculate the close-collision term (quasi-free scattering)
    double dcsRuth = 0.;
    double f = 0.;
    if (energy > 0. && integral > 0.) {
      dcsRuth = integral / (egamma * egamma);
      f = dcsRuth / (dcsLog + dcsDensity + dcsCerenkov);
    }
    rutherford.push_back(f);
    dcs.push_back(dcsLog + dcsDensity + dcsCerenkov + dcsRuth);
    // If requested, write the cross-section terms to file.
    if (debug) {
      outfile << egamma << "  " << eps1 << "  " << eps2 << "  "
              << dcsLog * c2 << "  " << dcsDensity * c2 << "  "
              << dcsCerenkov * c2 << "  " << dcsRuth * c2 << "\n";
    }
  }
  if (debug) outfile.close();
         
  // Compute the cumulative distribution, 
  // total cross-section and stopping power. 
  cdf.clear();
  cdf.push_back(0.);
  dedx = 0.;
  double cs = 0.;
  for (int i = 1; i < nSteps; ++i) {
    cs += 0.5 * (dcs[i - 1] + dcs[i]) * (energies[i] - energies[i - 1]);
    cdf.push_back(cs);
    dedx += 0.5 * (dcs[i - 1] * energies[i - 1] + dcs[i] * energies[i]) * 
                  (energies[i] - energies[i - 1]);
  }

  // Add the contribution of high energy transfers to the stopping power 
  // and the total cross-section
  energy = energies.back();
  if (energy < emax) {  
    cs   += c1 * ComputeCsTail(energy, emax);
    dedx += c1 * ComputeDeDxTail(energy, emax);
  } else {
    std::cerr << className << "::SetupCrossSectionTable:\n";
    std::cerr << "    Max. energy transfer lower than optical data range.\n";
  }
   
  if (cs <= 0.) {
    std::cerr << "TrackPAI:SetupCrossSectionTable:\n";
    std::cerr << "    Total cross-section <= 0.\n";
    return false;
  }

  // Normalise the cumulative distribution.
  for (int i = nSteps; i--;) cdf[i] /= cs;
      
  cs   *= c2;
  dedx *= c2;
    
  // Compute the inelastic mean free path
  imfp = 1. / cs;
    
  return true;
  
}

double 
TrackPAI::ComputeMaxTransfer() const {
  
  if (isElectron) {
    // Max. transfer for electrons is half the kinetic energy.
    return 0.5 * (energy - mass);
  }
  
  // Other particles.
  const double bg2 = beta2 / (1. - beta2);
  const double mass2 = mass * mass;

  return 2. * mass2 * ElectronMass * bg2 / 
         (mass2 + ElectronMass * ElectronMass + 2. * energy * ElectronMass);

}

double 
TrackPAI::ComputeCsTail(const double emin, const double emax) {

  if (isElectron) {
    // Electrons
    const double ek = energy - mass;
    return 1. / emin - 1. / emax - 
           2 * emin / (ek * ek) - 
           emin * emin / ((ek - emin) * ek * ek);
  } else if (mass == ElectronMass) {
    // Positrons
    const double ek = energy - mass;
    return 1. / emin - 1. / emax + 
           3 * (emax - emin) / (ek * ek) - 
           (emax - emin) * (ek * (emax + emin) + 
                            (emin * emin + emin * emax + emax * emax) / 3.) / 
           pow(ek, 4) - 
           (2. / ek) * log(emax / emin);
  }
  
  return 1. / emin - 1. / emax - 
         beta2 * log(emax / emin) / emax;

  /*
  switch (spin) {
    case 0:
      // Spin 0
      return 1. / emin - 1. / emax - 
             beta2 * log(emax / emin) / emax;
      break;
    case 1:
      // Spin 1/2
      return 1. / emin - 1. / emax - 
             beta2 * log(emax / emin) / emax + 
             (emax - emin) / (2 * energy * energy);
      break;
    case 2: {
      // Spin 1
      const double e2 = 2 * energy * energy;
      const double ec = mass * mass / ElectronMass;
      const double a = 1. / (3 * ec);
      const double b = (emax - emin);
      return 1. / emin - 1. / emax + 
             a * b * (emin + e2 + emax) / e2 - 
             beta2 * a * b / emax + 
             (a - beta2 / emax) * log(emax / emin);
      break;
    }
    default:
      break;
  }
  */
  // Rutherford type cross-section
  return 1. / emin - 1. / emax;  

}

double 
TrackPAI::ComputeDeDxTail(const double emin, const double emax) {

  if (isElectron) {
    const double ek = energy - mass;
    return -log(emin * (ek - emin) / (ek * ek)) + 
           (1. / (8 * (emin - ek) * ek * ek)) * 
           (-4 * pow(emin, 3) + 4 * emin * emin * ek + 
           emin * ek * ek * (17. - 16. * CLog2) + 
           pow(ek, 3) * (-9. + 16. * CLog2));
  } else if (mass == ElectronMass) {
    // Positron
    const double ek = energy - mass;
    return log(ek / emin) - (ek - emin) * (ek - emin) * 
           (3. * emin * emin - 2. * emin * ek + 11. * ek * ek) / 
           (12. * pow(ek, 4));
  }
 
  return log(emax / emin) - 
          beta2 * (emax - emin) / emax;

  /* 
  switch (spin) {
    case 0:
      return log(emax / emin) - 
             beta2 * (emax - emin) / emax;
      break;
    case 1: 
      // Spin 1/2
      return log(emax / emin) - 
             beta2 * (emax - emin) / emax + 
             (emax * emax - emin * emin) / (2 * energy * energy);
      break;
    case 2: {
      // Spin 1
      double e2 = energy * energy;
      double ec = mass * mass / ElectronMass;
      return log(emax / emin) + 
             (pow(emax, 3) - pow(emin, 3)) / (9. * e2 * ec) +
             (emax * emax - emin * emin) / (6. * e2) + 
             (emax - emin) * (2. - (1. + emin / emax + 
                                    6 * ec / emax) * beta2) / (6. * ec);
      break;
    }
    default:
      break;
  }
  //*/

  // Rutherford cross-section
  return log(emax / emin);

}

double 
TrackPAI::SampleAsymptoticCs(double u) {

  const double emin = energies.back();
  // Rescale the random number
  u = (u - cdf.back()) / (1. - cdf.back());
  
  if (isElectron) {
    return SampleAsymptoticCsElectron(emin, u);
  } else if (mass == ElectronMass) {
    return SampleAsymptoticCsPositron(emin, u);
  }
 
  return SampleAsymptoticCsSpinZero(emin, u);

  /* 
  switch (spin) {
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
  */
  // Rutherford cross-section (analytic inversion)
  return emin * emax / (u * emin + (1. - u) * emax);
  
}

double 
TrackPAI::SampleAsymptoticCsSpinZero(const double emin, double u) {

  const double a = emin / emax;
  const double b = beta2 * a;
  u *= (1. - a + b * log(a));
  double eLow = emin, eUp = emax;
  double eM;
  while (eUp - eLow > 1.) {
    eM = 0.5 * (eUp + eLow);
    if (u >= 1. - emin / eM - b * log(eM / emin)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }
  
  return 0.5 * (eLow + eUp);
  
}

double 
TrackPAI::SampleAsymptoticCsSpinHalf(const double emin, double u) {

  const double a = emin / emax;
  const double b = beta2 * a;
  const double c = emin / (2. * energy * energy);
  u *= 1. - a + b * log(a) + (emax - emin) * c;
  double eLow = emin, eUp = emax;
  double eM;
  while (eUp - eLow > 1.) {
    eM = 0.5 * (eUp + eLow);
    if (u >= 1. - emin / eM - b * log(eM / emin) + (eM - emin) * c) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }
  
  return 0.5 * (eLow + eUp);
  
}

double 
TrackPAI::SampleAsymptoticCsSpinOne(const double emin, double u) {

  const double e2 = 2 * energy * energy;
  const double ec = mass * mass / ElectronMass;
  const double a = 2 * ec / e2 - beta2 / emax;
  const double b = 1.5 * ec / emin;
  const double c = 1. - 1.5 * ec * beta2 / emax;
  u *= (emax - emin) * 
       (0.5 * (emin + emax) / e2 + a + b / emax) + 
       c * log(emax / emin);
  double eLow = emin, eUp = emax;
  double eM;
  while (eUp - eLow > 1.) {
    eM = 0.5 * (eUp + eLow);
    if (u >= (eM - emin) * ((emin + eM) / e2 + a + b / eM) + 
             c * log(eM / emin)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }
  
  return 0.5 * (eLow + eUp);

}

double 
TrackPAI::SampleAsymptoticCsElectron(const double emin, double u) {

  const double ek = energy - mass;
  const double ek2 = ek * ek;
  const double a = ek / (emin * (ek - emin));
  const double norm = 1. / emin - 0.5 / ek - 
                      emin * emin / ((ek - emin) * ek2) -
                      2. * emin / ek2;
  u *= norm;
  double eLow = emin, eUp = emax, eM;
  while (eUp - eLow > 1.) {
    eM = 0.5 * (eUp + eLow);
    if (u >= a - 1. / eM + (eM - emin) / ek2 + 1. / (ek - eM)) {
      eLow = eM;
    } else {
      eUp = eM;
    }
  }
  return 0.5 * (eLow + eUp);
  
}

double 
TrackPAI::SampleAsymptoticCsPositron(const double emin, double u) {

  const double ek = energy - mass;
  const double ek2 = ek * ek;
  const double ek3 = ek2 * ek;
  const double ek4 = 3 * ek3 * ek;
  const double emin2 = emin * emin;
  const double a = 1. / emin;
  const double b = 3. / ek2;
  const double c = 2. / ek;
  u *= 1. / emin - 1. / emax + 3 * (emax - emin) / ek2 - 
       (emax - emin) * (emax + emin) / ek3 + 
       (emax - emin) * (emin * emin + emin * emax + emax * emax) / ek4 - 
       (2. / ek) * log(emax / emin);
  double eLow = emin, eUp = emax;
  double eM, eM2;
  while (eUp - eLow > 1.) {
    eM = 0.5 * (eUp + eLow);
    eM2 = eM * eM;
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

double 
TrackPAI::LossFunction(const double eps1, const double eps2) {

  const double eps = eps1 * eps1 + eps2 * eps2;
  if (eps <= 0.) {
    std::cerr << className << "::LossFunction:\n";
    std::cerr << "    Dielectric function is zero.\n";
    return 0.;
  }
  return eps2 / (eps1 * eps1 + eps2 * eps2);

}

}
