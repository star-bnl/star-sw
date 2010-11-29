#include <iostream>
#include <cmath>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "Numerics.hh"

namespace Garfield {

int Medium::idCounter = -1;

Medium::Medium() :
  className("Medium"), 
  id(++idCounter), name(""), 
  temperature(293.15), pressure(760.), 
  epsilon(1.), 
  nComponents(1), atomicNumber(1.), atomicWeight(0.), density(0.),
  driftable(false), microscopic(false), ionisable(false),
  wValue(0.), fanoFactor(0.),
  isChanged(true),
  debug(false),
  map2d(false) {
  
  // Initialise the transport tables.
  nEfields = 0;
  nBfields = 1;
  nAngles  = 1;
  
  eFields.clear();
  bFields.clear(); bFields.resize(1); bFields[0] = 0.;
  bAngles.clear(); bAngles.resize(1); bAngles[0] = 0.;
 
  hasElectronVelocityE   = false; tabElectronVelocityE.clear();
  hasElectronVelocityB   = false; tabElectronVelocityB.clear();
  hasElectronVelocityExB = false; tabElectronVelocityExB.clear();
  hasElectronDiffLong    = false; tabElectronDiffLong.clear();
  hasElectronDiffTrans   = false; tabElectronDiffTrans.clear();
  hasElectronTownsend    = false; tabElectronTownsend.clear();
  hasElectronAttachment  = false; tabElectronAttachment.clear();
  hasElectronDiffTens    = false; tabElectronDiffTens.clear();
 
  hasHoleVelocityE   = false; tabHoleVelocityE.clear();
  hasHoleVelocityB   = false; tabHoleVelocityB.clear();
  hasHoleVelocityExB = false; tabHoleVelocityExB.clear();
  hasHoleDiffLong    = false; tabHoleDiffLong.clear();
  hasHoleDiffTrans   = false; tabHoleDiffTrans.clear();
  hasHoleTownsend    = false; tabHoleTownsend.clear();
  hasHoleAttachment  = false; tabHoleAttachment.clear();
  hasHoleDiffTens    = false; tabHoleDiffTens.clear();

  hasIonMobility     = false; tabIonMobility.clear();
  hasIonDiffLong     = false; tabIonDiffLong.clear();
  hasIonDiffTrans    = false; tabIonDiffTrans.clear(); 
  hasIonDissociation = false; tabIonDissociation.clear();
  
  extrLowVelocity     = 0; extrHighVelocity     = 1;
  extrLowDiffusion    = 0; extrHighDiffusion    = 1;
  extrLowTownsend     = 0; extrHighTownsend     = 1;
  extrLowAttachment   = 0; extrHighAttachment   = 1;
  extrLowDissociation = 0; extrHighDissociation = 1;
  
  intpVelocity     = 2;
  intpDiffusion    = 2;
  intpTownsend     = 2;
  intpAttachment   = 2;
  intpDissociation = 2;
 
  thrElectronTownsend = thrElectronAttachment = 0;
  thrHoleTownsend = thrHoleAttachment = 0;
  thrIonDissociation = 0;

}

void 
Medium::SetTemperature(const double t) {

  if (t <= 0.) {
    std::cerr << className << "::SetTemperature:\n";
    std::cerr << "    Temperature [K] must be greater than zero.\n";
    return;
  }
  temperature = t;
  isChanged = true;

}

void 
Medium::SetPressure(const double p) {

  if (p <= 0.) {
    std::cerr << className << "::SetPressure:\n";
    std::cerr << "    Pressure [Torr] must be greater than zero.\n";
    return;
  }
  pressure = p;
  isChanged = true;  

}

void 
Medium::SetDielectricConstant(const double eps) {

  if (eps < 1.) {
    std::cerr << className << "::SetDielectricConstant:\n";
    std::cerr << "    Dielectric constant must be >= 1.\n";
    return;
  }
  epsilon = eps;
  isChanged = true;  

}

double
Medium::GetMassDensity() const {

  return density * AtomicMassUnit * atomicWeight;
  
}

void
Medium::GetComponent(const int i, std::string& label, double& f) {

  if (i < 0 || i >= nComponents) {
    std::cerr << className << "::GetComponent:\n";
    std::cerr << "    Index out of range.\n";
  }
  
  label = name; f = 1.;
  
}

void 
Medium::SetAtomicNumber(const double z) {

  if (z < 1.) {
    std::cerr << className << "::SetAtomicNumber:\n";
    std::cerr << "    Atomic number must be >= 1.\n";
    return;
  }
  atomicNumber = z;
  isChanged = true;  

}

void 
Medium::SetAtomicWeight(const double a) {

  if (a <= 0.) {
    std::cerr << className << "::SetAtomicWeight:\n";
    std::cerr << "    Atomic weight must be greater than zero.\n";
    return;
  }
  atomicWeight = a;
  isChanged = true;  

}

void 
Medium::SetNumberDensity(const double n) {

  if (n <= 0.) {
    std::cerr << className << "::SetNumberDensity:\n";
    std::cerr << "    Density [cm-3] must be greater than zero.\n";
    return;
  }
  density = n;
  isChanged = true;  

}

void 
Medium::SetMassDensity(const double rho) {

  if (rho <= 0.) {
    std::cerr << className << "::SetMassDensity:\n";
    std::cerr << "    Density [g/cm3] must be greater than zero.\n";
    return;
  }

  if (atomicWeight <= 0.) {
    std::cerr << className << "::SetMassDensity:\n";
    std::cerr << "    Atomic weight is not defined.\n";
    return;
  }
  density = rho / (AtomicMassUnit * atomicWeight);
  isChanged = true;  

}

bool 
Medium::ElectronVelocity(const double ex, const double ey, const double ez, 
                         const double bx, const double by, const double bz, 
                         double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  // Make sure there is at least a table of velocities along E.
  if (!hasElectronVelocityE) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return false; 
 
  // Compute the magnitude of the magnetic field.
  const double b = sqrt(bx * bx + by * by + bz * bz);
  
  // Compute the angle between B field and E field.
  double ebang = 0.;
  if (e * b > 0.) {
    const double eb = fabs(ex * bx + ey * by + ez * bz);
    if (eb > 0.2 * e * b) {
      ebang = asin(std::min(1., 
                            sqrt(pow(ex * by - ey * bx, 2) +
                                 pow(ex * bz - ez * bx, 2) +
                                 pow(ez * by - ey * bz, 2)) / (e * b)));
    } else {
      ebang = acos(std::min(1., eb / (e * b)));
    }
  } else {
    ebang = bAngles[0];
  }
  
  if (b < Small) {
    // No magnetic field.

    // Calculate the velocity along E.
    double ve = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields,
                         intpVelocity, 
                         extrLowVelocity, extrHighVelocity);
    }
    const double q = -1.;
    const double mu = q * ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
    
  } else if (hasElectronVelocityB && hasElectronVelocityExB) {
    // Magnetic field, velocities along ExB and Bt available
    
    // Compute unit vectors along E, E x B and Bt.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, 
                      ez * bx - ex * bz, 
                      ex * by - ey * bx};
    const double exb = sqrt(uexb[0] * uexb[0] + 
                            uexb[1] * uexb[1] + 
                            uexb[2] * uexb[2]);
    if (exb > 0.) {
      uexb[0] /= exb; uexb[1] /= exb; uexb[2] /= exb;
    } else {
      uexb[0] = ue[0];
      uexb[1] = ue[1];
      uexb[2] = ue[2];
    }

    double ubt[3] = {
      uexb[1] * ez - uexb[2] * ey, 
      uexb[2] * ex - uexb[0] * ez, 
      uexb[0] * ey - uexb[1] * ex
    };
    const double bt = sqrt(ubt[0] * ubt[0] + 
                           ubt[1] * ubt[1] + 
                           ubt[2] * ubt[2]);

    if (bt > 0.) {
      ubt[0] /= bt; ubt[1] /= bt; ubt[2] /= bt;
    } else {
      ubt[0] = ue[0];
      ubt[1] = ue[1];
      ubt[2] = ue[2];
    }

    // Calculate the velocities in all directions.
    double ve = 0., vbt = 0., vexb = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabElectronVelocityExB, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, vexb, intpVelocity)) {
        std::cerr << className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along ExB failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabElectronVelocityB, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, vbt, intpVelocity)) {
        std::cerr << className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along Bt failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields,
                         intpVelocity,
                         extrLowVelocity, extrHighVelocity);
      vbt = Interpolate1D(e0, tabElectronVelocityB[0][0], eFields,
                          intpVelocity,
                          extrLowVelocity, extrHighVelocity);
      vexb = Interpolate1D(e0, tabElectronVelocityExB[0][0], eFields,
                           intpVelocity,
                           extrLowVelocity, extrHighVelocity);
    }
    const double q = -1.;  
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);
    
  } else {
    // Magnetic field, velocities along ExB, Bt not available
    
    // Calculate the velocity along E.
    double ve = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields, 
                         intpVelocity,
                         extrLowVelocity, extrHighVelocity);
    }

    const double q = -1.;
    const double mu = q * ve / e;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(mu * b, 2);
    vx = mu * (ex + 
               mu * (ey * bz - ez * by) +
               mu * mu * bx * eb) / nom;
    vy = mu * (ey + 
               mu * (ez * bx - ex * bz) +
               mu * mu * bx * eb) / nom;
    vz = mu * (ez + 
               mu * (ex * by - ey * bx) +
               mu * mu * bz * eb) / nom;
  }
  
  return true;
  
}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    
    // Interpolate.
    if (hasElectronDiffLong) {
      if (!Numerics::Boxin3(tabElectronDiffLong, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dl, intpDiffusion)) {
        dl = 0.;
      }
    }
    if (hasElectronDiffTrans) {
      if (!Numerics::Boxin3(tabElectronDiffTrans,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dt, intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (hasElectronDiffLong) {
      dl = Interpolate1D(e0, tabElectronDiffLong[0][0], eFields,
                         intpDiffusion, 
                         extrLowDiffusion, extrHighDiffusion);
    }
    if (hasElectronDiffTrans) {
      dt = Interpolate1D(e0, tabElectronDiffTrans[0][0], eFields,
                         intpDiffusion, 
                         extrLowDiffusion, extrHighDiffusion);
    }
  }

  // If no data available, calculate 
  // the diffusion coefficients using the Einstein relation
  if (!hasElectronDiffLong) {
    dl = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  if (!hasElectronDiffTrans) {
    dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  
  // Verify values and apply scaling.
  if (dl < 0.) dl = 0.;
  if (dt < 0.) dt = 0.;
  dl = ScaleDiffusion(dl);
  dt = ScaleDiffusion(dt);

  return true;

}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = cov[0][1] = cov[0][2] = 0.;
  cov[1][0] = cov[1][1] = cov[1][2] = 0.;
  cov[2][0] = cov[2][1] = cov[2][2] = 0.;
 
  if (!hasElectronDiffTens) return false;
 
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    double diff = 0.;
    for (int l = 0; l < 6; ++l) {
      if (!Numerics::Boxin3(tabElectronDiffTens[l], 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, diff, intpDiffusion)) {
        diff = 0.;
      }
      // Apply scaling.
      diff = ScaleDiffusionTensor(diff);
      if (l < 3) {
        cov[l][l] = diff;
      } else if (l == 3) {
        cov[0][1] = cov[1][0] = diff;
      } else if (l == 4) {
        cov[0][2] = cov[2][0] = diff;
      } else if (l == 5) {
        cov[1][2] = cov[2][1] = diff;
      }
    }
  } else {
    // Interpolate.
    for (int l = 0; l < 6; ++l) {
      double diff = Interpolate1D(e0, tabElectronDiffTens[l][0][0], eFields,
                                  intpDiffusion,
                                  extrLowDiffusion, extrHighDiffusion);
      // Apply scaling.
      diff = ScaleDiffusionTensor(diff);
      if (l < 3) {
        cov[l][l] = diff;
      } else if (l == 3) {
        cov[0][1] = cov[1][0] = diff;
      } else if (l == 4) {
        cov[0][2] = cov[2][0] = diff;
      } else if (l == 5) {
        cov[1][2] = cov[2][1] = diff;
      }
    }
  }
  
  return true;

}

bool 
Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& alpha) {

  alpha = 0.;
  if (!hasElectronTownsend) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrElectronTownsend]) {
      if (!Numerics::Boxin3(tabElectronTownsend, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, alpha, 1)) {
        alpha = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabElectronTownsend,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, alpha, intpTownsend)) {
        alpha = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrElectronTownsend]) {
      alpha = Interpolate1D(e0, tabElectronTownsend[0][0], eFields,
                            1, extrLowTownsend, extrHighTownsend);
    } else {
      alpha = Interpolate1D(e0, tabElectronTownsend[0][0], eFields,
                            intpTownsend,
                            extrLowTownsend, extrHighTownsend);
    }
  }
  
  if (alpha < -20.) {
    alpha = 0.;
  } else {
    alpha = exp(alpha);
  }
  
  // Apply scaling.
  alpha = ScaleTownsend(alpha); 
  return true;

}

bool 
Medium::ElectronAttachment(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& eta) {

  eta = 0.;
  if (!hasElectronAttachment) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrElectronAttachment]) {
      if (!Numerics::Boxin3(tabElectronAttachment, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, eta, 1)) {
        eta = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabElectronAttachment,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, eta, intpAttachment)) {
        eta = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrElectronAttachment]) {
      eta = Interpolate1D(e0, tabElectronAttachment[0][0], eFields, 
                          1, extrLowAttachment, extrHighAttachment);
    } else {
      eta = Interpolate1D(e0, tabElectronAttachment[0][0], eFields,
                          intpAttachment,
                          extrLowAttachment, extrHighAttachment);
    }
  }
  
  if (eta < -20.) {
    eta = 0.;
  } else {
    eta = exp(eta);
  }
  
  // Apply scaling.
  eta = ScaleAttachment(eta); 
  return true;

}

double 
Medium::GetElectronEnergy(const double px, const double py, const double pz,
                          double& vx, double& vy, double& vz, 
                          const int band) {

  if (band != 0) {
    std::cerr << className << "::GetElectronEnergy:\n";
    std::cerr << "    Unknown band index.\n";
  }
  
  vx = SpeedOfLight * px / ElectronMass;
  vy = SpeedOfLight * py / ElectronMass;
  vz = SpeedOfLight * pz / ElectronMass;
  
  return 0.5 * (px * px + py * py + pz * pz) / ElectronMass;
  
}

void
Medium::GetElectronMomentum(const double e, 
                            double& px, double& py, double& pz, 
                            const int band) {

  if (band != 0) {
    std::cerr << className << "::GetElectronMomentum:\n";
    std::cerr << "    Unknown band index.\n";
  }

  const double p = sqrt(2. * ElectronMass * e) / SpeedOfLight;
  const double ctheta = 1. - 2. * RndmUniform();
  const double stheta = sqrt(1. - ctheta * ctheta);
  const double phi = TwoPi * RndmUniform();
  
  px = p * stheta * cos(phi);
  py = p * stheta * sin(phi);
  pz = p * ctheta;
  
}

double 
Medium::GetElectronNullCollisionRate() {

  if (debug) {
    std::cerr << className << "::GetElectronNullCollisionRate:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return 0.;
  
}

double 
Medium::GetElectronCollisionRate(const double e, const int band) {

  if (debug) {
    std::cerr << className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Electron collision rate at energy "
              << e << " eV (band " << band << ") not available.\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return 0.;

}

bool 
Medium::GetElectronCollision(const double e, int& type, int& level,
                             double& e1, 
                             double& dx, double& dy, double& dz, 
                             int& nsec, double& esec,
                             int& band) {
  
  type = level = -1;
  e1 = e;
  nsec = 0;
  esec = 0.;
  band = 0;
  const double ctheta = 1. - 2 * RndmUniform();
  const double stheta = sqrt(1. - ctheta * ctheta);
  const double phi = TwoPi * RndmUniform();
  dx = cos(phi) * stheta;
  dy = sin(phi) * stheta;
  dz = ctheta;
 
  if (debug) {
    std::cerr << className << "::GetElectronCollision:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return false;
              
}

bool 
Medium::GetDeexcitationProduct(const int i, double& t, double& s, 
                               int& type, double& energy) {

  if (debug) {
    std::cerr << className << "::GetDeexcitationProduct:\n";
    std::cerr << "    Deexcitation product " << i << " requested.\n";
    std::cerr << "    This medium does not support de-excitation.\n";
    std::cerr << "    Program bug!\n";
  } 
  t = s = energy = 0.;
  type = 0;
  return false;

}
                
bool 
Medium::HoleVelocity(const double ex, const double ey, const double ez, 
                     const double bx, const double by, const double bz, 
                     double& vx, double& vy, double& vz) {
            
  vx = vy = vz = 0.;
  // Make sure there is at least a table of velocities along E.
  if (!hasHoleVelocityE) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  // Compute the magnitude of the magnetic field.
  const double b = sqrt(bx * bx + by * by + bz * bz);
  
  // Compute the angle between B field and E field.
  double ebang = 0.;
  if (e * b > 0.) {
    const double eb = fabs(ex * bx + ey * by + ez * bz);
    if (eb > 0.2 * e * b) {
      ebang = asin(std::min(1., 
                            sqrt(pow(ex * by - ey * bx, 2) +
                                 pow(ex * bz - ez * bx, 2) +
                                 pow(ez * by - ey * bz, 2)) / (e * b)));
    } else {
      ebang = acos(std::min(1., eb / (e * b)));
    }
  } else {
    ebang = bAngles[0];
  }
  
  if (b < Small) {
    // No magnetic field.
    // Calculate the velocity along E.
    double ve = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields,
                         intpVelocity, 
                         extrLowVelocity, extrHighVelocity);
    }
    const double q = 1.;
    const double mu = q * ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
    
  } else if (hasHoleVelocityB && hasHoleVelocityExB) {
    // Magnetic field, velocities along ExB and Bt available
    
    // Compute unit vectors along E, E x B and Bt.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, 
                      ez * bx - ex * bz, 
                      ex * by - ey * bx};
    const double exb = sqrt(uexb[0] * uexb[0] + 
                            uexb[1] * uexb[1] + 
                            uexb[2] * uexb[2]);
    if (exb > 0.) {
      uexb[0] /= exb; uexb[1] /= exb; uexb[2] /= exb;
    } else {
      uexb[0] = ue[0];
      uexb[1] = ue[1];
      uexb[2] = ue[2];
    }

    double ubt[3] = {
      uexb[1] * ez - uexb[2] * ey, 
      uexb[2] * ex - uexb[0] * ez, 
      uexb[0] * ey - uexb[1] * ex
    };
    const double bt = sqrt(ubt[0] * ubt[0] + 
                           ubt[1] * ubt[1] + 
                           ubt[2] * ubt[2]);

    if (bt > 0.) {
      ubt[0] /= bt; ubt[1] /= bt; ubt[2] /= bt;
    } else {
      ubt[0] = ue[0];
      ubt[1] = ue[1];
      ubt[2] = ue[2];
    }

    // Calculate the velocities in all directions.
    double ve = 0., vbt = 0., vexb = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabHoleVelocityExB, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, vexb, intpVelocity)) {
        std::cerr << className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along ExB failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabHoleVelocityB, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, vbt, intpVelocity)) {
        std::cerr << className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along Bt failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields,
                         intpVelocity,
                         extrLowVelocity, extrHighVelocity);
      vbt = Interpolate1D(e0, tabHoleVelocityB[0][0], eFields,
                          intpVelocity,
                          extrLowVelocity, extrHighVelocity);
      vexb = Interpolate1D(e0, tabHoleVelocityExB[0][0], eFields,
                           intpVelocity,
                           extrLowVelocity, extrHighVelocity);
    }
    const double q = 1.;  
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);
    
  } else {
    // Magnetic field, velocities along ExB, Bt not available
    
    // Calculate the velocity along E.
    double ve = 0.;
    if (map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, ve, intpVelocity)) {
        std::cerr << className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields,
                         intpVelocity, 
                         extrLowVelocity, extrHighVelocity);
    }

    const double q = 1.;
    const double mu = q * ve / e;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(mu * b, 2);
    vx = mu * (ex + 
               mu * (ey * bz - ez * by) +
               mu * mu * bx * eb) / nom;
    vy = mu * (ey + 
               mu * (ez * bx - ex * bz) +
               mu * mu * by * eb) / nom;
    vz = mu * (ez + 
               mu * (ex * by - ey * bx) +
               mu * mu * bz * eb) / nom;
  }
  
  return true;

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    
    // Interpolate.
    if (hasHoleDiffLong) {
      if (!Numerics::Boxin3(tabHoleDiffLong, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dl, intpDiffusion)) {
        dl = 0.;
      }
    }
    if (hasHoleDiffTrans) {
      if (!Numerics::Boxin3(tabHoleDiffTrans,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dt, intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (hasHoleDiffLong) {
      dl = Interpolate1D(e0, tabHoleDiffLong[0][0], eFields,
                         intpDiffusion, 
                         extrLowDiffusion, extrHighDiffusion);
    }
    if (hasHoleDiffTrans) {
      dt = Interpolate1D(e0, tabHoleDiffTrans[0][0], eFields,
                         intpDiffusion, 
                         extrLowDiffusion, extrHighDiffusion);
    }
  }

  // If no data available, calculate 
  // the diffusion coefficients using the Einstein relation
  if (!hasHoleDiffLong) {
    dl = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  if (!hasHoleDiffTrans) {
    dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  
  // Verify values and apply scaling.
  if (dl < 0.) dl = 0.;
  if (dt < 0.) dt = 0.;
  dl = ScaleDiffusion(dl);
  dt = ScaleDiffusion(dt);

  return true;

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = cov[0][1] = cov[0][2] = 0.;
  cov[1][0] = cov[1][1] = cov[1][2] = 0.;
  cov[2][0] = cov[2][1] = cov[2][2] = 0.;
 
  if (!hasHoleDiffTens) return false;
 
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    double diff = 0.;
    for (int l = 0; l < 6; ++l) {
      if (!Numerics::Boxin3(tabHoleDiffTens[l], 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, diff, intpDiffusion)) {
        diff = 0.;
      }
      // Apply scaling.
      diff = ScaleDiffusionTensor(diff);
      if (l < 3) {
        cov[l][l] = diff;
      } else if (l == 3) {
        cov[0][1] = cov[1][0] = diff;
      } else if (l == 4) {
        cov[0][2] = cov[2][0] = diff;
      } else if (l == 5) {
        cov[1][2] = cov[2][1] = diff;
      }
    }
  } else {
    // Interpolate.
    for (int l = 0; l < 6; ++l) {
      double diff = Interpolate1D(e0, tabHoleDiffTens[l][0][0], eFields,
                                  intpDiffusion,
                                  extrLowDiffusion, extrHighDiffusion);
      // Apply scaling.
      diff = ScaleDiffusionTensor(diff);
      if (l < 3) {
        cov[l][l] = diff;
      } else if (l == 3) {
        cov[0][1] = cov[1][0] = diff;
      } else if (l == 4) {
        cov[0][2] = cov[2][0] = diff;
      } else if (l == 5) {
        cov[1][2] = cov[2][1] = diff;
      }
    }
  }
  
  return true;

}

bool 
Medium::HoleTownsend(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& alpha) {

  alpha = 0.;
  if (!hasHoleTownsend) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrHoleTownsend]) {
      if (!Numerics::Boxin3(tabHoleTownsend, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, alpha, 1)) {
        alpha = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabHoleTownsend,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, alpha, intpTownsend)) {
        alpha = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrHoleTownsend]) {
      alpha = Interpolate1D(e0, tabHoleTownsend[0][0], eFields, 
                            1, extrLowTownsend, extrHighTownsend);
    } else {
      alpha = Interpolate1D(e0, tabHoleTownsend[0][0], eFields,
                            intpTownsend,
                            extrLowTownsend, extrHighTownsend);
    }
  }
  
  if (alpha < -20.) {
    alpha = 0.;
  } else {
    alpha = exp(alpha);
  }
  
  // Apply scaling.
  alpha = ScaleTownsend(alpha); 
  return true;

}

bool 
Medium::HoleAttachment(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double& eta) {
            
  eta = 0.;
  if (!hasHoleAttachment) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrHoleAttachment]) {
      if (!Numerics::Boxin3(tabHoleAttachment, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, eta, 1)) {
        eta = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabHoleAttachment,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, eta, intpAttachment)) {
        eta = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrHoleAttachment]) {
      eta = Interpolate1D(e0, tabHoleAttachment[0][0], eFields, 
                          1, extrLowAttachment, extrHighAttachment);
    } else {
      eta = Interpolate1D(e0, tabHoleAttachment[0][0], eFields,
                          intpAttachment,
                          extrLowAttachment, extrHighAttachment); 
    }
  }
  
  if (eta < -20.) {
    eta = 0.;
  } else {
    eta = exp(eta);
  }
  
  // Apply scaling.
  eta = ScaleAttachment(eta);
  return true;

}

bool 
Medium::IonVelocity(const double ex, const double ey, const double ez, 
                    const double bx, const double by, const double bz, 
                    double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!hasIonMobility) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  // Compute the magnitude of the electric field.
  const double b = sqrt(bx * bx + by * by + bz * bz);  

  double mu = 0.;
  if (map2d) {
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    if (!Numerics::Boxin3(tabIonMobility,
                          bAngles, bFields, eFields,
                          nAngles, nBfields, nEfields,
                          ebang, b, e0, mu, intpVelocity)) {
      mu = 0.;
    }
  } else {
    mu = Interpolate1D(e0, tabIonMobility[0][0], eFields,
                       intpVelocity, 
                       extrLowVelocity, extrHighVelocity);
  }
  
  const double q = 1.;
  mu *= q;
  if (b < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(mu * b, 2);
    vx = mu * (ex + 
               mu * (ey * bz - ez * by) +
               mu * mu * bx * eb) / nom;
    vy = mu * (ey +
               mu * (ez * bx - ex * bz) +
               mu * mu * by * eb) / nom;
    vz = mu * (ez +
               mu * mu * bz * eb) / nom;
  }
  
  return true;
    
}

bool 
Medium::IonDiffusion(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    
    // Interpolate.
    if (hasIonDiffLong) {
      if (!Numerics::Boxin3(tabIonDiffLong, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dl, intpDiffusion)) {
        dl = 0.;
      }
    }
    if (hasIonDiffTrans) {
      if (!Numerics::Boxin3(tabIonDiffTrans,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, dt, intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (hasIonDiffLong) {
      dl = Interpolate1D(e0, tabIonDiffLong[0][0], eFields,
                         intpDiffusion, 
                         extrLowDiffusion, extrHighDiffusion);
    }
    if (hasIonDiffTrans) {
      dt = Interpolate1D(e0, tabIonDiffTrans[0][0], eFields, 
                         intpDiffusion,
                         extrLowDiffusion, extrHighDiffusion);
    }
  }

  // If no data available, calculate 
  // the diffusion coefficients using the Einstein relation
  if (!hasIonDiffLong) {
    dl = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  if (!hasIonDiffTrans) {
    dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  
  return true;

}

bool 
Medium::IonDissociation(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& diss) {

  diss = 0.;
  if (!hasIonDissociation) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(1., 
                              sqrt(pow(ex * by - ey * bx, 2) +
                                   pow(ex * bz - ez * bx, 2) +
                                   pow(ez * by - ey * bz, 2)) / (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrIonDissociation]) {
      if (!Numerics::Boxin3(tabIonDissociation, 
                            bAngles, bFields, eFields, 
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, diss, 1)) {
        diss = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabIonDissociation,
                            bAngles, bFields, eFields,
                            nAngles, nBfields, nEfields,
                            ebang, b, e0, diss, intpDissociation)) {
        diss = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrIonDissociation]) {
      diss = Interpolate1D(e0, tabIonDissociation[0][0], eFields, 
                           1, extrLowDissociation, extrHighDissociation);
    } else {
      diss = Interpolate1D(e0, tabHoleTownsend[0][0], eFields,
                           intpDissociation,
                           extrLowDissociation, extrHighDissociation); 
    }
  }
  
  if (diss < -20.) {
    diss = 0.;
  } else {
    diss = exp(diss);
  }
  
  // Apply scaling.
  diss = ScaleDissociation(diss);  
  return true;

}

bool 
Medium::GetOpticalDataRange(double& emin, double& emax, const int i) {

  if (i < 0 || i >= nComponents) {
    std::cerr << className << "::GetOpticalDataRange:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (debug) {
    std::cerr << className << "::GetOpticalDataRange:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  emin = emax = 0.;
  return false;

}

bool 
Medium::GetDielectricFunction(const double e, 
                              double& eps1, double& eps2, const int i) {

  if (i < 0 || i >= nComponents) {
    std::cerr << className << "::GetDielectricFunction:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (e < 0.) {
    std::cerr << className << "::GetDielectricFunction:\n";
    std::cerr << "    Energy must be > 0.\n";
    return false;
  }

  if (debug) {
    std::cerr << className << "::GetDielectricFunction:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  eps1 = 1.; eps2 = 0.;
  return false;

}

bool 
Medium::GetPhotoAbsorptionCrossSection(const double e, 
                                       double& sigma, const int i) {

  if (i < 0 || i >= nComponents) {
    std::cerr << className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (e < 0.) {
    std::cerr << className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Energy must be > 0.\n";
    return false;
  }

  if (debug) {
    std::cerr << className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  sigma = 0.;
  return false;

}

double
Medium::GetPhotonCollisionRate(const double e) {

  double sigma = 0.;
  if (!GetPhotoAbsorptionCrossSection(e, sigma)) return 0.;
  
  return sigma * density * SpeedOfLight;

}

bool
Medium::GetPhotonCollision(const double e, int& type, int& level, 
                           double& e1, double& ctheta, 
                           int& nsec, double& esec) {

  type = level = -1;
  e1 = e;
  ctheta = 1.;
  nsec = 0;
  esec = 0.;
  return false;

}

void
Medium::SetExtrapolationMethodVelocity(const std::string extrLow, 
                                       const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) { 
    extrLowVelocity = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodVelocity:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighVelocity = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodVelocity:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodDiffusion(const std::string extrLow, 
                                        const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrLowDiffusion = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodDiffusion:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighDiffusion = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodDiffusion:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodTownsend(const std::string extrLow, 
                                       const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrLowTownsend = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodTownsend:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighTownsend = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodTownsend:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodAttachment(const std::string extrLow, 
                                         const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrLowAttachment = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighAttachment = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodIonMobility(const std::string extrLow,
                                          const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrLowMobility = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodIonMobility:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }
  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighMobility = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodIonMobility:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodIonDissociation(const std::string extrLow, 
                                              const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrLowDissociation = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodIonDissociation:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrHighDissociation = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodIonDissociation:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

bool
Medium::GetExtrapolationIndex(std::string extrStr, int& extrNb) {

  // Convert to upper-case
  for (unsigned int i = 0; i < extrStr.length(); ++i) {
    extrStr[i] = toupper(extrStr[i]);
  }

  if (extrStr == "CONST" || extrStr == "CONSTANT") {
    extrNb = 0;
  } else if (extrStr == "LIN" || extrStr == "LINEAR") {
    extrNb = 1;
  } else if (extrStr == "EXP" || extrStr == "EXPONENTIAL") {
    extrNb = 2;
  } else {
    return false;
  }
  
  return true;
  
}

void
Medium::SetInterpolationMethodVelocity(const int intrp) {

  if (intrp > 0) {
    intpVelocity = intrp;
  }

}

void
Medium::SetInterpolationMethodDiffusion(const int intrp) {

  if (intrp > 0) {
    intpDiffusion = intrp;
  }

}

void
Medium::SetInterpolationMethodTownsend(const int intrp) {

  if (intrp > 0) {
    intpTownsend = intrp;
  }

}

void
Medium::SetInterpolationMethodAttachment(const int intrp) {

  if (intrp > 0) {
    intpAttachment = intrp;
  }

}

void
Medium::SetInterpolationMethodIonMobility(const int intrp) {

  if (intrp > 0) {
    intpMobility = intrp;
  }

}

void
Medium::SetInterpolationMethodIonDissociation(const int intrp) {

  if (intrp > 0) {
    intpDissociation = intrp;
  }

}

double
Medium::Interpolate1D(const double e, 
                      const std::vector<double>& table,
                      const std::vector<double>& fields,
                      const int intpMeth,
                      const int extrLow, const int extrHigh) {

  // This function is a generalized version of the Fortran functions 
  // GASVEL, GASVT1, GASVT2, GASLOR, GASMOB, GASDFT, and GASDFL
  // for the case of a 1D table. All variables are generic. 
  
  const int nSizeTable = fields.size();
  
  if (e < 0. || nSizeTable < 1) return 0.;

  double result = 0.;  
  
  if (nSizeTable == 1) {
    // Only one point
    result = table[0];
  } else if (e < fields[0]) {
    // Extrapolation towards small fields
    if (fields[0] >= fields[1]) {
      if (debug) {
        std::cerr << className << "::Interpolate1D:\n";
        std::cerr << "    First two field values coincide.\n";
        std::cerr << "    No extrapolation to lower fields.\n";
      }
      result = table[0];
    } else if (extrLow == 1) {
      // Linear extrapolation
      const double extr4 = (table[1] - table[0]) / (fields[1] - fields[0]);
      const double extr3 = table[0] - extr4 * fields[0];
      result = extr3 + extr4 * e;
    } else if (extrLow == 2) {
      // Logarithmic extrapolation
      const double extr4 = log(table[1] / table[0]) / 
                           (fields[1] - fields[0]);
      const double extr3 = log(table[0] - extr4 * fields[0]);
      result = std::exp(std::min(50., extr3 + extr4 * e));
    } else {
      result = table[0];
    }
  } else if (e > fields[nSizeTable - 1]) {
    // Extrapolation towards large fields
    if (fields[nSizeTable - 1] <= fields[nSizeTable - 2]) {
      if (debug) {
        std::cerr << className << "::Interpolate1D:\n";
        std::cerr << "    Last two field values coincide.\n";
        std::cerr << "    No extrapolation to higher fields.\n";
      }
      result = table[nSizeTable - 1];
    } else if (extrHigh == 1) {
      // Linear extrapolation
      const double extr2 = (table[nSizeTable - 1] - table[nSizeTable - 2]) / 
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 = table[nSizeTable - 1] - 
                           extr2 * fields[nSizeTable - 1];
      result = extr1 + extr2 * e;
    } else if (extrHigh == 2) {
      // Logarithmic extrapolation
      const double extr2 = log(table[nSizeTable - 1] / 
                               table[nSizeTable - 2]) /
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 = log(table[nSizeTable - 1]) - 
                           extr2 * fields[nSizeTable - 1];
      result = exp(std::min(50., extr1 + extr2 * e));
    } else {
      result = table[nSizeTable - 1];
    }
  } else {
    // Intermediate points, spline interpolation (not implemented).
    // Intermediate points, Newtonian interpolation
    result = Numerics::Divdif(table, fields, nSizeTable, e, intpMeth);
  }

  return result;

}

void
Medium::InitParamArrays(const int eRes, const int bRes, const int aRes,
                   std::vector<std::vector<std::vector<double> > >& tab,
                   const double val) {

  if (eRes <= 0 || bRes <= 0 || aRes <= 0) {
    std::cerr << className << "::InitParamArrays:\n";
    std::cerr << "    Invalid grid.\n";
    return;
  }

  tab.resize(aRes);
  for (int i = aRes; i--;) {
    tab[i].resize(bRes);
    for (int j = bRes; j--;) {
      tab[i][j].resize(eRes);
      for (int k = eRes; k--;) {
        tab[i][j][k] = val;
      }
    }
  }

}
        
void
Medium::InitParamTensor(const int eRes, const int bRes, 
                        const int aRes, const int tRes,
       std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
       const double val) {

  if (eRes <= 0 || bRes <= 0 || aRes <= 0 || tRes <= 0) {
    std::cerr << className << "::InitParamArrays:\n";
    std::cerr << "    Invalid grid.\n";
    return;
  }

  tab.resize(tRes);
  for (int l = tRes; l--;) {
    tab[l].resize(aRes);
    for (int i = aRes; i--;) {
      tab[l][i].resize(bRes);
      for (int j = bRes; j--;) {
        tab[l][i][j].resize(eRes);
        for (int k = eRes; k--;) {
          tab[l][i][j][k] = val;
        }
      }
    }
  }

}

}
