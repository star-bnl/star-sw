#include <iostream>
#include <cmath>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "Numerics.hh"

namespace Garfield {

int Medium::idCounter = -1;

double Medium::inverseElectronMass = SpeedOfLight * SpeedOfLight / 
                                     ElectronMass;

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
  
  eFields.clear();
  bFields.clear();
  bAngles.clear();
  
  tabElectronVelocityE.clear();
  tabElectronVelocityB.clear();
  tabElectronVelocityExB.clear();
  tabElectronDiffLong.clear();
  tabElectronDiffTrans.clear();
  tabElectronTownsend.clear();
  tabElectronAttachment.clear();
  
  tabHoleVelocityE.clear();
  tabHoleVelocityB.clear();
  tabHoleVelocityExB.clear();
  tabHoleDiffLong.clear();
  tabHoleDiffTrans.clear();
  tabHoleTownsend.clear();
  tabHoleAttachment.clear();
  
  tabIonDissociation.clear();
  
  extrVelocityLow = extrVelocityHigh = 0;
  extrDiffusionLow = extrDiffusionHigh = 0;
  extrTownsendLow = extrTownsendHigh = 0;
  extrAttachmentLow = extrAttachmentHigh = 0;
  extrDissociationLow = extrDissociationHigh = 0;
  
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
    std::cerr << className << "::SetDielectricConstant: \n";
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
    std::cerr << className << "::GetComponent: Index out of range\n";
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
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute unit vectors along E, E x B and Btrans.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, 
                      ez * bx - ex * bz, 
                      ex * by - ey * bx};
    const double exb = sqrt(uexb[0] * uexb[0] + 
                            uexb[1] * uexb[1] + 
                            uexb[2] * uexb[2]);

    double ubt[3] = {
      uexb[1] * ez - uexb[2] * ey, 
      uexb[2] * ex - uexb[0] * ez, 
      uexb[0] * ey - uexb[1] * ex
    };
    const double bt = sqrt(ubt[0] * ubt[0] + 
                           ubt[1] * ubt[1] + 
                           ubt[2] * ubt[2]);

    if (b > 0.) {
      uexb[0] /= exb; uexb[1] /= exb; uexb[2] /= exb;
      ubt[0] /= bt; ubt[1] /= bt; ubt[2] /= bt;
    } else {
      uexb[0] = ubt[0] = ue[0];
      uexb[1] = ubt[1] = ue[1];
      uexb[2] = ubt[2] = ue[2];
    }

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
  
    // Calculate the velocities in all directions.
    double ve = 0.;
    double vexb = 0.;
    double vbt = 0.;
    const double q = 1.;
  
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);
  } else {
    const double v = Interpolate1D(e, tabElectronVelocityE[0][0], eFields, 
                                   extrVelocityLow, extrVelocityHigh, 0);
  	const double mu = v / e;
    const double q = 1.;
    vx = q * mu * ex;
    vy = q * mu * ey;
    vz = q * mu * ez;
  }
  
  // Verify value and apply scaling.
  return true;
  
}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    dl = Interpolate1D(e, tabElectronDiffLong[0][0], eFields, 
                       extrDiffusionLow, extrDiffusionHigh, 0);
    dt = Interpolate1D(e, tabElectronDiffTrans[0][0], eFields, 
                       extrDiffusionLow, extrDiffusionHigh, 0);
  }
  
  // Calculate diffusion coefficients using the Einstein relation
  // dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  
  // Verify value and apply scaling.
  return true;

}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.

  }
  
  return true;

}

bool 
Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& alpha) {

  alpha = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.
    alpha = Interpolate1D(e, tabElectronTownsend[0][0], eFields, 
                          extrTownsendLow, extrTownsendHigh, 0);
  }
  
  // Apply scaling.
  
  return true;

}

bool 
Medium::ElectronAttachment(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& eta) {

  eta = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.
    eta = Interpolate1D(e, tabElectronAttachment[0][0], eFields, 
                        extrAttachmentLow, extrAttachmentHigh, 0);
  }
  
  // Apply scaling.
  
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
  
  vx = inverseElectronMass * px;
  vy = inverseElectronMass * py;
  vz = inverseElectronMass * pz;
  
  return 0.5 * inverseElectronMass * (px * px + py * py + pz * pz);
  
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
                             double& e1, double& ctheta, 
                             int& nsec, double& esec,
                             int& band) {
  
  type = level = -1;
  e1 = e;
  ctheta = 1;
  nsec = 0;
  esec = 0.;
  band = 0;
 
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
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
  if (map2d) {
    // Compute the magnitude of the magnetic field
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute unit vectors along E, E x B and Btrans.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, 
                      ez * bx - ex * bz, 
                      ex * by - ey * bx};
    const double exb = sqrt(uexb[0] * uexb[0] + 
                            uexb[1] * uexb[1] + 
                            uexb[2] * uexb[2]);

    double ubt[3] = {
      uexb[1] * ez - uexb[2] * ey, 
      uexb[2] * ex - uexb[0] * ez, 
      uexb[0] * ey - uexb[1] * ex
    };
    const double bt = sqrt(ubt[0] * ubt[0] + 
                           ubt[1] * ubt[1] + 
                           ubt[2] * ubt[2]);

    if (b > 0.) {
      uexb[0] /= exb; uexb[1] /= exb; uexb[2] /= exb;
      ubt[0] /= bt; ubt[1] /= bt; ubt[2] /= bt;
    } else {
      uexb[0] = ubt[0] = ue[0];
      uexb[1] = ubt[1] = ue[1];
      uexb[2] = ubt[2] = ue[2];
    }

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
  
    // Calculate the velocities in all directions.
    double ve = 0.;
    double vexb = 0.;
    double vbt = 0.;
    const double q = 1.;
  
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);
  } else {
    const double v = Interpolate1D(e, tabHoleVelocityE[0][0], eFields, 
                                   extrVelocityLow, extrVelocityHigh, 0);
  	const double mu = v / e;
    const double q = 1.;
    vx = q * mu * ex;
    vy = q * mu * ey;
    vz = q * mu * ez;
  }
  
  // Verify value and apply scaling.
  return true;

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    dl = Interpolate1D(e, tabHoleDiffLong[0][0], eFields, 
                       extrDiffusionLow, extrDiffusionHigh, 0);
    dt = Interpolate1D(e, tabHoleDiffTrans[0][0], eFields, 
                       extrDiffusionLow, extrDiffusionHigh, 0);
  }
  
  // Calculate diffusion coefficients using the Einstein relation
  // dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  
  // Verify value and apply scaling.
  return true;

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.

  }
  
  return true;

}

bool 
Medium::HoleTownsend(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& alpha) {

  alpha = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.
    alpha = Interpolate1D(e, tabHoleTownsend[0][0], eFields, 
                          extrTownsendLow, extrTownsendHigh, 0);
  }
  
  // Apply scaling.
  
  return true;

}

bool 
Medium::HoleAttachment(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double& eta) {
            
  eta = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.
    eta = Interpolate1D(e, tabHoleAttachment[0][0], eFields, 
                        extrAttachmentLow, extrAttachmentHigh, 0);
  }
  
  // Apply scaling.
  
  return true;

}

bool 
Medium::IonVelocity(const double ex, const double ey, const double ez, 
                    const double bx, const double by, const double bz, 
                    double& vx, double& vy, double& vz) {

  if (debug) {
    std::cerr << className << "::IonVelocity:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  vx = vy = vz = 0.;
  return false;

}

bool 
Medium::IonDiffusion(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& dl, double& dt) {

  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  // Calculate diffusion coefficients using the Einstein relation
  if (e < Small) {
    dl = dt = 0.;
  } else {
    dl = dt = sqrt(2. * BoltzmannConstant * temperature / e);
  }
  return true;
  
  const double b = sqrt(bx * bx + by * by + bz * bz);

}

bool 
Medium::IonDissociation(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& diss) {

  diss = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e < Small) return true;
  
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

  } else {
    // Interpolate.
    diss = Interpolate1D(e, tabIonDissociation[0][0], eFields, 
                         extrDissociationLow, extrDissociationHigh, 0);
  }
  
  // Apply scaling.
  
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
    extrVelocityLow = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodVelocity:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrVelocityHigh = iExtr;
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
    extrDiffusionLow = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodDiffusion:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrDiffusionHigh = iExtr;
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
    extrTownsendLow = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodTownsend:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrTownsendHigh = iExtr;
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
    extrAttachmentLow = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrAttachmentHigh = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }

}

void
Medium::SetExtrapolationMethodDissociation(const std::string extrLow, 
                                           const std::string extrHigh) {

  int iExtr;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    extrDissociationLow = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodDissociation:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    extrDissociationHigh = iExtr;
  } else {
    std::cerr << className << "::SetExtrapolationMethodDissociation:\n";
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

double
Medium::Interpolate1D(const double e, 
                      const std::vector<double>& table,
                      const std::vector<double>& fields,
                      const int iMeth, const int extrLow, const int extrHigh) {

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
      const double extr4 = std::log(table[1] / table[0]) / (fields[1] - fields[0]);
      const double extr3 = std::log(table[0] - extr4 * fields[0]);
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
      const double extr1 = table[nSizeTable - 1] - extr2 * fields[nSizeTable - 1];
      result = extr1 + extr2 * e;
    } else if (extrHigh == 2) {
      // Logarithmic extrapolation
      const double extr2 = log(table[nSizeTable - 1] / table[nSizeTable - 2]) /
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 = log(table[nSizeTable - 1]) - extr2 * fields[nSizeTable - 1];
      result = exp(std::min(50., extr1 + extr2 * e));
	  } else {
      result = table[nSizeTable - 1];
    }
  } else {
  	// Intermediate points, spline interpolation (not implemented).
	  // Intermediate points, Newtonian interpolation
	  result = Numerics::Divdif(table, fields, nSizeTable, e, iMeth);
	}
  
	return result;

}

}
