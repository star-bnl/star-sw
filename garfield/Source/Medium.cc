#include <iostream>
#include <iomanip>
#include <cmath>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "Numerics.hh"

namespace Garfield {

int Medium::m_idCounter = -1;

Medium::Medium()
    : m_className("Medium"),
      m_id(++m_idCounter),
      m_name(""),
      m_temperature(293.15),
      m_pressure(760.),
      m_epsilon(1.),
      m_nComponents(1),
      m_z(1.),
      m_a(0.),
      m_density(0.),
      m_driftable(false),
      m_microscopic(false),
      m_ionisable(false),
      m_w(0.),
      m_fano(0.),
      m_isChanged(true),
      m_debug(false),
      m_map2d(false) {

  // Initialise the transport tables.
  m_nEfields = 0;
  m_nBfields = 1;
  m_nAngles = 1;

  eFields.clear();
  bFields.clear();
  bFields.assign(1, 0.);
  bAngles.clear();
  bAngles.assign(1, 0.);

  m_hasElectronVelocityE = false;
  tabElectronVelocityE.clear();
  m_hasElectronVelocityB = false;
  tabElectronVelocityB.clear();
  m_hasElectronVelocityExB = false;
  tabElectronVelocityExB.clear();
  m_hasElectronDiffLong = false;
  tabElectronDiffLong.clear();
  m_hasElectronDiffTrans = false;
  tabElectronDiffTrans.clear();
  m_hasElectronTownsend = false;
  tabElectronTownsend.clear();
  m_hasElectronAttachment = false;
  tabElectronAttachment.clear();
  m_hasElectronDiffTens = false;
  tabElectronDiffTens.clear();

  m_hasHoleVelocityE = false;
  tabHoleVelocityE.clear();
  m_hasHoleVelocityB = false;
  tabHoleVelocityB.clear();
  m_hasHoleVelocityExB = false;
  tabHoleVelocityExB.clear();
  m_hasHoleDiffLong = false;
  tabHoleDiffLong.clear();
  m_hasHoleDiffTrans = false;
  tabHoleDiffTrans.clear();
  m_hasHoleTownsend = false;
  tabHoleTownsend.clear();
  m_hasHoleAttachment = false;
  tabHoleAttachment.clear();
  m_hasHoleDiffTens = false;
  tabHoleDiffTens.clear();

  m_hasIonMobility = false;
  tabIonMobility.clear();
  m_hasIonDiffLong = false;
  tabIonDiffLong.clear();
  m_hasIonDiffTrans = false;
  tabIonDiffTrans.clear();
  m_hasIonDissociation = false;
  tabIonDissociation.clear();

  m_extrLowVelocity = 0;
  m_extrHighVelocity = 1;
  m_extrLowDiffusion = 0;
  m_extrHighDiffusion = 1;
  m_extrLowTownsend = 0;
  m_extrHighTownsend = 1;
  m_extrLowAttachment = 0;
  m_extrHighAttachment = 1;
  m_extrLowMobility = 0;
  m_extrHighMobility = 1;
  m_extrLowDissociation = 0;
  m_extrHighDissociation = 1;

  m_intpVelocity = 2;
  m_intpDiffusion = 2;
  m_intpTownsend = 2;
  m_intpAttachment = 2;
  m_intpMobility = 2;
  m_intpDissociation = 2;

  thrElectronTownsend = thrElectronAttachment = 0;
  thrHoleTownsend = thrHoleAttachment = 0;
  thrIonDissociation = 0;

  // Set the default grid.
  SetFieldGrid(100., 100000., 20, true, 0., 0., 1, 0., 0., 1);
}

void Medium::SetTemperature(const double t) {

  if (t <= 0.) {
    std::cerr << m_className << "::SetTemperature:\n";
    std::cerr << "    Temperature [K] must be greater than zero.\n";
    return;
  }
  m_temperature = t;
  m_isChanged = true;
}

void Medium::SetPressure(const double p) {

  if (p <= 0.) {
    std::cerr << m_className << "::SetPressure:\n";
    std::cerr << "    Pressure [Torr] must be greater than zero.\n";
    return;
  }
  m_pressure = p;
  m_isChanged = true;
}

void Medium::SetDielectricConstant(const double eps) {

  if (eps < 1.) {
    std::cerr << m_className << "::SetDielectricConstant:\n";
    std::cerr << "    Dielectric constant must be >= 1.\n";
    return;
  }
  m_epsilon = eps;
  m_isChanged = true;
}

double Medium::GetMassDensity() const {

  return m_density * AtomicMassUnit * m_a;
}

void Medium::GetComponent(const unsigned int i, 
                          std::string& label, double& f) {

  if (i >= m_nComponents) {
    std::cerr << m_className << "::GetComponent:\n";
    std::cerr << "    Index out of range.\n";
  }

  label = m_name;
  f = 1.;
}

void Medium::SetAtomicNumber(const double z) {

  if (z < 1.) {
    std::cerr << m_className << "::SetAtomicNumber:\n";
    std::cerr << "    Atomic number must be >= 1.\n";
    return;
  }
  m_z = z;
  m_isChanged = true;
}

void Medium::SetAtomicWeight(const double a) {

  if (a <= 0.) {
    std::cerr << m_className << "::SetAtomicWeight:\n";
    std::cerr << "    Atomic weight must be greater than zero.\n";
    return;
  }
  m_a = a;
  m_isChanged = true;
}

void Medium::SetNumberDensity(const double n) {

  if (n <= 0.) {
    std::cerr << m_className << "::SetNumberDensity:\n";
    std::cerr << "    Density [cm-3] must be greater than zero.\n";
    return;
  }
  m_density = n;
  m_isChanged = true;
}

void Medium::SetMassDensity(const double rho) {

  if (rho <= 0.) {
    std::cerr << m_className << "::SetMassDensity:\n";
    std::cerr << "    Density [g/cm3] must be greater than zero.\n";
    return;
  }

  if (m_a <= 0.) {
    std::cerr << m_className << "::SetMassDensity:\n";
    std::cerr << "    Atomic weight is not defined.\n";
    return;
  }
  m_density = rho / (AtomicMassUnit * m_a);
  m_isChanged = true;
}

bool Medium::ElectronVelocity(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  // Make sure there is at least a table of velocities along E.
  if (!m_hasElectronVelocityE) return false;

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
      ebang = asin(std::min(
          1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                   pow(ez * by - ey * bz, 2)) /
                  (e * b)));
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
    if (m_map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields, m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
    }
    const double q = -1.;
    const double mu = q * ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;

  } else if (m_hasElectronVelocityB && m_hasElectronVelocityExB) {
    // Magnetic field, velocities along ExB and Bt available

    // Compute unit vectors along E, E x B and Bt.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, ez * bx - ex * bz, ex * by - ey * bx};
    const double exb =
        sqrt(uexb[0] * uexb[0] + uexb[1] * uexb[1] + uexb[2] * uexb[2]);
    if (exb > 0.) {
      uexb[0] /= exb;
      uexb[1] /= exb;
      uexb[2] /= exb;
    } else {
      uexb[0] = ue[0];
      uexb[1] = ue[1];
      uexb[2] = ue[2];
    }

    double ubt[3] = {uexb[1] * ez - uexb[2] * ey, uexb[2] * ex - uexb[0] * ez,
                     uexb[0] * ey - uexb[1] * ex};
    const double bt = sqrt(ubt[0] * ubt[0] + ubt[1] * ubt[1] + ubt[2] * ubt[2]);

    if (bt > 0.) {
      ubt[0] /= bt;
      ubt[1] /= bt;
      ubt[2] /= bt;
    } else {
      ubt[0] = ue[0];
      ubt[1] = ue[1];
      ubt[2] = ue[2];
    }

    if (m_debug) {
      std::cout << std::setprecision(5);
      std::cout << m_className << "::ElectronVelocity:\n";
      std::cout << "    unit vector along E:     (" << ue[0] << ", " << ue[1]
                << ", " << ue[2] << ")\n";
      std::cout << "    unit vector along E x B: (" << uexb[0] << ", "
                << uexb[1] << ", " << uexb[2] << ")\n";
      std::cout << "    unit vector along Bt:    (" << ubt[0] << ", " << ubt[1]
                << ", " << ubt[2] << ")\n";
    }

    // Calculate the velocities in all directions.
    double ve = 0., vbt = 0., vexb = 0.;
    if (m_map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabElectronVelocityExB, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, vexb,
                            m_intpVelocity)) {
        std::cerr << m_className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along ExB failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabElectronVelocityB, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, vbt,
                            m_intpVelocity)) {
        std::cerr << m_className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along Bt failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields, 
                         m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
      vbt = Interpolate1D(e0, tabElectronVelocityB[0][0], eFields, 
                          m_intpVelocity,
                          m_extrLowVelocity, m_extrHighVelocity);
      vexb = Interpolate1D(e0, tabElectronVelocityExB[0][0], eFields,
                           m_intpVelocity, 
                           m_extrLowVelocity, m_extrHighVelocity);
    }
    const double q = -1.;
    if (ex * bx + ey * by + ez * bz > 0.) vbt = fabs(vbt);
     else vbt = -fabs(vbt);
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);

  } else {
    // Magnetic field, velocities along ExB, Bt not available

    // Calculate the velocity along E.
    double ve = 0.;
    if (m_map2d) {
      if (!Numerics::Boxin3(tabElectronVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::ElectronVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabElectronVelocityE[0][0], eFields, 
                         m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
    }

    const double q = -1.;
    const double mu = q * ve / e;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(mu * b, 2);
    vx = mu * (ex + mu * (ey * bz - ez * by) + mu * mu * bx * eb) / nom;
    vy = mu * (ey + mu * (ez * bx - ex * bz) + mu * mu * by * eb) / nom;
    vz = mu * (ez + mu * (ex * by - ey * bx) + mu * mu * bz * eb) / nom;
  }

  return true;
}

bool Medium::ElectronDiffusion(const double ex, const double ey,
                               const double ez, const double bx,
                               const double by, const double bz, double& dl,
                               double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }

    // Interpolate.
    if (m_hasElectronDiffLong) {
      if (!Numerics::Boxin3(tabElectronDiffLong, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, dl,
                            m_intpDiffusion)) {
        dl = 0.;
      }
    }
    if (m_hasElectronDiffTrans) {
      if (!Numerics::Boxin3(tabElectronDiffTrans, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, dt,
                            m_intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (m_hasElectronDiffLong) {
      dl = Interpolate1D(e0, tabElectronDiffLong[0][0], eFields, 
                         m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
    if (m_hasElectronDiffTrans) {
      dt = Interpolate1D(e0, tabElectronDiffTrans[0][0], eFields, 
                         m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (!m_hasElectronDiffLong || !m_hasElectronDiffTrans) {
    const double d = sqrt(2. * BoltzmannConstant * m_temperature / e);
    if (!m_hasElectronDiffLong) dl = d;
    if (!m_hasElectronDiffTrans) dt = d;
  }
  // Verify values and apply scaling.
  if (dl < 0.) dl = 0.;
  if (dt < 0.) dt = 0.;
  dl = ScaleDiffusion(dl);
  dt = ScaleDiffusion(dt);

  return true;
}

bool Medium::ElectronDiffusion(const double ex, const double ey,
                               const double ez, const double bx,
                               const double by, const double bz,
                               double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = cov[0][1] = cov[0][2] = 0.;
  cov[1][0] = cov[1][1] = cov[1][2] = 0.;
  cov[2][0] = cov[2][1] = cov[2][2] = 0.;

  if (!m_hasElectronDiffTens) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    double diff = 0.;
    for (int l = 0; l < 6; ++l) {
      if (!Numerics::Boxin3(tabElectronDiffTens[l], bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, diff,
                            m_intpDiffusion)) {
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
      double diff =
          Interpolate1D(e0, tabElectronDiffTens[l][0][0], eFields,
                        m_intpDiffusion, 
                        m_extrLowDiffusion, m_extrHighDiffusion);
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

bool Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& alpha) {

  alpha = 0.;
  if (!m_hasElectronTownsend) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrElectronTownsend]) {
      if (!Numerics::Boxin3(tabElectronTownsend, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, alpha,
                            1)) {
        alpha = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabElectronTownsend, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, alpha,
                            m_intpTownsend)) {
        alpha = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrElectronTownsend]) {
      alpha = Interpolate1D(e0, tabElectronTownsend[0][0], eFields, 1,
                            m_extrLowTownsend, m_extrHighTownsend);
    } else {
      alpha = Interpolate1D(e0, tabElectronTownsend[0][0], eFields,
                            m_intpTownsend, 
                            m_extrLowTownsend, m_extrHighTownsend);
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

bool Medium::ElectronAttachment(const double ex, const double ey,
                                const double ez, const double bx,
                                const double by, const double bz, double& eta) {

  eta = 0.;
  if (!m_hasElectronAttachment) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrElectronAttachment]) {
      if (!Numerics::Boxin3(tabElectronAttachment, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, eta,
                            1)) {
        eta = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabElectronAttachment, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, eta,
                            m_intpAttachment)) {
        eta = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrElectronAttachment]) {
      eta = Interpolate1D(e0, tabElectronAttachment[0][0], eFields, 1,
                          m_extrLowAttachment, m_extrHighAttachment);
    } else {
      eta =
          Interpolate1D(e0, tabElectronAttachment[0][0], eFields,
                        m_intpAttachment, 
                        m_extrLowAttachment, m_extrHighAttachment);
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

double Medium::GetElectronEnergy(const double px, const double py,
                                 const double pz, double& vx, double& vy,
                                 double& vz, const int band) {

  if (band > 0) {
    std::cerr << m_className << "::GetElectronEnergy:\n";
    std::cerr << "    Unknown band index.\n";
  }

  vx = SpeedOfLight * px / ElectronMass;
  vy = SpeedOfLight * py / ElectronMass;
  vz = SpeedOfLight * pz / ElectronMass;

  return 0.5 * (px * px + py * py + pz * pz) / ElectronMass;
}

void Medium::GetElectronMomentum(const double e, double& px, double& py,
                                 double& pz, int& band) {

  const double p = sqrt(2. * ElectronMass * e) / SpeedOfLight;
  const double ctheta = 1. - 2. * RndmUniform();
  const double stheta = sqrt(1. - ctheta * ctheta);
  const double phi = TwoPi * RndmUniform();

  px = p * stheta * cos(phi);
  py = p * stheta * sin(phi);
  pz = p * ctheta;

  band = -1;
}

double Medium::GetElectronNullCollisionRate(const int /*band*/) {

  if (m_debug) {
    std::cerr << m_className << "::GetElectronNullCollisionRate:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return 0.;
}

double Medium::GetElectronCollisionRate(const double /*e*/, 
                                        const int /*band*/) {

  if (m_debug) {
    std::cerr << m_className << "::GetElectronCollisionRate:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return 0.;
}

bool Medium::GetElectronCollision(const double e, int& type, int& level,
                                  double& e1, double& dx, double& dy,
                                  double& dz, int& nion, int& ndxc, int& band) {

  type = level = -1;
  e1 = e;
  nion = ndxc = band = 0;
  const double ctheta = 1. - 2 * RndmUniform();
  const double stheta = sqrt(1. - ctheta * ctheta);
  const double phi = TwoPi * RndmUniform();
  dx = cos(phi) * stheta;
  dy = sin(phi) * stheta;
  dz = ctheta;

  if (m_debug) {
    std::cerr << m_className << "::GetElectronCollision:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  return false;
}

bool Medium::GetIonisationProduct(const int i, int& type, double& energy) {

  if (m_debug) {
    std::cerr << m_className << "::GetIonisationProduct:\n";
    std::cerr << "    Ionisation product " << i << " requested.\n";
    std::cerr << "    Not supported. Program bug!\n";
  }
  type = 0;
  energy = 0.;
  return false;
}

bool Medium::GetDeexcitationProduct(const int i, double& t, double& s,
                                    int& type, double& energy) {

  if (m_debug) {
    std::cerr << m_className << "::GetDeexcitationProduct:\n";
    std::cerr << "    Deexcitation product " << i << " requested.\n";
    std::cerr << "    Not supported. Program bug!\n";
  }
  t = s = energy = 0.;
  type = 0;
  return false;
}

bool Medium::HoleVelocity(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  // Make sure there is at least a table of velocities along E.
  if (!m_hasHoleVelocityE) return false;

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
      ebang = asin(std::min(
          1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                   pow(ez * by - ey * bz, 2)) /
                  (e * b)));
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
    if (m_map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields, m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
    }
    const double q = 1.;
    const double mu = q * ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;

  } else if (m_hasHoleVelocityB && m_hasHoleVelocityExB) {
    // Magnetic field, velocities along ExB and Bt available

    // Compute unit vectors along E, E x B and Bt.
    double ue[3] = {ex / e, ey / e, ez / e};
    double uexb[3] = {ey * bz - ez * by, ez * bx - ex * bz, ex * by - ey * bx};
    const double exb =
        sqrt(uexb[0] * uexb[0] + uexb[1] * uexb[1] + uexb[2] * uexb[2]);
    if (exb > 0.) {
      uexb[0] /= exb;
      uexb[1] /= exb;
      uexb[2] /= exb;
    } else {
      uexb[0] = ue[0];
      uexb[1] = ue[1];
      uexb[2] = ue[2];
    }

    double ubt[3] = {uexb[1] * ez - uexb[2] * ey, uexb[2] * ex - uexb[0] * ez,
                     uexb[0] * ey - uexb[1] * ex};
    const double bt = sqrt(ubt[0] * ubt[0] + ubt[1] * ubt[1] + ubt[2] * ubt[2]);

    if (bt > 0.) {
      ubt[0] /= bt;
      ubt[1] /= bt;
      ubt[2] /= bt;
    } else {
      ubt[0] = ue[0];
      ubt[1] = ue[1];
      ubt[2] = ue[2];
    }

    // Calculate the velocities in all directions.
    double ve = 0., vbt = 0., vexb = 0.;
    if (m_map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabHoleVelocityExB, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, vexb,
                            m_intpVelocity)) {
        std::cerr << m_className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along ExB failed.\n";
        return false;
      }
      if (!Numerics::Boxin3(tabHoleVelocityB, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, vbt,
                            m_intpVelocity)) {
        std::cerr << m_className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along Bt failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields, m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
      vbt = Interpolate1D(e0, tabHoleVelocityB[0][0], eFields, m_intpVelocity,
                          m_extrLowVelocity, m_extrHighVelocity);
      vexb = Interpolate1D(e0, tabHoleVelocityExB[0][0], eFields, m_intpVelocity,
                           m_extrLowVelocity, m_extrHighVelocity);
    }
    const double q = 1.;
    if (ex * bx + ey * by + ez * bz > 0.) vbt = fabs(vbt);
     else vbt = -fabs(vbt);
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);

  } else {
    // Magnetic field, velocities along ExB, Bt not available

    // Calculate the velocity along E.
    double ve = 0.;
    if (m_map2d) {
      if (!Numerics::Boxin3(tabHoleVelocityE, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, ve,
                            m_intpVelocity)) {
        std::cerr << m_className << "::HoleVelocity:\n";
        std::cerr << "    Interpolation of velocity along E failed.\n";
        return false;
      }
    } else {
      ve = Interpolate1D(e0, tabHoleVelocityE[0][0], eFields, m_intpVelocity,
                         m_extrLowVelocity, m_extrHighVelocity);
    }

    const double q = 1.;
    const double mu = q * ve / e;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + pow(mu * b, 2);
    vx = mu * (ex + mu * (ey * bz - ez * by) + mu * mu * bx * eb) / nom;
    vy = mu * (ey + mu * (ez * bx - ex * bz) + mu * mu * by * eb) / nom;
    vz = mu * (ez + mu * (ex * by - ey * bx) + mu * mu * bz * eb) / nom;
  }

  return true;
}

bool Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }

    // Interpolate.
    if (m_hasHoleDiffLong) {
      if (!Numerics::Boxin3(tabHoleDiffLong, bAngles, bFields, eFields, m_nAngles,
                            m_nBfields, m_nEfields, ebang, b, e0, dl,
                            m_intpDiffusion)) {
        dl = 0.;
      }
    }
    if (m_hasHoleDiffTrans) {
      if (!Numerics::Boxin3(tabHoleDiffTrans, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, dt,
                            m_intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (m_hasHoleDiffLong) {
      dl = Interpolate1D(e0, tabHoleDiffLong[0][0], eFields, m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
    if (m_hasHoleDiffTrans) {
      dt = Interpolate1D(e0, tabHoleDiffTrans[0][0], eFields, m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (!m_hasHoleDiffLong) {
    dl = sqrt(2. * BoltzmannConstant * m_temperature / e);
  }
  if (!m_hasHoleDiffTrans) {
    dt = sqrt(2. * BoltzmannConstant * m_temperature / e);
  }

  // Verify values and apply scaling.
  if (dl < 0.) dl = 0.;
  if (dt < 0.) dt = 0.;
  dl = ScaleDiffusion(dl);
  dt = ScaleDiffusion(dt);

  return true;
}

bool Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double cov[3][3]) {

  // Initialise the tensor.
  cov[0][0] = cov[0][1] = cov[0][2] = 0.;
  cov[1][0] = cov[1][1] = cov[1][2] = 0.;
  cov[2][0] = cov[2][1] = cov[2][2] = 0.;

  if (!m_hasHoleDiffTens) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    double diff = 0.;
    for (int l = 0; l < 6; ++l) {
      if (!Numerics::Boxin3(tabHoleDiffTens[l], bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, diff,
                            m_intpDiffusion)) {
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
      double diff =
          Interpolate1D(e0, tabHoleDiffTens[l][0][0], eFields, m_intpDiffusion,
                        m_extrLowDiffusion, m_extrHighDiffusion);
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

bool Medium::HoleTownsend(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& alpha) {

  alpha = 0.;
  if (!m_hasHoleTownsend) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrHoleTownsend]) {
      if (!Numerics::Boxin3(tabHoleTownsend, bAngles, bFields, eFields, m_nAngles,
                            m_nBfields, m_nEfields, ebang, b, e0, alpha, 1)) {
        alpha = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabHoleTownsend, bAngles, bFields, eFields, m_nAngles,
                            m_nBfields, m_nEfields, ebang, b, e0, alpha,
                            m_intpTownsend)) {
        alpha = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrHoleTownsend]) {
      alpha = Interpolate1D(e0, tabHoleTownsend[0][0], eFields, 1,
                            m_extrLowTownsend, m_extrHighTownsend);
    } else {
      alpha = Interpolate1D(e0, tabHoleTownsend[0][0], eFields, m_intpTownsend,
                            m_extrLowTownsend, m_extrHighTownsend);
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

bool Medium::HoleAttachment(const double ex, const double ey, const double ez,
                            const double bx, const double by, const double bz,
                            double& eta) {

  eta = 0.;
  if (!m_hasHoleAttachment) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrHoleAttachment]) {
      if (!Numerics::Boxin3(tabHoleAttachment, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, eta,
                            1)) {
        eta = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabHoleAttachment, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, eta,
                            m_intpAttachment)) {
        eta = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrHoleAttachment]) {
      eta = Interpolate1D(e0, tabHoleAttachment[0][0], eFields, 1,
                          m_extrLowAttachment, m_extrHighAttachment);
    } else {
      eta = Interpolate1D(e0, tabHoleAttachment[0][0], eFields, 
                          m_intpAttachment,
                          m_extrLowAttachment, m_extrHighAttachment);
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

bool Medium::IonVelocity(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  if (!m_hasIonMobility) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  // Compute the magnitude of the electric field.
  const double b = sqrt(bx * bx + by * by + bz * bz);

  double mu = 0.;
  if (m_map2d) {
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    if (!Numerics::Boxin3(tabIonMobility, bAngles, bFields, eFields, m_nAngles,
                          m_nBfields, m_nEfields, ebang, b, e0, mu, 
                          m_intpMobility)) {
      mu = 0.;
    }
  } else {
    mu = Interpolate1D(e0, tabIonMobility[0][0], eFields, m_intpMobility,
                       m_extrLowMobility, m_extrHighMobility);
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
    vx = mu * (ex + mu * (ey * bz - ez * by) + mu * mu * bx * eb) / nom;
    vy = mu * (ey + mu * (ez * bx - ex * bz) + mu * mu * by * eb) / nom;
    vz = mu * (ez + mu * (ex * by - ey * bx) + mu * mu * bz * eb) / nom;
  }

  return true;
}

bool Medium::IonDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& dl, double& dt) {

  dl = dt = 0.;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);
    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }

    // Interpolate.
    if (m_hasIonDiffLong) {
      if (!Numerics::Boxin3(tabIonDiffLong, bAngles, bFields, eFields, m_nAngles,
                            m_nBfields, m_nEfields, ebang, b, e0, dl,
                            m_intpDiffusion)) {
        dl = 0.;
      }
    }
    if (m_hasIonDiffTrans) {
      if (!Numerics::Boxin3(tabIonDiffTrans, bAngles, bFields, eFields, m_nAngles,
                            m_nBfields, m_nEfields, ebang, b, e0, dt,
                            m_intpDiffusion)) {
        dt = 0.;
      }
    }
  } else {
    if (m_hasIonDiffLong) {
      dl = Interpolate1D(e0, tabIonDiffLong[0][0], eFields, m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
    if (m_hasIonDiffTrans) {
      dt = Interpolate1D(e0, tabIonDiffTrans[0][0], eFields, m_intpDiffusion,
                         m_extrLowDiffusion, m_extrHighDiffusion);
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (!m_hasIonDiffLong) {
    dl = sqrt(2. * BoltzmannConstant * m_temperature / e);
  }
  if (!m_hasIonDiffTrans) {
    dt = sqrt(2. * BoltzmannConstant * m_temperature / e);
  }

  return true;
}

bool Medium::IonDissociation(const double ex, const double ey, const double ez,
                             const double bx, const double by, const double bz,
                             double& diss) {

  diss = 0.;
  if (!m_hasIonDissociation) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  if (m_map2d) {
    // Compute the magnitude of the magnetic field.
    const double b = sqrt(bx * bx + by * by + bz * bz);

    // Compute the angle between B field and E field.
    double ebang = 0.;
    if (e * b > 0.) {
      const double eb = fabs(ex * bx + ey * by + ez * bz);
      if (eb > 0.2 * e * b) {
        ebang = asin(std::min(
            1., sqrt(pow(ex * by - ey * bx, 2) + pow(ex * bz - ez * bx, 2) +
                     pow(ez * by - ey * bz, 2)) /
                    (e * b)));
      } else {
        ebang = acos(std::min(1., eb / (e * b)));
      }
    } else {
      ebang = bAngles[0];
    }
    // Interpolate.
    if (e0 < eFields[thrIonDissociation]) {
      if (!Numerics::Boxin3(tabIonDissociation, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, diss,
                            1)) {
        diss = -30.;
      }
    } else {
      if (!Numerics::Boxin3(tabIonDissociation, bAngles, bFields, eFields,
                            m_nAngles, m_nBfields, m_nEfields, ebang, b, e0, diss,
                            m_intpDissociation)) {
        diss = -30.;
      }
    }
  } else {
    // Interpolate.
    if (e0 < eFields[thrIonDissociation]) {
      diss = Interpolate1D(e0, tabIonDissociation[0][0], eFields, 1,
                           m_extrLowDissociation, m_extrHighDissociation);
    } else {
      diss = Interpolate1D(e0, tabHoleTownsend[0][0], eFields, 
                           m_intpDissociation,
                           m_extrLowDissociation, m_extrHighDissociation);
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

bool Medium::GetOpticalDataRange(double& emin, double& emax, 
                                 const unsigned int i) {

  if (i >= m_nComponents) {
    std::cerr << m_className << "::GetOpticalDataRange:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (m_debug) {
    std::cerr << m_className << "::GetOpticalDataRange:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  emin = emax = 0.;
  return false;
}

bool Medium::GetDielectricFunction(const double e, double& eps1, double& eps2,
                                   const unsigned int i) {

  if (i >= m_nComponents) {
    std::cerr << m_className << "::GetDielectricFunction:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (e < 0.) {
    std::cerr << m_className << "::GetDielectricFunction:\n";
    std::cerr << "    Energy must be > 0.\n";
    return false;
  }

  if (m_debug) {
    std::cerr << m_className << "::GetDielectricFunction:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  eps1 = 1.;
  eps2 = 0.;
  return false;
}

bool Medium::GetPhotoAbsorptionCrossSection(const double e, double& sigma,
                                            const unsigned int i) {

  if (i >= m_nComponents) {
    std::cerr << m_className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Component " << i << " does not exist.\n";
    return false;
  }

  if (e < 0.) {
    std::cerr << m_className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Energy must be > 0.\n";
    return false;
  }

  if (m_debug) {
    std::cerr << m_className << "::GetPhotoAbsorptionCrossSection:\n";
    std::cerr << "    Function is not implemented.\n";
  }
  sigma = 0.;
  return false;
}

double Medium::GetPhotonCollisionRate(const double e) {

  double sigma = 0.;
  if (!GetPhotoAbsorptionCrossSection(e, sigma)) return 0.;

  return sigma * m_density * SpeedOfLight;
}

bool Medium::GetPhotonCollision(const double e, int& type, int& level,
                                double& e1, double& ctheta, int& nsec,
                                double& esec) {

  type = level = -1;
  e1 = e;
  ctheta = 1.;
  nsec = 0;
  esec = 0.;
  return false;
}

void Medium::ResetElectronVelocity() {

  tabElectronVelocityE.clear();
  tabElectronVelocityB.clear();
  tabElectronVelocityExB.clear();
  m_hasElectronVelocityE = false;
  m_hasElectronVelocityB = false;
  m_hasElectronVelocityExB = false;
}

void Medium::ResetElectronDiffusion() {

  tabElectronDiffLong.clear();
  tabElectronDiffTrans.clear();
  tabElectronDiffTens.clear();
  m_hasElectronDiffLong = false;
  m_hasElectronDiffTrans = false;
  m_hasElectronDiffTens = false;
}

void Medium::ResetElectronTownsend() {

  tabElectronTownsend.clear();
  m_hasElectronTownsend = false;
}

void Medium::ResetElectronAttachment() {

  tabElectronAttachment.clear();
  m_hasElectronAttachment = false;
}

void Medium::ResetHoleVelocity() {

  tabHoleVelocityE.clear();
  tabHoleVelocityB.clear();
  tabHoleVelocityExB.clear();
  m_hasHoleVelocityE = false;
  m_hasHoleVelocityB = false;
  m_hasHoleVelocityExB = false;
}

void Medium::ResetHoleDiffusion() {

  tabHoleDiffLong.clear();
  tabHoleDiffTrans.clear();
  tabHoleDiffTens.clear();
  m_hasHoleDiffLong = false;
  m_hasHoleDiffTrans = false;
  m_hasHoleDiffTens = false;
}

void Medium::ResetHoleTownsend() {

  tabHoleTownsend.clear();
  m_hasHoleTownsend = false;
}

void Medium::ResetHoleAttachment() {

  tabHoleAttachment.clear();
  m_hasHoleAttachment = false;
}

void Medium::ResetIonMobility() {

  tabIonMobility.clear();
  m_hasIonMobility = false;
}

void Medium::ResetIonDiffusion() {

  tabIonDiffLong.clear();
  tabIonDiffTrans.clear();
  m_hasIonDiffLong = false;
  m_hasIonDiffTrans = false;
}

void Medium::ResetIonDissociation() {

  tabIonDissociation.clear();
  m_hasIonDissociation = false;
}

void Medium::SetFieldGrid(double emin, double emax, int ne, bool logE,
                          double bmin, double bmax, int nb, double amin,
                          double amax, int na) {

  // Check if the requested E-field range makes sense.
  if (ne <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of E-fields must be > 0.\n";
    return;
  }

  if (emin < 0. || emax < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Electric fields must be positive.\n";
    return;
  }

  if (emax < emin) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Swapping min./max. E-field.\n";
    const double etemp = emin;
    emin = emax;
    emax = etemp;
  }

  double estep = 0.;
  if (logE) {
    // Logarithmic scale
    if (emin < Small) {
      std::cerr << m_className << "::SetFieldGrid:\n";
      std::cerr << "    Min. E-field must be non-zero for log. scale.\n";
      return;
    }
    if (ne == 1) {
      std::cerr << m_className << "::SetFieldGrid:\n";
      std::cerr << "    Number of E-fields must be > 1 for log. scale.\n";
      return;
    }
    estep = pow(emax / emin, 1. / (ne - 1.));
  } else {
    // Linear scale
    if (ne > 1) estep = (emax - emin) / (ne - 1.);
  }

  // Check if the requested B-field range makes sense.
  if (nb <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of B-fields must be > 0.\n";
    return;
  }
  if (bmax < 0. || bmin < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Magnetic fields must be positive.\n";
    return;
  }
  if (bmax < bmin) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Swapping min./max. B-field.\n";
    const double btemp = bmin;
    bmin = bmax;
    bmax = btemp;
  }

  double bstep = 0.;
  if (nb > 1) bstep = (bmax - bmin) / (nb - 1.);

  // Check if the requested angular range makes sense.
  if (na <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of angles must be > 0.\n";
    return;
  }
  if (amax < 0. || amin < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Angles must be positive.\n";
    return;
  }
  if (amax < amin) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Swapping min./max. angle.\n";
    const double atemp = amin;
    amin = amax;
    amax = atemp;
  }
  double astep = 0.;
  if (na > 1) astep = (amax - amin) / (na - 1.);

  // Setup the field grids.
  std::vector<double> eFieldsNew(ne);
  std::vector<double> bFieldsNew(nb);
  std::vector<double> bAnglesNew(na);
  for (int i = 0; i < ne; ++i) {
    if (logE) {
      eFieldsNew[i] = emin * pow(estep, i);
    } else {
      eFieldsNew[i] = emin + i * estep;
    }
  }
  for (int i = 0; i < nb; ++i) {
    bFieldsNew[i] = bmin + i * bstep;
  }
  if (na == 1 && nb == 1 && fabs(bmin) < 1.e-4) {
    bAnglesNew[0] = HalfPi;
  } else {
    for (int i = 0; i < na; ++i) {
      bAnglesNew[i] = amin + i * astep;
    }
  }
  SetFieldGrid(eFieldsNew, bFieldsNew, bAnglesNew);
}

void Medium::SetFieldGrid(const std::vector<double>& efields,
                          const std::vector<double>& bfields,
                          const std::vector<double>& angles) {

  if (efields.empty()) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of E-fields must be > 0.\n";
    return;
  }
  if (bfields.empty()) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of B-fields must be > 0.\n";
    return;
  }
  if (angles.empty()) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Number of angles must be > 0.\n";
    return;
  }

  // Make sure the values are not negative.
  if (efields[0] < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    E-fields must be >= 0.\n";
  }
  if (bfields[0] < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    B-fields must be >= 0.\n";
  }
  if (angles[0] < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n";
    std::cerr << "    Angles must be >= 0.\n";
  }

  const unsigned int nEfieldsNew = efields.size();
  const unsigned int nBfieldsNew = bfields.size();
  const unsigned int nAnglesNew = angles.size();
  // Make sure the values are in strictly monotonic, ascending order.
  if (nEfieldsNew > 1) {
    for (unsigned int i = 1; i < nEfieldsNew; ++i) {
      if (efields[i] <= efields[i - 1]) {
        std::cerr << m_className << "::SetFieldGrid:\n";
        std::cerr << "    E-fields are not in ascending order.\n";
        return;
      }
    }
  }
  if (nBfieldsNew > 1) {
    for (unsigned int i = 1; i < nBfieldsNew; ++i) {
      if (bfields[i] <= bfields[i - 1]) {
        std::cerr << m_className << "::SetFieldGrid:\n";
        std::cerr << "    B-fields are not in ascending order.\n";
        return;
      }
    }
  }
  if (nAnglesNew > 1) {
    for (unsigned int i = 1; i < nAnglesNew; ++i) {
      if (angles[i] <= angles[i - 1]) {
        std::cerr << m_className << "::SetFieldGrid:\n";
        std::cerr << "    Angles are not in ascending order.\n";
        return;
      }
    }
  }

  if (m_debug) {
    std::cout << m_className << "::SetFieldGrid:\n";
    std::cout << "    E-fields:\n";
    for (unsigned int i = 0; i < nEfieldsNew; ++i) {
      std::cout << "      " << efields[i] << "\n";
    }
    std::cout << "    B-fields:\n";
    for (unsigned int i = 0; i < nBfieldsNew; ++i) {
      std::cout << "      " << bfields[i] << "\n";
    }
    std::cout << "    Angles:\n";
    for (unsigned int i = 0; i < nAnglesNew; ++i) {
      std::cout << "      " << angles[i] << "\n";
    }
  }

  // Clone the existing tables.
  // Electrons
  if (m_hasElectronVelocityE) {
    CloneTable(tabElectronVelocityE, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0.,
               "electron velocity along E");
  }
  if (m_hasElectronVelocityB) {
    CloneTable(tabElectronVelocityB, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0.,
               "electron velocity along Bt");
  }
  if (m_hasElectronVelocityExB) {
    CloneTable(tabElectronVelocityExB, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0.,
               "electron velocity along ExB");
  }
  if (m_hasElectronDiffLong) {
    CloneTable(tabElectronDiffLong, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "electron longitudinal diffusion");
  }
  if (m_hasElectronDiffTrans) {
    CloneTable(tabElectronDiffTrans, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "electron transverse diffusion");
  }
  if (m_hasElectronTownsend) {
    CloneTable(tabElectronTownsend, efields, bfields, angles, m_intpTownsend,
               m_extrLowTownsend, m_extrHighTownsend, -30.,
               "electron Townsend coefficient");
  }
  if (m_hasElectronAttachment) {
    CloneTable(tabElectronAttachment, efields, bfields, angles, m_intpAttachment,
               m_extrLowAttachment, m_extrHighAttachment, -30.,
               "electron attachment coefficient");
  }
  if (m_hasElectronDiffTens) {
    CloneTensor(tabElectronDiffTens, 6, efields, bfields, angles, m_intpDiffusion,
                m_extrLowDiffusion, m_extrHighDiffusion, 0.,
                "electron diffusion tensor");
  }

  // Holes
  if (m_hasHoleVelocityE) {
    CloneTable(tabHoleVelocityE, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0., 
               "hole velocity along E");
  }
  if (m_hasHoleVelocityB) {
    CloneTable(tabHoleVelocityB, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0., 
               "hole velocity along Bt");
  }
  if (m_hasHoleVelocityExB) {
    CloneTable(tabHoleVelocityExB, efields, bfields, angles, m_intpVelocity,
               m_extrLowVelocity, m_extrHighVelocity, 0.,
               "hole velocity along ExB");
  }
  if (m_hasHoleDiffLong) {
    CloneTable(tabHoleDiffLong, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "hole longitudinal diffusion");
  }
  if (m_hasHoleDiffTrans) {
    CloneTable(tabHoleDiffTrans, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "hole transverse diffusion");
  }
  if (m_hasHoleTownsend) {
    CloneTable(tabHoleTownsend, efields, bfields, angles, m_intpTownsend,
               m_extrLowTownsend, m_extrHighTownsend, -30.,
               "hole Townsend coefficient");
  }
  if (m_hasHoleAttachment) {
    CloneTable(tabHoleAttachment, efields, bfields, angles, m_intpAttachment,
               m_extrLowAttachment, m_extrHighAttachment, -30.,
               "hole attachment coefficient");
  }
  if (m_hasHoleDiffTens) {
    CloneTensor(tabHoleDiffTens, 6, efields, bfields, angles, m_intpDiffusion,
                m_extrLowDiffusion, m_extrHighDiffusion, 0.,
                "hole diffusion tensor");
  }

  // Ions
  if (m_hasIonMobility) {
    CloneTable(tabIonMobility, efields, bfields, angles, m_intpMobility,
               m_extrLowMobility, m_extrHighMobility, 0., 
               "ion mobility");
  }
  if (m_hasIonDiffLong) {
    CloneTable(tabIonDiffLong, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "ion longitudinal diffusion");
  }
  if (m_hasIonDiffTrans) {
    CloneTable(tabIonDiffTrans, efields, bfields, angles, m_intpDiffusion,
               m_extrLowDiffusion, m_extrHighDiffusion, 0.,
               "ion transverse diffusion");
  }
  if (m_hasIonDissociation) {
    CloneTable(tabIonDissociation, efields, bfields, angles, m_intpDissociation,
               m_extrLowDissociation, m_extrHighDissociation, -30.,
               "ion dissociation");
  }

  m_nEfields = nEfieldsNew;
  m_nBfields = nBfieldsNew;
  m_nAngles = nAnglesNew;
  if (m_nBfields > 1 || m_nAngles > 1) m_map2d = true;
  eFields.resize(m_nEfields);
  bFields.resize(m_nBfields);
  bAngles.resize(m_nAngles);
  eFields = efields;
  bFields = bfields;
  bAngles = angles;
}

void Medium::GetFieldGrid(std::vector<double>& efields,
                          std::vector<double>& bfields,
                          std::vector<double>& angles) {

  efields = eFields;
  bfields = bFields;
  angles = bAngles;
}

bool Medium::GetElectronVelocityE(const unsigned int ie, 
                                  const unsigned int ib, 
                                  const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronVelocityE:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasElectronVelocityE) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronVelocityE:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabElectronVelocityE[ia][ib][ie];
  return true;
}

bool Medium::GetElectronVelocityExB(const unsigned int ie, 
                                    const unsigned int ib, 
                                    const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronVelocityExB:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasElectronVelocityExB) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronVelocityExB:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabElectronVelocityExB[ia][ib][ie];
  return true;
}

bool Medium::GetElectronVelocityB(const unsigned int ie, 
                                  const unsigned int ib, 
                                  const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronVelocityB:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasElectronVelocityB) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronVelocityB:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabElectronVelocityB[ia][ib][ie];
  return true;
}

bool Medium::GetElectronLongitudinalDiffusion(const unsigned int ie, 
                                              const unsigned int ib,
                                              const unsigned int ia, 
                                              double& dl) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronLongitudinalDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dl = 0.;
    return false;
  }
  if (!m_hasElectronDiffLong) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronLongitudinalDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dl = 0.;
    return false;
  }

  dl = tabElectronDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetElectronTransverseDiffusion(const unsigned int ie, 
                                            const unsigned int ib,
                                            const unsigned int ia, 
                                            double& dt) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronTransverseDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dt = 0.;
    return false;
  }
  if (!m_hasElectronDiffTrans) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronTransverseDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dt = 0.;
    return false;
  }

  dt = tabElectronDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetElectronTownsend(const unsigned int ie, 
                                 const unsigned int ib, 
                                 const unsigned int ia, double& alpha) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronTownsend:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    alpha = 0.;
    return false;
  }
  if (!m_hasElectronTownsend) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronTownsend:\n";
      std::cerr << "    Data not available.\n";
    }
    alpha = 0.;
    return false;
  }

  alpha = tabElectronTownsend[ia][ib][ie];
  return true;
}

bool Medium::GetElectronAttachment(const unsigned int ie, 
                                   const unsigned int ib, 
                                   const unsigned int ia, double& eta) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetElectronAttachment:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    eta = 0.;
    return false;
  }
  if (!m_hasElectronAttachment) {
    if (m_debug) {
      std::cerr << m_className << "::GetElectronAttachment:\n";
      std::cerr << "    Data not available.\n";
    }
    eta = 0.;
    return false;
  }

  eta = tabElectronAttachment[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityE(const unsigned int ie, 
                              const unsigned int ib, 
                              const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleVelocityE:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasHoleVelocityE) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleVelocityE:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabHoleVelocityE[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityExB(const unsigned int ie, 
                                const unsigned int ib, 
                                const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleVelocityExB:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasHoleVelocityExB) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleVelocityExB:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabHoleVelocityExB[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityB(const unsigned int ie, 
                              const unsigned int ib, 
                              const unsigned int ia, double& v) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleVelocityB:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    v = 0.;
    return false;
  }
  if (!m_hasHoleVelocityB) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleVelocityB:\n";
      std::cerr << "    Data not available.\n";
    }
    v = 0.;
    return false;
  }

  v = tabHoleVelocityB[ia][ib][ie];
  return true;
}

bool Medium::GetHoleLongitudinalDiffusion(const unsigned int ie, 
                                          const unsigned int ib,
                                          const unsigned int ia, double& dl) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleLongitudinalDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dl = 0.;
    return false;
  }
  if (!m_hasHoleDiffLong) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleLongitudinalDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dl = 0.;
    return false;
  }

  dl = tabHoleDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetHoleTransverseDiffusion(const unsigned int ie, 
                                        const unsigned int ib,
                                        const unsigned int ia, double& dt) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleTransverseDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dt = 0.;
    return false;
  }
  if (!m_hasHoleDiffTrans) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleTransverseDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dt = 0.;
    return false;
  }

  dt = tabHoleDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetHoleTownsend(const unsigned int ie, 
                             const unsigned int ib, 
                             const unsigned int ia, double& alpha) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleTownsend:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    alpha = 0.;
    return false;
  }
  if (!m_hasHoleTownsend) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleTownsend:\n";
      std::cerr << "    Data not available.\n";
    }
    alpha = 0.;
    return false;
  }

  alpha = tabHoleTownsend[ia][ib][ie];
  return true;
}

bool Medium::GetHoleAttachment(const unsigned int ie, 
                               const unsigned int ib, 
                               const unsigned int ia, double& eta) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetHoleAttachment:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    eta = 0.;
    return false;
  }
  if (!m_hasHoleAttachment) {
    if (m_debug) {
      std::cerr << m_className << "::GetHoleAttachment:\n";
      std::cerr << "    Data not available.\n";
    }
    eta = 0.;
    return false;
  }

  eta = tabHoleAttachment[ia][ib][ie];
  return true;
}

bool Medium::GetIonMobility(const unsigned int ie, const unsigned int ib, 
                            const unsigned int ia, double& mu) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetIonMobility:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    mu = 0.;
    return false;
  }
  if (!m_hasIonMobility) {
    if (m_debug) {
      std::cerr << m_className << "::GetIonMobility:\n";
      std::cerr << "    Data not available.\n";
    }
    mu = 0.;
    return false;
  }

  mu = tabIonMobility[ia][ib][ie];
  return true;
}

bool Medium::GetIonLongitudinalDiffusion(const unsigned int ie, 
                                         const unsigned int ib,
                                         const unsigned int ia, double& dl) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetIonLongitudinalDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dl = 0.;
    return false;
  }
  if (!m_hasIonDiffLong) {
    if (m_debug) {
      std::cerr << m_className << "::GetIonLongitudinalDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dl = 0.;
    return false;
  }

  dl = tabIonDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetIonTransverseDiffusion(const unsigned int ie, 
                                       const unsigned int ib, 
                                       const unsigned int ia, double& dt) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetIonTransverseDiffusion:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    dt = 0.;
    return false;
  }
  if (!m_hasIonDiffTrans) {
    if (m_debug) {
      std::cerr << m_className << "::GetIonTransverseDiffusion:\n";
      std::cerr << "    Data not available.\n";
    }
    dt = 0.;
    return false;
  }

  dt = tabIonDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetIonDissociation(const unsigned int ie, 
                                const unsigned int ib, 
                                const unsigned int ia, double& diss) {

  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::GetIonDissociation:\n";
    std::cerr << "     Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    diss = 0.;
    return false;
  }
  if (!m_hasIonDissociation) {
    if (m_debug) {
      std::cerr << m_className << "::GetIonDissociation:\n";
      std::cerr << "    Data not available.\n";
    }
    diss = 0.;
    return false;
  }

  diss = tabIonDissociation[ia][ib][ie];
  return true;
}

void Medium::CloneTable(std::vector<std::vector<std::vector<double> > >& tab,
                        const std::vector<double>& efields,
                        const std::vector<double>& bfields,
                        const std::vector<double>& angles, 
                        const unsigned int intp,
                        const unsigned int extrLow, 
                        const unsigned int extrHigh,
                        const double init, const std::string& label) {

  if (m_debug) {
    std::cout << m_className << "::CloneTable:\n";
    std::cout << "    Copying values of " << label << " to new grid.\n";
  }

  // Get the dimensions of the new grid.
  const int nEfieldsNew = efields.size();
  const int nBfieldsNew = bfields.size();
  const int nAnglesNew = angles.size();

  // Create a temporary table to store the values at the new grid points.
  std::vector<std::vector<std::vector<double> > > tabClone;
  tabClone.clear();
  InitParamArrays(nEfieldsNew, nBfieldsNew, nAnglesNew, tabClone, init);

  // Fill the temporary table.
  for (int i = 0; i < nEfieldsNew; ++i) {
    for (int j = 0; j < nBfieldsNew; ++j) {
      for (int k = 0; k < nAnglesNew; ++k) {
        double val = 0.;
        if (m_map2d) {
          if (!Numerics::Boxin3(tab, bAngles, bFields, eFields, m_nAngles,
                                m_nBfields, m_nEfields, angles[k], bfields[j],
                                efields[i], val, intp)) {
            std::cerr << m_className << "::SetFieldGrid:\n";
            std::cerr << "    Interpolation of " << label << " failed.\n";
            std::cerr << "    Cannot copy value to new grid at: \n";
            std::cerr << "      E = " << efields[i] << "\n";
            std::cerr << "      B = " << bfields[j] << "\n";
            std::cerr << "      angle: " << angles[k] << "\n";
          } else {
            tabClone[k][j][i] = val;
          }
        } else {
          val = Interpolate1D(efields[i], tab[0][0], eFields, intp, extrLow,
                              extrHigh);
          tabClone[k][j][i] = val;
        }
      }
    }
  }
  // Re-dimension the original table.
  InitParamArrays(nEfieldsNew, nBfieldsNew, nAnglesNew, tab, init);
  // Copy the values to the original table.
  for (int i = 0; i < nEfieldsNew; ++i) {
    for (int j = 0; j < nBfieldsNew; ++j) {
      for (int k = 0; k < nAnglesNew; ++k) {
        tab[k][j][i] = tabClone[k][j][i];
      }
    }
  }
  tabClone.clear();
}

void Medium::CloneTensor(
    std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
    const unsigned int n, 
    const std::vector<double>& efields, const std::vector<double>& bfields, 
    const std::vector<double>& angles,
    const unsigned int intp, 
    const unsigned int extrLow, const unsigned int extrHigh, 
    const double init,
    const std::string& label) {

  // Get the dimensions of the new grid.
  const unsigned int nEfieldsNew = efields.size();
  const unsigned int nBfieldsNew = bfields.size();
  const unsigned int nAnglesNew = angles.size();

  // Create a temporary table to store the values at the new grid points.
  std::vector<std::vector<std::vector<std::vector<double> > > > tabClone;
  tabClone.clear();
  InitParamTensor(nEfieldsNew, nBfieldsNew, nAnglesNew, n, tabClone, init);

  // Fill the temporary table.
  for (unsigned int l = 0; l < n; ++l) {
    for (unsigned int i = 0; i < nEfieldsNew; ++i) {
      for (unsigned int j = 0; j < nBfieldsNew; ++j) {
        for (unsigned int k = 0; k < nAnglesNew; ++k) {
          double val = 0.;
          if (m_map2d) {
            if (!Numerics::Boxin3(tab[l], bAngles, bFields, eFields, m_nAngles,
                                  m_nBfields, m_nEfields, angles[k], bfields[j],
                                  efields[i], val, intp)) {
              std::cerr << m_className << "::SetFieldGrid:\n";
              std::cerr << "    Interpolation of " << label << " failed.\n";
              std::cerr << "    Cannot copy value to new grid at: \n";
              std::cerr << "      Index: " << l << "\n";
              std::cerr << "      E = " << efields[i] << "\n";
              std::cerr << "      B = " << bfields[j] << "\n";
              std::cerr << "      angle: " << angles[k] << "\n";
            } else {
              tabClone[l][k][j][i] = val;
            }
          } else {
            val = Interpolate1D(efields[i], tab[l][0][0], eFields, intp,
                                extrLow, extrHigh);
            tabClone[l][k][j][i] = val;
          }
        }
      }
    }
  }
  // Re-dimension the original table.
  InitParamTensor(nEfieldsNew, nBfieldsNew, nAnglesNew, n, tab, 0.);
  // Copy the values to the original table.
  for (unsigned int l = 0; l < n; ++l) {
    for (unsigned int i = 0; i < nEfieldsNew; ++i) {
      for (unsigned int j = 0; j < nBfieldsNew; ++j) {
        for (unsigned int k = 0; k < nAnglesNew; ++k) {
          tab[l][k][j][i] = tabClone[l][k][j][i];
        }
      }
    }
  }
}

bool Medium::SetIonMobility(const unsigned int ie, const unsigned int ib, 
                            const unsigned int ia, const double mu) {

  // Check the index.
  if (ie >= m_nEfields || ib >= m_nBfields || ia >= m_nAngles) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    Index (" << ie << ", " << ib << ", " << ia
              << ") out of range.\n";
    return false;
  }

  if (!m_hasIonMobility) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    Ion mobility table not initialised.\n";
    return false;
  }

  if (mu == 0.) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    Zero value not permitted.\n";
    return false;
  }

  tabIonMobility[ia][ib][ie] = mu;
  if (m_debug) {
    std::cout << m_className << "::SetIonMobility:\n";
    std::cout << "   Ion mobility at E = " << eFields[ie]
              << " V/cm, B = " << bFields[ib] << " T, angle " << bAngles[ia]
              << " set to " << mu << " cm2/(V ns).\n";
  }
  return true;
}

bool Medium::SetIonMobility(const std::vector<double>& efields,
                            const std::vector<double>& mobilities) {

  const int ne = efields.size();
  const int nm = mobilities.size();
  if (ne != nm) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    E-field and mobility arrays have different sizes.\n";
    return false;
  }

  ResetIonMobility();
  InitParamArrays(m_nEfields, m_nBfields, m_nAngles, tabIonMobility, 0.);
  for (unsigned int i = 0; i < m_nEfields; ++i) {
    const double e = eFields[i];
    const double mu = Interpolate1D(e, mobilities, efields, m_intpMobility,
                                    m_extrLowMobility, m_extrHighMobility);
    tabIonMobility[0][0][i] = mu;
    std::cout << "E = " << e << ", mu = " << mu << "\n";
  }

  if (m_map2d) {
    for (unsigned int i = 0; i < m_nAngles; ++i) {
      for (unsigned int j = 0; j< m_nBfields; ++j) {
        for (unsigned int k = 0; k < m_nEfields; ++k) {
          tabIonMobility[i][j][k] = tabIonMobility[0][0][k];
        }
      }
    }
  }
  m_hasIonMobility = true;

  return true;
}

void Medium::SetExtrapolationMethodVelocity(const std::string& extrLow,
                                            const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowVelocity = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodVelocity:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighVelocity = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodVelocity:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

void Medium::SetExtrapolationMethodDiffusion(const std::string& extrLow,
                                             const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowDiffusion = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodDiffusion:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighDiffusion = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodDiffusion:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

void Medium::SetExtrapolationMethodTownsend(const std::string& extrLow,
                                            const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowTownsend = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodTownsend:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighTownsend = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodTownsend:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

void Medium::SetExtrapolationMethodAttachment(const std::string& extrLow,
                                              const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowAttachment = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighAttachment = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodAttachment:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

void Medium::SetExtrapolationMethodIonMobility(const std::string& extrLow,
                                               const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowMobility = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodIonMobility:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }
  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighMobility = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodIonMobility:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

void Medium::SetExtrapolationMethodIonDissociation(const std::string& extrLow,
                                                   const std::string& extrHigh) {

  unsigned int iExtr = 0;
  if (GetExtrapolationIndex(extrLow, iExtr)) {
    m_extrLowDissociation = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodIonDissociation:\n";
    std::cerr << "    Unknown extrapolation method (" << extrLow << ")\n";
  }

  if (GetExtrapolationIndex(extrHigh, iExtr)) {
    m_extrHighDissociation = iExtr;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethodIonDissociation:\n";
    std::cerr << "    Unknown extrapolation method (" << extrHigh << ")\n";
  }
}

bool Medium::GetExtrapolationIndex(std::string extrStr, unsigned int extrNb) {

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

void Medium::SetInterpolationMethodVelocity(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpVelocity = intrp;
  }
}

void Medium::SetInterpolationMethodDiffusion(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpDiffusion = intrp;
  }
}

void Medium::SetInterpolationMethodTownsend(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpTownsend = intrp;
  }
}

void Medium::SetInterpolationMethodAttachment(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpAttachment = intrp;
  }
}

void Medium::SetInterpolationMethodIonMobility(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpMobility = intrp;
  }
}

void Medium::SetInterpolationMethodIonDissociation(const unsigned int intrp) {

  if (intrp > 0) {
    m_intpDissociation = intrp;
  }
}

double Medium::Interpolate1D(const double e, const std::vector<double>& table,
                             const std::vector<double>& fields,
                             const unsigned int intpMeth, const int extrLow,
                             const int extrHigh) {

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
      if (m_debug) {
        std::cerr << m_className << "::Interpolate1D:\n";
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
      const double extr4 = log(table[1] / table[0]) / (fields[1] - fields[0]);
      const double extr3 = log(table[0] - extr4 * fields[0]);
      result = std::exp(std::min(50., extr3 + extr4 * e));
    } else {
      result = table[0];
    }
  } else if (e > fields[nSizeTable - 1]) {
    // Extrapolation towards large fields
    if (fields[nSizeTable - 1] <= fields[nSizeTable - 2]) {
      if (m_debug) {
        std::cerr << m_className << "::Interpolate1D:\n";
        std::cerr << "    Last two field values coincide.\n";
        std::cerr << "    No extrapolation to higher fields.\n";
      }
      result = table[nSizeTable - 1];
    } else if (extrHigh == 1) {
      // Linear extrapolation
      const double extr2 = (table[nSizeTable - 1] - table[nSizeTable - 2]) /
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 =
          table[nSizeTable - 1] - extr2 * fields[nSizeTable - 1];
      result = extr1 + extr2 * e;
    } else if (extrHigh == 2) {
      // Logarithmic extrapolation
      const double extr2 = log(table[nSizeTable - 1] / table[nSizeTable - 2]) /
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 =
          log(table[nSizeTable - 1]) - extr2 * fields[nSizeTable - 1];
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

void Medium::InitParamArrays(
    const unsigned int eRes, const unsigned int bRes, 
    const unsigned int aRes,
    std::vector<std::vector<std::vector<double> > >& tab, const double val) {

  if (eRes == 0 || bRes == 0 || aRes == 0) {
    std::cerr << m_className << "::InitParamArrays:\n";
    std::cerr << "    Invalid grid.\n";
    return;
  }

  tab.assign(aRes, std::vector<std::vector<double> >(bRes, std::vector<double>(eRes, val))); 
  /*
  tab.resize(aRes);
  for (unsigned int i = 0; i < aRes; ++i) {
    tab[i].resize(bRes);
    for (unsigned int j = 0; j < bRes; ++j) {
      tab[i][j].assign(eRes, val);
    }
  }
  */
}

void Medium::InitParamTensor(
    const unsigned int eRes, const unsigned int bRes, 
    const unsigned int aRes, const unsigned int tRes,
    std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
    const double val) {

  if (eRes == 0 || bRes == 0 || aRes == 0 || tRes == 0) {
    std::cerr << m_className << "::InitParamArrays:\n";
    std::cerr << "    Invalid grid.\n";
    return;
  }

  tab.resize(tRes);
  for (unsigned int l = 0; l < tRes; ++l) {
    tab[l].resize(aRes);
    for (unsigned int i = 0; i < aRes; ++i) {
      tab[l][i].resize(bRes);
      for (unsigned int j = 0; j < bRes; ++j) {
        tab[l][i][j].assign(eRes, val);
      }
    }
  }
}
}
