#include <iostream>
#include <iomanip>
#include <cmath>
#include <utility>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "Numerics.hh"

namespace {

void PrintNotImplemented(const std::string& cls, const std::string& fcn) {

  std::cerr << cls << "::" << fcn << ": Function is not implemented.\n";
}

void PrintOutOfRange(const std::string& cls, const std::string& fcn,
                     const unsigned int i, const unsigned int j,
                     const unsigned int k) {

  std::cerr << cls << "::" << fcn << ": Index (" << i << ", " << j << ", " << k
            << ") out of range.\n";
}

void PrintDataNotAvailable(const std::string& cls, const std::string& fcn) {

  std::cerr << cls << "::" << fcn << ": Data not available.\n";
}

bool CheckFields(const std::vector<double>& fields, const std::string& hdr,
                 const std::string& lbl) {

  if (fields.empty()) {
    std::cerr << hdr << ": Number of " << lbl << " must be > 0.\n";
    return false;
  }

  // Make sure the values are not negative.
  if (fields.front() < 0.) {
    std::cerr << hdr << ": " << lbl << " must be >= 0.\n";
    return false;
  }

  const size_t nEntries = fields.size();
  // Make sure the values are in strictly monotonic, ascending order.
  if (nEntries > 1) {
    for (size_t i = 1; i < nEntries; ++i) {
      if (fields[i] <= fields[i - 1]) {
        std::cerr << hdr << ": " << lbl << " are not in ascending order.\n";
        return false;
      }
    }
  }
  return true;
}

}

namespace Garfield {

int Medium::m_idCounter = -1;

Medium::Medium()
    : m_id(++m_idCounter) {

  // Initialise the transport tables.
  m_bFields.assign(1, 0.);
  m_bAngles.assign(1, 0.);

  // Set the default grid.
  SetFieldGrid(100., 100000., 20, true, 0., 0., 1, 0., 0., 1);
}

Medium::~Medium() {}

void Medium::SetTemperature(const double t) {

  if (t <= 0.) {
    std::cerr << m_className << "::SetTemperature:\n"
              << "    Temperature [K] must be greater than zero.\n";
    return;
  }
  m_temperature = t;
  m_isChanged = true;
}

void Medium::SetPressure(const double p) {

  if (p <= 0.) {
    std::cerr << m_className << "::SetPressure:\n"
              << "    Pressure [Torr] must be greater than zero.\n";
    return;
  }
  m_pressure = p;
  m_isChanged = true;
}

void Medium::SetDielectricConstant(const double eps) {

  if (eps < 1.) {
    std::cerr << m_className << "::SetDielectricConstant:\n"
              << "    Dielectric constant must be >= 1.\n";
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
    std::cerr << m_className << "::GetComponent: Index out of range.\n";
  }

  label = m_name;
  f = 1.;
}

void Medium::SetAtomicNumber(const double z) {

  if (z < 1.) {
    std::cerr << m_className << "::SetAtomicNumber:\n"
              << "    Atomic number must be >= 1.\n";
    return;
  }
  m_z = z;
  m_isChanged = true;
}

void Medium::SetAtomicWeight(const double a) {

  if (a <= 0.) {
    std::cerr << m_className << "::SetAtomicWeight:\n"
              << "    Atomic weight must be greater than zero.\n";
    return;
  }
  m_a = a;
  m_isChanged = true;
}

void Medium::SetNumberDensity(const double n) {

  if (n <= 0.) {
    std::cerr << m_className << "::SetNumberDensity:\n"
              << "    Density [cm-3] must be greater than zero.\n";
    return;
  }
  m_density = n;
  m_isChanged = true;
}

void Medium::SetMassDensity(const double rho) {

  if (rho <= 0.) {
    std::cerr << m_className << "::SetMassDensity:\n"
              << "    Density [g/cm3] must be greater than zero.\n";
    return;
  }

  if (m_a <= 0.) {
    std::cerr << m_className << "::SetMassDensity:\n"
              << "    Atomic weight is not defined.\n";
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
  if (m_eVelocityE.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return false;

  // Compute the magnitude of the magnetic field.
  const double b = sqrt(bx * bx + by * by + bz * bz);
  // Compute the angle between B field and E field.
  const double ebang = GetAngle(ex, ey, ez, bx, by, bz, e, b);

  if (b < Small) {
    // No magnetic field. Calculate the velocity along E.
    double ve = 0.;
    if (!Interpolate(e0, b, ebang, m_eVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::ElectronVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    const double mu = -ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;

  } else if (!m_eVelocityB.empty() && !m_eVelocityExB.empty()) {
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
      std::cout << m_className << "::ElectronVelocity:\n"
                << "    unit vector along E:     (" << ue[0] << ", " << ue[1]
                << ", " << ue[2] << ")\n";
      std::cout << "    unit vector along E x B: (" << uexb[0] << ", "
                << uexb[1] << ", " << uexb[2] << ")\n";
      std::cout << "    unit vector along Bt:    (" << ubt[0] << ", " << ubt[1]
                << ", " << ubt[2] << ")\n";
    }

    // Calculate the velocities in all directions.
    double ve = 0., vbt = 0., vexb = 0.;
    if (!Interpolate(e0, b, ebang, m_eVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::ElectronVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    if (!Interpolate(e0, b, ebang, m_eVelocityExB, vexb, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::ElectronVelocity:\n"
                << "    Interpolation of velocity along ExB failed.\n";
      return false;
    }
    if (!Interpolate(e0, b, ebang, m_eVelocityB, vbt, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::ElectronVelocity:\n"
                << "    Interpolation of velocity along Bt failed.\n";
      return false;
    }
    if (ex * bx + ey * by + ez * bz > 0.) {
      vbt = fabs(vbt);
    } else {
      vbt = -fabs(vbt);
    }
    vx = -(ve * ue[0] + vbt * ubt[0] - vexb * uexb[0]);
    vy = -(ve * ue[1] + vbt * ubt[1] - vexb * uexb[1]);
    vz = -(ve * ue[2] + vbt * ubt[2] - vexb * uexb[2]);

  } else {
    // Magnetic field, velocities along ExB, Bt not available.

    // Calculate the velocity along E.
    double ve = 0.;
    if (!Interpolate(e0, b, ebang, m_eVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::ElectronVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    const double mu = -ve / e;
    const double mu2 = mu * mu;
    const double eb = bx * ex + by * ey + bz * ez;
    const double f = 1. / (1. + mu2 * b * b);
    vx = f * (ex + mu * (ey * bz - ez * by) + mu2 * bx * eb);
    vy = f * (ey + mu * (ez * bx - ex * bz) + mu2 * by * eb);
    vz = f * (ez + mu * (ex * by - ey * bx) + mu2 * bz * eb);
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

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  if (!m_eDiffLong.empty()) {
    if (!Interpolate(e0, b, ebang, m_eDiffLong, dl, m_intpDiff, m_extrDiff)) {
      dl = 0.;
    }
  }
  if (!m_eDiffTrans.empty()) {
    if (!Interpolate(e0, b, ebang, m_eDiffTrans, dt, m_intpDiff, m_extrDiff)) {
      dt = 0.;
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (m_eDiffLong.empty() || m_eDiffTrans.empty()) {
    const double d = sqrt(2. * BoltzmannConstant * m_temperature / e);
    if (m_eDiffLong.empty()) dl = d;
    if (m_eDiffTrans.empty()) dt = d;
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

  if (m_eDiffTens.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  for (int j = 0; j < 6; ++j) {
    // Interpolate.
    double y = 0.;
    if (!Interpolate(e0, b, ebang, m_eDiffTens[j], y, m_intpDiff, m_extrDiff)) {
      y = 0.;
    }
    // Apply scaling.
    y = ScaleDiffusionTensor(y);
    if (j < 3) {
      cov[j][j] = y;
    } else if (j == 3) {
      cov[0][1] = cov[1][0] = y;
    } else if (j == 4) {
      cov[0][2] = cov[2][0] = y;
    } else if (j == 5) {
      cov[1][2] = cov[2][1] = y; 
    }
  }

  return true;
}

bool Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& alpha) {

  alpha = 0.;
  if (m_eTownsend.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  const auto intp = e0 < m_eFields[thrElectronTownsend] ? 1 : m_intpTownsend;
  if (!Interpolate(e0, b, ebang, m_eTownsend, alpha, intp, m_extrTownsend)) {
    alpha = -30.;
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
  if (m_eAttachment.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  const auto intp = e0 < m_eFields[thrElectronAttachment] ? 1 : m_intpAttachment;
  if (!Interpolate(e0, b, ebang, m_eAttachment, eta, intp, m_extrAttachment)) {
    eta = -30.;
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

bool Medium::ElectronLorentzAngle(const double ex, const double ey,
                                  const double ez, const double bx,
                                  const double by, const double bz, 
                                  double& lor) {

  lor = 0.;
  if (m_eLorentzAngle.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  if (!Interpolate(e0, b, ebang, m_eLorentzAngle, lor, m_intpLorentzAngle, m_extrLorentzAngle)) {
    lor = 0.;
  }
  // Apply scaling.
  lor = ScaleLorentzAngle(lor);
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
  RndmDirection(px, py, pz, p);
  band = -1;
}

double Medium::GetElectronNullCollisionRate(const int /*band*/) {

  if (m_debug) PrintNotImplemented(m_className, "GetElectronNullCollisionRate");
  return 0.;
}

double Medium::GetElectronCollisionRate(const double /*e*/, 
                                        const int /*band*/) {

  if (m_debug) PrintNotImplemented(m_className, "GetElectronCollisionRate");
  return 0.;
}

bool Medium::GetElectronCollision(const double e, int& type, int& level,
                                  double& e1, double& dx, double& dy,
                                  double& dz, 
                                  std::vector<std::pair<int, double> >& /*secondaries*/, 
                                  int& ndxc, int& band) {

  type = level = -1;
  e1 = e;
  ndxc = band = 0;
  RndmDirection(dx, dy, dz);

  if (m_debug) PrintNotImplemented(m_className, "GetElectronCollision");
  return false;
}

bool Medium::GetDeexcitationProduct(const unsigned int /*i*/, 
                                    double& t, double& s,
                                    int& type, double& energy) const {

  if (m_debug) PrintNotImplemented(m_className, "GetDeexcitationProduct");
  t = s = energy = 0.;
  type = 0;
  return false;
}

bool Medium::HoleVelocity(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& vx, double& vy, double& vz) {

  vx = vy = vz = 0.;
  // Make sure there is at least a table of velocities along E.
  if (m_hVelocityE.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = sqrt(bx * bx + by * by + bz * bz);

  // Compute the angle between B field and E field.
  const double ebang = GetAngle(ex, ey, ez, bx, by, bz, e, b);

  if (b < Small) {
    // No magnetic field. Calculate the velocity along E.
    double ve = 0.;
    if (!Interpolate(e0, b, ebang, m_hVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::HoleVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    constexpr double q = 1.;
    const double mu = q * ve / e;
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;

  } else if (!m_hVelocityB.empty() && !m_hVelocityExB.empty()) {
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
    if (!Interpolate(e0, b, ebang, m_hVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::HoleVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    if (!Interpolate(e0, b, ebang, m_hVelocityExB, vexb, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::HoleVelocity:\n"
                << "    Interpolation of velocity along ExB failed.\n";
      return false;
    }
    if (!Interpolate(e0, b, ebang, m_hVelocityB, vbt, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::HoleVelocity:\n"
                << "    Interpolation of velocity along Bt failed.\n";
      return false;
    }
    constexpr double q = 1.;
    if (ex * bx + ey * by + ez * bz > 0.) vbt = fabs(vbt);
     else vbt = -fabs(vbt);
    vx = q * (ve * ue[0] + q * q * vbt * ubt[0] + q * vexb * uexb[0]);
    vy = q * (ve * ue[1] + q * q * vbt * ubt[1] + q * vexb * uexb[1]);
    vz = q * (ve * ue[2] + q * q * vbt * ubt[2] + q * vexb * uexb[2]);

  } else {
    // Magnetic field, velocities along ExB, Bt not available
    // Calculate the velocity along E.
    double ve = 0.;
    if (!Interpolate(e0, b, ebang, m_hVelocityE, ve, m_intpVel, m_extrVel)) {
      std::cerr << m_className << "::HoleVelocity:\n"
                << "    Interpolation of velocity along E failed.\n";
      return false;
    }
    constexpr double q = 1.;
    const double mu = q * ve / e;
    const double mu2 = mu * mu;
    const double eb = bx * ex + by * ey + bz * ez;
    const double nom = 1. + mu2 * b * b;
    vx = mu * (ex + mu * (ey * bz - ez * by) + mu2 * bx * eb) / nom;
    vy = mu * (ey + mu * (ez * bx - ex * bz) + mu2 * by * eb) / nom;
    vz = mu * (ez + mu * (ex * by - ey * bx) + mu2 * bz * eb) / nom;
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

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  if (!m_hDiffLong.empty()) {
    if (!Interpolate(e0, b, ebang, m_hDiffLong, dl, m_intpDiff, m_extrDiff)) {
      dl = 0.;
    }
  }
  if (!m_hDiffTrans.empty()) {
    if (!Interpolate(e0, b, ebang, m_hDiffTrans, dt, m_intpDiff, m_extrDiff)) {
      dt = 0.;
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (m_hDiffLong.empty() || m_hDiffTrans.empty()) {
    const double d = sqrt(2. * BoltzmannConstant * m_temperature / e);
    if (m_hDiffLong.empty()) dl = d;
    if (m_hDiffTrans.empty()) dt = d;
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

  if (m_hDiffTens.empty()) return false;

  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  for (int j = 0; j < 6; ++j) {
    double y = 0.;
    if (!Interpolate(e0, b, ebang, m_hDiffTens[j], y, m_intpDiff, m_extrDiff)) {
      y = 0.;
    }
    // Apply scaling.
    y = ScaleDiffusionTensor(y);
    if (j < 3) {
      cov[j][j] = y;
    } else if (j == 3) {
      cov[0][1] = cov[1][0] = y;
    } else if (j == 4) {
      cov[0][2] = cov[2][0] = y;
    } else if (j == 5) {
      cov[1][2] = cov[2][1] = y;
    }
  }
  return true;
}

bool Medium::HoleTownsend(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double& alpha) {

  alpha = 0.;
  if (m_hTownsend.empty()) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  const auto intp = e0 < m_eFields[thrHoleTownsend] ? 1 : m_intpTownsend; 
  if (!Interpolate(e0, b, ebang, m_hTownsend, alpha, intp, m_extrTownsend)) {
    alpha = -30.;
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
  if (m_hAttachment.empty()) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  const int intp = e0 < m_eFields[thrHoleAttachment] ? 1 : m_intpAttachment;
  if (!Interpolate(e0, b, ebang, m_hAttachment, eta, intp, m_extrAttachment)) {
    eta = -30.;
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
  if (m_ionMobility.empty()) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;
  // Compute the magnitude of the electric field.
  const double b = sqrt(bx * bx + by * by + bz * bz);

  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;
  double mu = 0.;
  if (!Interpolate(e0, b, ebang, m_ionMobility, mu, m_intpMobility, m_extrMobility)) {
    mu = 0.;
  }

  constexpr double q = 1.;
  mu *= q;
  if (b < Small) {
    vx = mu * ex;
    vy = mu * ey;
    vz = mu * ez;
  } else {
    const double eb = bx * ex + by * ey + bz * ez;
    const double mu2 = mu * mu;
    const double nom = 1. + mu2 * b * b;
    vx = mu * (ex + mu * (ey * bz - ez * by) + mu2 * bx * eb) / nom;
    vy = mu * (ey + mu * (ez * bx - ex * bz) + mu2 * by * eb) / nom;
    vz = mu * (ez + mu * (ex * by - ey * bx) + mu2 * bz * eb) / nom;
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

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  if (!m_ionDiffLong.empty()) {
    if (!Interpolate(e0, b, ebang, m_ionDiffLong, dl, m_intpDiff, m_extrDiff)) {
      dl = 0.;
    }
  }
  if (!m_ionDiffTrans.empty()) {
    if (!Interpolate(e0, b, ebang, m_ionDiffTrans, dt, m_intpDiff, m_extrDiff)) {
      dt = 0.;
    }
  }

  // If no data available, calculate
  // the diffusion coefficients using the Einstein relation
  if (m_ionDiffLong.empty() || m_ionDiffTrans.empty()) {
    const double d = sqrt(2. * BoltzmannConstant * m_temperature / e);
    if (m_ionDiffLong.empty()) dl = d;
    if (m_ionDiffTrans.empty()) dt = d;
  }
  return true;
}

bool Medium::IonDissociation(const double ex, const double ey, const double ez,
                             const double bx, const double by, const double bz,
                             double& diss) {

  diss = 0.;
  if (m_ionDissociation.empty()) return false;
  // Compute the magnitude of the electric field.
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  const double e0 = ScaleElectricField(e);
  if (e < Small || e0 < Small) return true;

  // Compute the magnitude of the magnetic field.
  const double b = m_map2d ? sqrt(bx * bx + by * by + bz * bz) : 0.;
  // Compute the angle between B field and E field.
  const double ebang = m_map2d ? GetAngle(ex, ey, ez, bx, by, bz, e, b) : 0.;

  // Interpolate.
  const int intp = e0 < m_eFields[thrIonDissociation] ? 1 : m_intpDissociation;
  if (!Interpolate(e0, b, ebang, m_ionDissociation, diss, intp, m_extrDissociation)) {
    diss = -30.;
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

  if (m_debug) PrintNotImplemented(m_className, "GetOpticalDataRange");
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

  if (m_debug) PrintNotImplemented(m_className, "GetDielectricFunction");
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
    PrintNotImplemented(m_className, "GetPhotoAbsorptionCrossSection");
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

void Medium::SetFieldGrid(double emin, double emax, const size_t ne, bool logE,
                          double bmin, double bmax, const size_t nb, 
                          double amin, double amax, const size_t na) {

  // Check if the requested E-field range makes sense.
  if (ne <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n"
              << "    Number of E-fields must be > 0.\n";
    return;
  }

  if (emin < 0. || emax < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n"
              << "    Electric fields must be positive.\n";
    return;
  }

  if (emax < emin) {
    std::cerr << m_className << "::SetFieldGrid: Swapping min./max. E-field.\n";
    std::swap(emin, emax);
  }

  double estep = 0.;
  if (logE) {
    // Logarithmic scale
    if (emin < Small) {
      std::cerr << m_className << "::SetFieldGrid:\n"
                << "    Min. E-field must be non-zero for log. scale.\n";
      return;
    }
    if (ne == 1) {
      std::cerr << m_className << "::SetFieldGrid:\n"
                << "    Number of E-fields must be > 1 for log. scale.\n";
      return;
    }
    estep = pow(emax / emin, 1. / (ne - 1.));
  } else {
    // Linear scale
    if (ne > 1) estep = (emax - emin) / (ne - 1.);
  }

  // Check if the requested B-field range makes sense.
  if (nb <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n"
              << "    Number of B-fields must be > 0.\n";
    return;
  }
  if (bmax < 0. || bmin < 0.) {
    std::cerr << m_className << "::SetFieldGrid:\n"
              << "    Magnetic fields must be positive.\n";
    return;
  }
  if (bmax < bmin) {
    std::cerr << m_className << "::SetFieldGrid: Swapping min./max. B-field.\n";
    std::swap(bmin, bmax);
  }

  const double bstep = nb > 1 ? (bmax - bmin) / (nb - 1.) : 0.;

  // Check if the requested angular range makes sense.
  if (na <= 0) {
    std::cerr << m_className << "::SetFieldGrid:\n"
              << "    Number of angles must be > 0.\n";
    return;
  }
  if (amax < 0. || amin < 0.) {
    std::cerr << m_className << "::SetFieldGrid:"
              << "    Angles must be positive.\n";
    return;
  }
  if (amax < amin) {
    std::cerr << m_className << "::SetFieldGrid: Swapping min./max. angle.\n";
    std::swap(amin, amax);
  }
  const double astep = na > 1 ? (amax - amin) / (na - 1.) : 0;

  // Setup the field grids.
  std::vector<double> eFieldsNew(ne);
  std::vector<double> bFieldsNew(nb);
  std::vector<double> bAnglesNew(na);
  for (size_t i = 0; i < ne; ++i) {
    eFieldsNew[i] = logE ? emin * pow(estep, i) : emin + i * estep;
  }
  for (size_t i = 0; i < nb; ++i) {
    bFieldsNew[i] = bmin + i * bstep;
  }
  if (na == 1 && nb == 1 && fabs(bmin) < 1.e-4) {
    bAnglesNew[0] = HalfPi;
  } else {
    for (size_t i = 0; i < na; ++i) {
      bAnglesNew[i] = amin + i * astep;
    }
  }
  SetFieldGrid(eFieldsNew, bFieldsNew, bAnglesNew);
}

void Medium::SetFieldGrid(const std::vector<double>& efields,
                          const std::vector<double>& bfields,
                          const std::vector<double>& angles) {

  const std::string hdr = m_className + "::SetFieldGrid";
  if (!CheckFields(efields, hdr, "E-fields")) return;
  if (!CheckFields(bfields, hdr, "B-fields")) return;
  if (!CheckFields(angles, hdr, "angles")) return;

  if (m_debug) {
    std::cout << m_className << "::SetFieldGrid:\n";
    std::cout << "    E-fields:\n";
    for (const auto efield : efields) std::cout << "      " << efield << "\n";
    std::cout << "    B-fields:\n";
    for (const auto bfield : bfields) std::cout << "      " << bfield << "\n";
    std::cout << "    Angles:\n";
    for (const auto angle : angles) std::cout << "      " << angle << "\n";
  }

  // Clone the existing tables.
  // Electrons
  CloneTable(m_eVelocityE, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
             "electron velocity along E");
  CloneTable(m_eVelocityB, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
             "electron velocity along Bt");
  CloneTable(m_eVelocityExB, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
              "electron velocity along ExB");
  CloneTable(m_eDiffLong, efields, bfields, angles, m_intpDiff, m_extrDiff, 0.,
             "electron longitudinal diffusion");
  CloneTable(m_eDiffTrans, efields, bfields, angles, m_intpDiff, m_extrDiff, 0.,
             "electron transverse diffusion");
  CloneTable(m_eTownsend, efields, bfields, angles, m_intpTownsend,
             m_extrTownsend, -30., "electron Townsend coefficient");
  CloneTable(m_eAttachment, efields, bfields, angles, m_intpAttachment,
             m_extrAttachment, -30., "electron attachment coefficient");
  CloneTable(m_eLorentzAngle, efields, bfields, angles, m_intpLorentzAngle,
             m_extrLorentzAngle, 0., "electron Lorentz angle");
  if (!m_eDiffTens.empty()) {
    CloneTensor(m_eDiffTens, 6, efields, bfields, angles, m_intpDiff,
                m_extrDiff, 0., "electron diffusion tensor");
  }

  // Holes
  CloneTable(m_hVelocityE, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
             "hole velocity along E");
  CloneTable(m_hVelocityB, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
             "hole velocity along Bt");
  CloneTable(m_hVelocityExB, efields, bfields, angles, m_intpVel, m_extrVel, 0.,
             "hole velocity along ExB");
  CloneTable(m_hDiffLong, efields, bfields, angles, m_intpDiff, m_extrDiff, 0.,
             "hole longitudinal diffusion");
  CloneTable(m_hDiffTrans, efields, bfields, angles, m_intpDiff, m_extrDiff, 0.,
             "hole transverse diffusion");
  CloneTable(m_hTownsend, efields, bfields, angles, m_intpTownsend,
             m_extrTownsend, -30., "hole Townsend coefficient");
  CloneTable(m_hAttachment, efields, bfields, angles, m_intpAttachment,
             m_extrAttachment, -30., "hole attachment coefficient");
  if (!m_hDiffTens.empty()) {
    CloneTensor(m_hDiffTens, 6, efields, bfields, angles, m_intpDiff,
                m_extrDiff, 0., "hole diffusion tensor");
  }

  // Ions
  CloneTable(m_ionMobility, efields, bfields, angles, m_intpMobility,
             m_extrMobility, 0., "ion mobility");
  CloneTable(m_ionDiffLong, efields, bfields, angles, m_intpDiff,
             m_extrDiff, 0., "ion longitudinal diffusion");
  CloneTable(m_ionDiffTrans, efields, bfields, angles, m_intpDiff,
             m_extrDiff, 0., "ion transverse diffusion");
  CloneTable(m_ionDissociation, efields, bfields, angles, m_intpDissociation,
             m_extrDissociation, -30., "ion dissociation");

  if (bfields.size() > 1 || angles.size() > 1) m_map2d = true;
  m_eFields = efields;
  m_bFields = bfields;
  m_bAngles = angles;
}

void Medium::GetFieldGrid(std::vector<double>& efields,
                          std::vector<double>& bfields,
                          std::vector<double>& angles) {

  efields = m_eFields;
  bfields = m_bFields;
  angles = m_bAngles;
}

bool Medium::GetElectronVelocityE(const unsigned int ie, 
                                  const unsigned int ib, 
                                  const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronVelocityE", ie, ib, ia);
    return false;
  }
  if (m_eVelocityE.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronVelocityE");
    return false;
  }
  v = m_eVelocityE[ia][ib][ie];
  return true;
}

bool Medium::GetElectronVelocityExB(const unsigned int ie, 
                                    const unsigned int ib, 
                                    const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronVelocityExB", ie, ib, ia);
    return false;
  }
  if (m_eVelocityExB.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronVelocityExB");
    return false;
  }
  v = m_eVelocityExB[ia][ib][ie];
  return true;
}

bool Medium::GetElectronVelocityB(const unsigned int ie, 
                                  const unsigned int ib, 
                                  const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronVelocityB", ie, ib, ia);
    return false;
  }
  if (m_eVelocityB.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronVelocityB");
    return false;
  }
  v = m_eVelocityB[ia][ib][ie];
  return true;
}

bool Medium::GetElectronLongitudinalDiffusion(const unsigned int ie, 
                                              const unsigned int ib,
                                              const unsigned int ia, 
                                              double& dl) {
  dl = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronLongitudinalDiffusion", ie, ib, ia);
    return false;
  }
  if (m_eDiffLong.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetElectronLongitudinalDiffusion");
    }
    return false;
  }
  dl = m_eDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetElectronTransverseDiffusion(const unsigned int ie, 
                                            const unsigned int ib,
                                            const unsigned int ia, 
                                            double& dt) {
  dt = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronTransverseDiffusion", ie, ib, ia);
    return false;
  }
  if (m_eDiffTrans.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetElectronTransverseDiffusion");
    }
    return false;
  }
  dt = m_eDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetElectronTownsend(const unsigned int ie, 
                                 const unsigned int ib, 
                                 const unsigned int ia, double& alpha) {
  alpha = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronTownsend", ie, ib, ia);
    return false;
  }
  if (m_eTownsend.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronTownsend");
    return false;
  }
  alpha = m_eTownsend[ia][ib][ie];
  return true;
}

bool Medium::GetElectronAttachment(const unsigned int ie, 
                                   const unsigned int ib, 
                                   const unsigned int ia, double& eta) {
 eta = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronAttachment", ie, ib, ia);
    return false;
  }
  if (m_eAttachment.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronAttachment");
    return false;
  }
  eta = m_eAttachment[ia][ib][ie];
  return true;
}

bool Medium::GetElectronLorentzAngle(const unsigned int ie, 
                                     const unsigned int ib, 
                                     const unsigned int ia, double& lor) {
  lor = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetElectronLorentzAngle", ie, ib, ia);
    return false;
  }
  if (m_eLorentzAngle.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetElectronLorentzAngle");
    return false;
  }
  lor = m_eLorentzAngle[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityE(const unsigned int ie, 
                              const unsigned int ib, 
                              const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleVelocityE", ie, ib, ia);
    return false;
  }
  if (m_hVelocityE.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetHoleVelocityE");
    return false;
  }
  v = m_hVelocityE[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityExB(const unsigned int ie, 
                                const unsigned int ib, 
                                const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleVelocityExB", ie, ib, ia);
    return false;
  }
  if (m_hVelocityExB.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetHoleVelocityExB");
    return false;
  }
  v = m_hVelocityExB[ia][ib][ie];
  return true;
}

bool Medium::GetHoleVelocityB(const unsigned int ie, 
                              const unsigned int ib, 
                              const unsigned int ia, double& v) {
  v = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleVelocityB", ie, ib, ia);
    return false;
  }
  if (m_hVelocityB.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetHoleVelocityB");
    return false;
  }
  v = m_hVelocityB[ia][ib][ie];
  return true;
}

bool Medium::GetHoleLongitudinalDiffusion(const unsigned int ie, 
                                          const unsigned int ib,
                                          const unsigned int ia, double& dl) {
  dl = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleLongitudinalDiffusion", ie, ib, ia);
    return false;
  }
  if (m_hDiffLong.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetHoleLongitudinalDiffusion");
    }
    return false;
  }
  dl = m_hDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetHoleTransverseDiffusion(const unsigned int ie, 
                                        const unsigned int ib,
                                        const unsigned int ia, double& dt) {
  dt = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleTransverseDiffusion", ie, ib, ia);
    return false;
  }
  if (m_hDiffTrans.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetHoleTransverseDiffusion");
    }
    return false;
  }
  dt = m_hDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetHoleTownsend(const unsigned int ie, 
                             const unsigned int ib, 
                             const unsigned int ia, double& alpha) {
  alpha = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleTownsend", ie, ib, ia);
    return false;
  }
  if (m_hTownsend.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetHoleTownsend");
    return false;
  }
  alpha = m_hTownsend[ia][ib][ie];
  return true;
}

bool Medium::GetHoleAttachment(const unsigned int ie, 
                               const unsigned int ib, 
                               const unsigned int ia, double& eta) {
  eta = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetHoleAttachment", ie, ib, ia);
    return false;
  }
  if (m_hAttachment.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetHoleAttachment");
    return false;
  }
  eta = m_hAttachment[ia][ib][ie];
  return true;
}

bool Medium::GetIonMobility(const unsigned int ie, const unsigned int ib, 
                            const unsigned int ia, double& mu) {
  mu = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetIonMobility", ie, ib, ia);
    return false;
  }
  if (m_ionMobility.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetIonMobility");
    return false;
  }
  mu = m_ionMobility[ia][ib][ie];
  return true;
}

bool Medium::GetIonLongitudinalDiffusion(const unsigned int ie, 
                                         const unsigned int ib,
                                         const unsigned int ia, double& dl) {
  dl = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetIonLongitudinalDiffusion", ie, ib, ia);
    return false;
  }
  if (m_ionDiffLong.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetIonLongitudinalDiffusion");
    }
    return false;
  }
  dl = m_ionDiffLong[ia][ib][ie];
  return true;
}

bool Medium::GetIonTransverseDiffusion(const unsigned int ie, 
                                       const unsigned int ib, 
                                       const unsigned int ia, double& dt) {
  dt = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetIonTransverseDiffusion", ie, ib, ia);
    return false;
  }
  if (m_ionDiffTrans.empty()) {
    if (m_debug) {
      PrintDataNotAvailable(m_className, "GetIonTransverseDiffusion");
    }
    return false;
  }
  dt = m_ionDiffTrans[ia][ib][ie];
  return true;
}

bool Medium::GetIonDissociation(const unsigned int ie, 
                                const unsigned int ib, 
                                const unsigned int ia, double& diss) {
  diss = 0.;
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "GetIonDissociation", ie, ib, ia);
    return false;
  }
  if (m_ionDissociation.empty()) {
    if (m_debug) PrintDataNotAvailable(m_className, "GetIonDissociation");
    return false;
  }
  diss = m_ionDissociation[ia][ib][ie];
  return true;
}

void Medium::CloneTable(std::vector<std::vector<std::vector<double> > >& tab,
                        const std::vector<double>& efields,
                        const std::vector<double>& bfields,
                        const std::vector<double>& angles, 
                        const unsigned int intp,
                        const std::pair<unsigned int, unsigned int>& extr,
                        const double init, const std::string& label) {

  if (m_debug) {
    std::cout << m_className << "::CloneTable: Copying " << label
              << " to new grid.\n";
  }

  if (tab.empty()) {
    if (m_debug) std::cout << m_className << "::CloneTable: Table is empty.\n";
    return;
  }
  // Get the dimensions of the new grid.
  const auto nE = efields.size();
  const auto nB = bfields.size();
  const auto nA = angles.size();

  // Create a temporary table to store the values at the new grid points.
  std::vector<std::vector<std::vector<double> > > tabClone;
  InitTable(nE, nB, nA, tabClone, init);

  // Fill the temporary table.
  for (size_t i = 0; i < nE; ++i) {
    const double e = efields[i];
    for (size_t j = 0; j < nB; ++j) {
      const double b = bfields[j];
      for (size_t k = 0; k < nA; ++k) {
        const double a = angles[k];
        double val = 0.;
        if (!Interpolate(e, b, a, tab, val, intp, extr)) {
          std::cerr << m_className << "::CloneTable:\n"
                    << "    Interpolation of " << label << " failed.\n"
                    << "    Cannot copy value to new grid at E = " << e 
                    << ", B = " << b << ", angle: " << a << "\n";
          continue;
        }
        tabClone[k][j][i] = val;
      }
    }
  }
  // Copy the values to the original table.
  tab.swap(tabClone);
  tabClone.clear();
}

void Medium::CloneTensor(
    std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
    const unsigned int n, 
    const std::vector<double>& efields, const std::vector<double>& bfields, 
    const std::vector<double>& angles,
    const unsigned int intp, 
    const std::pair<unsigned int, unsigned int>& extr,
    const double init,
    const std::string& label) {

  // If the table does not exist, do nothing.
  if (tab.empty()) return;

  // Get the dimensions of the new grid.
  const unsigned int nE = efields.size();
  const unsigned int nB = bfields.size();
  const unsigned int nA = angles.size();

  // Create a temporary table to store the values at the new grid points.
  std::vector<std::vector<std::vector<std::vector<double> > > > tabClone;
  InitTensor(nE, nB, nA, n, tabClone, init);

  // Fill the temporary table.
  for (unsigned int l = 0; l < n; ++l) {
    for (unsigned int i = 0; i < nE; ++i) {
      const double e = efields[i];
      for (unsigned int j = 0; j < nB; ++j) {
        const double b = bfields[j];
        for (unsigned int k = 0; k < nA; ++k) {
          const double a = angles[k];
          double val = 0.;
          if (!Interpolate(e, b, a, tab[l], val, intp, extr)) {
            std::cerr << m_className << "::CloneTensor:\n"
                      << "    Interpolation of " << label << " failed.\n"
                      << "    Cannot copy value to new grid at index " << l 
                      << ", E = " << e << ", B = "  << b << ", angle: " 
                      << a << "\n";
            continue;
          }
          tabClone[l][k][j][i] = val;
        }
      }
    }
  }
  // Copy the values to the original table.
  tab.swap(tabClone);
}

bool Medium::SetIonMobility(const unsigned int ie, const unsigned int ib, 
                            const unsigned int ia, const double mu) {

  // Check the index.
  if (ie >= m_eFields.size() || ib >= m_bFields.size() || 
      ia >= m_bAngles.size()) {
    PrintOutOfRange(m_className, "SetIonMobility", ie, ib, ia);
    return false;
  }

  if (m_ionMobility.empty()) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    Ion mobility table not initialised.\n";
    return false;
  }

  if (mu == 0.) {
    std::cerr << m_className << "::SetIonMobility:\n";
    std::cerr << "    Zero value not permitted.\n";
    return false;
  }

  m_ionMobility[ia][ib][ie] = mu;
  if (m_debug) {
    std::cout << m_className << "::SetIonMobility:\n";
    std::cout << "   Ion mobility at E = " << m_eFields[ie]
              << " V/cm, B = " << m_bFields[ib] << " T, angle " << m_bAngles[ia]
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
  const unsigned int nEfields = m_eFields.size();
  const unsigned int nBfields = m_bFields.size();
  const unsigned int nAngles = m_bAngles.size();
  InitTable(nEfields, nBfields, nAngles, m_ionMobility, 0.);
  for (unsigned int i = 0; i < nEfields; ++i) {
    const double e = m_eFields[i];
    const double mu = Interpolate1D(e, mobilities, efields, m_intpMobility,
                                    m_extrMobility);
    m_ionMobility[0][0][i] = mu;
  }

  if (m_map2d) {
    for (unsigned int i = 0; i < nAngles; ++i) {
      for (unsigned int j = 0; j< nBfields; ++j) {
        for (unsigned int k = 0; k < nEfields; ++k) {
          m_ionMobility[i][j][k] = m_ionMobility[0][0][k];
        }
      }
    }
  }
  return true;
}


void Medium::SetExtrapolationMethodVelocity(const std::string& low,
                                            const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrVel, "Velocity");
}

void Medium::SetExtrapolationMethodDiffusion(const std::string& low,
                                             const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrDiff, "Diffusion");
}

void Medium::SetExtrapolationMethodTownsend(const std::string& low,
                                            const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrTownsend, "Townsend");
}

void Medium::SetExtrapolationMethodAttachment(const std::string& low,
                                              const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrAttachment, "Attachment");
}

void Medium::SetExtrapolationMethodIonMobility(const std::string& low,
                                               const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrMobility, "IonMobility");
}

void Medium::SetExtrapolationMethodIonDissociation(const std::string& low,
                                                   const std::string& high) {

  SetExtrapolationMethod(low, high, m_extrDissociation, "IonDissociation");
}

void Medium::SetExtrapolationMethod(const std::string& low,
                                    const std::string& high,
                                    std::pair<unsigned int, unsigned int>& extr,
                                    const std::string& fcn) {


  unsigned int i = 0;
  if (GetExtrapolationIndex(low, i)) {
    extr.first = i;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethod" << fcn << ":\n"
              << "    Unknown extrapolation method (" << low << ")\n";
  } 
  unsigned int j = 0;
  if (GetExtrapolationIndex(high, j)) {
    extr.second = j;
  } else {
    std::cerr << m_className << "::SetExtrapolationMethod" << fcn << ":\n"
              << "    Unknown extrapolation method (" << high << ")\n";
  }
}

bool Medium::GetExtrapolationIndex(std::string str, unsigned int& nb) const {

  // Convert to upper-case
  for (unsigned int i = 0; i < str.length(); ++i) {
    str[i] = toupper(str[i]);
  }

  if (str == "CONST" || str == "CONSTANT") {
    nb = 0;
  } else if (str == "LIN" || str == "LINEAR") {
    nb = 1;
  } else if (str == "EXP" || str == "EXPONENTIAL") {
    nb = 2;
  } else {
    return false;
  }

  return true;
}

void Medium::SetInterpolationMethodVelocity(const unsigned int intrp) {

  if (intrp > 0) m_intpVel = intrp;
}

void Medium::SetInterpolationMethodDiffusion(const unsigned int intrp) {

  if (intrp > 0) m_intpDiff = intrp;
}

void Medium::SetInterpolationMethodTownsend(const unsigned int intrp) {

  if (intrp > 0) m_intpTownsend = intrp;
}

void Medium::SetInterpolationMethodAttachment(const unsigned int intrp) {

  if (intrp > 0) m_intpAttachment = intrp;
}

void Medium::SetInterpolationMethodIonMobility(const unsigned int intrp) {

  if (intrp > 0) m_intpMobility = intrp;
}

void Medium::SetInterpolationMethodIonDissociation(const unsigned int intrp) {

  if (intrp > 0) m_intpDissociation = intrp;
}

double Medium::GetAngle(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        const double e, const double b) const {

  // Ion
  /*
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
      ebang = m_bAngles[0];
    }
  */
  // Hole
  /*
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
      ebang = m_bAngles[0];
    }
  */
  /*
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
      ebang = m_bAngles[0];
    }
  */
  if (e * b <= 0.) return m_bAngles[0];
  const double eb = fabs(ex * bx + ey * by + ez * bz);
  if (eb > 0.2 * e * b) {
    const double ebxy = ex * by - ey * bx;
    const double ebxz = ex * bz - ez * bx;
    const double ebzy = ez * by - ey * bz;
    return asin(std::min(1., sqrt(ebxy * ebxy + ebxz * ebxz + ebzy * ebzy) / (e * b)));
  }
  return acos(std::min(1., eb / (e * b)));
} 

bool Medium::Interpolate(const double e, const double b, const double a,
    const std::vector<std::vector<std::vector<double> > >& table, double& y,
    const unsigned int intp, const std::pair<unsigned int, unsigned int>& extr) const {

  if (table.empty()) {
    y = 0.;
    return false; // TODO: true!
  }

  if (m_map2d) {
    if (!Numerics::Boxin3(table, m_bAngles, m_bFields, m_eFields,
                          m_bAngles.size(), m_bFields.size(), m_eFields.size(), a, b, e, y,
                          intp)) {
      return false;
    }
  } else {
    y = Interpolate1D(e, table[0][0], m_eFields, intp, extr);
  }
  return true;
}

double Medium::Interpolate1D(const double e, const std::vector<double>& table,
                             const std::vector<double>& fields,
                             const unsigned int intpMeth, 
                             const std::pair<unsigned int, unsigned int>& extr) const {

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
    } else if (extr.first == 1) {
      // Linear extrapolation
      const double extr4 = (table[1] - table[0]) / (fields[1] - fields[0]);
      const double extr3 = table[0] - extr4 * fields[0];
      result = extr3 + extr4 * e;
    } else if (extr.first == 2) {
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
    } else if (extr.second == 1) {
      // Linear extrapolation
      const double extr2 = (table[nSizeTable - 1] - table[nSizeTable - 2]) /
                           (fields[nSizeTable - 1] - fields[nSizeTable - 2]);
      const double extr1 =
          table[nSizeTable - 1] - extr2 * fields[nSizeTable - 1];
      result = extr1 + extr2 * e;
    } else if (extr.second == 2) {
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

void Medium::InitTable(const size_t nE, const size_t nB, const size_t nA,
    std::vector<std::vector<std::vector<double> > >& tab, const double val) {

  if (nE == 0 || nB == 0 || nA == 0) {
    std::cerr << m_className << "::InitTable: Invalid grid.\n";
    return;
  }
  tab.assign(nA, std::vector<std::vector<double> >(nB, std::vector<double>(nE, val)));
}

void Medium::InitTensor(
    const size_t nE, const size_t nB, const size_t nA, const size_t nT,
    std::vector<std::vector<std::vector<std::vector<double> > > >& tab,
    const double val) {

  if (nE == 0 || nB == 0 || nA == 0 || nT == 0) {
    std::cerr << m_className << "::InitTensor: Invalid grid.\n";
    return;
  }

  tab.assign(nT, std::vector<std::vector<std::vector<double> > >(nA, 
        std::vector<std::vector<double> >(nB, std::vector<double>(nE, val))));
}
}
