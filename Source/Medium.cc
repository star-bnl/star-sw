#include <iostream>
#include <cmath>

#include "Medium.hh"
#include "FundamentalConstants.hh"
#include "Plotting.hh"

namespace Garfield {

int Medium::idCounter = -1;

Medium::Medium() : 
  id(++idCounter), name(""), 
  temperature(293.15), pressure(760.), 
  epsilon(1.), 
  nComponents(1), atomicNumber(1.), atomicWeight(0.), density(0.),
  driftable(false), microscopic(false), ionisable(false),
  isChanged(true),
  debug(false) {

  efields.clear();
  bfields.clear();
  bangles.clear();
  
}

void 
Medium::SetTemperature(const double t) {

  if (t <= 0.) {
    std::cerr << "Medium::SetTemperature:" << std::endl;
    std::cerr << "    Temperature [K] must be greater than zero." << std::endl;
    return;
  }
  temperature = t;
  isChanged = true;

}

void 
Medium::SetPressure(const double p) {

  if (p <= 0.) {
    std::cerr << "Medium::SetPressure:" << std::endl;
    std::cerr << "    Pressure [Torr] must be greater than zero." << std::endl;
    return;
  }
  pressure = p;
  isChanged = true;  

}

void 
Medium::SetDielectricConstant(const double eps) {

  if (eps < 1.) {
    std::cerr << "Medium::SetDielectricConstant: " << std::endl;
    std::cerr << "    Dielectric constant must be >= 1." << std::endl;
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
    std::cerr << "Medium::GetComponent: Index out of range" << std::endl;
  }
  
  label = name; f = 1.;
  
}

void 
Medium::SetAtomicNumber(const double z) {

  if (z < 1.) {
    std::cerr << "Medium::SetAtomicNumber:" << std::endl;
    std::cerr << "    Atomic number must be >= 1." << std::endl;
    return;
  }
  atomicNumber = z;
  isChanged = true;  

}

void 
Medium::SetAtomicWeight(const double a) {

  if (a <= 0.) {
    std::cerr << "Medium::SetAtomicWeight:" << std::endl;
    std::cerr << "    Atomic weight must be greater than zero." << std::endl;
    return;
  }
  atomicWeight = a;
  isChanged = true;  

}

void 
Medium::SetNumberDensity(const double n) {

  if (n <= 0.) {
    std::cerr << "Medium::SetNumberDensity:" << std::endl;
    std::cerr << "    Density [cm-3] must be greater than zero." << std::endl;
    return;
  }
  density = n;
  isChanged = true;  

}

void 
Medium::SetMassDensity(const double rho) {

  if (rho <= 0.) {
    std::cerr << "Medium::SetMassDensity:" << std::endl;
    std::cerr << "    Density [g/cm3] must be greater than zero." << std::endl;
    return;
  }

  if (atomicWeight <= 0.) {
    std::cerr << "Medium::SetMassDensity:" << std::endl;
    std::cerr << "    Atomic weight is not defined." << std::endl;
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
  // Compute the magnitude of the electric field
  const double e = sqrt(ex * ex + ey * ey + ez * ez);
  if (e <= 0.) return false;
  // Compute the magnitude of the magnetic field
  const double b = sqrt(bx * bx + by * by + bz * bz);

  // Compute unit vectors along E, E x B and Btrans
  double ue[3] = {ex / e, ey / e, ez / e};
  double uexb[3] = {ey * bz - ez * by, ez * bx - ex * bz, ex * by - ey * bx};
  const double exb = sqrt(uexb[0] * uexb[0] + uexb[1] * uexb[1] + uexb[2] * uexb[2]);

  double ubt[3] = {
    uexb[1] * ez - uexb[2] * ey, 
    uexb[2] * ex - uexb[0] * ez, 
    uexb[0] * ey - uexb[1] * ex
  };
  const double bt = sqrt(ubt[0] * ubt[0] + ubt[1] * ubt[1] + ubt[2] * ubt[2]);

  if (b > 0.) {
    uexb[0] /= exb; uexb[1] /= exb; uexb[2] /= exb;
    ubt[0] /= bt; ubt[1] /= bt; ubt[2] /= bt;
  } else {
    uexb[0] = ubt[0] = ue[0];
    uexb[1] = ubt[1] = ue[1];
    uexb[2] = ubt[2] = ue[2];
  }

  // Compute the angle between B field and E field
  const double eb = fabs(ex * bx + ey * by + ez * bz);
  

}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
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

}

bool 
Medium::ElectronDiffusion(const double ex, const double ey, const double ez,
                          const double bx, const double by, const double bz,
                          double cov[3][3]) {

  if (debug) {
    std::cerr << "Medium::ElectronDiffusionTensor:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  return false;

}

bool 
Medium::ElectronTownsend(const double ex, const double ey, const double ez,
                         const double bx, const double by, const double bz,
                         double& alpha) {

  if (debug) {
    std::cerr << "Medium::ElectronTownsend:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  alpha = 0.;
  return false;

}

bool 
Medium::ElectronAttachment(const double ex, const double ey, const double ez,
                           const double bx, const double by, const double bz,
                           double& eta) {

  if (debug) {
    std::cerr << "Medium::ElectronAttachment:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  eta = 0.;
  return false;

}

double 
Medium::GetElectronNullCollisionRate() {

  if (debug) {
    std::cerr << "Medium::GetElectronNullCollisionRate:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  return 0.;
  
}

double 
Medium::GetElectronCollisionRate(const double e) {

  if (debug) {
    std::cerr << "Medium::GetElectronCollisionRate:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  return 0.;

}

bool 
Medium::GetElectronCollision(const double e, int& type, int& level,
                     double& e1, double& ctheta, double& s, double& esec) {
  
  if (debug) {
    std::cerr << "Medium::GetElectronCollision:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  return 0.;
                            
}

int 
Medium::GetNumberOfLevels() {

  if (debug) {
    std::cerr << "Medium::GetNumberOfLevels:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  return 0;

}
                
bool 
Medium::HoleVelocity(const double ex, const double ey, const double ez, 
                     const double bx, const double by, const double bz, 
                     double& vx, double& vy, double& vz) {
            
  if (debug) {
    std::cerr << "Medium::HoleVelocity:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  vx = vy = vz = 0.;
  return false;            

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
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

}

bool 
Medium::HoleDiffusion(const double ex, const double ey, const double ez,
                      const double bx, const double by, const double bz,
                      double cov[3][3]) {

  if (debug) {
    std::cerr << "Medium::HoleDiffusionTensor:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  cov[0][0] = 0.; cov[0][1] = 0.; cov[0][2] = 0.;
  cov[1][0] = 0.; cov[1][1] = 0.; cov[1][2] = 0.;
  cov[2][0] = 0.; cov[2][1] = 0.; cov[2][2] = 0.;
  return false;

}

bool 
Medium::HoleTownsend(const double ex, const double ey, const double ez,
                     const double bx, const double by, const double bz,
                     double& alpha) {

  if (debug) {
    std::cerr << "Medium::HoleTownsend:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  alpha = 0.;
  return false;

}

bool 
Medium::HoleAttachment(const double ex, const double ey, const double ez,
                       const double bx, const double by, const double bz,
                       double& eta) {
            
  if (debug) {
    std::cerr << "Medium::HoleAttachment:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  eta = 0.;
  return false;

}

bool 
Medium::IonVelocity(const double ex, const double ey, const double ez, 
                    const double bx, const double by, const double bz, 
                    double& vx, double& vy, double& vz) {

  if (debug) {
    std::cerr << "Medium::IonVelocity:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
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

}

bool 
Medium::IonDissociation(const double ex, const double ey, const double ez,
                        const double bx, const double by, const double bz,
                        double& diss) {

  if (debug) {
    std::cerr << "Medium::IonDissociation:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  diss = 0.;
  return false;

}

bool 
Medium::GetOpticalDataRange(double& emin, double& emax, const int i) {

  if (debug) {
    std::cerr << "Medium::GetOpticalDataRange:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  return false;

}

bool 
Medium::GetDielectricFunction(const double e, 
                              double& eps1, double& eps2, const int i) {

  if (debug) {
    std::cerr << "Medium::GetDielectricFunction:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
  }
  eps1 = 1.; eps2 = 0.;
  return false;

}

bool 
Medium::GetPhotoAbsorptionCrossSection(const double e, 
                                       double& sigma, const int i) {

  if (debug) {
    std::cerr << "Medium::GetPhotoAbsorptionCrossSection:" << std::endl;
    std::cerr << "    " << name << ": Function is not implemented." 
              << std::endl;
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
Medium::GetPhotonCollision(const double e, int& type, int& level, double& e1,
                           double& ctheta, double& s, double& esec) {

  return false;

}

void 
Medium::PlotElectronVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine.PlotVelocity(this, true, false, false);
  
}

void 
Medium::PlotHoleVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine.PlotVelocity(this, false, true, false);

}

void 
Medium::PlotIonVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine.PlotVelocity(this, false, false, true);

}

void 
Medium::PlotElectronHoleVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine.PlotVelocity(this, true, true, false);

}

void 
Medium::PlotElectronIonVelocity(const double emin, const double emax) {

  PlotVelocityCommon(emin, emax);
  plottingEngine.PlotVelocity(this, false, false, true);

}

void 
Medium::PlotElectronTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine.PlotTownsend(this, true, false);

}

void 
Medium::PlotHoleTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine.PlotTownsend(this, false, true);

}

void 
Medium::PlotElectronHoleTownsend(const double emin, const double emax) {

  PlotTownsendCommon(emin, emax);
  plottingEngine.PlotTownsend(this, true, true);

}

void 
Medium::PlotElectronAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine.PlotAttachment(this, true, false);

}

void 
Medium::PlotHoleAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine.PlotAttachment(this, false, true);
  
}

void 
Medium::PlotElectronHoleAttachment(const double emin, const double emax) {

  PlotAttachmentCommon(emin, emax);
  plottingEngine.PlotAttachment(this, true, true);
  
}

void
Medium::PlotVelocityCommon(const double emin, const double emax) {

  plottingEngine.SetRangeX(emin, emax);
  plottingEngine.SetLabelX("electric field [V/cm]");
  plottingEngine.SetLabelY("drift velocity [V/cm]");    
  plottingEngine.SetTitle(name);

}

void
Medium::PlotTownsendCommon(const double emin, const double emax) {

  plottingEngine.SetRangeX(emin, emax);
  plottingEngine.SetLabelX("electric field [V/cm]");
  plottingEngine.SetLabelY("Townsend coefficient [1/cm]");    
  plottingEngine.SetTitle(name);  

}

void
Medium::PlotAttachmentCommon(const double emin, const double emax) {

  plottingEngine.SetRangeX(emin, emax);
  plottingEngine.SetLabelX("electric field [V/cm]");
  plottingEngine.SetLabelY("attachment coefficient [1/cm]");
  plottingEngine.SetTitle(name);

}

bool
Medium::BoxInterpolation3d(std::vector<std::vector<std::vector<double> > >& value,
                    std::vector<double>& xAxis, std::vector<double>& yAxis, std::vector<double>& zAxis, 
                    double x, double y, double z, double& f, const int ip) {


  // Shape functions
  double fX[4] = {0., 0., 0., 0.};
  double fY[4] = {0., 0., 0., 0.};
  double fZ[4] = {0., 0., 0., 0.};

  int iX0, iX1;
  int iY0, iY1;
  int iZ0, iZ1;

  f = 0.;

  // Check the interpolation order
  if (ip < 0 || ip > 2) {
    std::cerr << "Medium::BoxInterpolation3d:" << std::endl;
    std::cerr << "    Incorrect interpolation order. No interpolation." << std::endl;
    return false;
  }

  // Compute the shape functions and ranges
  if (!ComputeShapeFunctions(xAxis, x, ip, fX[0], fX[1], fX[2], fX[3], iX0, iX1)) {
    std::cerr << "Medium::BoxInterpolation3d:" << std::endl;
    std::cerr << "    Incorrect grid in x direction. No interpolation." << std::endl;
    return false;
  }
  if (!ComputeShapeFunctions(yAxis, y, ip, fY[0], fY[1], fY[2], fY[3], iY0, iY1)) {
    std::cerr << "Medium::BoxInterpolation3d:" << std::endl;
    std::cerr << "    Incorrect grid in y direction. No interpolation." << std::endl;
    return false;
  }
  if (!ComputeShapeFunctions(zAxis, z, ip, fZ[0], fZ[1], fZ[2], fZ[3], iZ0, iZ1)) {
    std::cerr << "Medium::BoxInterpolation3d:" << std::endl;
    std::cerr << "    Incorrect grid in z direction. No interpolation." << std::endl;
    return false;
  }

  for (int iX = iX0; iX <= iX1; ++iX) {
    for (int iY = iY0; iY <= iY1; ++iY) {
      for (int iZ = iZ0; iZ <= iZ1; ++iZ) {
        f += value[iX][iY][iZ] * fX[iX - iX0] * fY[iY - iY0] * fZ[iZ - iZ0];
      }
    }
  }

  return true;

}

bool
Medium::ComputeShapeFunctions(std::vector<double>& axis, const double x, const int ip, 
                              double& f1, double& f2, double& f3, double& f4, int& i0, int& i1) {

  
  const int n = axis.size();
                              
  // Make sure we have enough points
  if (n < 1) {
    std::cerr << "Medium::ComputeShapeFunctions:" << std::endl;
    std::cerr << "    Incorrect number of points." << std::endl;
    return false;
  }
 
  // Zeroth order interpolation
  if (ip == 0 || n <= 1) {
    // Find the nearest node
    double d = fabs(x - axis[0]);
    double d1 = d;
    int iNode = 0;
    for (int i = 1; i < n; ++i) {
      d1 = fabs(x - axis[i]);
      if (d1 < d) {
        d = d1;
        iNode = i;
      }
    }
    // Set the summing range
    i0 = i1 = iNode;
    // Set the shape functions
    f1 = 1.; f2 = 0.; f3 = 0.; f4 = 0.;
    return true;
  }

  // First order interpolation
  if (ip == 1 || n <= 2) {
    // Find the grid segment containing this point
    int iGrid = 0;
    for (int i = 1; i < n; ++i) {
      if ((axis[i - 1] - x) * (x - axis[i]) >= 0.) iGrid = i;
    }
    // Ensure there won't be any divisions by zero
    if (axis[iGrid] == axis[iGrid - 1]) return false;
    // Compute local coordinates
    const double xLocal = (x - axis[iGrid - 1]) / (axis[iGrid] - axis[iGrid - 1]);
    // Set the summing range
    i0 = iGrid - 1;
    i1 = iGrid;
    // Set the shape functions
    f1 = 1. - xLocal;
    f2 = xLocal;
    f3 = 0.;
    f4 = 0.;
    return true;
  }

  // Second order interpolation
  if (ip == 2) {
    // Find the grid segment containing this point
    int iGrid = 0;
    for (int i = 1; i < n; ++i) {
      if ((axis[i - 1] - x) * (x - axis[i]) >= 0.) iGrid = i;
    }
    // Compute the local coordinate for this grid segment
    const double xLocal = (x - axis[iGrid - 1]) / (axis[iGrid] - axis[iGrid - 1]);
    // Set the summing range and shape functions
    if (iGrid == 1) {
      i0 = iGrid - 1; i1 = iGrid + 1;
      if (axis[i0] == axis[i0 + 1] || axis[i0] == axis[i0 + 2] || axis[i0 + 1] == axis[i0 + 2]) {
        std::cerr << "Medium::ComputeShapeFunctions:" << std::endl;
        std::cerr << "    One or more grid points coincide. No interpolation" << std::endl;
        return false;
      }
      f1 =  (x - axis[i0 + 1]) * (x - axis[i0 + 2]) / 
           ((axis[i0] - axis[i0 + 1]) * (axis[i0]     - axis[i0 + 2]));
      f2 =  (x - axis[i0])     * (x - axis[i0 + 2]) / 
           ((axis[i0 + 1] - axis[i0]) * (axis[i0 + 1] - axis[i0 + 2]));
      f3 =  (x - axis[i0])     * (x - axis[i0 + 1]) / 
           ((axis[i0 + 2] - axis[i0]) * (axis[i0 + 2] - axis[i0 + 1]));
      f4 = 0.;
      return true;
    } else if (iGrid == n - 1) {
      i0 = iGrid - 2; i1 = iGrid;
      if (axis[i0] == axis[i0 + 1] || axis[i0] == axis[i0 + 2] || axis[i0 + 1] == axis[i0 + 2]) {
        std::cerr << "Medium::ComputeShapeFunctions:" << std::endl;
        std::cerr << "    One or more grid points coincide. No interpolation." << std::endl;
        return false;
      }
      f1 =  (x - axis[i0 + 1]) * (x - axis[i0 + 2]) /
           ((axis[i0] - axis[i0 + 1]) * (axis[i0] - axis[i0 + 2]));
      f2 =  (x - axis[i0]) * (x - axis[i0 + 2]) /
           ((axis[i0 + 1] - axis[i0]) * (axis[i0 + 1] - axis[i0 + 2]));
      f3 =  (x - axis[i0])     * (x - axis[i0 + 1]) /
           ((axis[i0 + 2] - axis[i0]) * (axis[i0 + 2] - axis[i0 + 1]));
      f4 = 0.;
      return true;
    } else {
      i0 = iGrid - 2; i1 = iGrid + 1;
      if (axis[i0] == axis[i0 + 1] || axis[i0] == axis[i0 + 2] || axis[i0] == axis[i0 + 3] || 
          axis[i0 + 1] == axis[i0 + 2] || axis[i0 + 1] == axis[i0 + 2] || 
          axis[i0 + 1] == axis[i0 + 3] || axis[i0 + 2] == axis[i0 + 3]) {
        std::cerr << "Medium::ComputeShapeFunctions:" << std::endl;
        std::cerr << "    One or more grid points coincide. No interpolation." << std::endl;
        return false;
      }
      f1 =  (1. - xLocal) * (x - axis[i0 + 1]) * (x - axis[i0 + 2]) / 
                           ((axis[i0] - axis[i0 + 1]) * (axis[i0] - axis[i0 + 2]));
      f2 =  (1. - xLocal) * (x - axis[i0]) * (x - axis[i0 + 2]) / 
                           ((axis[i0 + 1] - axis[i0]) * (axis[i0 + 1] - axis[i0 + 2])) + 
                  xLocal  * (x - axis[i0 + 1]) * (x - axis[i0 + 3]) /
                           ((axis[i0 + 1] - axis[i0 + 2]) * (axis[i0 + 1] - axis[i0 + 3]));
      f3 =  (1. - xLocal) * (x - axis[i0]) * (x - axis[i0 + 1]) / 
                           ((axis[i0 + 2] - axis[i0]) * (axis[i0 + 2] - axis[i0 + 1])) + 
                  xLocal  * (x - axis[i0 + 1]) * (x - axis[i0 + 3]) /
                           ((axis[i0 + 2] - axis[i0 + 1]) * (axis[i0 + 2] - axis[i0 + 3]));
      f4 =        xLocal  * (x - axis[i0 + 1]) * (x - axis[i0 + 2]) / 
                           ((axis[i0 + 3] - axis[i0 + 1]) * (axis[i0 + 3] - axis[i0 + 2]));
      return true;
    }
  }  

  return false;

}

}
