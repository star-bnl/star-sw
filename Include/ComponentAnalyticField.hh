#ifndef G_COMPONENT_ANALYTIC_FIELD_H
#define G_COMPONENT_ANALYTIC_FIELD_H

#include <cmath>
#include <complex>

#include "ComponentBase.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

class ComponentAnalyticField : public ComponentBase {

 public:
  // Constructor
  ComponentAnalyticField();
  // Destructor
  ~ComponentAnalyticField() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);

  bool GetVoltageRange(double& pmin, double& pmax);

  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  bool GetBoundingBox(double& x0, double& y0, double& z0, double& x1,
                      double& y1, double& z1);

  bool IsWireCrossed(double x0, double y0, double z0, double x1, double y1,
                     double z1, double& xc, double& yc, double& zc);

  bool IsInTrapRadius(const double q0, const double x0, const double y0, 
                      const double z0, double& xw, double& yx, double& rw);

  // Add a wire at (x, y) .
  void AddWire(const double x, const double y, const double diameter,
               const double voltage, const std::string label,
               const double length = 100., const double tension = 50.,
               const double rho = 19.3, const int ntrap = 5);
  // Add a tube.
  void AddTube(const double radius, const double voltage, const int nEdges,
               const std::string label);
  // Add a plane at constant x
  void AddPlaneX(const double x, const double voltage, const std::string label);
  // Add a plane at constant y
  void AddPlaneY(const double y, const double voltage, const std::string label);

  void AddStripOnPlaneX(const char direction, const double x, const double smin,
                        const double smax, const std::string label,
                        const double gap = -1.);
  void AddStripOnPlaneY(const char direction, const double y, const double smin,
                        const double smax, const std::string label,
                        const double gap = -1.);

  // Set the periodic length [cm] in x/y direction
  void SetPeriodicityX(const double s);
  void SetPeriodicityY(const double s);
  bool GetPeriodicityX(double& s);
  bool GetPeriodicityY(double& s);

  void AddCharge(const double x, const double y, const double z,
                 const double q);
  void ClearCharges();
  void PrintCharges();

  std::string GetCellType() { return cellType; }
  // Cells are classified according to the number
  // and orientation of planes, the presence of
  // periodicities and the location of the wires
  // as one of the following types:
  //
  // A    non-periodic cells with at most 1 x- and 1 y-plane
  // B1X  x-periodic cells without x-planes and at most 1 y-plane
  // B1Y  y-periodic cells without y-planes and at most 1 x-plane
  // B2X  cells with 2 x-planes and at most 1 y-plane
  // B2Y  cells with 2 y-planes and at most 1 x-plane
  // C1   doubly periodic cells without planes
  // C2X  doubly periodic cells with x-planes
  // C2Y  doubly periodic cells with y-planes
  // C3   double periodic cells with x- and y-planes
  // D1   round tubes without axial periodicity
  // D2   round tubes with axial periodicity
  // D3   polygonal tubes without axial periodicity

  void AddReadout(const std::string label);

  void EnableChargeCheck() { chargeCheck = true; }
  void DisableChargeCheck() { chargeCheck = false; }

  int GetNumberOfWires() { return nWires; }
  bool GetWire(const int i, double& x, double& y, double& diameter,
               double& voltage, std::string& label, double& length,
               double& charge, int& ntrap);

  int GetNumberOfPlanesX();
  int GetNumberOfPlanesY();
  bool GetPlaneX(const int i, double& x, double& voltage, std::string& label);
  bool GetPlaneY(const int i, double& y, double& voltage, std::string& label);

  bool GetTube(double& r, double& voltage, int& nEdges, std::string& label);

 private:
  bool chargeCheck;

  bool cellset;
  bool sigset;

  bool polar;

  // Cell type (as string and number)
  std::string cellType;
  int iCellType;

  // Bounding box
  double xmin, xmax;
  double ymin, ymax;
  double zmin, zmax;

  // Voltage range
  double vmin, vmax;

  // Periodicities
  bool perx, pery;
  double sx, sy;

  // Signals
  int nFourier;
  std::string cellTypeFourier;
  bool fperx, fpery;
  int mxmin, mxmax, mymin, mymax;
  int mfexp;

  int nReadout;
  std::vector<std::string> readout;

  // Wires
  int nWires;
  struct wire {
    // Location
    double x;
    double y;
    // Diameter
    double d;
    // Potential
    double v;
    // Charge
    double e;
    // Label
    std::string type;
    // Length
    double u;
    // Readout group
    int ind;
    // Trap Radius - tracked particle "trapped" if within
    // nTrap*radius of wire.
    int nTrap;
  };
  std::vector<wire> w;

  // Stretching weight
  std::vector<double> weight;
  // Density
  std::vector<double> dens;
  // Mirror charges for force calculations
  std::vector<double> cnalso;

  // Option for computation of dipole terms
  bool dipole;
  // Dipole angle and amplitude
  std::vector<double> cosph2;
  std::vector<double> sinph2;
  std::vector<double> amp2;

  // Parameters for B2 type cells
  std::vector<double> b2sin;
  // Parameters for C type cells
  int mode;
  std::complex<double> zmult;
  double p1, p2, c1;
  // Parameters for D3 type cells
  // Conformal mapping in polygons
  std::vector<std::complex<double> > wmap;
  double kappa;
  // Tables of coefficients
  std::vector<std::vector<double> > cc1;
  std::vector<std::vector<double> > cc2;

  // Reference potential
  double v0;
  double corvta, corvtb, corvtc;

  // Planes
  // Existence
  bool ynplan[4];
  bool ynplax, ynplay;
  // Coordinates
  double coplan[4];
  double coplax, coplay;
  // Voltages
  double vtplan[4];

  struct strip {
    // Label
    std::string type;
    // Readout group
    int ind;
    // Coordinates
    double smin, smax;
    double gap;
  };

  struct plane {
    // Labels
    std::string type;
    // Readout group
    int ind;
    // Background weighting fields
    double ewxcor;
    double ewycor;
    // x/y strips
    int nStrips1;
    std::vector<strip> strips1;
    // z strips
    int nStrips2;
    std::vector<strip> strips2;
  };

  std::vector<plane> planes;

  // Tube
  bool tube;
  int mtube, ntube;
  double cotube;
  double vttube;

  // Capacitance matrix
  std::vector<std::vector<double> > a;
  // Signal matrix
  std::vector<std::vector<std::complex<double> > > sigmat;
  // Induced charges on planes
  std::vector<std::vector<double> > qplane;

  // Point charges
  int n3d;
  struct charge3d {
    double x, y, z;
    double e;
  };
  std::vector<charge3d> ch3d;
  int nTermBessel;
  int nTermPoly;

  // Gravity
  double down[3];

  void UpdatePeriodicity();
  void Reset() { CellInit(); }

  void CellInit();
  bool Prepare();
  bool CellCheck();
  bool CellType();
  bool PrepareStrips();

  bool PrepareSignals();
  bool SetupWireSignals();
  bool SetupPlaneSignals();

  // Calculation of charges
  bool Setup();
  bool SetupA00();
  bool SetupB1X();
  bool SetupB1Y();
  bool SetupB2X();
  bool SetupB2Y();
  bool SetupC10();
  bool SetupC2X();
  bool SetupC2Y();
  bool SetupC30();
  bool SetupD10();
  bool SetupD20();
  bool SetupD30();

  bool IprA00(const int mx, const int my);
  bool IprB2X(const int my);
  bool IprB2Y(const int mx);
  bool IprC2X();
  bool IprC2Y();
  bool IprC30();
  bool IprD10();
  bool IprD30();

  bool SetupDipole() { return true; }

  // Inversion of capacitance matrix
  bool Charge();

  // Evaluation of the electric field
  int Field(const double xin, const double yin, const double zin, double& ex,
            double& ey, double& ez, double& volt, const bool opt);
  void FieldA00(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldB1X(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldB1Y(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldB2X(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldB2Y(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldC10(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldC2X(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldC2Y(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldC30(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldD10(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldD20(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);
  void FieldD30(const double xpos, const double ypos, double& ex, double& ey,
                double& volt, const bool opt);

  // Field due to point charges
  void Field3dA00(const double x, const double y, const double z, double& ex,
                  double& ey, double& ez, double& volt);
  void Field3dB2X(const double x, const double y, const double z, double& ex,
                  double& ey, double& ez, double& volt);
  void Field3dB2Y(const double x, const double y, const double z, double& ex,
                  double& ey, double& ez, double& volt);
  void Field3dD10(const double x, const double y, const double z, double& ex,
                  double& ey, double& ez, double& volt);
  // Evaluation of the weighting field
  bool Wfield(const double xpos, const double ypos, const double zpos,
              double& ex, double& ey, double& ez, double& volt, const int isw,
              const bool opt);
  void WfieldWireA00(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int mx, const int my,
                     const int sw, const bool opt);
  void WfieldWireB2X(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int my, const int sw,
                     const bool opt);
  void WfieldWireB2Y(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int mx, const int sw,
                     const bool opt);
  void WfieldWireC2X(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int sw, const bool opt);
  void WfieldWireC2Y(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int sw, const bool opt);
  void WfieldWireC30(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int sw, const bool opt);
  void WfieldWireD10(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int sw, const bool opt);
  void WfieldWireD30(const double xpos, const double ypos, double& ex,
                     double& ey, double& volt, const int sw, const bool opt);
  void WfieldPlaneA00(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int mx, const int my,
                      const int iplane, const bool opt);
  void WfieldPlaneB2X(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int my, const int iplane,
                      const bool opt);
  void WfieldPlaneB2Y(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int mx, const int iplane,
                      const bool opt);
  void WfieldPlaneC2X(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int iplane,
                      const bool opt);
  void WfieldPlaneC2Y(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int iplane,
                      const bool opt);
  void WfieldPlaneC30(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int iplane,
                      const bool opt);
  void WfieldPlaneD10(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int iplane,
                      const bool opt);
  void WfieldPlaneD30(const double xpos, const double ypos, double& ex,
                      double& ey, double& volt, const int iplane,
                      const bool opt);
  void WfieldStripZ(const double xpos, const double ypos, double& ex,
                    double& ey, double& volt, const int ip, const int is,
                    const bool opt);
  void WfieldStripXy(const double xpos, const double ypos, const double zpos,
                     double& ex, double& ey, double& ez, double& volt,
                     const int ip, const int is, const bool opt);

  // Auxiliary functions for C type cells
  double Ph2(const double xpos, const double ypos);
  double Ph2Lim(const double radius) {
    return -log(abs(zmult) * radius * (1. - 3. * p1 + 5. * p2));
  }
  void E2Sum(const double xpos, const double ypos, double& ex, double& ey);

  // Mapping function for D30 type cells
  void ConformalMap(std::complex<double> z, std::complex<double>& ww,
                    std::complex<double>& wd);
  void InitializeCoefficientTables();

  bool InTube(const double x0, const double y0, const double a, const int n);

  // Transformation between cartesian and polar coordinates
  void Cartesian2Polar(const double x0, const double y0, double& r,
                       double& theta) {

    if (x0 == 0. && y0 == 0.) {
      r = theta = 0.;
      return;
    }
    r = sqrt(x0 * x0 + y0 * y0);
    theta = 180. * atan2(y0, x0) / Pi;
  }

  void Polar2Cartesian(const double r, const double theta, double& x0,
                       double& y0) {

    x0 = r * cos(Pi * theta / 180.);
    y0 = r * sin(Pi * theta / 180.);
  }

  // Transformation (r, theta) to (rho, phi) via the map
  // (r, theta) = (exp(rho), 180 * phi / Pi).
  void RTheta2RhoPhi(const double rho, const double phi, double& r,
                     double& theta) {

    r = exp(rho);
    theta = 180. * phi / Pi;
  }
};
}

#endif
