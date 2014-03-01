// Two-dimensional implementation of nearly exact Boundary Element Method

#ifndef G_COMPONENT_NEBEM_2D_H
#define G_COMPONENT_NEBEM_2D_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentNeBem2d : public ComponentBase {

 public:
  // Constructor
  ComponentNeBem2d();
  ~ComponentNeBem2d() {}

  // Calculate the drift field [V/cm] at (x, y, z)
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  // Calculate the drift field [V/cm] and potential [V] at (x, y, z)
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  // Calculate the voltage range [V]
  bool GetVoltageRange(double& vmin, double& vmax);

  void SetProjectionX() { projAxis = 0; }
  void SetProjectionY() { projAxis = 1; }
  void SetProjectionZ() { projAxis = 2; }

  void AddPanel(const double x0, const double y0, const double x1,
                const double y1, const int bctype, const double bcval,
                const double lambda);
  void AddWire(const double x0, const double y0, const double d,
               const double bcval);

  void SetNumberOfDivisions(const int ndiv);
  void SetNumberOfCollocationPoints(const int ncoll);
  void SetMinimumElementSize(const double min);
  void EnableAutoResizing() { autoSize = true; }
  void DisableAutoResizing() { autoSize = false; }
  void EnableRandomCollocation() { randomCollocation = true; }
  void DisableRandomCollocation() { randomCollocation = false; }
  void SetMaxNumberOfIterations(const int niter);

  int GetNumberOfPanels() { return nPanels; }
  int GetNumberOfWires() { return nWires; }
  int GetNumberOfElements() { return nElements; }

 private:
  static const int Local2Global = -1;
  static const int Global2Local = 1;

  // Influence matrix
  std::vector<std::vector<double> > influenceMatrix;
  // Inverse of the influence matrix
  std::vector<std::vector<double> > inverseMatrix;
  // Right hand side vector (boundary conditions)
  std::vector<double> boundaryConditions;
  // Temporary arrays used during LU decomposition
  std::vector<double> col;
  std::vector<int> index;

  int projAxis;
  int nDivisions;
  int nCollocationPoints;
  double minSize;
  bool autoSize;
  bool randomCollocation;
  int nMaxIterations;

  int nPanels;
  struct panel {
    // Coordinates of boundary points
    double x0, y0;
    double x1, y1;
    // Boundary condition type and value
    // 0: conductor (fixed potential), 1: conductor (floating),
    // 2: DD interface, 3: known charge density
    int bcType;
    double bcValue;
    // Ratio of relative dielectric permittivities
    double lambda;
  };
  std::vector<panel> panels;

  int nWires;
  struct wire {
    // Center point
    double cX, cY;
    // Diameter
    double d;
    // Boundary condition
    double bcValue;
  };
  std::vector<wire> wires;

  int nElements;
  // Array of boundary elements
  struct element {
    // Geometric type
    // 0: line, 1: (thin) wire
    int geoType;
    // Center point
    double cX, cY;
    // Half length
    double len;
    // Rotation angle
    double phi;
    // Charge density
    double solution;
    // Boundary condition type and value
    int bcType;
    double bcValue;
    // Ratio of relative dielectric permittivities
    double lambda;
  };
  std::vector<element> elements;

  bool matrixInversionFlag;

  bool Initialise();
  bool Discretise();
  bool ComputeInfluenceMatrix();
  void SplitElement(const int iel);
  bool InvertMatrix();
  bool LUDecomposition();
  void LUSubstitution();
  bool GetBoundaryConditions();
  bool Solve();
  bool CheckConvergence();

  void Rotate(const double xIn, const double yIn, const double phi,
              const int opt, double& xOut, double& yOut);

  // Compute the potential
  bool ComputePotential(const int gt, const double len, const double x,
                        const double y, double& p) {

    switch (gt) {
      case 0:
        p = LinePotential(len, x, y);
        break;
      case 1:
        p = WirePotential(len, x, y);
        break;
      default:
        return false;
    }
    return true;
  }

  // Compute the field
  bool ComputeFlux(const int gt, const double len, const double rot,
                   const double x, const double y, double& ex, double& ey) {

    double fx, fy;
    switch (gt) {
      case 0:
        LineFlux(len, x, y, fx, fy);
        break;
      case 1:
        WireFlux(len, x, y, fx, fy);
        break;
      default:
        return false;
    }
    // Transformation to global coordinate system
    Rotate(fx, fy, rot, Local2Global, ex, ey);
    return true;
  }

  double LinePotential(const double a, const double x, const double y);
  double WirePotential(const double r0, const double x, const double y);
  void LineFlux(const double a, const double x, const double y, double& ex,
                double& ey);
  void WireFlux(const double r0, const double x, const double y, double& ex,
                double& ey);

  void Reset();
  void UpdatePeriodicity();
};
}

#endif
