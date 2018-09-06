#ifndef G_COMPONENT_NEBEM_2D_H
#define G_COMPONENT_NEBEM_2D_H

#include "ComponentBase.hh"

namespace Garfield {

/// Two-dimensional implementation of the nearly exact Boundary Element Method.

class ComponentNeBem2d : public ComponentBase {

 public:
  /// Constructor
  ComponentNeBem2d();
  /// Destructor
  ~ComponentNeBem2d() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) override;
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status) override;
  bool GetVoltageRange(double& vmin, double& vmax) override;

  void SetProjectionX() { projAxis = 0; }
  void SetProjectionY() { projAxis = 1; }
  void SetProjectionZ() { projAxis = 2; }

  void AddPanel(const double x0, const double y0, const double x1,
                const double y1, const unsigned int bctype, const double bcval,
                const double lambda);
  void AddWire(const double x0, const double y0, const double d,
               const double bcval);

  void SetNumberOfDivisions(const unsigned int ndiv);
  void SetNumberOfCollocationPoints(const unsigned int ncoll);
  void SetMinimumElementSize(const double min);
  void EnableAutoResizing(const bool on = true) { m_autoSize = on; }
  void EnableRandomCollocation(const bool on = true) { 
    m_randomCollocation = on; 
  }
  void SetMaxNumberOfIterations(const unsigned int niter);

  unsigned int GetNumberOfPanels() const { return m_panels.size(); }
  unsigned int GetNumberOfWires() const { return m_wires.size(); }
  unsigned int GetNumberOfElements() const { return m_elements.size(); }

 private:
  static const int Local2Global = -1;
  static const int Global2Local = 1;
  static const double InvEpsilon0;
  static const double InvTwoPiEpsilon0;

  int projAxis = 2;
  unsigned int m_nDivisions = 5;
  unsigned int m_nCollocationPoints = 3;
  double m_minSize = 1.e-3;
  bool m_autoSize = false;
  bool m_randomCollocation = false;
  unsigned int m_nMaxIterations = 3;

  struct Panel {
    // Coordinates of boundary points
    double x0, y0;
    double x1, y1;
    // Boundary condition type and value
    // 0: conductor (fixed potential), 1: conductor (floating),
    // 2: DD interface, 3: known charge density
    unsigned int bcType;
    double bcValue;
    // Ratio of relative dielectric permittivities
    double lambda;
  };
  std::vector<Panel> m_panels;

  struct Wire {
    // Center point
    double cX, cY;
    // Diameter
    double d;
    // Boundary condition
    double bcValue;
  };
  std::vector<Wire> m_wires;

  // Array of boundary elements
  struct Element {
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
    unsigned int bcType;
    double bcValue;
    // Ratio of relative dielectric permittivities
    double lambda;
  };
  std::vector<Element> m_elements;

  bool m_matrixInversionFlag = false;

  bool Initialise();
  bool Discretise();
  bool ComputeInfluenceMatrix(std::vector<std::vector<double> >& infmat) const;
  void SplitElement(const unsigned int iel);
  bool InvertMatrix(std::vector<std::vector<double> >& influenceMatrix,
                    std::vector<std::vector<double> >& inverseMatrix) const;
  bool LUDecomposition(std::vector<std::vector<double> >& mat,
                       std::vector<int>& index) const;
  void LUSubstitution(const std::vector<std::vector<double> >& mat,
                      const std::vector<int>& index,
                      std::vector<double>& col) const;

  bool GetBoundaryConditions(std::vector<double>& bc) const;
  bool Solve(const std::vector<std::vector<double> >& inverseMatrix,
             const std::vector<double>& bc);
  bool CheckConvergence() const;

  void Rotate(const double xIn, const double yIn, const double phi,
              const int opt, double& xOut, double& yOut) const;

  // Compute the potential
  bool ComputePotential(const int gt, const double len, const double x,
                        const double y, double& p) const {

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
                   const double x, const double y, 
                   double& ex, double& ey) const {

    double fx = 0., fy = 0.;
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

  double LinePotential(const double a, const double x, const double y) const;
  double WirePotential(const double r0, const double x, const double y) const;
  void LineFlux(const double a, const double x, const double y, double& ex,
                double& ey) const;
  void WireFlux(const double r0, const double x, const double y, double& ex,
                double& ey) const;

  void Reset() override;
  void UpdatePeriodicity() override;
};
}

#endif
