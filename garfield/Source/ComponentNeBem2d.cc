#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>

#include "ComponentNeBem2d.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

ComponentNeBem2d::ComponentNeBem2d()
    : projAxis(2),
      nDivisions(5),
      nCollocationPoints(3),
      minSize(1.e-3),
      autoSize(false),
      randomCollocation(false),
      nMaxIterations(3),
      nPanels(0),
      nWires(0),
      nElements(0),
      matrixInversionFlag(false) {

  m_className = "ComponentNeBem2d";

  influenceMatrix.clear();
  inverseMatrix.clear();
}

void ComponentNeBem2d::ElectricField(const double x, const double y,
                                     const double z, double& ex, double& ey,
                                     double& ez, double& v, Medium*& m,
                                     int& status) {

  ex = ey = ez = v = 0.;
  status = 0;
  // Check if the requested point is inside a medium
  m = GetMedium(x, y, z);
  if (m == NULL) {
    status = -6;
    return;
  }

  if (!m_ready) {
    if (!Initialise()) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Initialisation failed.\n";
      status = -11;
      return;
    }
    m_ready = true;
  }

  double dx = 0., dy = 0.;
  double xLoc, yLoc;
  double fx, fy, u;

  // Sum up the contributions from all boundary elements
  for (int i = nElements; i--;) {
    dx = x - elements[i].cX;
    dy = y - elements[i].cY;
    // Transform to local coordinate system
    Rotate(dx, dy, elements[i].phi, Global2Local, xLoc, yLoc);
    // Compute the potential
    if (!ComputePotential(elements[i].geoType, elements[i].len, xLoc, yLoc,
                          u)) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Potential contribution from element " << i
                << " could not be calculated.\n";
      status = -11;
      return;
    }
    // Compute the field
    if (!ComputeFlux(elements[i].geoType, elements[i].len, elements[i].phi,
                     xLoc, yLoc, fx, fy)) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Field contribution from element " << i
                << " could not be calculated.\n";
      status = -11;
      return;
    }
    v += u * elements[i].solution;
    ex += fx * elements[i].solution;
    ey += fy * elements[i].solution;
  }
}

void ComponentNeBem2d::ElectricField(const double x, const double y,
                                     const double z, double& ex, double& ey,
                                     double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

bool ComponentNeBem2d::GetVoltageRange(double& vmin, double& vmax) {

  if (nPanels <= 0 && nWires <= 0) return false;
  bool gotValue = false;

  for (int i = nPanels; i--;) {
    if (panels[i].bcType != 0) continue;
    if (!gotValue) {
      vmin = vmax = panels[i].bcValue;
      gotValue = true;
    } else {
      if (panels[i].bcValue < vmin) vmin = panels[i].bcValue;
      if (panels[i].bcValue > vmax) vmax = panels[i].bcValue;
    }
  }

  for (int i = nWires; i--;) {
    if (!gotValue) {
      vmin = vmax = wires[i].bcValue;
      gotValue = true;
    } else {
      if (wires[i].bcValue < vmin) vmin = wires[i].bcValue;
      if (wires[i].bcValue > vmax) vmax = wires[i].bcValue;
    }
  }

  return gotValue;
}

void ComponentNeBem2d::AddPanel(const double x0, const double y0,
                                const double x1, const double y1,
                                const int bctype, const double bcval,
                                const double lambda) {

  const double dx = x1 - x0;
  const double dy = y1 - y0;
  if (dx * dx + dy * dy <= Small) {
    std::cerr << m_className << "::AddPanel:\n";
    std::cerr << "    Panel length must be greater than zero.\n";
    return;
  }

  panel newPanel;
  newPanel.x0 = x0;
  newPanel.y0 = y0;
  newPanel.x1 = x1;
  newPanel.y1 = y1;
  if (bctype < 0 || bctype > 3) {
    std::cerr << m_className << "::AddPanel:\n";
    std::cerr << "    Unknown boundary condition type: " << bctype << "\n";
    return;
  }
  newPanel.bcType = bctype;
  newPanel.bcValue = bcval;
  newPanel.lambda = lambda;
  panels.push_back(newPanel);
  ++nPanels;

  if (m_debug) {
    std::cout << m_className << "::AddPanel:\n";
    std::cout << "    From: (" << x0 << ", " << y0 << ")\n";
    std::cout << "    To:   (" << x1 << ", " << y1 << ")\n";
    switch (bctype) {
      case 0:
        std::cout << "    Type: Conductor\n";
        std::cout << "    Potential: " << bcval << " V\n";
        break;
      case 1:
        std::cout << "    Floating conductor\n";
        break;
      case 2:
        std::cout << "    Dielectric-dielectric interface\n";
        std::cout << "    Lambda: " << lambda << "\n";
        break;
      case 3:
        std::cout << "    Surface charge\n";
        break;
      default:
        std::cout << "    Unknown boundary condition (program bug!)\n";
    }
  }

  m_ready = false;
  matrixInversionFlag = false;
}

void ComponentNeBem2d::AddWire(const double x0, const double y0, const double d,
                               const double bcval) {

  if (d < Small) {
    std::cerr << m_className << "::AddWire:\n";
    std::cerr << "    Wire diameter must be greater than zero.\n";
    return;
  }

  wire newWire;
  newWire.cX = x0;
  newWire.cY = y0;
  newWire.d = d;
  newWire.bcValue = bcval;
  wires.push_back(newWire);
  ++nWires;

  if (m_debug) {
    std::cout << m_className << "::AddWire:\n";
    std::cout << "    Center: (" << x0 << ", " << y0 << ")\n";
    std::cout << "    Diameter: " << d << " cm\n";
    std::cout << "    Potential: " << bcval << " V\n";
  }

  m_ready = false;
  matrixInversionFlag = false;
}

void ComponentNeBem2d::SetNumberOfDivisions(const int ndiv) {

  if (ndiv <= 0) {
    std::cerr << m_className << "::SetNumberOfDivisions:\n";
    std::cerr << "    Number of divisions must be greater than zero.\n";
    return;
  }

  nDivisions = ndiv;
  m_ready = false;
  matrixInversionFlag = false;
}

void ComponentNeBem2d::SetNumberOfCollocationPoints(const int ncoll) {

  if (ncoll <= 0) {
    std::cerr << m_className << "::SetNumberOfCollocationPoints:\n";
    std::cerr << "    Number of coll. points must be greater than zero.\n";
    return;
  }

  nCollocationPoints = ncoll;
  m_ready = false;
  matrixInversionFlag = false;
}

void ComponentNeBem2d::SetMinimumElementSize(const double min) {

  if (min < Small) {
    std::cerr << m_className << "::SetMinimumElementSize:\n";
    std::cerr << "    Provided element size is too small.\n";
    return;
  }

  minSize = min;
  m_ready = false;
  matrixInversionFlag = false;
}

void ComponentNeBem2d::SetMaxNumberOfIterations(const int niter) {

  if (niter <= 0) {
    std::cerr << m_className << "::SetMaxNumberOfIterations:\n";
    std::cerr << "    Number of iterations must be greater than zero.\n";
    return;
  }

  nMaxIterations = niter;
}

bool ComponentNeBem2d::Initialise() {

  // Break up panels into elements
  if (!Discretise()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Discretisation failed.\n";
    return false;
  }
  if (m_debug) {
    std::cout << m_className << "::Initialise:\n";
    std::cout << "    Discretisation ok.\n";
  }

  bool converged = false;
  int nIter = 0;
  while (!converged) {
    ++nIter;
    if (autoSize) {
      std::cout << m_className << "::Initialise:\n";
      std::cout << "    Iteration " << nIter << "\n";
    }
    // Compute the influence matrix
    if (!ComputeInfluenceMatrix()) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "     Error computing the influence matrix.\n";
      return false;
    }

    // Invert the influence matrix
    if (!InvertMatrix()) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "     Error inverting the influence matrix.\n";
      return false;
    }

    if (m_debug) {
      std::cout << m_className << "::Initialise:\n";
      std::cout << "    Matrix inversion ok.\n";
    }

    // Compute the right hand side vector
    if (!GetBoundaryConditions()) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "     Error computing the potential vector.\n";
      return false;
    }

    // Solve for the charge distribution
    if (!Solve()) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error in Solve function.\n";
      return false;
    }
    if (m_debug) {
      std::cout << m_className << "::Initialise:\n";
      std::cout << "    Solution ok.\n";
    }

    converged = CheckConvergence();
    if (!autoSize) break;
    if (nIter >= nMaxIterations) break;
  }

  return true;
}

bool ComponentNeBem2d::Discretise() {

  elements.clear();
  nElements = 0;
  element newElement;

  if (m_debug) {
    std::cout << m_className << "::Discretise:\n";
    std::cout << "  Panel  BC Type  Bc Value  Rotation  Length\n";
  }
  newElement.geoType = 0;
  double dx = 0., dy = 0.;
  for (int j = nPanels; j--;) {
    newElement.bcType = panels[j].bcType;
    newElement.bcValue = panels[j].bcValue;
    newElement.lambda = panels[j].lambda;
    dx = panels[j].x1 - panels[j].x0;
    dy = panels[j].y1 - panels[j].y0;
    newElement.phi = atan2(dy, dx);
    newElement.len = 0.5 * sqrt(dx * dx + dy * dy) / nDivisions;
    dx /= nDivisions;
    dy /= nDivisions;
    newElement.cX = panels[j].x0 - 0.5 * dx;
    newElement.cY = panels[j].y0 - 0.5 * dy;
    for (int i = 0; i < nDivisions; ++i) {
      newElement.cX += dx;
      newElement.cY += dy;
      elements.push_back(newElement);
      ++nElements;
      if (m_debug) {
        std::cout << "  " << j << "  " << newElement.bcType << "  "
                  << newElement.bcValue << "  " << newElement.phi << "  "
                  << newElement.len << "\n";
      }
    }
  }

  newElement.geoType = 1;
  newElement.phi = 0.;
  // Treat wires always as conductors
  newElement.bcType = 0;
  newElement.lambda = 1.;
  for (int j = nWires; j--;) {
    newElement.cX = wires[j].cX;
    newElement.cY = wires[j].cY;
    newElement.len = wires[j].d / 2.;
    newElement.bcValue = wires[j].bcValue;
    elements.push_back(newElement);
    ++nElements;
  }

  return true;
}

bool ComponentNeBem2d::ComputeInfluenceMatrix() {

  if (matrixInversionFlag) return true;

  // Coordinates, rotation and length of target and source element
  double xF, yF, phiF;
  double xS, yS, phiS, lenS;
  // Geometric type
  int gtS;
  // Boundary type of target element
  int etF;

  // Global and local distance
  double dx, dy;
  double du, dv;
  // Field components in local and global coordinates
  double fx, fy;
  double ex, ey;

  // Influence coefficient
  double infCoeff;

  // Re-dimension the influence matrix
  int nEntries = nElements + 1;
  influenceMatrix.resize(nEntries);
  for (int i = nEntries; i--;) influenceMatrix[i].resize(nEntries);

  // Loop over the target elements (F)
  for (int iF = 0; iF < nElements; ++iF) {
    phiF = elements[iF].phi;
    // Boundary type
    etF = elements[iF].bcType;
    // Collocation point
    xF = elements[iF].cX;
    yF = elements[iF].cY;

    // Loop over the source elements (S)
    for (int jS = 0; jS < nElements; ++jS) {
      gtS = elements[jS].geoType;
      xS = elements[jS].cX;
      yS = elements[jS].cY;
      phiS = elements[jS].phi;
      lenS = elements[jS].len;
      // Transform to local coordinate system of source element
      dx = xF - xS;
      dy = yF - yS;
      Rotate(dx, dy, phiS, Global2Local, du, dv);
      infCoeff = 0.;
      // Depending on the element type at the field point
      // different boundary conditions need to be applied
      switch (etF) {
        // Conductor at fixed potential
        case 0:
          if (!ComputePotential(gtS, lenS, du, dv, infCoeff)) {
            return false;
          }
          break;
        // Floating conductor (not implemented)
        case 1:
          if (!ComputePotential(gtS, lenS, du, dv, infCoeff)) {
            return false;
          }
          break;
        // Dielectric-dielectric interface
        // Normal component of the displacement vector is continuous
        case 2:
          if (iF == jS) {
            // Self-influence
            infCoeff = 1. / (2. * elements[jS].lambda * VacuumPermittivity);
          } else {
            // Compute flux at field point in global coordinate system
            if (!ComputeFlux(gtS, lenS, phiS, du, dv, fx, fy)) {
              return false;
            }
            // Rotate to local coordinate system of field element
            Rotate(fx, fy, phiF, Global2Local, ex, ey);
            infCoeff = ey;
          }
          break;
        default:
          std::cerr << m_className << "::ComputeInfluenceMatrix:\n";
          std::cerr << "    Unknown boundary type: " << etF << ".\n";
          return false;
          break;
      }
      influenceMatrix[iF][jS] = infCoeff;
    }
  }

  // Add charge neutrality condition
  for (int i = 0; i < nElements; ++i) {
    influenceMatrix[nElements][i] = elements[i].len;
    influenceMatrix[i][nElements] = 0.;
  }
  influenceMatrix[nElements][nElements] = 0.;

  return true;
}

void ComponentNeBem2d::SplitElement(const int iel) {

  // Make sure the element is a line
  if (elements[iel].geoType != 0) return;

  double phi = elements[iel].phi;
  double len = elements[iel].len / 2.;
  elements[iel].len = len;
  double x0 = elements[iel].cX;
  double y0 = elements[iel].cY;

  double dx = 0., dy = 0.;
  Rotate(len, 0., phi, Local2Global, dx, dy);

  element newElement;
  newElement.geoType = elements[iel].geoType;
  newElement.len = elements[iel].len;
  newElement.phi = elements[iel].phi;
  newElement.bcType = elements[iel].bcType;
  newElement.bcValue = elements[iel].bcValue;
  newElement.lambda = elements[iel].lambda;

  elements[iel].cX = x0 + dx;
  elements[iel].cY = y0 + dy;
  newElement.cX = x0 - dx;
  newElement.cY = y0 - dx;

  elements.push_back(newElement);
  ++nElements;
}

bool ComponentNeBem2d::InvertMatrix() {

  // Check if matrix inversion has already been done
  if (matrixInversionFlag) return true;

  const int nEntries = nElements + 1;

  // Initialise the inverse influence matrix
  inverseMatrix.resize(nEntries);
  for (int i = nEntries; i--;) inverseMatrix[i].resize(nEntries);

  // Initialise temporary arrays for LU decomposition/substitution
  col.resize(nEntries);
  index.resize(nEntries);

  // Decompose the influence matrix
  if (!LUDecomposition()) {
    std::cerr << m_className << "::InvertMatrix:\n";
    std::cerr << "    LU Decomposition failed.\n";
    return false;
  }

  // Invert the matrix
  for (int j = 0; j < nEntries; ++j) {
    for (int i = 0; i < nEntries; ++i) col[i] = 0.;
    col[j] = 1.;
    LUSubstitution();
    for (int i = 0; i < nEntries; ++i) inverseMatrix[i][j] = col[i];
  }

  // Clear the influence matrix and the temporary arrays
  influenceMatrix.clear();
  col.clear();
  index.clear();

  // Set flag that the matrix has been inverted
  matrixInversionFlag = true;

  return true;
}

bool ComponentNeBem2d::LUDecomposition() {

  // The influence matrix is replaced by the LU decomposition of a rowwise
  // permutation of itself.
  // The implementation is based on:
  // W. H. Press,
  // Numerical recipes in C++: the Art of Scientific Computing (version 2.11)

  const int n = nElements;

  // v stores the implicit scaling of each row
  std::vector<double> v;
  v.resize(n);

  int i, j, k;
  // Loop over rows to get the implicit scaling information.
  double big = 0., temp = 0.;
  for (i = 0; i < n; ++i) {
    big = 0.;
    for (j = 0; j < n; ++j) {
      temp = fabs(influenceMatrix[i][j]);
      if (temp > big) big = temp;
    }
    if (big == 0.) return false;
    // Save the scaling
    v[i] = 1. / big;
  }

  // Loop over columns
  double sum = 0., dum = 0.;
  int imax = 0;
  for (j = 0; j < n; ++j) {
    for (i = 0; i < j; ++i) {
      sum = influenceMatrix[i][j];
      for (k = 0; k < i; ++k)
        sum -= influenceMatrix[i][k] * influenceMatrix[k][j];
      influenceMatrix[i][j] = sum;
    }
    // Initialise for the search for the largest pivot element
    big = 0.;
    for (i = j; i < n; ++i) {
      sum = influenceMatrix[i][j];
      for (k = 0; k < j; ++k)
        sum -= influenceMatrix[i][k] * influenceMatrix[k][j];
      influenceMatrix[i][j] = sum;
      // Is the figure of merit for the pivot better than the best so far?
      dum = v[i] * fabs(sum);
      if (dum >= big) {
        big = dum;
        imax = i;
      }
    }
    // Do we need to interchange rows?
    if (j != imax) {
      for (k = 0; k < n; ++k) {
        dum = influenceMatrix[imax][k];
        influenceMatrix[imax][k] = influenceMatrix[j][k];
        influenceMatrix[j][k] = dum;
      }
      // Interchange the scale factor
      v[imax] = v[j];
    }
    index[j] = imax;
    if (influenceMatrix[j][j] == 0.) influenceMatrix[j][j] = Small;
    if (j != n - 1) {
      // Divide by the pivot element
      dum = 1. / (influenceMatrix[j][j]);
      for (i = j + 1; i < n; ++i) influenceMatrix[i][j] *= dum;
    }
  }

  return true;
}

void ComponentNeBem2d::LUSubstitution() {

  const int n = nElements;

  double sum = 0.;
  int i, j;
  int ii = 0, ip = 0;

  // Forward substitution
  for (i = 0; i < n; ++i) {
    ip = index[i];
    sum = col[ip];
    col[ip] = col[i];
    if (ii != 0) {
      for (j = ii - 1; j < i; ++j) sum -= influenceMatrix[i][j] * col[j];
    } else if (sum != 0.) {
      ii = i + 1;
    }
    col[i] = sum;
  }

  // Backsubstitution
  for (i = n - 1; i >= 0; i--) {
    sum = col[i];
    for (j = i + 1; j < n; ++j) sum -= influenceMatrix[i][j] * col[j];
    col[i] = sum / influenceMatrix[i][i];
  }
}

bool ComponentNeBem2d::GetBoundaryConditions() {

  const int nEntries = nElements + 1;

  // Initialise the right-hand side vector
  boundaryConditions.resize(nEntries);

  for (int i = nElements; i--;) {
    switch (elements[i].bcType) {
      // Conductor at fixed potential
      case 0:
        boundaryConditions[i] = elements[i].bcValue;
        break;
      // Floating conductor
      case 1:
        boundaryConditions[i] = 0.;
        break;
      // Dielectric
      case 2:
        boundaryConditions[i] = 0.;
        break;
      // Other cases should not occur
      default:
        boundaryConditions[i] = 0.;
        return false;
    }
  }
  boundaryConditions[nElements] = 0.;

  return true;
}

bool ComponentNeBem2d::Solve() {

  const int nEntries = nElements + 1;

  int i, j;
  double solution = 0.;
  for (i = nElements; i--;) {
    solution = 0.;
    for (j = nEntries; j--;) {
      solution += inverseMatrix[i][j] * boundaryConditions[j];
    }
    elements[i].solution = solution;
  }

  if (m_debug) {
    std::cout << m_className << "::Solve:\n";
    std::cout << "  Element  Solution\n";
    for (i = 0; i < nElements; ++i) {
      std::cout << "  " << i << "  " << elements[i].solution << "\n";
    }
  }

  return true;
}

bool ComponentNeBem2d::CheckConvergence() {

  // Potential and normal component of the electric field
  // evaluated at the collocation points
  std::vector<double> v;
  std::vector<double> ne;
  v.resize(nCollocationPoints);
  ne.resize(nCollocationPoints);

  double ex = 0., ey = 0.;
  double fx = 0., fy = 0., u = 0.;
  double r;

  double x = 0., y = 0.;
  double dx = 0., dy = 0.;
  double xLoc, yLoc;

  if (m_debug) {
    std::cout << m_className << "::CheckConvergence:\n";
    std::cout << "element #  type      LHS      RHS\n";
  }
  for (int i = nElements; i--;) {
    for (int k = nCollocationPoints; k--;) v[k] = ne[k] = 0.;
    // Sum up the contributions from all boundary elements
    for (int j = nElements; j--;) {
      // Loop over the collocation points
      for (int k = nCollocationPoints; k--;) {
        x = elements[i].cX;
        y = elements[i].cY;
        if (elements[i].geoType == 0) {
          // Panel
          Rotate(2. * elements[i].len, 0., elements[i].phi, Local2Global, dx,
                 dy);
          x -= dx / 2.;
          y -= dy / 2.;
          if (randomCollocation) {
            r = RndmUniformPos();
          } else {
            r = (k + 1.) / (nCollocationPoints + 1.);
          }
          x += r * dx;
          y += r * dy;
        } else {
          // Wire
          r = TwoPi * RndmUniform();
          x += elements[i].len * cos(r);
          y += elements[i].len * sin(r);
        }

        dx = x - elements[j].cX;
        dy = y - elements[j].cY;
        // Transform to local coordinate system
        Rotate(dx, dy, elements[j].phi, Global2Local, xLoc, yLoc);
        // Compute the potential
        ComputePotential(elements[j].geoType, elements[j].len, xLoc, yLoc, u);
        // Compute the field
        ComputeFlux(elements[j].geoType, elements[j].len, elements[j].phi, xLoc,
                    yLoc, fx, fy);
        // Rotate to the local coordinate system of the test element
        Rotate(fx, fy, elements[i].phi, Global2Local, ex, ey);
        v[k] += u * elements[j].solution;
        ne[k] += ey * elements[j].solution;
      }
    }
    double v0 = 0., ne0 = 0.;
    for (int k = nCollocationPoints; k--;) {
      v0 += v[k];
      ne0 += ne[k];
    }
    v0 /= nCollocationPoints;
    ne0 /= nCollocationPoints;
    double ne1 = 0.;
    if (elements[i].bcType == 2) {
      // Dielectric-dielectric interface
      ne1 = ne0 + elements[i].solution /
                      (2. * elements[i].lambda * VacuumPermittivity);
    }
    if (m_debug) {
      if (elements[i].bcType == 0) {
        std::cout << std::setw(5) << i << "  cond.   " << std::setw(10) << v0
                  << "  " << std::setw(10) << elements[i].bcValue << "\n";
      } else if (elements[i].bcType == 2) {
        std::cout << std::setw(5) << i << "  diel.   " << std::setw(10) << ne0
                  << "  " << ne1 << "         0\n";
      }
    }
  }

  return true;
}

void ComponentNeBem2d::Rotate(const double xIn, const double yIn,
                              const double phi, const int opt, double& xOut,
                              double& yOut) {

  // Rotation angle represents clockwise rotation about the origin
  // Transformation to local coordinates (opt = 1): clockwise rotation
  // Transformation to global coordinates (opt = -1): anti-clockwise rotation

  if (fabs(phi) < 1.e-12) {
    xOut = xIn;
    yOut = yIn;
    return;
  }

  const double c = cos(phi);
  const double s = opt * sin(phi);
  xOut = c * xIn + s * yIn;
  yOut = -s * xIn + c * yIn;
}

double ComponentNeBem2d::LinePotential(const double a, const double x,
                                       const double y) {

  double p = 0.;
  const double amx = a - x;
  const double apx = a + x;
  if (fabs(y) > Small) {
    const double y2 = y * y;
    p = 2. * a - y * (atan(amx / y) + atan(apx / y)) -
        0.5 * amx * log(amx * amx + y2) - 0.5 * apx * log(apx * apx + y2);
  } else if (fabs(x) != a) {
    p = 2. * a - 0.5 * amx * log(amx * amx) - 0.5 * apx * log(apx * apx);
  } else {
    p = 2. * a * (1. - log(2. * a));
  }

  return p / TwoPiEpsilon0;
}

double ComponentNeBem2d::WirePotential(const double r0, const double x,
                                       const double y) {

  const double r = sqrt(x * x + y * y);
  if (r >= r0) {
    return -log(r) * r0 / VacuumPermittivity;
  }

  // Inside the wire the potential is constant
  return -log(r0) * r0 / VacuumPermittivity;
}

void ComponentNeBem2d::LineFlux(const double a, const double x, const double y,
                                double& ex, double& ey) {

  const double amx = a - x;
  const double apx = a + x;
  if (fabs(y) > 0.) {
    const double y2 = y * y;
    ex = 0.5 * log((apx * apx + y2) / (amx * amx + y2));
    ey = atan(amx / y) + atan(apx / y);
  } else if (fabs(x) != a) {
    ex = 0.5 * log(apx * apx / (amx * amx));
    ey = 0.;
  } else {
    // Singularity at the end points of the line
    const double eps = 1.e-12;
    ex = 0.25 *
         log(pow(apx * apx - eps * eps, 2) / pow(amx * amx - eps * eps, 2));
    ey = 0.;
  }

  ex /= TwoPiEpsilon0;
  ey /= TwoPiEpsilon0;
}

void ComponentNeBem2d::WireFlux(const double r0, const double x, const double y,
                                double& ex, double& ey) {

  const double r = sqrt(x * x + y * y);
  if (r > r0) {
    const double r2 = r * r;
    ex = x * r0 / r2;
    ey = y * r0 / r2;
  } else if (r == r0) {
    ex = 0.5 * x / r0;
    ey = 0.5 * y / r0;
  } else {
    // Inside the wire the field is zero.
    ex = ey = 0.;
  }

  ex = ex / VacuumPermittivity;
  ey = ey / VacuumPermittivity;
}

void ComponentNeBem2d::Reset() {

  panels.clear();
  nPanels = 0;

  wires.clear();
  nWires = 0;

  elements.clear();
  nElements = 0;
}

void ComponentNeBem2d::UpdatePeriodicity() {

  std::cerr << m_className << "::UpdatePeriodicity:\n";
  std::cerr << "    Periodicities are not supported.\n";
}
}
