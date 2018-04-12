#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <numeric>

#include "ComponentNeBem2d.hh"
#include "Random.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

const double ComponentNeBem2d::InvEpsilon0 = 1. / VacuumPermittivity;
const double ComponentNeBem2d::InvTwoPiEpsilon0 = 1. / TwoPiEpsilon0;

ComponentNeBem2d::ComponentNeBem2d() : ComponentBase() {

  m_className = "ComponentNeBem2d";
}

void ComponentNeBem2d::ElectricField(const double x, const double y,
                                     const double z, double& ex, double& ey,
                                     double& ez, double& v, Medium*& m,
                                     int& status) {

  ex = ey = ez = v = 0.;
  status = 0;
  // Check if the requested point is inside a medium
  m = GetMedium(x, y, z);
  if (!m) {
    status = -6;
    return;
  }

  if (!m_ready) {
    if (!Initialise()) {
      std::cerr << m_className << "::ElectricField: Initialisation failed.\n";
      status = -11;
      return;
    }
    m_ready = true;
  }

  // Sum up the contributions from all boundary elements
  for (const auto& element : m_elements) {
    const double phi = element.phi;
    const double dx = x - element.cX;
    const double dy = y - element.cY;
    // Transform to local coordinate system
    double xLoc = 0., yLoc = 0.;
    Rotate(dx, dy, phi, Global2Local, xLoc, yLoc);
    // Compute the potential
    double u = 0.;
    if (!ComputePotential(element.geoType, element.len, xLoc, yLoc, u)) {
      std::cerr << m_className << "::ElectricField:\n"
                << "    Cannot calculate potential from element at " 
                << element.cX << ", " << element.cY << ".\n"; 
      status = -11;
      return;
    }
    // Compute the field
    double fx = 0., fy = 0.;
    if (!ComputeFlux(element.geoType, element.len, phi, xLoc, yLoc, fx, fy)) {
      std::cerr << m_className << "::ElectricField:\n"
                << "    Cannot calculate field from element at " 
                << element.cX << ", " << element.cY << ".\n"; 
      status = -11;
      return;
    }
    v += u * element.solution;
    ex += fx * element.solution;
    ey += fy * element.solution;
  }
}

void ComponentNeBem2d::ElectricField(const double x, const double y,
                                     const double z, double& ex, double& ey,
                                     double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

bool ComponentNeBem2d::GetVoltageRange(double& vmin, double& vmax) {

  if (m_panels.empty() && m_wires.empty()) return false;
  bool gotValue = false;
  for (const auto& panel : m_panels) {
    if (panel.bcType != 0) continue;
    if (!gotValue) {
      vmin = vmax = panel.bcValue;
      gotValue = true;
    } else {
      vmin = std::min(vmin, panel.bcValue);
      vmax = std::max(vmax, panel.bcValue);
    }
  }

  for (const auto& wire : m_wires) {
    if (!gotValue) {
      vmin = vmax = wire.bcValue;
      gotValue = true;
    } else {
      vmin = std::min(vmin, wire.bcValue);
      vmax = std::max(vmax, wire.bcValue);
    }
  }
  return gotValue;
}

void ComponentNeBem2d::AddPanel(const double x0, const double y0,
                                const double x1, const double y1,
                                const unsigned int bctype, const double bcval,
                                const double lambda) {

  const double dx = x1 - x0;
  const double dy = y1 - y0;
  if (dx * dx + dy * dy < Small) {
    std::cerr << m_className << "::AddPanel:\n"
              << "    Panel length must be greater than zero.\n";
    return;
  }
  if (bctype > 3) {
    std::cerr << m_className << "::AddPanel:\n"
              << "    Unknown boundary condition type: " << bctype << "\n";
    return;
  }

  Panel newPanel;
  newPanel.x0 = x0;
  newPanel.y0 = y0;
  newPanel.x1 = x1;
  newPanel.y1 = y1;
  newPanel.bcType = bctype;
  newPanel.bcValue = bcval;
  newPanel.lambda = lambda;
  m_panels.push_back(std::move(newPanel));

  if (m_debug) {
    std::cout << m_className << "::AddPanel:\n"
              << "    From: (" << x0 << ", " << y0 << ")\n"
              << "    To:   (" << x1 << ", " << y1 << ")\n";
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
  m_matrixInversionFlag = false;
}

void ComponentNeBem2d::AddWire(const double x0, const double y0, const double d,
                               const double bcval) {

  if (d < Small) {
    std::cerr << m_className << "::AddWire:\n"
              << "    Wire diameter must be greater than zero.\n";
    return;
  }

  Wire newWire;
  newWire.cX = x0;
  newWire.cY = y0;
  newWire.d = d;
  newWire.bcValue = bcval;
  m_wires.push_back(std::move(newWire));

  if (m_debug) {
    std::cout << m_className << "::AddWire:\n"
              << "    Center: (" << x0 << ", " << y0 << ")\n"
              << "    Diameter: " << d << " cm\n"
              << "    Potential: " << bcval << " V\n";
  }

  m_ready = false;
  m_matrixInversionFlag = false;
}

void ComponentNeBem2d::SetNumberOfDivisions(const unsigned int ndiv) {

  if (ndiv == 0) {
    std::cerr << m_className << "::SetNumberOfDivisions:\n"
              << "    Number of divisions must be greater than zero.\n";
    return;
  }

  m_nDivisions = ndiv;
  m_ready = false;
  m_matrixInversionFlag = false;
}

void ComponentNeBem2d::SetNumberOfCollocationPoints(const unsigned int ncoll) {

  if (ncoll == 0) {
    std::cerr << m_className << "::SetNumberOfCollocationPoints:\n"
              << "    Number of coll. points must be greater than zero.\n";
    return;
  }

  m_nCollocationPoints = ncoll;
  m_ready = false;
  m_matrixInversionFlag = false;
}

void ComponentNeBem2d::SetMinimumElementSize(const double minsize) {

  if (minsize < Small) {
    std::cerr << m_className << "::SetMinimumElementSize:\n"
              << "    Provided element size is too small.\n";
    return;
  }

  m_minSize = minsize;
  m_ready = false;
  m_matrixInversionFlag = false;
}

void ComponentNeBem2d::SetMaxNumberOfIterations(const unsigned int niter) {

  if (niter == 0) {
    std::cerr << m_className << "::SetMaxNumberOfIterations:\n"
              << "    Number of iterations must be greater than zero.\n";
    return;
  }

  m_nMaxIterations = niter;
}

bool ComponentNeBem2d::Initialise() {

  // Break up panels into elements
  if (!Discretise()) {
    std::cerr << m_className << "::Initialise: Discretisation failed.\n";
    return false;
  }
  if (m_debug) {
    std::cout << m_className << "::Initialise: Discretisation ok.\n";
  }

  const unsigned int nEntries = m_elements.size() + 1;
  std::vector<std::vector<double> > influenceMatrix(nEntries, std::vector<double>(nEntries, 0.)); 
  std::vector<std::vector<double> > inverseMatrix(nEntries, std::vector<double>(nEntries, 0.)); 

  bool converged = false;
  unsigned int nIter = 0;
  while (!converged) {
    ++nIter;
    if (m_autoSize) {
      std::cout << m_className << "::Initialise: Iteration " << nIter << "\n";
    }
    // Compute the influence matrix.
    influenceMatrix.assign(nEntries, std::vector<double>(nEntries, 0.));
    if (!ComputeInfluenceMatrix(influenceMatrix)) {
      std::cerr << m_className << "::Initialise:\n"
                << "     Error computing the influence matrix.\n";
      return false;
    }

    // Invert the influence matrix.
    inverseMatrix.assign(nEntries, std::vector<double>(nEntries, 0.));
    if (!InvertMatrix(influenceMatrix, inverseMatrix)) {
      std::cerr << m_className << "::Initialise:\n"
                << "     Error inverting the influence matrix.\n";
      return false;
    }
    // Set flag that the matrix has been inverted.
    m_matrixInversionFlag = true;

    if (m_debug) {
      std::cout << m_className << "::Initialise: Matrix inversion ok.\n";
    }

    // Compute the right hand side vector (boundary conditions).
    std::vector<double> boundaryConditions(nEntries, 0.);
    if (!GetBoundaryConditions(boundaryConditions)) {
      std::cerr << m_className << "::Initialise:\n"
                << "     Error computing the potential vector.\n";
      return false;
    }

    // Solve for the charge distribution.
    if (!Solve(inverseMatrix, boundaryConditions)) {
      std::cerr << m_className << "::Initialise: Error in Solve function.\n";
      return false;
    }
    if (m_debug) std::cout << m_className << "::Initialise: Solution ok.\n";
    converged = CheckConvergence();
    if (!m_autoSize) break;
    if (nIter >= m_nMaxIterations) break;
  }

  return true;
}

bool ComponentNeBem2d::Discretise() {

  m_elements.clear();

  if (m_debug) {
    std::cout << m_className << "::Discretise:\n";
    std::cout << "  Panel  BC Type  Bc Value  Rotation  Length\n";
  }
  unsigned int j = 0;
  for (const auto& panel : m_panels) {
    Element newElement;
    newElement.geoType = 0;
    newElement.bcType = panel.bcType;
    newElement.bcValue = panel.bcValue;
    newElement.lambda = panel.lambda;
    double dx = panel.x1 - panel.x0;
    double dy = panel.y1 - panel.y0;
    newElement.phi = atan2(dy, dx);
    newElement.len = 0.5 * sqrt(dx * dx + dy * dy) / m_nDivisions;
    dx /= m_nDivisions;
    dy /= m_nDivisions;
    newElement.cX = panel.x0 - 0.5 * dx;
    newElement.cY = panel.y0 - 0.5 * dy;
    for (unsigned int i = 0; i < m_nDivisions; ++i) {
      newElement.cX += dx;
      newElement.cY += dy;
      m_elements.push_back(newElement);
      if (m_debug) {
        std::cout << "  " << j << "  " << newElement.bcType << "  "
                  << newElement.bcValue << "  " << newElement.phi << "  "
                  << newElement.len << "\n";
      }
    }
    ++j;
  }
  for (const auto& wire : m_wires) {
    Element newElement;
    newElement.geoType = 1;
    newElement.phi = 0.;
    // Treat wires always as conductors
    newElement.bcType = 0;
    newElement.lambda = 1.;
    newElement.cX = wire.cX;
    newElement.cY = wire.cY;
    newElement.len = wire.d / 2.;
    newElement.bcValue = wire.bcValue;
    m_elements.push_back(std::move(newElement));
  }

  return true;
}

bool ComponentNeBem2d::ComputeInfluenceMatrix(
    std::vector<std::vector<double> >& infmat) const {

  if (m_matrixInversionFlag) return true;

  // Loop over the target elements (F)
  const unsigned int nElements = m_elements.size();
  for (unsigned int iF = 0; iF < nElements; ++iF) {
    const double phiF = m_elements[iF].phi;
    // Boundary type
    const unsigned int etF = m_elements[iF].bcType;
    // Collocation point
    const double xF = m_elements[iF].cX;
    const double yF = m_elements[iF].cY;

    // Loop over the source elements (S)
    for (unsigned int jS = 0; jS < nElements; ++jS) {
      const int gtS = m_elements[jS].geoType;
      const double xS = m_elements[jS].cX;
      const double yS = m_elements[jS].cY;
      const double phiS = m_elements[jS].phi;
      const double lenS = m_elements[jS].len;
      // Transform to local coordinate system of source element
      const double dx = xF - xS;
      const double dy = yF - yS;
      double du = 0.;
      double dv = 0.;
      Rotate(dx, dy, phiS, Global2Local, du, dv);
      // Influence coefficient
      double infCoeff = 0.;
      // Depending on the element type at the field point
      // different boundary conditions need to be applied
      switch (etF) {
        // Conductor at fixed potential
        case 0:
          if (!ComputePotential(gtS, lenS, du, dv, infCoeff)) return false;
          break;
        // Floating conductor (not implemented)
        case 1:
          if (!ComputePotential(gtS, lenS, du, dv, infCoeff)) return false;
          break;
        // Dielectric-dielectric interface
        // Normal component of the displacement vector is continuous
        case 2:
          if (iF == jS) {
            // Self-influence
            infCoeff = 1. / (2. * m_elements[jS].lambda * VacuumPermittivity);
          } else {
            // Compute flux at field point in global coordinate system
            double fx = 0., fy = 0.;
            if (!ComputeFlux(gtS, lenS, phiS, du, dv, fx, fy)) {
              return false;
            }
            // Rotate to local coordinate system of field element
            double ex = 0., ey = 0.;
            Rotate(fx, fy, phiF, Global2Local, ex, ey);
            infCoeff = ey;
          }
          break;
        default:
          std::cerr << m_className << "::ComputeInfluenceMatrix:\n"
                    << "    Unknown boundary type: " << etF << ".\n";
          return false;
          break;
      }
      infmat[iF][jS] = infCoeff;
    }
  }

  // Add charge neutrality condition
  for (unsigned int i = 0; i < nElements; ++i) {
    infmat[nElements][i] = m_elements[i].len;
    infmat[i][nElements] = 0.;
  }
  infmat[nElements][nElements] = 0.;

  return true;
}

void ComponentNeBem2d::SplitElement(const unsigned int iel) {

  // Make sure the element is a line
  if (m_elements[iel].geoType != 0) return;

  const double phi = m_elements[iel].phi;
  const double len = 0.5 * m_elements[iel].len;
  m_elements[iel].len = len;

  double dx = 0., dy = 0.;
  Rotate(len, 0., phi, Local2Global, dx, dy);

  Element newElement;
  newElement.geoType = m_elements[iel].geoType;
  newElement.len = len;
  newElement.phi = phi;
  newElement.bcType = m_elements[iel].bcType;
  newElement.bcValue = m_elements[iel].bcValue;
  newElement.lambda = m_elements[iel].lambda;

  const double x0 = m_elements[iel].cX;
  const double y0 = m_elements[iel].cY;
  m_elements[iel].cX = x0 + dx;
  m_elements[iel].cY = y0 + dy;
  newElement.cX = x0 - dx;
  newElement.cY = y0 - dx;

  m_elements.push_back(std::move(newElement));
}

bool ComponentNeBem2d::InvertMatrix(
    std::vector<std::vector<double> >& influenceMatrix,
    std::vector<std::vector<double> >& inverseMatrix) const {

  // Check if matrix inversion has already been done
  if (m_matrixInversionFlag) return true;

  const unsigned int nEntries = m_elements.size() + 1;

  // Temporary arrays for LU decomposition/substitution
  std::vector<double> col(nEntries, 0.);
  std::vector<int> index(nEntries, 0);

  // Decompose the influence matrix
  if (!LUDecomposition(influenceMatrix, index)) {
    std::cerr << m_className << "::InvertMatrix: LU decomposition failed.\n";
    return false;
  }

  // Initialise the inverse influence matrix
  inverseMatrix.assign(nEntries, std::vector<double>(nEntries, 0.));
  // Invert the matrix.
  for (unsigned int j = 0; j < nEntries; ++j) {
    col.assign(nEntries, 0.);
    col[j] = 1.;
    LUSubstitution(influenceMatrix, index, col);
    for (unsigned int i = 0; i < nEntries; ++i) inverseMatrix[i][j] = col[i];
  }

  // Clear the influence matrix.
  influenceMatrix.clear();

  return true;
}

bool ComponentNeBem2d::LUDecomposition(std::vector<std::vector<double> >& mat,
                                       std::vector<int>& index) const {

  // The influence matrix is replaced by the LU decomposition of a rowwise
  // permutation of itself. The implementation is based on:
  // W. H. Press,
  // Numerical recipes in C++: the Art of Scientific Computing (version 2.11)

  const unsigned int n = m_elements.size();

  // v stores the implicit scaling of each row
  std::vector<double> v(n, 0.);

  // Loop over rows to get the implicit scaling information.
  for (unsigned int i = 0; i < n; ++i) {
    double big = 0.;
    for (unsigned int j = 0; j < n; ++j) {
      big = std::max(big, fabs(mat[i][j]));
    }
    if (big == 0.) return false;
    // Save the scaling
    v[i] = 1. / big;
  }

  // Loop over columns
  unsigned int imax = 0;
  for (unsigned int j = 0; j < n; ++j) {
    for (unsigned int i = 0; i < j; ++i) {
      double sum = mat[i][j];
      for (unsigned int k = 0; k < i; ++k) {
        sum -= mat[i][k] * mat[k][j];
      }
      mat[i][j] = sum;
    }
    // Initialise for the search for the largest pivot element
    double big = 0.;
    for (unsigned int i = j; i < n; ++i) {
      double sum = mat[i][j];
      for (unsigned int k = 0; k < j; ++k) {
        sum -= mat[i][k] * mat[k][j];
      }
      mat[i][j] = sum;
      // Is the figure of merit for the pivot better than the best so far?
      const double dum = v[i] * fabs(sum);
      if (dum >= big) {
        big = dum;
        imax = i;
      }
    }
    // Do we need to interchange rows?
    if (j != imax) {
      for (unsigned k = 0; k < n; ++k) {
        const double dum = mat[imax][k];
        mat[imax][k] = mat[j][k];
        mat[j][k] = dum;
      }
      // Interchange the scale factor
      v[imax] = v[j];
    }
    index[j] = imax;
    if (mat[j][j] == 0.) mat[j][j] = Small;
    if (j != n - 1) {
      // Divide by the pivot element
      const double dum = 1. / mat[j][j];
      for (unsigned int i = j + 1; i < n; ++i) {
        mat[i][j] *= dum;
      }
    }
  }

  return true;
}

void ComponentNeBem2d::LUSubstitution(const std::vector<std::vector<double> >& mat,
                                      const std::vector<int>& index,
                                      std::vector<double>& col) const {

  const unsigned int n = m_elements.size();

  unsigned int ii = 0;
  // Forward substitution
  for (unsigned i = 0; i < n; ++i) {
    const unsigned int ip = index[i];
    double sum = col[ip];
    col[ip] = col[i];
    if (ii != 0) {
      for (unsigned j = ii - 1; j < i; ++j) {
        sum -= mat[i][j] * col[j];
      }
    } else if (sum != 0.) {
      ii = i + 1;
    }
    col[i] = sum;
  }

  // Backsubstitution
  for (int i = n - 1; i >= 0; i--) {
    double sum = col[i];
    for (unsigned j = i + 1; j < n; ++j) {
      sum -= mat[i][j] * col[j];
    }
    col[i] = sum / mat[i][i];
  }
}

bool ComponentNeBem2d::GetBoundaryConditions(std::vector<double>& bc) const {

  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    if (m_elements[i].bcType > 2) {
      // Unknown type of boundary condition, should not occur.
      return false;
    }
    if (m_elements[i].bcType != 0) continue;
    // Conductor at fixed potential
    bc[i] = m_elements[i].bcValue;
  }
  return true;
}

bool ComponentNeBem2d::Solve(const std::vector<std::vector<double> >& invmat,
                             const std::vector<double>& bc) {

  const unsigned int nElements = m_elements.size();
  const unsigned int nEntries = bc.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    double solution = 0.;
    for (unsigned int j = 0; j < nEntries; ++j) {
      solution += invmat[i][j] * bc[j];
    }
    m_elements[i].solution = solution;
  }

  if (m_debug) {
    std::cout << m_className << "::Solve:\n  Element  Solution\n";
    for (unsigned int i = 0; i < nElements; ++i) {
      std::cout << "  " << i << "  " << m_elements[i].solution << "\n";
    }
  }

  return true;
}

bool ComponentNeBem2d::CheckConvergence() const {

  // Potential and normal component of the electric field
  // evaluated at the collocation points
  std::vector<double> v(m_nCollocationPoints, 0.);
  std::vector<double> ne(m_nCollocationPoints, 0.);

  double ex = 0., ey = 0.;
  double fx = 0., fy = 0., u = 0.;
  double r;

  double dx = 0., dy = 0.;
  double xLoc, yLoc;

  if (m_debug) {
    std::cout << m_className << "::CheckConvergence:\n";
    std::cout << "element #  type      LHS      RHS\n";
  }
  const double scale = 1. / m_nCollocationPoints;
  const unsigned int nElements = m_elements.size();
  unsigned int i = 0;
  for (const auto& element : m_elements) {
    v.assign(m_nCollocationPoints, 0.);
    ne.assign(m_nCollocationPoints, 0.);
 
    // Sum up the contributions from all boundary elements
    for (unsigned int j = 0; j < nElements; ++j) {
      // Loop over the collocation points
      for (unsigned int k = 0; k < m_nCollocationPoints; ++k) {
        double x = element.cX;
        double y = element.cY;
        if (element.geoType == 0) {
          // Panel
          Rotate(2. * element.len, 0., element.phi, Local2Global, dx, dy);
          x -= 0.5 * dx;
          y -= 0.5 * dy;
          if (m_randomCollocation) {
            r = RndmUniformPos();
          } else {
            r = (k + 1.) / (m_nCollocationPoints + 1.);
          }
          x += r * dx;
          y += r * dy;
        } else {
          // Wire
          r = TwoPi * RndmUniform();
          x += element.len * cos(r);
          y += element.len * sin(r);
        }

        dx = x - m_elements[j].cX;
        dy = y - m_elements[j].cY;
        // Transform to local coordinate system
        Rotate(dx, dy, m_elements[j].phi, Global2Local, xLoc, yLoc);
        // Compute the potential
        ComputePotential(m_elements[j].geoType, m_elements[j].len, xLoc, yLoc, u);
        // Compute the field
        ComputeFlux(m_elements[j].geoType, m_elements[j].len, m_elements[j].phi, xLoc,
                    yLoc, fx, fy);
        // Rotate to the local coordinate system of the test element
        Rotate(fx, fy, element.phi, Global2Local, ex, ey);
        v[k] += u * m_elements[j].solution;
        ne[k] += ey * m_elements[j].solution;
      }
    }
    const double v0 = scale * std::accumulate(v.begin(), v.end(), 0.);
    const double ne0 = scale * std::accumulate(ne.begin(), ne.end(), 0.);
    double ne1 = 0.;
    if (element.bcType == 2) {
      // Dielectric-dielectric interface
      ne1 = ne0 + 0.5 * InvEpsilon0 * element.solution / element.lambda;
    }
    if (m_debug) {
      if (element.bcType == 0) {
        std::cout << std::setw(5) << i << "  cond.   " << std::setw(10) << v0
                  << "  " << std::setw(10) << element.bcValue << "\n";
      } else if (element.bcType == 2) {
        std::cout << std::setw(5) << i << "  diel.   " << std::setw(10) << ne0
                  << "  " << ne1 << "         0\n";
      }
    }
    ++i;
  }

  return true;
}

void ComponentNeBem2d::Rotate(const double xIn, const double yIn,
                              const double phi, const int opt, double& xOut,
                              double& yOut) const {

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
                                       const double y) const {

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

  return InvTwoPiEpsilon0 * p;
}

double ComponentNeBem2d::WirePotential(const double r0, const double x,
                                       const double y) const {

  const double r = sqrt(x * x + y * y);
  if (r >= r0) {
    return -log(r) * r0 * InvEpsilon0;
  }

  // Inside the wire the potential is constant
  return -log(r0) * r0 * InvEpsilon0;
}

void ComponentNeBem2d::LineFlux(const double a, const double x, const double y,
                                double& ex, double& ey) const {

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
    const double eps2 = 1.e-24;
    ex = 0.25 * log(pow(apx * apx - eps2, 2) / pow(amx * amx - eps2, 2));
    ey = 0.;
  }
  ex *= InvTwoPiEpsilon0;
  ey *= InvTwoPiEpsilon0;
}

void ComponentNeBem2d::WireFlux(const double r0, const double x, const double y,
                                double& ex, double& ey) const {

  const double r02 = r0 * r0;
  const double r2 = x * x + y * y;
  if (r2 > r02) {
    ex = x * r0 / r2;
    ey = y * r0 / r2;
  } else if (r2 == r02) {
    ex = 0.5 * x / r0;
    ey = 0.5 * y / r0;
  } else {
    // Inside the wire the field is zero.
    ex = ey = 0.;
    return;
  }

  ex *= InvEpsilon0;
  ey *= InvEpsilon0;
}

void ComponentNeBem2d::Reset() {

  m_panels.clear();
  m_wires.clear();
  m_elements.clear();
}

void ComponentNeBem2d::UpdatePeriodicity() {

  std::cerr << m_className << "::UpdatePeriodicity:\n"
            << "    Periodicities are not supported.\n";
}
}
