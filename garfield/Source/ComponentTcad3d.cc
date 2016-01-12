#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <string>
#include <algorithm>

#include "ComponentTcad3d.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

ComponentTcad3d::ComponentTcad3d()
    : ComponentBase(),
      m_nRegions(0),
      m_nVertices(0),
      m_nElements(0),
      m_lastElement(0) {

  m_className = "ComponentTcad3d";

  m_regions.clear();
  m_vertices.clear();
  m_elements.clear();

  for (int i = nMaxVertices; i--;) m_w[i] = 0.;
}

void ComponentTcad3d::ElectricField(const double xin, const double yin,
                                    const double zin, double& ex, double& ey,
                                    double& ez, double& p, Medium*& m,
                                    int& status) {

  m = 0;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  // Initialise the electric field and potential.
  ex = ey = ez = p = 0.;

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xMirrored = false;
  const double cellsx = m_xMaxBoundingBox - m_xMinBoundingBox;
  if (m_xPeriodic) {
    x = m_xMinBoundingBox + fmod(x - m_xMinBoundingBox, cellsx);
    if (x < m_xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = m_xMinBoundingBox + fmod(x - m_xMinBoundingBox, cellsx);
    if (xNew < m_xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = m_xMinBoundingBox + m_xMaxBoundingBox - xNew;
      xMirrored = true;
    }
    x = xNew;
  }
  bool yMirrored = false;
  const double cellsy = m_yMaxBoundingBox - m_yMinBoundingBox;
  if (m_yPeriodic) {
    y = m_yMinBoundingBox + fmod(y - m_yMinBoundingBox, cellsy);
    if (y < m_yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = m_yMinBoundingBox + fmod(y - m_yMinBoundingBox, cellsy);
    if (yNew < m_yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = m_yMinBoundingBox + m_yMaxBoundingBox - yNew;
      yMirrored = true;
    }
    y = yNew;
  }
  bool zMirrored = false;
  const double cellsz = m_zMaxBoundingBox - m_zMinBoundingBox;
  if (m_zPeriodic) {
    z = m_zMinBoundingBox + fmod(z - m_zMinBoundingBox, cellsz);
    if (z < m_zMinBoundingBox) z += cellsz;
  } else if (m_zMirrorPeriodic) {
    double zNew = m_zMinBoundingBox + fmod(z - m_zMinBoundingBox, cellsz);
    if (zNew < m_zMinBoundingBox) zNew += cellsz;
    int nz = int(floor(0.5 + (zNew - z) / cellsz));
    if (nz != 2 * (nz / 2)) {
      zNew = m_zMinBoundingBox + m_zMaxBoundingBox - zNew;
      zMirrored = true;
    }
    z = zNew;
  }

  // Check if the point is inside the bounding box.
  if (x < m_xMinBoundingBox || x > m_xMaxBoundingBox || y < m_yMinBoundingBox ||
      y > m_yMaxBoundingBox || z < m_zMinBoundingBox || z > m_zMaxBoundingBox) {
    if (m_debug) {
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Point (" << x << ", " << y << ", " << z
                << ") is outside the bounding box.\n";
    }
    status = -11;
    return;
  }

  // Assume this will work.
  status = 0;
  // Check if the point is still located in the previously found element.
  int i = m_lastElement;
  switch (m_elements[i].type) {
    case 2:
      if (CheckTriangle(x, y, z, i)) {
        ex = m_w[0] * m_vertices[m_elements[i].vertex[0]].ex +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ex +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ex;
        ey = m_w[0] * m_vertices[m_elements[i].vertex[0]].ey +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ey +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ey;
        ez = m_w[0] * m_vertices[m_elements[i].vertex[0]].ez +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ez +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ez;
        p = m_w[0] * m_vertices[m_elements[i].vertex[0]].p +
            m_w[1] * m_vertices[m_elements[i].vertex[1]].p +
            m_w[2] * m_vertices[m_elements[i].vertex[2]].p;
        if (xMirrored) ex = -ex;
        if (yMirrored) ey = -ey;
        if (zMirrored) ez = -ez;
        m = m_regions[m_elements[i].region].medium;
        if (!m_regions[m_elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    case 5:
      if (CheckTetrahedron(x, y, z, i)) {
        ex = m_w[0] * m_vertices[m_elements[i].vertex[0]].ex +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ex +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ex +
             m_w[3] * m_vertices[m_elements[i].vertex[3]].ex;
        ey = m_w[0] * m_vertices[m_elements[i].vertex[0]].ey +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ey +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ey +
             m_w[3] * m_vertices[m_elements[i].vertex[3]].ey;
        ez = m_w[0] * m_vertices[m_elements[i].vertex[0]].ez +
             m_w[1] * m_vertices[m_elements[i].vertex[1]].ez +
             m_w[2] * m_vertices[m_elements[i].vertex[2]].ez +
             m_w[3] * m_vertices[m_elements[i].vertex[3]].ez;
        p = m_w[0] * m_vertices[m_elements[i].vertex[0]].p +
            m_w[1] * m_vertices[m_elements[i].vertex[1]].p +
            m_w[2] * m_vertices[m_elements[i].vertex[2]].p +
            m_w[3] * m_vertices[m_elements[i].vertex[3]].p;
        if (xMirrored) ex = -ex;
        if (yMirrored) ey = -ey;
        if (zMirrored) ez = -ez;
        m = m_regions[m_elements[i].region].medium;
        if (!m_regions[m_elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    default:
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Unknown element type (" << m_elements[i].type << ").\n";
      status = -11;
      return;
      break;
  }

  // The point is not in the previous element.
  // We have to loop over all elements.
  for (i = m_nElements; i--;) {
    switch (m_elements[i].type) {
      case 2:
        if (CheckTriangle(x, y, z, i)) {
          ex = m_w[0] * m_vertices[m_elements[i].vertex[0]].ex +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ex +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ex;
          ey = m_w[0] * m_vertices[m_elements[i].vertex[0]].ey +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ey +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ey;
          ez = m_w[0] * m_vertices[m_elements[i].vertex[0]].ez +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ez +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ez;
          p = m_w[0] * m_vertices[m_elements[i].vertex[0]].p +
              m_w[1] * m_vertices[m_elements[i].vertex[1]].p +
              m_w[2] * m_vertices[m_elements[i].vertex[2]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          if (zMirrored) ez = -ez;
          m_lastElement = i;
          m = m_regions[m_elements[i].region].medium;
          if (!m_regions[m_elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      case 5:
        if (CheckTetrahedron(x, y, z, i)) {
          ex = m_w[0] * m_vertices[m_elements[i].vertex[0]].ex +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ex +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ex +
               m_w[3] * m_vertices[m_elements[i].vertex[3]].ex;
          ey = m_w[0] * m_vertices[m_elements[i].vertex[0]].ey +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ey +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ey +
               m_w[3] * m_vertices[m_elements[i].vertex[3]].ey;
          ez = m_w[0] * m_vertices[m_elements[i].vertex[0]].ez +
               m_w[1] * m_vertices[m_elements[i].vertex[1]].ez +
               m_w[2] * m_vertices[m_elements[i].vertex[2]].ez +
               m_w[3] * m_vertices[m_elements[i].vertex[3]].ez;
          p = m_w[0] * m_vertices[m_elements[i].vertex[0]].p +
              m_w[1] * m_vertices[m_elements[i].vertex[1]].p +
              m_w[2] * m_vertices[m_elements[i].vertex[2]].p +
              m_w[3] * m_vertices[m_elements[i].vertex[3]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          if (zMirrored) ez = -ez;
          m_lastElement = i;
          m = m_regions[m_elements[i].region].medium;
          if (!m_regions[m_elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      default:
        std::cerr << m_className << "::ElectricField:\n";
        std::cerr << "    Invalid element type (" << m_elements[i].type
                  << ").\n";
        status = -11;
        return;
        break;
    }
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Point (" << x << ", " << y << ", " << z
              << ") is outside the mesh.\n";
  }
  status = -6;
  return;
}

void ComponentTcad3d::ElectricField(const double x, const double y,
                                    const double z, double& ex, double& ey,
                                    double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

Medium* ComponentTcad3d::GetMedium(const double xin, const double yin,
                                   const double zin) {

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return NULL;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  const double cellsx = m_xMaxBoundingBox - m_xMinBoundingBox;
  if (m_xPeriodic) {
    x = m_xMinBoundingBox + fmod(x - m_xMinBoundingBox, cellsx);
    if (x < m_xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = m_xMinBoundingBox + fmod(x - m_xMinBoundingBox, cellsx);
    if (xNew < m_xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = m_xMinBoundingBox + m_xMaxBoundingBox - xNew;
    }
    x = xNew;
  }
  const double cellsy = m_yMaxBoundingBox - m_yMinBoundingBox;
  if (m_yPeriodic) {
    y = m_yMinBoundingBox + fmod(y - m_yMinBoundingBox, cellsy);
    if (y < m_yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = m_yMinBoundingBox + fmod(y - m_yMinBoundingBox, cellsy);
    if (yNew < m_yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = m_yMinBoundingBox + m_yMaxBoundingBox - yNew;
    }
    y = yNew;
  }
  const double cellsz = m_zMaxBoundingBox - m_zMinBoundingBox;
  if (m_zPeriodic) {
    z = m_zMinBoundingBox + fmod(z - m_zMinBoundingBox, cellsz);
    if (z < m_zMinBoundingBox) z += cellsz;
  } else if (m_zMirrorPeriodic) {
    double zNew = m_zMinBoundingBox + fmod(z - m_zMinBoundingBox, cellsz);
    if (zNew < m_zMinBoundingBox) zNew += cellsz;
    int nz = int(floor(0.5 + (zNew - z) / cellsz));
    if (nz != 2 * (nz / 2)) {
      zNew = m_zMinBoundingBox + m_zMaxBoundingBox - zNew;
    }
    z = zNew;
  }

  // Check if the point is inside the bounding box.
  if (x < m_xMinBoundingBox || x > m_xMaxBoundingBox || y < m_yMinBoundingBox ||
      y > m_yMaxBoundingBox || z < m_zMinBoundingBox || z > m_zMaxBoundingBox) {
    return NULL;
  }

  // Check if the point is still located in the previous element.
  int i = m_lastElement;
  switch (m_elements[i].type) {
    case 2:
      if (CheckTriangle(x, y, z, i)) {
        return m_regions[m_elements[i].region].medium;
      }
      break;
    case 5:
      if (CheckTetrahedron(x, y, z, i)) {
        return m_regions[m_elements[i].region].medium;
      }
      break;
    default:
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Invalid element type (" << m_elements[i].type << ").\n";
      return NULL;
      break;
  }

  // The point is not in the previous element.
  // We have to loop over all elements.
  for (i = m_nElements; i--;) {
    switch (m_elements[i].type) {
      case 2:
        if (CheckTriangle(x, y, z, i)) {
          m_lastElement = i;
          return m_regions[m_elements[i].region].medium;
        }
        break;
      case 5:
        if (CheckTetrahedron(x, y, z, i)) {
          m_lastElement = i;
          return m_regions[m_elements[i].region].medium;
        }
        break;
      default:
        std::cerr << m_className << "::GetMedium:\n";
        std::cerr << "    Invalid element type (" << m_elements[i].type
                  << ").\n";
        return NULL;
        break;
    }
  }
  // The point is outside the mesh.
  return NULL;
}

bool ComponentTcad3d::Initialise(const std::string gridfilename,
                                 const std::string datafilename) {

  m_ready = false;
  // Import mesh data from .grd file.
  if (!LoadGrid(gridfilename)) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Importing mesh data failed.\n";
    return false;
  }

  // Import electric field and potential from .dat file.
  if (!LoadData(datafilename)) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Importing electric field and potential failed.\n";
    return false;
  }

  // Find min./max. coordinates and potentials.
  m_xMaxBoundingBox = m_vertices[m_elements[0].vertex[0]].x;
  m_yMaxBoundingBox = m_vertices[m_elements[0].vertex[0]].y;
  m_zMaxBoundingBox = m_vertices[m_elements[0].vertex[0]].z;
  m_xMinBoundingBox = m_xMaxBoundingBox;
  m_yMinBoundingBox = m_yMaxBoundingBox;
  m_zMinBoundingBox = m_zMaxBoundingBox;
  m_pMax = m_pMin = m_vertices[m_elements[0].vertex[0]].p;
  for (int i = m_nElements; i--;) {
    for (int j = 0; j <= m_elements[i].type; ++j) {
      if (m_vertices[m_elements[i].vertex[j]].x < m_xMinBoundingBox) {
        m_xMinBoundingBox = m_vertices[m_elements[i].vertex[j]].x;
      } else if (m_vertices[m_elements[i].vertex[j]].x > m_xMaxBoundingBox) {
        m_xMaxBoundingBox = m_vertices[m_elements[i].vertex[j]].x;
      }
      if (m_vertices[m_elements[i].vertex[j]].y < m_yMinBoundingBox) {
        m_yMinBoundingBox = m_vertices[m_elements[i].vertex[j]].y;
      } else if (m_vertices[m_elements[i].vertex[j]].y > m_yMaxBoundingBox) {
        m_yMaxBoundingBox = m_vertices[m_elements[i].vertex[j]].y;
      }
      if (m_vertices[m_elements[i].vertex[j]].z < m_zMinBoundingBox) {
        m_zMinBoundingBox = m_vertices[m_elements[i].vertex[j]].z;
      } else if (m_vertices[m_elements[i].vertex[j]].z > m_zMaxBoundingBox) {
        m_zMaxBoundingBox = m_vertices[m_elements[i].vertex[j]].z;
      }
      if (m_vertices[m_elements[i].vertex[j]].p < m_pMin) {
        m_pMin = m_vertices[m_elements[i].vertex[j]].p;
      } else if (m_vertices[m_elements[i].vertex[j]].p > m_pMax) {
        m_pMax = m_vertices[m_elements[i].vertex[j]].p;
      }
    }
  }

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Bounding box:\n";
  std::cout << "      " << m_xMinBoundingBox << " < x [cm] < "
            << m_xMaxBoundingBox << "\n";
  std::cout << "      " << m_yMinBoundingBox << " < y [cm] < "
            << m_yMaxBoundingBox << "\n";
  std::cout << "      " << m_zMinBoundingBox << " < z [cm] < "
            << m_zMaxBoundingBox << "\n";
  std::cout << "    Voltage range:\n";
  std::cout << "      " << m_pMin << " < V < " << m_pMax << "\n";

  bool ok = true;

  // Count the number of elements belonging to a region.
  std::vector<int> nElementsRegion;
  nElementsRegion.resize(m_nRegions);
  for (int i = m_nRegions; i--;) nElementsRegion[i] = 0;

  // Count the different element shapes.
  int nTriangles = 0;
  int nTetrahedra = 0;
  int nOtherShapes = 0;

  // Check if there are elements which are not part of any region.
  int nLoose = 0;
  std::vector<int> looseElements;
  looseElements.clear();

  // Check if there are degenerate elements.
  int nDegenerate = 0;
  std::vector<int> degenerateElements;
  degenerateElements.clear();

  for (int i = m_nElements; i--;) {
    if (m_elements[i].type == 2) {
      ++nTriangles;
      if (m_elements[i].vertex[0] == m_elements[i].vertex[1] ||
          m_elements[i].vertex[1] == m_elements[i].vertex[2] ||
          m_elements[i].vertex[2] == m_elements[i].vertex[0]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (m_elements[i].type == 5) {
      if (m_elements[i].vertex[0] == m_elements[i].vertex[1] ||
          m_elements[i].vertex[0] == m_elements[i].vertex[2] ||
          m_elements[i].vertex[0] == m_elements[i].vertex[3] ||
          m_elements[i].vertex[1] == m_elements[i].vertex[2] ||
          m_elements[i].vertex[1] == m_elements[i].vertex[3] ||
          m_elements[i].vertex[2] == m_elements[i].vertex[3]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
      ++nTetrahedra;
    } else {
      // Other shapes should not occur, since they were excluded in LoadGrid.
      ++nOtherShapes;
    }
    if (m_elements[i].region >= 0 && m_elements[i].region < m_nRegions) {
      ++nElementsRegion[m_elements[i].region];
    } else {
      looseElements.push_back(i);
      ++nLoose;
    }
  }

  if (nDegenerate > 0) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    The following elements are degenerate:\n";
    for (int i = nDegenerate; i--;) {
      std::cerr << "      " << degenerateElements[i] << "\n";
    }
    ok = false;
  }

  if (nLoose > 0) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    The following elements are not part of any region:\n";
    for (int i = nLoose; i--;) {
      std::cerr << "      " << looseElements[i] << "\n";
    }
    ok = false;
  }

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Number of regions: " << m_nRegions << "\n";
  for (int i = 0; i < m_nRegions; ++i) {
    std::cout << "      " << i << ": " << m_regions[i].name << ", "
              << nElementsRegion[i] << " elements\n";
  }

  std::cout << "    Number of elements: " << m_nElements << "\n";
  if (nTriangles > 0) {
    std::cout << "      " << nTriangles << " triangles\n";
  }
  if (nTetrahedra > 0) {
    std::cout << "      " << nTetrahedra << " tetrahedra\n";
  }
  if (nOtherShapes > 0) {
    std::cout << "      " << nOtherShapes << " elements of unknown type\n";
    std::cout << "      Program bug!\n";
    m_ready = false;
    Cleanup();
    return false;
  }
  if (m_debug) {
    // For each element, print the indices of the constituting vertices.
    for (int i = 0; i < m_nElements; ++i) {
      if (m_elements[i].type == 2) {
        std::cout << "      " << i << ": " << m_elements[i].vertex[0] << "  "
                  << m_elements[i].vertex[1] << "  " << m_elements[i].vertex[2]
                  << " (triangle, region " << m_elements[i].region << ")\n";
      } else if (m_elements[i].type == 5) {
        std::cout << "      " << i << ": " << m_elements[i].vertex[0] << "  "
                  << m_elements[i].vertex[1] << "  " << m_elements[i].vertex[2]
                  << "  " << m_elements[i].vertex[3] << " (tetrahedron, region "
                  << m_elements[i].region << ")\n";
      }
    }
  }

  std::cout << "    Number of vertices: " << m_nVertices << "\n";
  if (m_debug) {
    for (int i = 0; i < m_nVertices; ++i) {
      std::cout << "      " << i << ": (x, y, z) = (" << m_vertices[i].x << ", "
                << m_vertices[i].y << ", " << m_vertices[i].z
                << "), V = " << m_vertices[i].p << "\n";
    }
  }

  if (!ok) {
    m_ready = false;
    Cleanup();
    return false;
  }

  m_ready = true;
  UpdatePeriodicity();
  return true;
}

bool ComponentTcad3d::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                     double& xmax, double& ymax, double& zmax) {

  if (!m_ready) return false;
  xmin = m_xMinBoundingBox;
  ymin = m_yMinBoundingBox;
  zmin = m_zMinBoundingBox;
  xmax = m_xMaxBoundingBox;
  ymax = m_yMaxBoundingBox;
  zmax = m_zMaxBoundingBox;
  if (m_xPeriodic || m_xMirrorPeriodic) {
    xmin = -INFINITY;
    xmax = +INFINITY;
  }
  if (m_yPeriodic || m_yMirrorPeriodic) {
    ymin = -INFINITY;
    ymax = +INFINITY;
  }
  if (m_zPeriodic || m_zMirrorPeriodic) {
    zmin = -INFINITY;
    zmax = +INFINITY;
  }
  return true;
}

bool ComponentTcad3d::GetVoltageRange(double& vmin, double& vmax) {

  if (!m_ready) return false;
  vmin = m_pMin;
  vmax = m_pMax;
  return true;
}

void ComponentTcad3d::PrintRegions() {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    Field map not yet initialised.\n";
    return;
  }

  if (m_nRegions < 0) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    No regions are currently defined.\n";
    return;
  }

  std::cout << m_className << "::PrintRegions:\n";
  std::cout << "    Currently " << m_nRegions << " regions are defined.\n";
  std::cout << "      Index  Name      Medium\n";
  for (int i = 0; i < m_nRegions; ++i) {
    std::cout << "      " << i << "  " << m_regions[i].name;
    if (m_regions[i].medium == 0) {
      std::cout << "      none  ";
    } else {
      std::cout << "      " << m_regions[i].medium->GetName();
    }
    if (m_regions[i].drift) {
      std::cout << " (active region)\n";
    } else {
      std::cout << "\n";
    }
  }
}

void ComponentTcad3d::GetRegion(const int i, std::string& name, bool& active) {

  if (i < 0 || i >= m_nRegions) {
    std::cerr << m_className << "::GetRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  name = m_regions[i].name;
  active = m_regions[i].drift;
}

void ComponentTcad3d::SetDriftRegion(const int i) {

  if (i < 0 || i >= m_nRegions) {
    std::cerr << m_className << "::SetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  m_regions[i].drift = true;
}

void ComponentTcad3d::UnsetDriftRegion(const int i) {

  if (i < 0 || i >= m_nRegions) {
    std::cerr << m_className << "::UnsetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  m_regions[i].drift = false;
}

void ComponentTcad3d::SetMedium(const int i, Medium* medium) {

  if (i < 0 || i >= m_nRegions) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }

  if (medium == 0) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  m_regions[i].medium = medium;
}

bool ComponentTcad3d::GetMedium(const int i, Medium*& m) const {

  if (i < 0 || i >= m_nRegions) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return false;
  }

  m = m_regions[i].medium;
  if (m == 0) return false;
  return true;
}

bool ComponentTcad3d::GetElement(const int i, double& vol, double& dmin,
                                 double& dmax, int& type) {

  if (i < 0 || i >= m_nElements) {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Element index (" << i << ") out of range.\n";
    return false;
  }

  type = m_elements[i].type;
  if (m_elements[i].type == 2) {
    // Triangle
    const double vx = (m_vertices[m_elements[i].vertex[1]].y -
                       m_vertices[m_elements[i].vertex[0]].y) *
                          (m_vertices[m_elements[i].vertex[2]].z -
                           m_vertices[m_elements[i].vertex[0]].z) -
                      (m_vertices[m_elements[i].vertex[1]].z -
                       m_vertices[m_elements[i].vertex[0]].z) *
                          (m_vertices[m_elements[i].vertex[2]].y -
                           m_vertices[m_elements[i].vertex[0]].y);
    const double vy = (m_vertices[m_elements[i].vertex[1]].z -
                       m_vertices[m_elements[i].vertex[0]].z) *
                          (m_vertices[m_elements[i].vertex[2]].x -
                           m_vertices[m_elements[i].vertex[0]].x) -
                      (m_vertices[m_elements[i].vertex[1]].x -
                       m_vertices[m_elements[i].vertex[0]].x) *
                          (m_vertices[m_elements[i].vertex[2]].z -
                           m_vertices[m_elements[i].vertex[0]].z);
    const double vz = (m_vertices[m_elements[i].vertex[1]].x -
                       m_vertices[m_elements[i].vertex[0]].x) *
                          (m_vertices[m_elements[i].vertex[2]].y -
                           m_vertices[m_elements[i].vertex[0]].y) -
                      (m_vertices[m_elements[i].vertex[1]].y -
                       m_vertices[m_elements[i].vertex[0]].y) *
                          (m_vertices[m_elements[i].vertex[2]].x -
                           m_vertices[m_elements[i].vertex[0]].x);
    vol = sqrt(vx * vx + vy * vy + vz * vz);
    const double a = sqrt(pow(m_vertices[m_elements[i].vertex[1]].x -
                                  m_vertices[m_elements[i].vertex[0]].x,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[1]].y -
                                  m_vertices[m_elements[i].vertex[0]].y,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[1]].z -
                                  m_vertices[m_elements[i].vertex[0]].z,
                              2));
    const double b = sqrt(pow(m_vertices[m_elements[i].vertex[2]].x -
                                  m_vertices[m_elements[i].vertex[0]].x,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[2]].y -
                                  m_vertices[m_elements[i].vertex[0]].y,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[2]].z -
                                  m_vertices[m_elements[i].vertex[0]].z,
                              2));
    const double c = sqrt(pow(m_vertices[m_elements[i].vertex[1]].x -
                                  m_vertices[m_elements[i].vertex[2]].x,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[1]].y -
                                  m_vertices[m_elements[i].vertex[2]].y,
                              2) +
                          pow(m_vertices[m_elements[i].vertex[1]].z -
                                  m_vertices[m_elements[i].vertex[2]].z,
                              2));
    dmin = dmax = a;
    if (b < dmin) dmin = b;
    if (c < dmin) dmin = c;
    if (b > dmax) dmax = b;
    if (c > dmax) dmax = c;
  } else if (m_elements[i].type == 5) {
    // Tetrahedron
    vol = fabs((m_vertices[m_elements[i].vertex[3]].x -
                m_vertices[m_elements[i].vertex[0]].x) *
                   ((m_vertices[m_elements[i].vertex[1]].y -
                     m_vertices[m_elements[i].vertex[0]].y) *
                        (m_vertices[m_elements[i].vertex[2]].z -
                         m_vertices[m_elements[i].vertex[0]].z) -
                    (m_vertices[m_elements[i].vertex[2]].y -
                     m_vertices[m_elements[i].vertex[0]].y) *
                        (m_vertices[m_elements[i].vertex[1]].z -
                         m_vertices[m_elements[i].vertex[0]].z)) +
               (m_vertices[m_elements[i].vertex[3]].y -
                m_vertices[m_elements[i].vertex[0]].y) *
                   ((m_vertices[m_elements[i].vertex[1]].z -
                     m_vertices[m_elements[i].vertex[0]].z) *
                        (m_vertices[m_elements[i].vertex[2]].x -
                         m_vertices[m_elements[i].vertex[0]].x) -
                    (m_vertices[m_elements[i].vertex[2]].z -
                     m_vertices[m_elements[i].vertex[0]].z) *
                        (m_vertices[m_elements[i].vertex[1]].x -
                         m_vertices[m_elements[i].vertex[0]].x)) +
               (m_vertices[m_elements[i].vertex[3]].z -
                m_vertices[m_elements[i].vertex[0]].z) *
                   ((m_vertices[m_elements[i].vertex[1]].x -
                     m_vertices[m_elements[i].vertex[0]].x) *
                        (m_vertices[m_elements[i].vertex[2]].y -
                         m_vertices[m_elements[i].vertex[0]].y) -
                    (m_vertices[m_elements[i].vertex[3]].x -
                     m_vertices[m_elements[i].vertex[0]].x) *
                        (m_vertices[m_elements[i].vertex[1]].y -
                         m_vertices[m_elements[i].vertex[0]].y))) /
          6.;
    // Loop over all pairs of m_vertices.
    for (int j = 0; j < nMaxVertices - 1; ++j) {
      for (int k = j + 1; k < nMaxVertices; ++k) {
        // Compute distance.
        const double dist = sqrt(pow(m_vertices[m_elements[i].vertex[j]].x -
                                         m_vertices[m_elements[i].vertex[k]].x,
                                     2) +
                                 pow(m_vertices[m_elements[i].vertex[j]].y -
                                         m_vertices[m_elements[i].vertex[k]].y,
                                     2) +
                                 pow(m_vertices[m_elements[i].vertex[j]].z -
                                         m_vertices[m_elements[i].vertex[k]].z,
                                     2));
        if (k == 1) {
          dmin = dist;
          dmax = dist;
        } else {
          if (dist < dmin) dmin = dist;
          if (dist > dmax) dmax = dist;
        }
      }
    }
  } else {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Unexpected element type (" << type << ").\n";
    return false;
  }
  return true;
}

bool ComponentTcad3d::GetElement(const int i, double& vol, double& dmin,
                                 double& dmax, int& type, int& node1,
                                 int& node2, int& node3, int& node4, int& node5,
                                 int& node6, int& node7, int& reg) {

  if (!GetElement(i, vol, dmin, dmax, type)) return false;
  node1 = m_elements[i].vertex[0];
  node2 = m_elements[i].vertex[1];
  node3 = m_elements[i].vertex[2];
  node4 = m_elements[i].vertex[3];
  node5 = m_elements[i].vertex[4];
  node6 = m_elements[i].vertex[5];
  node7 = m_elements[i].vertex[6];
  reg = m_elements[i].region;
  return true;
}

bool ComponentTcad3d::GetNode(const int i, double& x, double& y, double& z,
                              double& v, double& ex, double& ey, double& ez) {

  if (i < 0 || i >= m_nVertices) {
    std::cerr << m_className << "::GetNode:\n";
    std::cerr << "    Node index (" << i << ") out of range.\n";
    return false;
  }

  x = m_vertices[i].x;
  y = m_vertices[i].y;
  z = m_vertices[i].z;
  v = m_vertices[i].p;
  ex = m_vertices[i].ex;
  ey = m_vertices[i].ey;
  ez = m_vertices[i].ez;
  return true;
}

bool ComponentTcad3d::LoadData(const std::string datafilename) {

  std::ifstream datafile;
  datafile.open(datafilename.c_str(), std::ios::in);
  if (!datafile) {
    std::cerr << m_className << "::LoadData:\n";
    std::cerr << "    Could not open file " << datafilename << ".\n";
    return false;
  }

  std::string line;
  std::istringstream data;

  std::vector<bool> isInRegion(m_nVertices);
  std::vector<int> fillCount(m_nVertices, 0);
  for (int i = m_nVertices; i--;) {
    m_vertices[i].p = 0.;
    m_vertices[i].ex = 0.;
    m_vertices[i].ey = 0.;
    m_vertices[i].ez = 0.;
    m_vertices[i].isShared = false;
  }

  std::string::size_type pBra, pKet, pEq;

  while (!datafile.fail()) {
    // Read one line.
    std::getline(datafile, line);
    // Strip white space from beginning of line.
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Find data section.
    if (line.substr(0, 8) == "function") {
      // Read type of data set.
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" found.
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading file " << datafilename << ".\n";
        std::cerr << "    Line:\n";
        std::cerr << "    " << line << "\n";
        datafile.close();
        Cleanup();
        return false;
      }
      line = line.substr(pEq + 1);
      std::string dataset;
      data.str(line);
      data >> dataset;
      data.clear();
      if (dataset == "ElectrostaticPotential") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < m_nRegions; ++j) {
          if (name == m_regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = m_nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < m_nElements; ++j) {
          if (m_elements[j].region != index) continue;
          for (int k = 0; k <= m_elements[j].type; ++k) {
            isInRegion[m_elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < m_nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of m_vertices
          // and the number of potential values.
          if (ivertex >= m_nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Error reading file " << datafilename << "\n";
            std::cerr << "    Dataset has more values than "
                      << "there are vertices in region " << name << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          m_vertices[ivertex].p = val;
          ++fillCount[ivertex];
          ++ivertex;
        }
      } else if (dataset == "ElectricField") {
        // Same procedure as for the potential.
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << ".\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        int index = -1;
        for (int j = 0; j < m_nRegions; ++j) {
          if (name == m_regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // In case of the electric field, there are three values per vertex.
        nValues = nValues / 3;
        for (int j = m_nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < m_nElements; ++j) {
          if (m_elements[j].region != index) continue;
          for (int k = 0; k <= m_elements[j].type; ++k) {
            isInRegion[m_elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val1, val2, val3;
        for (int j = 0; j < nValues; ++j) {
          datafile >> val1 >> val2 >> val3;
          while (ivertex < m_nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          if (ivertex >= m_nVertices) {
            std::cerr << m_className << "::LoadData\n"
                      << "    Error reading file " << datafilename << "\n"
                      << "    Dataset has more values than"
                      << " there are vertices in region " << name << ".\n";
            datafile.close();
            Cleanup();
            return false;
          }
          m_vertices[ivertex].ex = val1;
          m_vertices[ivertex].ey = val2;
          m_vertices[ivertex].ez = val3;
          ++ivertex;
        }
      }
    }
  }
  if (datafile.fail() && !datafile.eof()) {
    std::cerr << m_className << "::LoadData\n";
    std::cerr << "    Error reading file " << datafilename << "\n";
    datafile.close();
    Cleanup();
    return false;
  }

  for (int i = m_nVertices; i--;) {
    if (fillCount[i] > 1) m_vertices[i].isShared = true;
  }

  datafile.close();

  return true;
}

bool ComponentTcad3d::LoadGrid(const std::string gridfilename) {

  // Open the file containing the mesh description.
  std::ifstream gridfile;
  gridfile.open(gridfilename.c_str(), std::ios::in);
  if (!gridfile) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not open file " << gridfilename << ".\n";
    return false;
  }

  std::string line;
  std::istringstream data;

  // Delete existing mesh information.
  Cleanup();
  std::string::size_type pBra, pKet, pEq;
  // Count line numbers.
  int iLine = 0;

  // Get the number of regions.
  while (!gridfile.fail()) {
    // Read one line.
    std::getline(gridfile, line);
    ++iLine;
    // Strip white space from the beginning of the line.
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'nb_regions'.
    if (line.substr(0, 10) == "nb_regions") {
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" sign found.
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of regions.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pEq + 1);
      data.str(line);
      data >> m_nRegions;
      data.clear();
      break;
    }
    if (gridfile.fail()) break;
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'nb_regions' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  m_regions.resize(m_nRegions);
  for (int j = m_nRegions; j--;) {
    m_regions[j].name = "";
    m_regions[j].drift = false;
    m_regions[j].medium = 0;
  }

  if (m_debug) {
    std::cout << m_className << "::LoadGrid:\n";
    std::cout << "    Found " << m_nRegions << " regions.\n";
  }

  // Get the region names.
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'regions'.
    if (line.substr(0, 7) == "regions") {
      // Get region names (given in brackets).
      pBra = line.find('[');
      pKet = line.find(']');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read region names.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      for (int j = 0; j < m_nRegions; ++j) {
        data >> m_regions[j].name;
        data.clear();
        // Assume by default that all regions are active.
        m_regions[j].drift = true;
        m_regions[j].medium = 0;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'regions' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the vertices.
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Vertices'.
    if (line.substr(0, 8) == "Vertices") {
      // Get number of vertices (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of vertices.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> m_nVertices;
      data.clear();
      m_vertices.resize(m_nVertices);
      // Get the coordinates of this vertex.
      for (int j = 0; j < m_nVertices; ++j) {
        gridfile >> m_vertices[j].x >> m_vertices[j].y >> m_vertices[j].z;
        // Change units from micron to cm.
        m_vertices[j].x *= 1.e-4;
        m_vertices[j].y *= 1.e-4;
        m_vertices[j].z *= 1.e-4;
      }
      iLine += m_nVertices - 1;
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Vertices' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the "edges" (lines connecting two vertices).
  int nEdges = 0;
  // Temporary arrays for storing edge points.
  std::vector<int> edgeP1;
  std::vector<int> edgeP2;
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Edges'.
    if (line.substr(0, 5) == "Edges") {
      // Get the number of edges (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of edges.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> nEdges;
      data.clear();
      edgeP1.resize(nEdges);
      edgeP2.resize(nEdges);
      // Get the indices of the two endpoints.
      for (int j = 0; j < nEdges; ++j) {
        gridfile >> edgeP1[j] >> edgeP2[j];
      }
      iLine += nEdges - 1;
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Edges' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  for (int i = nEdges; i--;) {
    // Make sure the indices of the edge endpoints are not out of range.
    if (edgeP1[i] < 0 || edgeP1[i] >= m_nVertices || edgeP2[i] < 0 ||
        edgeP2[i] >= m_nVertices) {
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Vertex index of edge " << i << " out of range.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    // Make sure the edge is non-degenerate.
    if (edgeP1[i] == edgeP2[i]) {
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Edge " << i << " is degenerate.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
  }

  // Get the "faces".
  int nFaces = 0;
  std::vector<face> faces;
  faces.clear();

  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Faces'.
    if (line.substr(0, 5) == "Faces") {
      // Get the number of faces (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of faces.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> nFaces;
      data.clear();
      faces.resize(nFaces);
      // Get the indices of the edges constituting this face.
      for (int j = 0; j < nFaces; ++j) {
        gridfile >> faces[j].type;
        if (faces[j].type != 3 && faces[j].type != 4) {
          std::cerr << m_className << "::LoadGrid:\n";
          std::cerr << "    Face with index " << j
                    << " has invalid number of edges (" << faces[j].type
                    << ").\n";
          Cleanup();
          gridfile.close();
          return false;
        }
        for (int k = 0; k < faces[j].type; ++k) {
          gridfile >> faces[j].edge[k];
        }
      }
      iLine += nFaces - 1;
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Faces' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the elements.
  int edge0, edge1, edge2;
  int face0, face1, face2, face3;
  int type;
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Elements'.
    if (line.substr(0, 8) == "Elements") {
      // Get number of elements (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of elements.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> m_nElements;
      data.clear();
      // Resize array of elements.
      m_elements.resize(m_nElements);
      // Get type and constituting edges of each element.
      for (int j = 0; j < m_nElements; ++j) {
        ++iLine;
        gridfile >> type;
        if (type == 2) {
          // Triangle
          gridfile >> edge0 >> edge1 >> edge2;
          // Get the vertices.
          // Negative edge index means that the sequence of the two points
          // is supposed to be inverted.
          // The actual index is then given by "-index - 1".
          // For our purposes, the orientation does not matter.
          // Make sure the indices are not out of range.
          if (edge0 >= nEdges || -edge0 - 1 >= nEdges || edge1 >= nEdges ||
              -edge1 - 1 >= nEdges || edge2 >= nEdges || -edge2 - 1 >= nEdges) {
            std::cerr << m_className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << " (line "
                      << iLine << ").\n";
            std::cerr << "    Edge index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          m_elements[j].vertex[0] = edgeP1[edge0];
          m_elements[j].vertex[1] = edgeP2[edge0];
          if (edgeP1[edge1] != m_elements[j].vertex[0] &&
              edgeP1[edge1] != m_elements[j].vertex[1]) {
            m_elements[j].vertex[2] = edgeP1[edge1];
          } else {
            m_elements[j].vertex[2] = edgeP2[edge1];
          }
        } else if (type == 5) {
          // Tetrahedron
          // Get the faces.
          // Negative face index means that the sequence of the edges
          // is supposed to be inverted.
          // For our purposes, the orientation does not matter.
          gridfile >> face0 >> face1 >> face2 >> face3;
          // Make sure the face indices are not out of range.
          if (face0 >= nFaces || -face0 - 1 >= nFaces || face1 >= nFaces ||
              -face1 - 1 >= nFaces || face2 >= nFaces || -face2 - 1 >= nFaces ||
              face3 >= nFaces || -face3 - 1 >= nFaces) {
            std::cerr << m_className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << " (line "
                      << iLine << ").\n";
            std::cerr << "    Face index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          if (face0 < 0) face0 = -face0 - 1;
          if (face1 < 0) face1 = -face1 - 1;
          // Get the edges of the first face.
          edge0 = faces[face0].edge[0];
          edge1 = faces[face0].edge[1];
          edge2 = faces[face0].edge[2];
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          if (edge2 < 0) edge2 = -edge2 - 1;
          // Make sure the edge indices are not out of range.
          if (edge0 >= nEdges || edge1 >= nEdges || edge2 >= nEdges) {
            std::cerr << m_className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << "\n";
            std::cerr << "    Edge index in element " << j
                      << " out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          // Get the first three vertices.
          m_elements[j].vertex[0] = edgeP1[edge0];
          m_elements[j].vertex[1] = edgeP2[edge0];
          if (edgeP1[edge1] != m_elements[j].vertex[0] &&
              edgeP1[edge1] != m_elements[j].vertex[1]) {
            m_elements[j].vertex[2] = edgeP1[edge1];
          } else {
            m_elements[j].vertex[2] = edgeP2[edge1];
          }
          // Get the fourth vertex from face 1.
          edge0 = faces[face1].edge[0];
          edge1 = faces[face1].edge[1];
          edge2 = faces[face1].edge[2];
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          if (edge2 < 0) edge2 = -edge2 - 1;
          if (edgeP1[edge0] != m_elements[j].vertex[0] &&
              edgeP1[edge0] != m_elements[j].vertex[1] &&
              edgeP1[edge0] != m_elements[j].vertex[2]) {
            m_elements[j].vertex[3] = edgeP1[edge0];
          } else if (edgeP2[edge0] != m_elements[j].vertex[0] &&
                     edgeP2[edge0] != m_elements[j].vertex[1] &&
                     edgeP2[edge0] != m_elements[j].vertex[2]) {
            m_elements[j].vertex[3] = edgeP2[edge0];
          } else if (edgeP1[edge1] != m_elements[j].vertex[0] &&
                     edgeP1[edge1] != m_elements[j].vertex[1] &&
                     edgeP1[edge1] != m_elements[j].vertex[2]) {
            m_elements[j].vertex[3] = edgeP1[edge1];
          } else if (edgeP2[edge1] != m_elements[j].vertex[0] &&
                     edgeP2[edge1] != m_elements[j].vertex[1] &&
                     edgeP2[edge1] != m_elements[j].vertex[2]) {
            m_elements[j].vertex[3] = edgeP2[edge1];
          } else {
            std::cerr << m_className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << "\n";
            std::cerr << "    Face 1 of element " << j << " is degenerate.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
        } else {
          // Other element types are not allowed.
          std::cerr << m_className << "::LoadGrid:\n"
                    << "    Error reading file " << gridfilename << " (line "
                    << iLine << ").\n";
          if (type == 0 || type == 1) {
            std::cerr << "    Invalid element type (" << type
                      << ") for 3d mesh.\n";
          } else {
            std::cerr << "    Element type " << type << " is not supported.\n";
            std::cerr << "    Remesh with option -t to create only"
                      << " triangles and tetrahedra.\n";
          }
          Cleanup();
          gridfile.close();
          return false;
        }
        m_elements[j].type = type;
        m_elements[j].region = -1;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Elements' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Assign regions to elements.
  std::string name;
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Region'.
    if (line.substr(0, 6) == "Region") {
      // Get region name (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read region name.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> name;
      data.clear();
      int index = -1;
      for (int j = 0; j < m_nRegions; ++j) {
        if (name == m_regions[j].name) {
          index = j;
          break;
        }
      }
      if (index == -1) {
        // Specified region name is not in the list.
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Unknown region " << name << ".\n";
        continue;
      }
      std::getline(gridfile, line);
      std::getline(gridfile, line);
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Could not read number of elements in region " << name
                  << ".\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      int nElementsRegion;
      int iElement;
      data.str(line);
      data >> nElementsRegion;
      data.clear();
      for (int j = 0; j < nElementsRegion; ++j) {
        gridfile >> iElement;
        m_elements[iElement].region = index;
      }
    }
  }

  gridfile.close();
  if (gridfile.fail() && !gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << ".\n";
    Cleanup();
    return false;
  }

  return true;
}

void ComponentTcad3d::Cleanup() {

  // Vertices
  m_vertices.clear();
  m_nVertices = 0;

  // Elements
  m_elements.clear();
  m_nElements = 0;

  // Regions
  m_regions.clear();
  m_nRegions = 0;
}

bool ComponentTcad3d::CheckTetrahedron(const double x, const double y,
                                       const double z, const int i) {

  m_w[0] = m_w[1] = m_w[2] = m_w[3] = 0.;

  const double x10 = m_vertices[m_elements[i].vertex[1]].x -
                     m_vertices[m_elements[i].vertex[0]].x;
  const double y10 = m_vertices[m_elements[i].vertex[1]].y -
                     m_vertices[m_elements[i].vertex[0]].y;
  const double z10 = m_vertices[m_elements[i].vertex[1]].z -
                     m_vertices[m_elements[i].vertex[0]].z;

  const double x20 = m_vertices[m_elements[i].vertex[2]].x -
                     m_vertices[m_elements[i].vertex[0]].x;
  const double y20 = m_vertices[m_elements[i].vertex[2]].y -
                     m_vertices[m_elements[i].vertex[0]].y;
  const double z20 = m_vertices[m_elements[i].vertex[2]].z -
                     m_vertices[m_elements[i].vertex[0]].z;

  const double x30 = m_vertices[m_elements[i].vertex[3]].x -
                     m_vertices[m_elements[i].vertex[0]].x;
  const double y30 = m_vertices[m_elements[i].vertex[3]].y -
                     m_vertices[m_elements[i].vertex[0]].y;
  const double z30 = m_vertices[m_elements[i].vertex[3]].z -
                     m_vertices[m_elements[i].vertex[0]].z;

  const double x21 = m_vertices[m_elements[i].vertex[2]].x -
                     m_vertices[m_elements[i].vertex[1]].x;
  const double y21 = m_vertices[m_elements[i].vertex[2]].y -
                     m_vertices[m_elements[i].vertex[1]].y;
  const double z21 = m_vertices[m_elements[i].vertex[2]].z -
                     m_vertices[m_elements[i].vertex[1]].z;

  const double x31 = m_vertices[m_elements[i].vertex[3]].x -
                     m_vertices[m_elements[i].vertex[1]].x;
  const double y31 = m_vertices[m_elements[i].vertex[3]].y -
                     m_vertices[m_elements[i].vertex[1]].y;
  const double z31 = m_vertices[m_elements[i].vertex[3]].z -
                     m_vertices[m_elements[i].vertex[1]].z;

  const double x32 = m_vertices[m_elements[i].vertex[3]].x -
                     m_vertices[m_elements[i].vertex[2]].x;
  const double y32 = m_vertices[m_elements[i].vertex[3]].y -
                     m_vertices[m_elements[i].vertex[2]].y;
  const double z32 = m_vertices[m_elements[i].vertex[3]].z -
                     m_vertices[m_elements[i].vertex[2]].z;

  m_w[0] =
      (x - m_vertices[m_elements[i].vertex[1]].x) * (y21 * z31 - y31 * z21) +
      (y - m_vertices[m_elements[i].vertex[1]].y) * (z21 * x31 - z31 * x21) +
      (z - m_vertices[m_elements[i].vertex[1]].z) * (x21 * y31 - x31 * y21);

  m_w[0] /= x10 * (y31 * z21 - y21 * z31) + y10 * (z31 * x21 - z21 * x31) +
            z10 * (x31 * y21 - x21 * y31);

  if (m_w[0] < 0.) return false;

  m_w[1] =
      (x - m_vertices[m_elements[i].vertex[2]].x) * (-y20 * z32 + y32 * z20) +
      (y - m_vertices[m_elements[i].vertex[2]].y) * (-z20 * x32 + z32 * x20) +
      (z - m_vertices[m_elements[i].vertex[2]].z) * (-x20 * y32 + x32 * y20);

  m_w[1] /= x21 * (y20 * z32 - y32 * z20) + y21 * (z20 * x32 - z32 * x20) +
            z21 * (x20 * y32 - x32 * y20);

  if (m_w[1] < 0.) return false;

  m_w[2] =
      (x - m_vertices[m_elements[i].vertex[3]].x) * (y30 * z31 - y31 * z30) +
      (y - m_vertices[m_elements[i].vertex[3]].y) * (z30 * x31 - z31 * x30) +
      (z - m_vertices[m_elements[i].vertex[3]].z) * (x30 * y31 - x31 * y30);

  m_w[2] /= x32 * (y31 * z30 - y30 * z31) + y32 * (z31 * x30 - z30 * x31) +
            z32 * (x31 * y30 - x30 * y31);

  if (m_w[2] < 0.) return false;

  m_w[3] =
      (x - m_vertices[m_elements[i].vertex[0]].x) * (y20 * z10 - y10 * z20) +
      (y - m_vertices[m_elements[i].vertex[0]].y) * (z20 * x10 - z10 * x20) +
      (z - m_vertices[m_elements[i].vertex[0]].z) * (x20 * y10 - x10 * y20);

  m_w[3] /= x30 * (y20 * z10 - y10 * z20) + y30 * (z20 * x10 - z10 * x20) +
            z30 * (x20 * y10 - x10 * y20);

  if (m_w[3] < 0.) return false;

  if (m_debug) {
    // Reconstruct the point from the local coordinates.
    const double xr = m_w[0] * m_vertices[m_elements[i].vertex[0]].x +
                      m_w[1] * m_vertices[m_elements[i].vertex[1]].x +
                      m_w[2] * m_vertices[m_elements[i].vertex[2]].x +
                      m_w[3] * m_vertices[m_elements[i].vertex[3]].x;
    const double yr = m_w[0] * m_vertices[m_elements[i].vertex[0]].y +
                      m_w[1] * m_vertices[m_elements[i].vertex[1]].y +
                      m_w[2] * m_vertices[m_elements[i].vertex[2]].y +
                      m_w[3] * m_vertices[m_elements[i].vertex[3]].y;
    const double zr = m_w[0] * m_vertices[m_elements[i].vertex[0]].z +
                      m_w[1] * m_vertices[m_elements[i].vertex[1]].z +
                      m_w[2] * m_vertices[m_elements[i].vertex[2]].z +
                      m_w[3] * m_vertices[m_elements[i].vertex[3]].z;
    std::cout << m_className << "::CheckTetrahedron:\n";
    std::cout << "    Original coordinates:      (" << x << ", " << y << ", "
              << z << ")\n";
    std::cout << "    Local coordinates:         (" << m_w[0] << ", " << m_w[1]
              << ", " << m_w[2] << ", " << m_w[3] << ")\n";
    std::cerr << "    Reconstructed coordinates: (" << xr << ", " << yr << ", "
              << zr << ")\n";
    std::cerr << "    Checksum: " << m_w[0] + m_w[1] + m_w[2] + m_w[3] - 1.
              << "\n";
  }

  return true;
}

bool ComponentTcad3d::CheckTriangle(const double x, const double y,
                                    const double z, const int i) {

  const double v1x = m_vertices[m_elements[i].vertex[1]].x -
                     m_vertices[m_elements[i].vertex[0]].x;
  const double v2x = m_vertices[m_elements[i].vertex[2]].x -
                     m_vertices[m_elements[i].vertex[0]].x;
  const double v1y = m_vertices[m_elements[i].vertex[1]].y -
                     m_vertices[m_elements[i].vertex[0]].y;
  const double v2y = m_vertices[m_elements[i].vertex[2]].y -
                     m_vertices[m_elements[i].vertex[0]].y;
  const double v1z = m_vertices[m_elements[i].vertex[1]].z -
                     m_vertices[m_elements[i].vertex[0]].z;
  const double v2z = m_vertices[m_elements[i].vertex[2]].z -
                     m_vertices[m_elements[i].vertex[0]].z;

  // Check whether the point lies in the plane of the triangle.
  // Compute the coefficients of the plane equation.
  const double a = v1y * v2z - v2y * v1z;
  const double b = v1z * v2x - v2z * v1x;
  const double c = v1x * v2y - v2x * v1y;
  const double d = a * m_vertices[m_elements[i].vertex[0]].x +
                   b * m_vertices[m_elements[i].vertex[0]].y +
                   c * m_vertices[m_elements[i].vertex[0]].z;
  // Check if the point satisfies the plane equation.
  if (a * x + b * y + c * z != d) return false;

  // Map (x, y) onto local variables (b, c) such that
  // P = A + b * (B - A) + c * (C - A)
  // A point P is inside the triangle ABC if b, c > 0 and b + c < 1;
  // b, c are also weighting factors for points B, C

  m_w[1] = ((x - m_vertices[m_elements[i].vertex[0]].x) * v2y -
            (y - m_vertices[m_elements[i].vertex[0]].y) * v2x) /
           (v1x * v2y - v1y * v2x);
  if (m_w[1] < 0. || m_w[1] > 1.) return false;

  m_w[2] = ((m_vertices[m_elements[i].vertex[0]].x - x) * v1y -
            (m_vertices[m_elements[i].vertex[0]].y - y) * v1x) /
           (v1x * v2y - v1y * v2x);
  if (m_w[2] < 0. || m_w[1] + m_w[2] > 1.) return false;

  // Weighting factor for point A
  m_w[0] = 1. - m_w[1] - m_w[2];

  return true;
}

void ComponentTcad3d::Reset() {

  Cleanup();
  m_ready = false;
}

void ComponentTcad3d::UpdatePeriodicity() {

  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Field map not available.\n";
    return;
  }

  // Check for conflicts.
  if (m_xPeriodic && m_xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    m_xPeriodic = m_xMirrorPeriodic = false;
  }

  if (m_yPeriodic && m_yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    m_yPeriodic = m_yMirrorPeriodic = false;
  }

  if (m_zPeriodic && m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along z requested; reset.\n";
    m_zPeriodic = m_zMirrorPeriodic = false;
  }

  if (m_xAxiallyPeriodic || m_yAxiallyPeriodic || m_zAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Axial symmetry is not supported; reset.\n";
    m_xAxiallyPeriodic = m_yAxiallyPeriodic = m_zAxiallyPeriodic = false;
  }

  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Rotation symmetry is not supported; reset.\n";
    m_xRotationSymmetry = m_yRotationSymmetry = m_zRotationSymmetry = false;
  }
}
}
