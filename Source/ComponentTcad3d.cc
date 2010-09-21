#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>

#include "ComponentTcad3d.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

ComponentTcad3d::ComponentTcad3d() : 
  ComponentBase(), 
  nRegions(0), nVertices(0), nElements(0),
  hasBoundingBox(false), 
  lastElement(0) {

  className = "ComponentTcad3d";
    
  regions.clear();
  vertices.clear();
  elements.clear();
  
  for (int i = nMaxVertices; i--;) w[i] = 0.;
  
}

void 
ComponentTcad3d::ElectricField(const double x, const double y, const double z,
                               double& ex, double& ey, double& ez, double& p,
                               Medium*& m, int& status) {
 
  m = 0; 
  // Make sure the field map has been loaded.
  if (!ready) {
    std::cerr << className << "::ElectricField:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }  

  // Initialise the electric field and potential.
  ex = ey = ez = p = 0.;
  
  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox ||
      y < yMinBoundingBox || y > yMaxBoundingBox ||
      z < zMinBoundingBox || z > zMaxBoundingBox) {
    if (debug) {
      std::cerr << className << "::ElectricField:\n";
      std::cerr << "    Point (" << x << ", " << y << ", " << z
                << ") is outside the bounding box.\n";
    }
    status = -11;
    return;
  }
  
  // Assume this will work.
  status = 0;
  // Check if the point is still located in the previously found element.
  int i = lastElement;
  switch (elements[i].type) {
    case 2:
      if (CheckTriangle(x, y, z, i)) {
        ex = w[0] * vertices[elements[i].vertex[0]].ex + 
             w[1] * vertices[elements[i].vertex[1]].ex + 
             w[2] * vertices[elements[i].vertex[2]].ex;
        ey = w[0] * vertices[elements[i].vertex[0]].ey + 
             w[1] * vertices[elements[i].vertex[1]].ey + 
             w[2] * vertices[elements[i].vertex[2]].ey;
        ez = w[0] * vertices[elements[i].vertex[0]].ez + 
             w[1] * vertices[elements[i].vertex[1]].ez + 
             w[2] * vertices[elements[i].vertex[2]].ez;
        p  = w[0] * vertices[elements[i].vertex[0]].p + 
             w[1] * vertices[elements[i].vertex[1]].p + 
             w[2] * vertices[elements[i].vertex[2]].p;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;  
    case 5:
      if (CheckTetrahedron(x, y, z, i)) {
        ex = w[0] * vertices[elements[i].vertex[0]].ex + 
             w[1] * vertices[elements[i].vertex[1]].ex + 
             w[2] * vertices[elements[i].vertex[2]].ex +
             w[3] * vertices[elements[i].vertex[3]].ex;
        ey = w[0] * vertices[elements[i].vertex[0]].ey + 
             w[1] * vertices[elements[i].vertex[1]].ey + 
             w[2] * vertices[elements[i].vertex[2]].ey +
             w[3] * vertices[elements[i].vertex[3]].ey;
        ez = w[0] * vertices[elements[i].vertex[0]].ez + 
             w[1] * vertices[elements[i].vertex[1]].ez + 
             w[2] * vertices[elements[i].vertex[2]].ez +
             w[3] * vertices[elements[i].vertex[3]].ez;
        p  = w[0] * vertices[elements[i].vertex[0]].p + 
             w[1] * vertices[elements[i].vertex[1]].p + 
             w[2] * vertices[elements[i].vertex[2]].p +
             w[3] * vertices[elements[i].vertex[3]].p;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;  
    default: 
      std::cerr << className << "::ElectricField:\n";
      std::cerr << "    Unknown element type (" 
                << elements[i].type << ").\n";
      status = -11;
      return;
      break;
  }
  
  // The point is not in the previous element.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    switch (elements[i].type) {
      case 2:
        if (CheckTriangle(x, y, z, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex + 
               w[1] * vertices[elements[i].vertex[1]].ex + 
               w[2] * vertices[elements[i].vertex[2]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey + 
               w[1] * vertices[elements[i].vertex[1]].ey + 
               w[2] * vertices[elements[i].vertex[2]].ey;
          ez = w[0] * vertices[elements[i].vertex[0]].ez + 
               w[1] * vertices[elements[i].vertex[1]].ez + 
               w[2] * vertices[elements[i].vertex[2]].ez;
          p  = w[0] * vertices[elements[i].vertex[0]].p + 
               w[1] * vertices[elements[i].vertex[1]].p + 
               w[2] * vertices[elements[i].vertex[2]].p;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5; 
          return;
        }
        break;    
      case 5:
        if (CheckTetrahedron(x, y, z, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex + 
               w[1] * vertices[elements[i].vertex[1]].ex + 
               w[2] * vertices[elements[i].vertex[2]].ex +
               w[3] * vertices[elements[i].vertex[3]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey + 
               w[1] * vertices[elements[i].vertex[1]].ey + 
               w[2] * vertices[elements[i].vertex[2]].ey +
               w[3] * vertices[elements[i].vertex[3]].ey;
          ez = w[0] * vertices[elements[i].vertex[0]].ez + 
               w[1] * vertices[elements[i].vertex[1]].ez + 
               w[2] * vertices[elements[i].vertex[2]].ez +
               w[3] * vertices[elements[i].vertex[3]].ez;
          p  = w[0] * vertices[elements[i].vertex[0]].p + 
               w[1] * vertices[elements[i].vertex[1]].p +
               w[2] * vertices[elements[i].vertex[2]].p +
               w[3] * vertices[elements[i].vertex[3]].p;               
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5; 
          return;
        }
        break;
      default:
        std::cerr << className << "::ElectricField:\n";
        std::cerr << "    Invalid element type (" 
                  << elements[i].type << ").\n";
        status = -11;
        return;
        break;
    }
  }
  // Point is outside the mesh.
  if (debug) {
    std::cerr << className << "::ElectricField:\n";
    std::cerr << "    Point (" << x << ", " << y << ", " << z
              << ") is outside the mesh.\n";
  }
  status = -6;
  return;

}

void 
ComponentTcad3d::ElectricField(const double x, const double y, const double z,
                               double& ex, double& ey, double& ez,
                               Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
  
}

bool 
ComponentTcad3d::GetMedium(const double x, const double y, const double z,
                           Medium*& m) {

  m = 0;
  // Make sure the field map has been loaded.
  if (!ready) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return false;
  }  
  
  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || 
      y < yMinBoundingBox || y > yMaxBoundingBox ||
      z < zMinBoundingBox || z > zMaxBoundingBox) {
    return false;
  }
  
  // Check if the point is still located in the previous element.
  int i = lastElement;
  switch (elements[i].type) {
    case 2:
      if (CheckTriangle(x, y, z, i)) {
        m = regions[elements[i].region].medium;
        if (m == 0) return false;
        return true;
      }
      break;  
    case 5:
      if (CheckTetrahedron(x, y, z, i)) {
        m = regions[elements[i].region].medium;
        if (m == 0) return false;
        return true;
      }
      break;  
    default: 
      std::cerr << className << "::GetMedium:\n";
      std::cerr << "    Invalid element type (" 
                << elements[i].type << ").\n";
      return false;
      break;
  }
  
  // The point is not in the previous element.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    switch (elements[i].type) {
      case 2:
        if (CheckTriangle(x, y, z, i)) {
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (m == 0) return false;
          return true;
        }
        break;
      case 5:
        if (CheckTetrahedron(x, y, z, i)) {
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (m == 0) return false;
          return true;
        }
        break;
      default:
        std::cerr << className << "::GetMedium:\n";
        std::cerr << "    Invalid element type (" 
                  << elements[i].type << ").\n";
        return false;
        break;
    }
  }
  // The point is outside the mesh.
  return false;

}
 
bool 
ComponentTcad3d::Initialise(const std::string gridfilename, 
                            const std::string datafilename) {
  
  ready = false;
  // Import mesh data from .grd file.
  if (!LoadGrid(gridfilename)) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    Importing mesh data failed.\n";
    return false;
  }
  
  // Import electric field and potential from .dat file.
  if (!LoadData(datafilename)) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    Importing electric field and potential failed.\n"; 
    return false;
  }
  
  // Find min./max. coordinates and potentials.
  xMaxBoundingBox = xMinBoundingBox = vertices[elements[0].vertex[0]].x;
  yMaxBoundingBox = yMinBoundingBox = vertices[elements[0].vertex[0]].y;  
  zMaxBoundingBox = zMinBoundingBox = vertices[elements[0].vertex[0]].z;
  pMax = pMin = vertices[elements[0].vertex[0]].p;
  for (int i = nElements; i--;) {
    for (int j = 0; j <= elements[i].type; ++j) {
      if (vertices[elements[i].vertex[j]].x < xMinBoundingBox) {
        xMinBoundingBox = vertices[elements[i].vertex[j]].x;
      } else if (vertices[elements[i].vertex[j]].x > xMaxBoundingBox) {
        xMaxBoundingBox = vertices[elements[i].vertex[j]].x;
      }
      if (vertices[elements[i].vertex[j]].y < yMinBoundingBox) {
        yMinBoundingBox = vertices[elements[i].vertex[j]].y;
      } else if (vertices[elements[i].vertex[j]].y > yMaxBoundingBox) {
        yMaxBoundingBox = vertices[elements[i].vertex[j]].y;
      }
      if (vertices[elements[i].vertex[j]].z < zMinBoundingBox) {
        zMinBoundingBox = vertices[elements[i].vertex[j]].z;
      } else if (vertices[elements[i].vertex[j]].z > zMaxBoundingBox) {
        zMaxBoundingBox = vertices[elements[i].vertex[j]].z;
      }
      if (vertices[elements[i].vertex[j]].p < pMin) {
        pMin = vertices[elements[i].vertex[j]].p;
      } else if (vertices[elements[i].vertex[j]].p > pMax) {
        pMax = vertices[elements[i].vertex[j]].p;
      }
    }
  }
  
  std::cout << className << "::Initialise:\n";
  std::cout << "    Bounding box:\n";
  std::cout << "      " << xMinBoundingBox << " < x [cm] < " 
                        << xMaxBoundingBox << "\n";
  std::cout << "      " << yMinBoundingBox << " < y [cm] < "
                        << yMaxBoundingBox << "\n";
  std::cout << "      " << zMinBoundingBox << " < z [cm] < "
                        << zMaxBoundingBox << "\n";
  std::cout << "    Voltage range:\n";
  std::cout << "      " << pMin << " < V < " << pMax << "\n";
  
  bool ok = true;
  
  // Count the number of elements belonging to a region.
  std::vector<int> nElementsRegion;
  nElementsRegion.resize(nRegions);
  for (int i = nRegions; i--;) nElementsRegion[i] = 0;
  
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
  
  for (int i = nElements; i--;) {
    if (elements[i].type == 2) {
      ++nTriangles;
      if (elements[i].vertex[0] == elements[i].vertex[1] ||
          elements[i].vertex[1] == elements[i].vertex[2] ||
          elements[i].vertex[2] == elements[i].vertex[0]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (elements[i].type == 5) {
      if (elements[i].vertex[0] == elements[i].vertex[1] ||
          elements[i].vertex[0] == elements[i].vertex[2] ||
          elements[i].vertex[0] == elements[i].vertex[3] ||
          elements[i].vertex[1] == elements[i].vertex[2] ||
          elements[i].vertex[1] == elements[i].vertex[3] ||
          elements[i].vertex[2] == elements[i].vertex[3]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
      ++nTetrahedra;
    } else {
      // Other shapes should not occur, since they were excluded in LoadGrid.
      ++nOtherShapes;
    }
    if (elements[i].region >= 0 && elements[i].region < nRegions) {
      ++nElementsRegion[elements[i].region];
    } else {
      looseElements.push_back(i);
      ++nLoose;
    }
  }
  
  if (nDegenerate > 0) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    The following elements are degenerate:\n";
    for (int i = nDegenerate; i--;) {
      std::cerr << "      " << degenerateElements[i] << "\n";
    }
    ok = false;
  }

  if (nLoose > 0) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    The following elements are not part of any region:\n";
    for (int i = nLoose; i--;) {
      std::cerr << "      " << looseElements[i] << "\n";
    }
    ok = false;
  }
  
  std::cout << className << "::Initialise:\n";
  std::cout << "    Number of regions: " << nRegions << "\n";
  for (int i = 0; i < nRegions; ++i) {
    std::cout << "      " << i << ": " << regions[i].name << ", "
              << nElementsRegion[i] << " elements\n";
  }
  
  std::cout << "    Number of elements: " << nElements << "\n";
  if (nTriangles > 0) {
    std::cout << "      " << nTriangles << " triangles\n";
  }
  if (nTetrahedra > 0) {
    std::cout << "      " << nTetrahedra << " tetrahedra\n";
  }
  if (nOtherShapes > 0) {
    std::cout << "      " << nOtherShapes << " elements of unknown type\n";
    std::cout << "      Program bug!\n";
    ready = false;
    Cleanup();
    return false;
  }
  if (debug) {
    // For each element, print the indices of the constituting vertices.
    for (int i = 0; i < nElements; ++i) {
      if (elements[i].type == 2) {
        std::cout << "      " << i << ": " 
                  << elements[i].vertex[0] << "  "
                  << elements[i].vertex[1] << "  "
                  << elements[i].vertex[2] 
                  << " (triangle, region " << elements[i].region << ")\n";
      } else if (elements[i].type == 5) {
        std::cout << "      " << i << ": "
                  << elements[i].vertex[0] << "  " 
                  << elements[i].vertex[1] << "  "
                  << elements[i].vertex[2] << "  "
                  << elements[i].vertex[3] 
                  << " (tetrahedron, region " << elements[i].region << ")\n";
      }
    }
  }
  
  std::cout << "    Number of vertices: " << nVertices << "\n";
  if (debug) {
    for (int i = 0; i < nVertices; ++i) {
      std::cout << "      " << i << ": (x, y, z) = (" 
                << vertices[i].x << ", " 
                << vertices[i].y << ", "
                << vertices[i].z << "), V = " << vertices[i].p << "\n";
    }
  }
  
  if (!ok) {
    ready = false;
    Cleanup();
    return false;
  }
  
  ready = true;
  return true;

}

bool 
ComponentTcad3d::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                double& xmax, double& ymax, double& zmax) {

  if (!ready) return false;
  xmin = xMinBoundingBox; ymin = yMinBoundingBox; zmin = zMinBoundingBox;
  xmax = xMaxBoundingBox; ymax = yMaxBoundingBox; zmax = zMaxBoundingBox;
  return true;

}

bool 
ComponentTcad3d::GetVoltageRange(double& vmin, double& vmax) {

  if (!ready) return false;
  vmin = pMin; vmax = pMax;
  return true;
  
}

void 
ComponentTcad3d::GetRegion(const int i, std::string& name, bool& active) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::GetRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }  
  name = regions[i].name;
  active = regions[i].drift;

}

void 
ComponentTcad3d::SetDriftRegion(const int i) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::SetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  regions[i].drift = true;

}

void
ComponentTcad3d::UnsetDriftRegion(const int i) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::UnsetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  regions[i].drift = false;
  
}

void 
ComponentTcad3d::SetMedium(const int i, Medium* medium) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  
  if (medium == 0) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }
  
  regions[i].medium = medium;
  
}

bool
ComponentTcad3d::GetMedium(const int i, Medium*& m) const {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return false;
  }
  
  m = regions[i].medium;
  if (m == 0) return false;
  return true;

}

bool 
ComponentTcad3d::LoadData(const std::string datafilename) {

  std::ifstream datafile;
  datafile.open(datafilename.c_str(), std::ios::in);
  if (!datafile) {
    std::cerr << className << "::LoadData:\n";
    std::cerr << "    Could not open file " << datafilename << ".\n";
    return false;
  }
  
  std::string line;
  std::istringstream data;
  
  std::vector<bool> isInRegion(nVertices);
  std::vector<int> fillCount(nVertices);
  
  for (int i = nVertices; i--;) {
    fillCount[i] = 0;
    vertices[i].p = 0.;
    vertices[i].ex = 0.;
    vertices[i].ey = 0.;
    vertices[i].ez = 0.;
    vertices[i].isShared = false;
  }
  
  std::string::size_type pBra, pKet, pEq;

  while (!datafile.fail()) {
    // Read one line.
    std::getline(datafile, line);
    // Strip white space from beginning of line.
    line.erase(line.begin(), std::find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find data section.
    if (line.substr(0, 8) == "function") {
      // Read type of data set.
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" found.
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading file " << datafilename << ".\n";
        std::cerr << "    Line:\n";
        std::cerr << "    " << line << "\n";
        datafile.close();
        Cleanup();  
        return false;
      }
      line = line.substr(pEq + 1);
      std::string dataset;
      data.str(line); data >> dataset; data.clear();
      if (dataset == "ElectrostaticPotential") {
        std::getline(datafile, line); std::getline(datafile, line);
        std::getline(datafile, line); std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('['); pKet = line.find(']');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name; 
        data.str(line); data >> name; data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('('); pKet = line.find(')');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close(); Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line); data >> nValues; data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          } 
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of potential values.
          if (ivertex >= nVertices) {
            std::cerr << className << "::LoadData:\n";
            std::cerr << "    Error reading file " << datafilename << "\n";
            std::cerr << "    Dataset has more values than "
                      << "there are vertices in region " 
                      << name << "\n";
            datafile.close(); Cleanup();
            return false;
          }
          vertices[ivertex].p = val;
          ++fillCount[ivertex];
          ++ivertex;
        }
      } else if (dataset == "ElectricField") {
        // Same procedure as for the potential.
        std::getline(datafile, line); std::getline(datafile, line);
        std::getline(datafile, line); std::getline(datafile, line);
        pBra = line.find('['); pKet = line.find(']');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << ".\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close(); Cleanup();          
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name; 
        data.str(line); data >> name; data.clear();
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        std::getline(datafile, line);
        pBra = line.find('('); pKet = line.find(')');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close(); Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line); data >> nValues; data.clear();
        // In case of the electric field, there are three values per vertex.
        nValues = nValues / 3;
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          } 
        }
        int ivertex = 0;
        double val1, val2, val3;
        for (int j = 0; j < nValues; ++j) {
          datafile >> val1 >> val2 >> val3;
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          if (ivertex >= nVertices) {
            std::cerr << className << "::LoadData\n"
                      << "    Error reading file " << datafilename << "\n"
                      << "    Dataset has more values than" 
                      << " there are vertices in region " 
                      << name << ".\n";
            datafile.close(); Cleanup();
            return false;
          }
          vertices[ivertex].ex = val1;
          vertices[ivertex].ey = val2;
          vertices[ivertex].ez = val3;
          ++ivertex;
        }
      }
    }
  }
  if (datafile.fail() && !datafile.eof()) {
    std::cerr << className << "::LoadData\n";
    std::cerr << "    Error reading file " << datafilename << "\n";
    datafile.close(); Cleanup();
    return false;
  }
  
  for (int i = nVertices; i--;) {
    if (fillCount[i] > 1) vertices[i].isShared = true;
  }
    
  datafile.close();

  return true;
  
}

bool 
ComponentTcad3d::LoadGrid(const std::string gridfilename) {

  // Open the file containing the mesh description.
  std::ifstream gridfile;
  gridfile.open(gridfilename.c_str(), std::ios::in);
  if (!gridfile) {
    std::cerr << className << "::LoadGrid:\n";
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
    std::getline(gridfile, line); ++iLine;
    // Strip white space from the beginning of the line.
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'nb_regions'.
    if (line.substr(0, 10) == "nb_regions") {
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" sign found.
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of regions.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pEq + 1);
      data.str(line); data >> nRegions; data.clear();
      break;
    }
    if (gridfile.fail()) break;
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'nb_regions' in file\n"; 
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  regions.resize(nRegions);
  for (int j = nRegions; j--;) {
    regions[j].name = "";
    regions[j].drift = false;
    regions[j].medium = 0;
  }
  
  if (debug) {
    std::cout << className << "::LoadGrid:\n";
    std::cout << "    Found " << nRegions << " regions.\n";
  }

  // Get the region names.
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'regions'.
    if (line.substr(0, 7) == "regions") {
      // Get region names (given in brackets).
      pBra = line.find('['); pKet = line.find(']');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read region names.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      for (int j = 0; j < nRegions; ++j) {
        data >> regions[j].name;
        data.clear();
        // Assume by default that all regions are active.
        regions[j].drift = true;
        regions[j].medium = 0;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'regions' in file\n"; 
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();        
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
    Cleanup();        
    gridfile.close();
    return false;
  }
    
  // Get the vertices.
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Vertices'.
    if (line.substr(0, 8) == "Vertices") {
      // Get number of vertices (given in brackets).
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of vertices.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nVertices; data.clear();
      vertices.resize(nVertices);
      // Get the coordinates of this vertex.
      for (int j = 0; j < nVertices; ++j) {
        gridfile >> vertices[j].x >> vertices[j].y >> vertices[j].z;
        // Change units from micron to cm.
        vertices[j].x *= 1.e-4;
        vertices[j].y *= 1.e-4;
        vertices[j].z *= 1.e-4;
      }
      iLine += nVertices - 1;
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Vertices' in file\n"; 
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();        
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
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
    std::getline(gridfile, line); ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Edges'.
    if (line.substr(0, 5) == "Edges") {
      // Get the number of edges (given in brackets).
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of edges.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nEdges; data.clear();
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
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Edges' in file\n"; 
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  
  for (int i = nEdges; i--;) {
    // Make sure the indices of the edge endpoints are not out of range.
    if (edgeP1[i] < 0 || edgeP1[i] >= nVertices ||
        edgeP2[i] < 0 || edgeP2[i] >= nVertices) {
      std::cerr << className << "::LoadGrid:\n";
      std::cerr << "    Vertex index of edge " << i 
                << " out of range.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    // Make sure the edge is non-degenerate.
    if (edgeP1[i] == edgeP2[i]) {
      std::cerr << className << "::LoadGrid:\n";
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
    std::getline(gridfile, line); ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Faces'.
    if (line.substr(0, 5) == "Faces") {
      // Get the number of faces (given in brackets).
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of faces.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nFaces; data.clear();
      faces.resize(nFaces);
      // Get the indices of the edges constituting this face.
      for (int j = 0; j < nFaces; ++j) {
        gridfile >> faces[j].type;
        if (faces[j].type != 3 && faces[j].type != 4) {
          std::cerr << className << "::LoadGrid:\n";
          std::cerr << "    Face with index " << j 
                    << " has invalid number of edges (" 
                    << faces[j].type << ").\n";
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
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Faces' in file\n"; 
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }  

  // Get the elements.
  int edge0, edge1, edge2;
  int face0, face1, face2, face3;
  int type;
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Elements'.
    if (line.substr(0, 8) == "Elements") {
      // Get number of elements (given in brackets).
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of elements.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nElements; data.clear();
      // Resize array of elements.
      elements.resize(nElements);
      // Get type and constituting edges of each element.
      for (int j = 0; j < nElements; ++j) {
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
          if (edge0 >= nEdges || -edge0 - 1 >= nEdges ||
              edge1 >= nEdges || -edge1 - 1 >= nEdges ||
              edge2 >= nEdges || -edge2 - 1 >= nEdges) {
            std::cerr << className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename
                      << " (line " << iLine << ").\n";
            std::cerr << "    Edge index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          elements[j].vertex[0] = edgeP1[edge0];
          elements[j].vertex[1] = edgeP2[edge0];
          if (edgeP1[edge1] != elements[j].vertex[0] &&
              edgeP1[edge1] != elements[j].vertex[1]) {
            elements[j].vertex[2] = edgeP1[edge1];
          } else {
            elements[j].vertex[2] = edgeP2[edge1];
          }
        } else if (type == 5) {
          // Tetrahedron
          // Get the faces.
          // Negative face index means that the sequence of the edges
          // is supposed to be inverted.
          // For our purposes, the orientation does not matter. 
          gridfile >> face0 >> face1 >> face2 >> face3;
          // Make sure the face indices are not out of range.
          if (face0 >= nFaces || -face0 - 1 >= nFaces ||
              face1 >= nFaces || -face1 - 1 >= nFaces ||
              face2 >= nFaces || -face2 - 1 >= nFaces ||
              face3 >= nFaces || -face3 - 1 >= nFaces) {
            std::cerr << className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename
                      << " (line " << iLine << ").\n";
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
            std::cerr << className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << "\n";
            std::cerr << "    Edge index in element " << j 
                      << " out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          // Get the first three vertices.
          elements[j].vertex[0] = edgeP1[edge0];
          elements[j].vertex[1] = edgeP2[edge0];
          if (edgeP1[edge1] != elements[j].vertex[0] &&
              edgeP1[edge1] != elements[j].vertex[1]) {
            elements[j].vertex[2] = edgeP1[edge1];
          } else {
            elements[j].vertex[2] = edgeP2[edge1];
          }
          // Get the fourth vertex from face 1.
          edge0 = faces[face1].edge[0];
          edge1 = faces[face1].edge[1];
          edge2 = faces[face1].edge[2];
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          if (edge2 < 0) edge2 = -edge2 - 1;
          if (edgeP1[edge0] != elements[j].vertex[0] &&
              edgeP1[edge0] != elements[j].vertex[1] &&
              edgeP1[edge0] != elements[j].vertex[2]) {
            elements[j].vertex[3] = edgeP1[edge0];
          } else if (edgeP2[edge0] != elements[j].vertex[0] &&
                     edgeP2[edge0] != elements[j].vertex[1] &&
                     edgeP2[edge0] != elements[j].vertex[2]) {
            elements[j].vertex[3] = edgeP2[edge0];
          } else if (edgeP1[edge1] != elements[j].vertex[0] &&
                     edgeP1[edge1] != elements[j].vertex[1] &&
                     edgeP1[edge1] != elements[j].vertex[2]) {
            elements[j].vertex[3] = edgeP1[edge1];
          } else if (edgeP2[edge1] != elements[j].vertex[0] &&
                     edgeP2[edge1] != elements[j].vertex[1] &&
                     edgeP2[edge1] != elements[j].vertex[2]) {
            elements[j].vertex[3] = edgeP2[edge1];
          } else {
            std::cerr << className << "::LoadGrid:\n";
            std::cerr << "    Error reading file " << gridfilename << "\n";
            std::cerr << "    Face 1 of element " << j 
                      << " is degenerate.\n";
            Cleanup();
            gridfile.close();
            return false;
          }            
        } else {
          // Other element types are not allowed.
          std::cerr << className << "::LoadGrid:\n" 
                    << "    Error reading file " << gridfilename 
                    << " (line " << iLine << ").\n";
          if (type == 0 || type == 1) {
            std::cerr << "    Invalid element type (" 
                      << type << ") for 3d mesh.\n";
          } else {
            std::cerr << "    Element type " << type 
                      << " is not supported.\n";
            std::cerr << "    Remesh with option -t to create only"
                      << " triangles and tetrahedra.\n";
          }
          Cleanup();        
          gridfile.close();
          return false;
        }
        elements[j].type = type;
        elements[j].region = -1;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Elements' in file\n"; 
    std::cerr << "    " <<  gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << iLine << ").\n";
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
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read region name.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> name; data.clear();
      int index = -1;
      for (int j = 0; j < nRegions; ++j) {
        if (name == regions[j].name) {
          index = j;
          break;
        }
      }
      if (index == -1) {
        // Specified region name is not in the list.
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Unknown region " << name << ".\n";
        continue;
      }
      std::getline(gridfile, line); std::getline(gridfile, line);
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Could not read number of elements in region " 
                  << name << ".\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      int nElementsRegion;
      int iElement;
      data.str(line); data >> nElementsRegion; data.clear();
      for (int j = 0; j < nElementsRegion; ++j) {
        gridfile >> iElement;
        elements[iElement].region = index;
      } 
    }
  }
  
  gridfile.close();  
  if (gridfile.fail() && !gridfile.eof()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << ".\n";
    Cleanup();
    return false;
  }
      
  return true;
  
}

void 
ComponentTcad3d::Cleanup() {

  // Vertices
  vertices.clear();
  nVertices = 0;
  
  // Elements
  elements.clear();
  nElements = 0;
  
  // Regions
  regions.clear();
  nRegions = 0;
  
}

bool
ComponentTcad3d::CheckTetrahedron(const double x, const double y, 
                                  const double z, const int i) {

  w[0] = w[1] = w[2] = w[3] = 0.;
  
  const double x10 = vertices[elements[i].vertex[1]].x -
                     vertices[elements[i].vertex[0]].x;
  const double y10 = vertices[elements[i].vertex[1]].y -
                     vertices[elements[i].vertex[0]].y;
  const double z10 = vertices[elements[i].vertex[1]].z -
                     vertices[elements[i].vertex[0]].z;
                     
  const double x20 = vertices[elements[i].vertex[2]].x -
                     vertices[elements[i].vertex[0]].x;
  const double y20 = vertices[elements[i].vertex[2]].y -
                     vertices[elements[i].vertex[0]].y;
  const double z20 = vertices[elements[i].vertex[2]].z -
                     vertices[elements[i].vertex[0]].z;
                     
  const double x30 = vertices[elements[i].vertex[3]].x -
                     vertices[elements[i].vertex[0]].x;
  const double y30 = vertices[elements[i].vertex[3]].y -
                     vertices[elements[i].vertex[0]].y;
  const double z30 = vertices[elements[i].vertex[3]].z -
                     vertices[elements[i].vertex[0]].z;

  const double x21 = vertices[elements[i].vertex[2]].x -
                     vertices[elements[i].vertex[1]].x;
  const double y21 = vertices[elements[i].vertex[2]].y -
                     vertices[elements[i].vertex[1]].y;
  const double z21 = vertices[elements[i].vertex[2]].z -
                     vertices[elements[i].vertex[1]].z;
  
  const double x31 = vertices[elements[i].vertex[3]].x -
                     vertices[elements[i].vertex[1]].x;
  const double y31 = vertices[elements[i].vertex[3]].y -
                     vertices[elements[i].vertex[1]].y;
  const double z31 = vertices[elements[i].vertex[3]].z -
                     vertices[elements[i].vertex[1]].z;
                     
  const double x32 = vertices[elements[i].vertex[3]].x -
                     vertices[elements[i].vertex[2]].x;
  const double y32 = vertices[elements[i].vertex[3]].y -
                     vertices[elements[i].vertex[2]].y;
  const double z32 = vertices[elements[i].vertex[3]].z -
                     vertices[elements[i].vertex[2]].z;

  w[0] = (x - vertices[elements[i].vertex[1]].x) * (y21 * z31 - y31 * z21) +
         (y - vertices[elements[i].vertex[1]].y) * (z21 * x31 - z31 * x21) +
         (z - vertices[elements[i].vertex[1]].z) * (x21 * y31 - x31 * y21);

  w[0] /= x10 * (y31 * z21 - y21 * z31) +
          y10 * (z31 * x21 - z21 * x31) +
          z10 * (x31 * y21 - x21 * y31);

  if (w[0] < 0.) return false;
              
  w[1] = (x - vertices[elements[i].vertex[2]].x) * (-y20 * z32 + y32 * z20) +
         (y - vertices[elements[i].vertex[2]].y) * (-z20 * x32 + z32 * x20) +
         (z - vertices[elements[i].vertex[2]].z) * (-x20 * y32 + x32 * y20);
  
  w[1] /= x21 * (y20 * z32 - y32 * z20) +
          y21 * (z20 * x32 - z32 * x20) +
          z21 * (x20 * y32 - x32 * y20);
  
  if (w[1] < 0.) return false;
  
  w[2] = (x - vertices[elements[i].vertex[3]].x) * (y30 * z31 - y31 * z30) +
         (y - vertices[elements[i].vertex[3]].y) * (z30 * x31 - z31 * x30) +
         (z - vertices[elements[i].vertex[3]].z) * (x30 * y31 - x31 * y30);
  
  w[2] /= x32 * (y31 * z30 - y30 * z31) +
          y32 * (z31 * x30 - z30 * x31) + 
          z32 * (x31 * y30 - x30 * y31);
      
  if (w[2] < 0.) return false;
  
  w[3] = (x - vertices[elements[i].vertex[0]].x) * (y20 * z10 - y10 * z20) +
         (y - vertices[elements[i].vertex[0]].y) * (z20 * x10 - z10 * x20) +
         (z - vertices[elements[i].vertex[0]].z) * (x20 * y10 - x10 * y20);
  
  w[3] /= x30 * (y20 * z10 - y10 * z20) +
          y30 * (z20 * x10 - z10 * x20) +
          z30 * (x20 * y10 - x10 * y20);
  
  if (w[3] < 0.) return false;
                 
  if (debug) {
    // Reconstruct the point from the local coordinates.
    const double xr = w[0] * vertices[elements[i].vertex[0]].x +
                      w[1] * vertices[elements[i].vertex[1]].x +
                      w[2] * vertices[elements[i].vertex[2]].x +
                      w[3] * vertices[elements[i].vertex[3]].x;
    const double yr = w[0] * vertices[elements[i].vertex[0]].y +
                      w[1] * vertices[elements[i].vertex[1]].y +
                      w[2] * vertices[elements[i].vertex[2]].y +
                      w[3] * vertices[elements[i].vertex[3]].y;
    const double zr = w[0] * vertices[elements[i].vertex[0]].z +
                      w[1] * vertices[elements[i].vertex[1]].z +
                      w[2] * vertices[elements[i].vertex[2]].z +
                      w[3] * vertices[elements[i].vertex[3]].z;
    std::cout << className << "::CheckTetrahedron:\n";
    std::cout << "    Original coordinates:      ("
              << x << ", " << y << ", " << z << ")\n";
    std::cout << "    Local coordinates:         ("
              << w[0] << ", " << w[1] << ", " 
              << w[2] << ", " << w[3] << ")\n";
    std::cerr << "    Reconstructed coordinates: (" 
              << xr << ", " << yr << ", " << zr << ")\n";
    std::cerr << "    Checksum: " << w[0] + w[1] + w[2] + w[3] - 1. << "\n";
  }
  
  return true;
  
}

bool 
ComponentTcad3d::CheckTriangle(const double x, 
                               const double y, const double z, const int i) {

  const double v1x = vertices[elements[i].vertex[1]].x -
                     vertices[elements[i].vertex[0]].x;
  const double v2x = vertices[elements[i].vertex[2]].x -
                     vertices[elements[i].vertex[0]].x;
  const double v1y = vertices[elements[i].vertex[1]].y -
                     vertices[elements[i].vertex[0]].y;
  const double v2y = vertices[elements[i].vertex[2]].y -
                     vertices[elements[i].vertex[0]].y;
  const double v1z = vertices[elements[i].vertex[1]].z -
                     vertices[elements[i].vertex[0]].z;
  const double v2z = vertices[elements[i].vertex[2]].z -
                     vertices[elements[i].vertex[0]].z;
    
  // Check whether the point lies in the plane of the triangle.
  // Compute the coefficients of the plane equation.
  const double a = v1y * v2z - v2y * v1z;
  const double b = v1z * v2x - v2z * v1x;
  const double c = v1x * v2y - v2x * v1y;
  const double d = a * vertices[elements[i].vertex[0]].x +
                   b * vertices[elements[i].vertex[0]].y +
                   c * vertices[elements[i].vertex[0]].z;
  // Check if the point satisfies the plane equation.
  if (a * x + b * y + c * z != d) return false;
  
  // Map (x, y) onto local variables (b, c) such that
  // P = A + b * (B - A) + c * (C - A)
  // A point P is inside the triangle ABC if b, c > 0 and b + c < 1;
  // b, c are also weighting factors for points B, C

  w[1] = ((x - vertices[elements[i].vertex[0]].x) * v2y - 
          (y - vertices[elements[i].vertex[0]].y) * v2x) / (v1x * v2y - v1y * v2x);
  if (w[1] < 0. || w[1] > 1.) return false;
  
  w[2] = ((vertices[elements[i].vertex[0]].x - x) * v1y - 
          (vertices[elements[i].vertex[0]].y - y) * v1x) / (v1x * v2y - v1y * v2x);
  if (w[2] < 0. || w[1] + w[2] > 1.) return false;
  
  // Weighting factor for point A
  w[0] = 1. - w[1] - w[2];
  
  return true;

}

void
ComponentTcad3d::Reset() {

  Cleanup(); 
  ready = false;
  
}

void 
ComponentTcad3d::UpdatePeriodicity() {

  if (debug) {
    std::cerr << className << "::UpdatePeriodicity\n:";
    std::cerr << "    Periodicities are not supported.\n";
  }
  
}

}
